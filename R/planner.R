# =============================================================================
# planner.R -- the intelligence layer
# -----------------------------------------------------------------------------
# Input: a topic string. Output: a full deliberation design (dimensions,
# expert roles, reasoning styles, debate questions, recommended mode and
# moderator, expected agreements/controversies, required evidence, agent
# count). The planner uses an LLM with the config vocabulary injected; if the
# LLM call or JSON parse fails it falls back to a transparent keyword heuristic
# so the app never dead-ends. Nothing here is topic-specific.
# =============================================================================

# Normalize a planner LLM result into the canonical plan shape, coercing types
# and clamping the agent count to a sane range.
.normalize_plan <- function(parsed, cfg) {
  as_chr_list <- function(x) if (is.null(x)) list() else as.list(unlist(x, use.names = FALSE))
  dims <- lapply(parsed$dimensions %||% list(), function(d) {
    list(name = as.character(d$name %||% "Uncategorized"),
         importance = suppressWarnings(as.numeric(d$importance %||% 0.6)),
         why = as.character(d$why %||% ""))
  })
  experts <- lapply(parsed$experts %||% list(), function(e) {
    list(role = as.character(e$role %||% "Generalist Facilitator"),
         reasoning = as.character(e$reasoning %||% ""),
         evidence = as.character(e$evidence %||% ""),
         why = as.character(e$why %||% ""))
  })
  n <- suppressWarnings(as.integer(round(as.numeric(parsed$recommended_num_agents %||% length(experts) %||% 3))))
  if (is.na(n) || n < 2) n <- max(2L, length(experts))
  n <- min(n, 20L)
  list(
    dimensions = dims,
    experts = experts,
    debate_questions = as_chr_list(parsed$debate_questions),
    recommended_mode = as.character(parsed$recommended_mode %||% "Round Table"),
    recommended_moderator = as.character(parsed$recommended_moderator %||% "Neutral"),
    expected_agreements = as_chr_list(parsed$expected_agreements),
    expected_controversies = as_chr_list(parsed$expected_controversies),
    required_evidence = as_chr_list(parsed$required_evidence),
    recommended_num_agents = n,
    rationale = as.character(parsed$rationale %||% ""),
    source = "llm"
  )
}

# Transparent, LLM-free fallback. Picks dimensions by keyword hits in the
# topic (defaulting to the highest-importance dimensions), derives experts
# from those dimensions' recommended categories, and makes conservative
# choices for mode/moderator. Clearly labeled source="heuristic".
planner_heuristic <- function(topic, cfg, n_agents_hint = NULL) {
  topic_l <- tolower(topic)
  score_dim <- function(d) {
    kws <- tolower(c(d$name, strsplit(d$description %||% "", "[ ,]+")[[1]]))
    hits <- sum(vapply(kws, function(k) if (nchar(k) > 3 && grepl(k, topic_l, fixed = TRUE)) 1 else 0, numeric(1)))
    (d$importance %||% 0.6) + 0.5 * hits
  }
  scored <- vapply(cfg$dimensions, score_dim, numeric(1))
  ord <- order(scored, decreasing = TRUE)
  chosen <- cfg$dimensions[utils::head(ord, 6)]
  dims <- lapply(chosen, function(d) list(name = d$name, importance = d$importance %||% 0.6,
                                          why = "Selected by keyword/importance heuristic."))
  cats <- unique(unlist(lapply(chosen, function(d) unlist(d$recommended_categories))))
  cats <- cats[!is.na(cats)]
  role_pool <- Filter(function(r) r$category %in% cats, cfg$roles)
  if (length(role_pool) == 0) role_pool <- cfg$roles
  n <- n_agents_hint %||% max(2L, min(4L, length(role_pool)))
  pick <- role_pool[utils::head(seq_along(role_pool), n)]
  experts <- lapply(pick, function(r) list(role = r$name, reasoning = r$reasoning %||% "",
                                           evidence = r$evidence %||% "",
                                           why = paste("Covers the", r$category, "dimension.")))
  list(
    dimensions = dims, experts = experts,
    debate_questions = lapply(chosen, function(d) (d$questions %||% list("What matters most here?"))[[1]]),
    recommended_mode = "Panel Discussion", recommended_moderator = "Neutral",
    expected_agreements = list("The topic is consequential and multi-dimensional."),
    expected_controversies = list("How to weigh competing dimensions against each other."),
    required_evidence = unique(unlist(lapply(pick, function(r) r$evidence))),
    recommended_num_agents = length(experts), rationale = "Heuristic fallback (LLM planner unavailable).",
    source = "heuristic"
  )
}

# Main entry. Tries the LLM planner; falls back to the heuristic on any
# failure. reasoning_effort is deliberately NULL: this is a structured-JSON
# call and reasoning tokens would starve the JSON output budget.
run_planner <- function(topic, cfg, provider_id, api_key, n_agents_hint = NULL, use_cache = TRUE,
                        problem_details = NULL) {
  if (is.null(topic) || nchar(trimws(topic)) == 0) {
    return(list(ok = FALSE, error = "Empty topic.", plan = NULL))
  }
  msgs <- build_planner_messages(topic, cfg, n_agents_hint, problem_details = problem_details)
  # 4000 tokens + a self-correcting retry (via llm_json): the plan JSON is large
  # and TRUNCATION is the most common cause of an unparseable reply.
  r <- llm_json(cfg, provider_id, msgs, api_key, max_tokens = 4000, temperature = 0.4,
                use_cache = use_cache)
  meta <- list(usage = r$usage, model = r$model, cached = r$cached)
  if (!isTRUE(r$ok)) {
    return(c(list(ok = TRUE, error = paste("LLM planner failed, used heuristic:", r$error),
                  plan = planner_heuristic(topic, cfg, n_agents_hint)), meta))
  }
  if (is.null(r$parsed)) {
    return(c(list(ok = TRUE,
                  error = paste0("Planner returned non-JSON after a retry; used heuristic. Model output began: ",
                                 json_snippet(r$text)),
                  plan = planner_heuristic(topic, cfg, n_agents_hint)), meta))
  }
  c(list(ok = TRUE, error = NULL, plan = .normalize_plan(parsed = r$parsed, cfg)), meta)
}
