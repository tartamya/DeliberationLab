# =============================================================================
# moderator.R -- per-round moderation + knowledge-graph extraction
# -----------------------------------------------------------------------------
# One structured-JSON LLM call per round (not per metric) keeps cost/latency
# bounded as rounds scale. The moderator's PERSONALITY comes from config
# (moderators.json -> style_prompt) but the OUTPUT SCHEMA is always the same,
# so downstream code (KG, analytics) doesn't care which moderator ran.
# Falls back to a base-R heuristic if the model doesn't return valid JSON, so
# a round never silently produces an empty graph.
# =============================================================================

# reasoning_effort is deliberately NULL here: structured-JSON call, reasoning
# tokens would starve the output budget and truncate the JSON.
moderator_call <- function(cfg, topic, round_texts, round_number, provider_id, api_key,
                           mode_name, moderator_name = "Neutral", use_cache = FALSE) {
  mod_cfg <- cfg_find(cfg$moderators, moderator_name)
  style <- if (!is.null(mod_cfg)) mod_cfg$style_prompt else "You are a neutral moderator."
  prompt <- paste0(
    style, "\n",
    "You are moderating a multi-agent discussion on: \"", topic, "\".\n",
    "Below are this round's contributions (round ", round_number, ", mode: ", mode_name, ").\n\n",
    paste(vapply(names(round_texts), function(n) paste0(n, ": ", round_texts[[n]]), character(1)),
          collapse = "\n\n"),
    "\n\nRespond with ONLY a single JSON object (no prose, no markdown fences) with this schema:\n",
    '{"agreements": [string], "disagreements": [string], "evidence_used": [string], ',
    '"fallacies": [string], "new_hypotheses": [string], "uncertainty": string, ',
    '"group_confidence_pct": number, ',
    '"nodes": [{"id": string, "type": "Idea|Claim|Evidence|Question|Assumption|Counterargument", "label": string}], ',
    '"edges": [{"from": string, "to": string, "relation": "Supports|Contradicts|DependsOn|Extends|Refines|Rejects"}]}'
  )
  # 3500 gives the schema headroom once several agents are in a round.
  # 4000 tokens + a self-correcting retry (via llm_json): the nodes/edges JSON
  # grows with the number of agents, so truncation is the usual cause of an
  # unparseable reply.
  r <- llm_json(cfg, provider_id, list(list(role = "user", content = prompt)),
                api_key, max_tokens = 4000, temperature = 0.2, use_cache = use_cache)
  meta <- list(usage = r$usage, model = r$model, cached = r$cached)
  if (!isTRUE(r$ok)) {
    return(c(list(ok = FALSE, error = r$error, data = heuristic_round_summary(round_texts)), meta))
  }
  if (is.null(r$parsed)) {
    return(c(list(ok = FALSE,
                  error = paste0("Moderator returned non-JSON after a retry; used heuristic. Output began: ",
                                 json_snippet(r$text)),
                  data = heuristic_round_summary(round_texts)), meta))
  }
  c(list(ok = TRUE, error = NULL, data = r$parsed), meta)
}

# Pure base-R fallback so a round never fails outright. Synthesizes one Claim
# node per non-empty turn (label = the turn's first sentence) so the graph is
# never empty when the conversation isn't.
heuristic_round_summary <- function(round_texts) {
  all_txt <- paste(unlist(round_texts), collapse = " ")
  conf_matches <- regmatches(all_txt, gregexpr("Confidence:\\s*([0-9]{1,3})%", all_txt, ignore.case = TRUE))[[1]]
  confs <- as.numeric(gsub("[^0-9]", "", conf_matches))
  nm <- names(round_texts)
  nodes <- lapply(seq_along(round_texts), function(i) {
    txt <- round_texts[[i]]
    if (is.null(txt) || length(txt) == 0 || is.na(txt) ||
        nchar(trimws(txt)) == 0 || grepl("^\\[ERROR", trimws(txt))) return(NULL)
    flat <- gsub("\\s+", " ", substr(txt, 1, 400))
    first_sentence <- trimws(sub("([.!?]).*$", "\\1", flat))
    label <- if (nchar(first_sentence) > 0) first_sentence else trimws(substr(txt, 1, 160))
    label <- substr(label, 1, 160)
    agent_nm <- if (!is.null(nm) && length(nm) >= i && nzchar(nm[i])) nm[i] else paste0("agent", i)
    list(id = paste0("turn_", gsub("[^A-Za-z0-9]", "_", agent_nm)), type = "Claim", label = label)
  })
  nodes <- Filter(Negate(is.null), nodes)
  list(agreements = list(), disagreements = list(), evidence_used = list(),
       fallacies = list(), new_hypotheses = list(),
       uncertainty = "Not machine-extracted this round (heuristic fallback).",
       group_confidence_pct = if (length(confs) > 0) mean(confs) else 50,
       nodes = nodes, edges = list())
}
