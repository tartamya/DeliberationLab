# =============================================================================
# debate_engine.R -- turn execution, phase protocol, and analytics
# -----------------------------------------------------------------------------
# Pure (reactive-free) helpers used by the server's round loop. The server
# owns the later::later() round-stepping (it needs the reactive session);
# everything that can be a plain function lives here so it is testable and
# reusable:
#   - select_phase()      : map a round to a debate-mode phase instruction
#   - run_turn()          : execute one agent turn against a provider
#   - compute_analytics() : per-turn novelty/agreement/drift/confidence
#   - auto_stop_reached() : novelty-plateau stopping rule
# Concurrency model: cooperative sequential (agents speak in order, each sees
# prior turns). The server yields to Shiny between rounds via later::later().
# =============================================================================

# ---- Phase selection --------------------------------------------------------
# Ladder (see debate_modes.json _comment): round 1 -> opening; final round ->
# closing; the round before final -> revision else reflection; other rounds
# alternate response / cross_examination. Missing phases fall back sensibly.
select_phase <- function(mode_cfg, round_number, n_rounds, agent_index = 1) {
  # Ordered stage protocols (e.g. Narrative Forge's myth-engineering loop):
  # `phase_sequence` is an array of {name, instruction} stages traversed in
  # order. Rounds are mapped linearly onto the sequence -- round 1 is always
  # the first stage and the final round the last, so a shorter run compresses
  # the loop (skipping middles) rather than truncating its ending, and a longer
  # run revisits stages. Takes precedence over the generic `phases` ladder.
  seq_ph <- mode_cfg$phase_sequence %||% list()
  if (length(seq_ph) > 0) {
    len <- length(seq_ph)
    idx <- if (n_rounds <= 1) 1 else 1 + round((round_number - 1) * (len - 1) / (n_rounds - 1))
    idx <- max(1L, min(len, as.integer(idx)))
    st <- seq_ph[[idx]]
    return(paste0("[Stage ", idx, "/", len, " -- ", st$name %||% "", "] ", st$instruction %||% ""))
  }
  phases <- mode_cfg$phases %||% list()
  get_phase <- function(nm) if (!is.null(phases[[nm]]) && nzchar(phases[[nm]])) phases[[nm]] else NULL
  fallback <- function() get_phase("response") %||% get_phase("opening") %||%
    "Give your perspective on the topic, informed by the discussion so far."

  if (round_number <= 1) return(get_phase("opening") %||% fallback())
  if (round_number >= n_rounds) return(get_phase("closing") %||% fallback())
  if (round_number == n_rounds - 1) {
    p <- get_phase("revision") %||% get_phase("reflection")
    if (!is.null(p)) return(p)
  }
  # Alternate response / cross_examination across the "middle" rounds, offset
  # by agent index so a round isn't monotone when several agents speak.
  pick_two <- c(get_phase("response"), get_phase("cross_examination"))
  pick_two <- Filter(Negate(is.null), pick_two)
  if (length(pick_two) == 0) return(fallback())
  pick_two[[((round_number + agent_index) %% length(pick_two)) + 1]]
}

# ---- One agent turn ---------------------------------------------------------
# Builds the messages and calls the provider. Returns a turn record; on API
# failure the text is an "[ERROR: ...]" placeholder so the run continues and
# the failure is visible in the transcript rather than crashing the round.
run_turn <- function(cfg, agent, phase_instruction, topic, history, kg,
                     mode_name, objective_fragment, dimensions_txt,
                     round_number, api_key, max_tokens, temperature,
                     reasoning_effort, current_confidence = NULL,
                     current_consensus = NULL, language = NULL, use_cache = FALSE,
                     problem_details = NULL, critical_rules = NULL,
                     history_override_txt = NULL, on_prompt = NULL) {
  msgs <- build_turn_messages(
    topic = topic, history = history, kg_summary = kg_summary_text(kg), agent = agent, cfg = cfg,
    phase_instruction = phase_instruction, mode_name = mode_name,
    objective_fragment = objective_fragment, round_number = round_number,
    max_tokens = max_tokens, dimensions_txt = dimensions_txt,
    current_confidence = current_confidence, current_consensus = current_consensus,
    language = language, problem_details = problem_details, critical_rules = critical_rules,
    history_override_txt = history_override_txt)
  # Hand the assembled prompt to the caller BEFORE the API call, so the run log
  # can report the input load of the exact messages being sent. Taking the real
  # `msgs` (rather than rebuilding them) means the figure cannot drift from what
  # actually goes out. Never allowed to break a turn.
  if (is.function(on_prompt)) try(on_prompt(msgs), silent = TRUE)
  res <- llm_chat(cfg, agent$provider, msgs, api_key, model = agent$model %||% NULL,
                  max_tokens = max_tokens, temperature = temperature,
                  reasoning_effort = reasoning_effort, use_cache = use_cache)
  text <- if (isTRUE(res$ok)) res$text else paste0("[ERROR: ", res$error, "]")
  # round stored as integer so downstream vapply(..., integer(1)) is well-typed.
  list(round = as.integer(round_number), agent = agent$name, provider = agent$provider,
       agent_id = agent$id, text = text, ok = isTRUE(res$ok),
       error = if (isTRUE(res$ok)) NULL else res$error,
       confidence = extract_confidence(text),
       # token usage carried through for cost accounting (see app.R usage log).
       usage = res$usage, model = res$model %||% agent$model,
       cached = isTRUE(res$cached))
}

# ---- Analytics (base-R, transparent heuristics) -----------------------------
tokenize <- function(txt) {
  words <- tolower(unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", txt), "\\s+")))
  words[nchar(words) > 2]
}
jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  if (length(a) == 0 && length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}
compute_novelty <- function(new_text, prior_texts) {
  if (length(prior_texts) == 0) return(1)
  new_tok <- tokenize(new_text)
  round(1 - max(vapply(prior_texts, function(p) jaccard(new_tok, tokenize(p)), numeric(1))), 3)
}
compute_agreement <- function(new_text, prev_text) {
  if (is.null(prev_text)) return(NA_real_)
  round(jaccard(tokenize(new_text), tokenize(prev_text)), 3)
}
compute_topic_drift <- function(new_text, topic) round(1 - jaccard(tokenize(new_text), tokenize(topic)), 3)
compute_word_count <- function(txt) length(tokenize(txt))

extract_confidence <- function(txt) {
  m <- regmatches(txt, regexpr("Confidence:\\s*([0-9]{1,3})%", txt, ignore.case = TRUE))
  if (length(m) > 0) as.numeric(gsub("[^0-9]", "", m)) else NA_real_
}

# One turn's analytics row. Turn i's metrics depend only on turns 1..i, so a
# row computed once never changes -- which is what makes incremental appending
# (analytics_append) exact, not approximate.
.analytics_row <- function(history, i, topic) {
  h <- history[[i]]
  prior_texts <- lapply(history[seq_len(i - 1)], function(x) x$text)
  prev_text <- if (i > 1) history[[i - 1]]$text else NULL
  data.frame(round = h$round, agent = h$agent, words = compute_word_count(h$text),
             novelty = compute_novelty(h$text, prior_texts),
             agreement = compute_agreement(h$text, prev_text),
             topic_drift = compute_topic_drift(h$text, topic),
             confidence = h$confidence %||% extract_confidence(h$text),
             stringsAsFactors = FALSE)
}

compute_analytics <- function(history, topic) {
  if (length(history) == 0) {
    return(data.frame(round = integer(), agent = character(), words = integer(),
                      novelty = numeric(), agreement = numeric(), topic_drift = numeric(),
                      confidence = numeric()))
  }
  out <- do.call(rbind, lapply(seq_along(history), function(i) .analytics_row(history, i, topic)))
  rownames(out) <- NULL
  out
}

# Incremental analytics: extend an existing table to cover the whole history,
# computing only the rows it does not already have. Rebuilding from scratch is
# quadratic per call (novelty compares each turn against all prior turns) and
# was measured at ~18s/call by turn 320.
#
# The caller updates analytics once per ROUND, by which point the history has
# grown by one entry PER AGENT -- so this must append however many rows are
# missing, not just one. (Assuming a single new row made the incremental path
# unreachable in any multi-agent debate, silently reverting to full recompute.)
#
# Falls back to a full recompute whenever the existing table cannot be a prefix
# of this history (fresh run, loaded session, cleared state), so the result can
# never drift from what compute_analytics would produce.
analytics_append <- function(analytics, history, topic) {
  n <- length(history)
  have <- if (is.null(analytics)) 0L else nrow(analytics)
  if (n == 0 || have >= n) return(compute_analytics(history, topic))
  new_rows <- lapply(seq.int(have + 1L, n), function(i) .analytics_row(history, i, topic))
  out <- rbind(analytics, do.call(rbind, new_rows))
  rownames(out) <- NULL
  out
}

# Novelty-plateau auto-stop: mean per-turn novelty over the last 3 rounds
# below `threshold`.
auto_stop_reached <- function(analytics, n_agents, threshold = 0.15) {
  if (is.null(analytics) || nrow(analytics) == 0) return(FALSE)
  recent <- utils::tail(analytics$novelty, 3 * max(1, n_agents))
  length(recent) > 0 && mean(recent, na.rm = TRUE) < threshold
}

# ---- Claims digest (independent-audit view) ---------------------------------
# A conclusion-free rendering of the debate for agents flagged with
# history_view = "claims_digest" (the Source Auditor): the moderator's per-round
# extractions -- claims, evidence, assumptions, questions, counterarguments --
# WITHOUT positions, group confidence, or agreement/disagreement lists, so the
# audit is formed from what was claimed and cited, not from who concluded what.
# Returns "" when no moderator data exists yet; the caller then falls back to
# the full transcript so the auditor never turns up empty-handed.
claims_digest <- function(history) {
  shown_types <- c("Claim", "Evidence", "Assumption", "Question", "Counterargument")
  parts <- character(0)
  for (h in history %||% list()) {
    m <- h$moderator
    if (is.null(m)) next
    nodes <- m$nodes %||% list()
    if (length(nodes) == 0) next
    by_type <- list()
    for (n in nodes) {
      ty <- as.character(n$type %||% "Idea"); lb <- trimws(as.character(n$label %||% ""))
      if (!(ty %in% shown_types) || !nzchar(lb)) next
      by_type[[ty]] <- c(by_type[[ty]], lb)
    }
    if (length(by_type) == 0) next
    lines <- vapply(names(by_type), function(ty)
      paste0("  ", ty, "s: ", paste(unique(by_type[[ty]]), collapse = "; ")), character(1))
    parts <- c(parts, paste0("[Round ", h$round %||% "?", "]\n", paste(lines, collapse = "\n")))
  }
  if (length(parts) == 0) return("")
  paste(parts, collapse = "\n\n")
}

# ---- Debate-quality scorecard -----------------------------------------------
# Derives a PROCESS-quality read from the moderator's per-round extractions plus
# the accumulated knowledge graph. Five 0-100 dimensions + an overall index and
# label. Measures rigour (grounding, clash, structure, reasoning, idea work) --
# NOT whether the conclusion is correct. $enough_data = FALSE when too sparse.
debate_quality <- function(history, kg) {
  mods <- Filter(Negate(is.null), lapply(history %||% list(), function(h) h$moderator))
  # nrow() is NULL for anything that isn't a data.frame, so default to 0 -- a
  # malformed kg must degrade to "not enough data", never crash the card.
  nn <- (if (!is.null(kg)) nrow(kg$nodes) else 0) %||% 0
  ne <- (if (!is.null(kg)) nrow(kg$edges) else 0) %||% 0
  if (length(mods) == 0 || nn == 0 || !is.data.frame(kg$nodes))
    return(list(enough_data = FALSE, n_rounds = length(mods)))

  ntype <- function(t) sum(kg$nodes$type == t)
  n_claim <- ntype("Claim"); n_evi <- ntype("Evidence"); n_counter <- ntype("Counterargument")
  erel  <- function(r) if (ne > 0) sum(kg$edges$relation == r) else 0
  e_contra <- erel("Contradicts") + erel("Rejects")
  e_dev    <- erel("Extends") + erel("Refines")

  tot   <- function(field) sum(vapply(mods, function(m) length(unlist(m[[field]])), numeric(1)))
  n_disagree <- tot("disagreements"); n_fallacy <- tot("fallacies"); n_newhyp <- tot("new_hypotheses")
  # Robust scalar-numeric: moderators may emit confidence as 70, "70" or "70%".
  num1 <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character((x %||% NA)[1]))))
  confs <- vapply(mods, function(m) num1(m$group_confidence_pct), numeric(1))
  confs <- confs[!is.na(confs)]

  clamp <- function(x) max(0, min(100, x))
  band  <- function(s) if (s >= 67) "High" else if (s >= 34) "Medium" else "Low"

  s_evi <- clamp(100 * (n_evi / max(1, n_claim)) / 0.6)                    # evidence per claim
  claim_ids  <- kg$nodes$id[kg$nodes$type == "Claim"]
  challenged <- if (e_contra > 0)
    length(intersect(unique(kg$edges$to[kg$edges$relation %in% c("Contradicts", "Rejects")]), claim_ids)) else 0
  s_eng <- clamp(100 * (challenged / max(1, n_claim)) / 0.5)               # claims challenged
  if (s_eng == 0 && n_disagree > 0) s_eng <- 25                           # partial credit
  s_con <- clamp(100 * (ne / max(1, nn)) / 1.0)                           # edges per node
  s_rea <- clamp(100 * (1 - n_fallacy / max(1, n_claim + n_counter)))     # inverse fallacy rate
  s_prod <- clamp(100 * ((n_newhyp + e_dev) / max(1, length(mods))) / 1.5) # new/refined ideas per round

  scores <- c(s_evi, s_eng, s_con, s_rea, s_prod)
  index  <- round(mean(scores))
  label  <- if (index >= 70) "Robust" else if (index >= 45) "Moderate" else "Shallow"
  conv <- if (length(confs) >= 2) {
    d <- confs[length(confs)] - confs[1]
    list(dir = if (d > 5) "converging" else if (d < -5) "diverging" else "stable",
         first = confs[1], last = confs[length(confs)], delta = round(d))
  } else NULL
  dims <- list(
    list(name = "Evidence grounding",   band = band(s_evi),  raw = sprintf("%d evidence / %d claims", n_evi, n_claim)),
    list(name = "Critical engagement",  band = band(s_eng),  raw = sprintf("%d of %d claims challenged", challenged, n_claim)),
    list(name = "Argument connectivity",band = band(s_con),  raw = sprintf("%.1f edges/node", ne / max(1, nn))),
    list(name = "Reasoning integrity",  band = band(s_rea),  raw = sprintf("%d fallacies flagged", n_fallacy)),
    list(name = "Idea productivity",    band = band(s_prod), raw = sprintf("%d new + %d refined", n_newhyp, e_dev)))
  list(enough_data = TRUE, index = index, label = label, dims = dims,
       convergence = conv, n_rounds = length(mods))
}
