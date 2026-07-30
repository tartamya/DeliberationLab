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
                     problem_details = NULL) {
  msgs <- build_turn_messages(
    topic = topic, history = history, kg_summary = kg_summary_text(kg), agent = agent, cfg = cfg,
    phase_instruction = phase_instruction, mode_name = mode_name,
    objective_fragment = objective_fragment, round_number = round_number,
    max_tokens = max_tokens, dimensions_txt = dimensions_txt,
    current_confidence = current_confidence, current_consensus = current_consensus,
    language = language, problem_details = problem_details)
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

compute_analytics <- function(history, topic) {
  if (length(history) == 0) {
    return(data.frame(round = integer(), agent = character(), words = integer(),
                      novelty = numeric(), agreement = numeric(), topic_drift = numeric(),
                      confidence = numeric()))
  }
  rows <- lapply(seq_along(history), function(i) {
    h <- history[[i]]
    prior_texts <- lapply(history[seq_len(i - 1)], function(x) x$text)
    prev_text <- if (i > 1) history[[i - 1]]$text else NULL
    data.frame(round = h$round, agent = h$agent, words = compute_word_count(h$text),
               novelty = compute_novelty(h$text, prior_texts),
               agreement = compute_agreement(h$text, prev_text),
               topic_drift = compute_topic_drift(h$text, topic),
               confidence = h$confidence %||% extract_confidence(h$text),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Novelty-plateau auto-stop: mean per-turn novelty over the last 3 rounds
# below `threshold`.
auto_stop_reached <- function(analytics, n_agents, threshold = 0.15) {
  if (is.null(analytics) || nrow(analytics) == 0) return(FALSE)
  recent <- utils::tail(analytics$novelty, 3 * max(1, n_agents))
  length(recent) > 0 && mean(recent, na.rm = TRUE) < threshold
}
