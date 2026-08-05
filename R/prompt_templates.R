# =============================================================================
# prompt_templates.R -- all prompt assembly in one place
# -----------------------------------------------------------------------------
# Keeps prompt construction out of the engine/planner/synthesis modules so the
# wording can be tuned in one file. Also home to extract_json_block(), the
# shared, robust JSON extractor used by every structured-output call (planner,
# moderator, synthesis).
# =============================================================================

# ---- Robust JSON extraction -------------------------------------------------
# Strips markdown fences and surrounding prose, then walks char-by-char
# tracking brace depth and string state to return the FIRST complete balanced
# {...} object. Robust to trailing text and braces inside string values --
# ported verbatim (it earned its keep) from the CIRL app.
extract_json_block <- function(txt) {
  txt <- gsub("```json|```", "", txt)
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  start <- which(chars == "{")[1]
  if (is.na(start)) return(txt)
  depth <- 0L; in_string <- FALSE; escape_next <- FALSE; end <- NA_integer_
  for (i in seq(start, length(chars))) {
    ch <- chars[i]
    if (in_string) {
      if (escape_next) escape_next <- FALSE
      else if (ch == "\\") escape_next <- TRUE
      else if (ch == '"') in_string <- FALSE
    } else {
      if (ch == '"') in_string <- TRUE
      else if (ch == "{") depth <- depth + 1L
      else if (ch == "}") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
    }
  }
  if (is.na(end)) return(txt) # unbalanced/truncated -- let fromJSON error honestly
  substr(txt, start, end)
}

# Parse a model's text into a list via extract_json_block(); NULL on failure.
parse_json_response <- function(txt) {
  tryCatch(jsonlite::fromJSON(extract_json_block(txt), simplifyVector = FALSE),
           error = function(e) NULL)
}

# A short one-line snippet of a model's raw output, for diagnostics when a
# structured-JSON reply couldn't be parsed.
json_snippet <- function(txt, n = 180) {
  s <- substr(gsub("[[:space:]]+", " ", trimws(txt %||% "")), 1, n)
  paste0("\"", s, if (nchar(s) >= n) "..." else "", "\"")
}

# Call an LLM expecting a JSON object, parse it, and -- on an unparseable reply
# -- do ONE self-correcting retry (show the model its bad output and demand
# strict, complete JSON). Shared by planner/moderator/consensus so all three
# recover the same way instead of silently falling back. reasoning_effort is
# always NULL (reasoning tokens would starve the JSON budget).
# Returns: list(parsed=, ok=, error=, usage=, model=, cached=, text=)
#   parsed = the parsed list, or NULL if it still couldn't be parsed
#   text   = the FIRST reply's raw text (for a diagnostic snippet)
# `fallbacks` = an ordered list of list(provider=, key=) to try if the primary
# provider errors or won't return parseable JSON (e.g. the meta provider timed
# out). The FIRST provider that returns a valid JSON object wins; if none do,
# the last attempt is returned so the caller can fall back to its heuristic.
# `provider` in the return records which provider actually produced the result.
llm_json <- function(cfg, provider_id, messages, api_key, max_tokens = 4000,
                     temperature = 0.2, use_cache = FALSE, fallbacks = list()) {
  # One provider's attempt: a call plus a single self-correcting JSON retry.
  attempt <- function(pid, key) {
    res <- llm_chat(cfg, pid, messages, key, max_tokens = max_tokens,
                    temperature = temperature, reasoning_effort = NULL, use_cache = use_cache)
    if (!isTRUE(res$ok)) {
      return(list(parsed = NULL, ok = FALSE, error = res$error, usage = res$usage,
                  model = res$model, cached = isTRUE(res$cached), text = NULL, provider = pid))
    }
    parsed <- parse_json_response(res$text)
    usage <- res$usage; model <- res$model
    if (is.null(parsed)) {
      retry <- c(messages,
        list(list(role = "assistant", content = substr(res$text %||% "", 1, 4000)),
             list(role = "user", content = paste0(
               "Your reply was NOT valid JSON and could not be parsed. Reply again with ONLY a single, ",
               "COMPLETE, valid JSON object matching the schema exactly -- no markdown fences, no comments, ",
               "no text before or after. Keep string values concise so the JSON is not truncated."))))
      res2 <- llm_chat(cfg, pid, retry, key, max_tokens = max_tokens,
                       temperature = min(temperature, 0.2), reasoning_effort = NULL, use_cache = FALSE)
      if (isTRUE(res2$ok)) {
        parsed <- parse_json_response(res2$text)
        g <- function(u, k) suppressWarnings(as.numeric((u %||% list())[[k]] %||% NA))
        usage <- list(  # attribute BOTH calls' tokens
          prompt_tokens = sum(c(g(res$usage, "prompt_tokens"), g(res2$usage, "prompt_tokens")), na.rm = TRUE),
          completion_tokens = sum(c(g(res$usage, "completion_tokens"), g(res2$usage, "completion_tokens")), na.rm = TRUE))
        model <- res2$model
      }
    }
    list(parsed = parsed, ok = TRUE, error = NULL, usage = usage, model = model,
         cached = isTRUE(res$cached), text = res$text, provider = pid)
  }
  # Primary first, then each fallback; stop at the first PARSED result.
  chain <- c(list(list(provider = provider_id, key = api_key)), fallbacks)
  last <- NULL
  for (att in chain) {
    r <- attempt(att$provider, att$key)
    if (!is.null(r$parsed)) return(r)   # valid JSON -> success
    last <- r
  }
  last                                   # none parsed -> caller uses heuristic
}

# ---- Agent persona ----------------------------------------------------------
# Turns an agent record (see agent_factory.R) into the system-prompt persona
# block. Reasoning-style and evidence-preference fragments come from config so
# the persona actually reflects the configured behavior.
build_persona <- function(agent, cfg) {
  rs <- cfg_find(cfg$reasoning_styles, agent$reasoning)
  reasoning_fragment <- if (!is.null(rs)) rs$prompt_fragment else ""
  paste0(
    "You are ", agent$name, ", acting as a ", agent$role,
    if (nzchar(agent$category %||% "")) paste0(" (", agent$category, ")") else "", ".\n",
    "Domain expertise: ", agent$expertise, ".\n",
    if (nzchar(agent$goal %||% "")) paste0("Your goal: ", agent$goal, ".\n") else "",
    # Custom roles may put free-text prose in `reasoning` rather than a configured
    # style name; emit it plainly instead of a style name followed by an empty fragment.
    if (nzchar(reasoning_fragment)) paste0("Reasoning style -- ", agent$reasoning, ": ", reasoning_fragment, "\n")
    else paste0("Reasoning style: ", agent$reasoning, ".\n"),
    "You prefer evidence of type: ", agent$evidence, ". Cite that kind of evidence when you can.\n",
    if (nzchar(agent$communication %||% "")) paste0("Communication style: ", agent$communication, ".\n") else "",
    if (nzchar(agent$bias %||% "")) paste0("Acknowledge this leaning honestly when relevant: ", agent$bias, ".\n") else "",
    # Prohibitions last and emphatic: for epistemic roles the boundary IS the
    # role -- Skeptic/Falsifier and Red-Team differ mainly in what each may not do.
    if (nzchar(agent$constraints %||% ""))
      paste0("STAY IN ROLE -- you must NOT: ", agent$constraints,
             "\nThis boundary defines your contribution; another participant covers what lies outside it.\n") else "",
    "Personality dials (0-1): creativity=", agent$creativity,
    ", skepticism=", agent$skepticism, ", risk_tolerance=", agent$risk_tolerance, ".\n",
    if (nzchar(agent$prompt %||% "")) paste0("Additional instructions: ", agent$prompt, "\n") else ""
  )
}

# Compact recent-history rendering for the turn prompt.
render_history <- function(history, window = 12) {
  recent <- utils::tail(history, window)
  if (length(recent) == 0) return("(no prior turns)")
  paste(vapply(recent, function(h) paste0("[Round ", h$round, "] ", h$agent, ": ", h$text), character(1)),
        collapse = "\n\n")
}

# ---- One agent turn ---------------------------------------------------------
# Assembles the full message list for a single agent turn: persona + shared
# context (topic, objective, dimensions, mode, round, moderator/consensus so
# far, KG summary) + the phase instruction for this turn.
build_turn_messages <- function(topic, history, kg_summary, agent, cfg,
                                phase_instruction, mode_name, objective_fragment,
                                round_number, max_tokens,
                                dimensions_txt = "", current_confidence = NULL,
                                current_consensus = NULL, history_window = 12,
                                language = NULL, problem_details = NULL,
                                critical_rules = NULL, history_override_txt = NULL) {
  persona <- build_persona(agent, cfg)
  # history_override_txt replaces the rendered transcript wholesale -- used for
  # the Source Auditor's independent-audit window, where the "discussion" shown
  # is a conclusion-free claims digest rather than the verbatim debate.
  override <- !is.null(history_override_txt) && nzchar(history_override_txt)
  history_txt <- if (override) paste0(
    "(Independent audit view: extracted claims and evidence only. Participants' positions, ",
    "confidence levels, and the verbatim debate are withheld from you for this round.)\n\n",
    history_override_txt) else render_history(history, history_window)
  # Critical rules bind EVERY agent regardless of persona. Placed at the very
  # top of the system prompt (before the persona's rhetorical framing) and
  # marked as overriding, so accuracy/uncertainty beats winning the argument.
  # These bind what an agent may ASSERT, not the stance it was assigned. The
  # earlier wording ("override any rhetorical aim of your persona") told every
  # agent to abandon its role, which flattened stance-taking roles (Optimist,
  # Skeptic, Proponent, Red-Team) into neutral analysis -- role collapse caused
  # by the app's own prompt. Honesty binds the claims; the role still stands.
  rules_block <- if (!is.null(critical_rules) && nzchar(trimws(critical_rules)))
    paste0("CRITICAL RULES -- these bind every factual claim you make and outrank any urge to win ",
           "the argument. Follow them exactly even when they weaken your side. They do NOT dissolve ",
           "your assigned role: keep arguing your assigned position, in your own voice, and make the ",
           "strongest case it deserves -- just never misstate evidence to do it:\n",
           trimws(critical_rules), "\n\n") else ""
  details_block <- if (!is.null(problem_details) && nzchar(trimws(problem_details)))
    paste0("\n\nProblem details / background:\n", trimws(problem_details)) else ""
  # Self-activating "engage with evidence" directive: only injected when the
  # recent discussion actually contains a cited Sources block -- specifically
  # "Sources:" followed by a numbered "[1]" list, the exact shape produced for
  # web citations. Matching the numbered list (not a bare "Sources:") avoids
  # false positives from an agent using the word "sources" in prose. Costs
  # nothing when absent. Forces agents to give sourced evidence due consideration.
  evidence_directive <- if (grepl("Sources:\\s*\\[[0-9]", history_txt))
    paste0("CITED EVIDENCE PRESENT: a participant has introduced sourced, cited evidence ",
           "(see the 'Sources:' entries in the recent discussion). You MUST engage with it ",
           "directly -- either accept it and build on it, or challenge it with a specific, ",
           "reasoned objection. Do not ignore or talk past sourced facts.\n\n") else ""
  sys <- paste0(
    rules_block,
    persona, "\n",
    if (nzchar(objective_fragment %||% "")) paste0(objective_fragment, "\n") else "",
    "Discussion mode: ", mode_name, ". Round: ", round_number, ".\n",
    if (nzchar(dimensions_txt)) paste0("Relevant dimensions to weigh: ", dimensions_txt, "\n") else "",
    if (!is.null(current_confidence)) paste0("Current group confidence: ", current_confidence, "\n") else "",
    if (!is.null(current_consensus) && nzchar(current_consensus)) paste0("Provisional consensus so far: ", current_consensus, "\n") else "",
    "Knowledge graph so far (claims/evidence/questions): ", kg_summary, "\n\n",
    evidence_directive,
    "INSTRUCTION FOR THIS TURN: ", phase_instruction, "\n\n",
    if (!is.null(language) && nzchar(language)) paste0("Respond in ", language, ".\n") else "",
    "Keep your response focused and under roughly ", max_tokens, " tokens. ",
    "End with a line of the exact form 'Confidence: NN%' giving your confidence in your OWN position."
  )
  user <- paste0("Topic: ", topic, details_block, "\n\nRecent discussion:\n", history_txt)
  list(list(role = "system", content = sys), list(role = "user", content = user))
}

# ---- Planner prompt ---------------------------------------------------------
# Feeds the LLM the available config vocabulary and asks it to design the
# deliberation. Vocabulary is passed so the model selects real, wired-up
# values (dimensions, roles, modes, moderators) rather than inventing labels
# the app can't act on -- though it may add new ones too.
build_planner_messages <- function(topic, cfg, n_agents_hint = NULL, problem_details = NULL) {
  vocab <- function(items, field = "name") paste(cfg_names(items, field), collapse = ", ")
  sys <- paste0(
    "You are the PLANNER for a multi-agent deliberation laboratory. Given a topic, you design ",
    "the deliberation. You know nothing topic-specific in advance -- infer everything from the topic.\n\n",
    "Available (prefer these, but you may add new items where genuinely needed):\n",
    "- Discussion dimensions: ", vocab(cfg$dimensions), "\n",
    "- Expert roles: ", vocab(cfg$roles), "\n",
    "- Reasoning styles: ", vocab(cfg$reasoning_styles), "\n",
    "- Debate modes: ", vocab(cfg$debate_modes), "\n",
    "- Moderator types: ", vocab(cfg$moderators), "\n",
    "- Evidence types: ", vocab(cfg$evidence_types), "\n\n",
    "HONOUR EXPLICIT DESIGN INSTRUCTIONS: the user's problem details may contain explicit ",
    "instructions about how to configure the deliberation -- e.g. the number of experts/agents, ",
    "specific expert roles or personas to include, which discussion dimensions to use or ",
    "emphasise, the debate mode, the moderator style, or specific debate questions. Treat every ",
    "such explicit instruction as a REQUIREMENT and reflect it EXACTLY in your JSON plan:\n",
    "- If a number of agents/experts is given, output EXACTLY that many entries in `experts` and ",
    "set `recommended_num_agents` to the same number.\n",
    "- If specific roles/personas are named, use them verbatim as the `experts` (in the given order).\n",
    "- If specific dimensions are named, put exactly those in `dimensions` (highest importance first).\n",
    "- If a debate mode / moderator is named, set `recommended_mode` / `recommended_moderator` to it.\n",
    "- If specific questions are given, use them as `debate_questions`.\n",
    "Follow the user's instruction even if it differs from the lists above (add new items as needed). ",
    "Design freely ONLY where the details are silent.\n\n",
    "Respond with ONLY a single JSON object (no prose, no markdown fences) with this schema:\n",
    '{"dimensions": [{"name": string, "importance": number, "why": string}], ',
    '"experts": [{"role": string, "reasoning": string, "evidence": string, "why": string}], ',
    '"debate_questions": [string], ',
    '"recommended_mode": string, "recommended_moderator": string, ',
    '"expected_agreements": [string], "expected_controversies": [string], ',
    '"required_evidence": [string], "recommended_num_agents": number, "rationale": string}'
  )
  # Labelled as instructions (not just background) so the model reads any panel/
  # dimension/mode directives inside it as requirements, per the system prompt.
  details_block <- if (!is.null(problem_details) && nzchar(trimws(problem_details)))
    paste0("\n\nProblem details / user instructions (honour any explicit design directives here):\n",
           trimws(problem_details), "\n") else ""
  user <- paste0("Topic: ", topic, "\n", details_block,
                 if (!is.null(n_agents_hint)) paste0("If the details above do NOT specify a count, aim for about ",
                                                     n_agents_hint, " agents.\n") else "",
                 "Design the deliberation now.")
  list(list(role = "system", content = sys), list(role = "user", content = user))
}

# Five-role variant (two-layer architecture): the four seated epistemic
# functions are FIXED -- the planner's job is to assign each one a
# topic-appropriate DOMAIN LENS (a Challenger challenging as a clinical
# pharmacologist finds different weaknesses than one challenging as a
# statistician). The fifth role, Synthesiser, is played by the consensus
# engine after the debate, so it is not part of the panel the planner designs.
build_planner_messages_five_role <- function(topic, cfg, problem_details = NULL) {
  vocab <- function(items, field = "name") paste(cfg_names(items, field), collapse = ", ")
  sys <- paste0(
    "You are the PLANNER for a multi-agent deliberation using a FIXED five-role adversarial-",
    "scrutiny panel. Four roles debate -- Proposer (builds the strongest case), Challenger ",
    "(attacks it), Steelman (tests whether the critique is fair), Source Auditor (checks what ",
    "the evidence actually says) -- and a Synthesiser writes the verdict afterwards; you do NOT ",
    "assign the Synthesiser.\n\n",
    "Your job is NOT to choose the roles. It is to assign each of the four fixed roles a DOMAIN ",
    "LENS suited to this topic: the disciplinary standpoint from which that role does its work ",
    "(e.g. for a drug question the Challenger might challenge as a clinical pharmacologist and ",
    "the Source Auditor audit as a biostatistician). Also design the dimensions and questions.\n\n",
    "Available vocabulary (prefer these where they fit):\n",
    "- Discussion dimensions: ", vocab(cfg$dimensions), "\n",
    "- Reasoning styles: ", vocab(cfg$reasoning_styles), "\n",
    "- Evidence types: ", vocab(cfg$evidence_types), "\n\n",
    "If the problem details contain explicit design instructions (dimensions, questions, specific ",
    "domain lenses), honour them exactly.\n\n",
    "Respond with ONLY a single JSON object (no prose, no markdown fences) with this schema:\n",
    '{"experts": [{"role": "Proposer"|"Challenger"|"Steelman"|"Source Auditor", ',
    '"lens": string (the domain lens, e.g. "clinical pharmacology"), ',
    '"reasoning": string, "evidence": string, "why": string}]  -- exactly these four roles, ',
    'in this order, ',
    '"dimensions": [{"name": string, "importance": number, "why": string}], ',
    '"debate_questions": [string], ',
    '"expected_agreements": [string], "expected_controversies": [string], ',
    '"required_evidence": [string], "rationale": string}'
  )
  details_block <- if (!is.null(problem_details) && nzchar(trimws(problem_details)))
    paste0("\n\nProblem details / user instructions:\n", trimws(problem_details), "\n") else ""
  user <- paste0("Topic: ", topic, "\n", details_block, "Assign the domain lenses and design the deliberation now.")
  list(list(role = "system", content = sys), list(role = "user", content = user))
}
