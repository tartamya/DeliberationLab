# =============================================================================
# agent_factory.R -- build agents from attributes, not fixed prompts
# -----------------------------------------------------------------------------
# An agent is a plain list of attributes (role, expertise, reasoning style,
# evidence preference, communication style, bias, personality dials, provider,
# plus mutable state: current position, confidence history, and memory). The
# persona prompt is DERIVED from these attributes at call time in
# prompt_templates.R::build_persona() -- so changing an attribute changes
# behavior, and new roles/styles are pure config.
# =============================================================================

# Create one agent. Defaults are filled from config where a role/reasoning is
# named. `memory` and `confidence_history` start empty and are updated by the
# engine via memory.R.
new_agent <- function(cfg, name, role = "", category = "", expertise = "",
                      reasoning = "Pragmatist", evidence = "Expert Consensus",
                      communication = "", bias = "", goal = "",
                      creativity = 0.5, skepticism = 0.5, risk_tolerance = 0.5,
                      confidence = 0.5, prompt = "", provider = NULL,
                      constraints = "", history_view = "", digest_rounds = 0,
                      rules = "", apply_rules = TRUE) {
  provider <- provider %||% (if (length(cfg$providers)) cfg$providers[[1]]$id else "openai")
  list(
    id = digest::digest(paste(name, role, Sys.time(), runif(1)), algo = "crc32"),
    name = name, role = role, category = category, expertise = expertise,
    reasoning = reasoning, evidence = evidence, communication = communication, bias = bias,
    # what this role must NOT do -- emitted as a hard prohibition in the persona.
    constraints = constraints,
    # history_view = "claims_digest" + digest_rounds = N: for rounds 2..N this
    # agent sees the moderator's conclusion-free claims digest instead of the
    # verbatim transcript (the Source Auditor's independent-audit window).
    history_view = history_view, digest_rounds = digest_rounds,
    # Per-participant critical rules. `rules` empty means inherit the Planner
    # tab's ruleset; non-empty overrides it for this agent alone. apply_rules
    # FALSE means no rules at all -- a free hand in the debate.
    rules = rules, apply_rules = isTRUE(apply_rules),
    goal = goal, creativity = creativity, skepticism = skepticism,
    risk_tolerance = risk_tolerance, confidence = confidence, prompt = prompt,
    provider = provider,
    # mutable state
    current_position = "", supporting_evidence = list(),
    confidence_history = numeric(0), memory = list()
  )
}

# Build an agent from a named role in roles.json, applying the role's
# defaults. `overrides` is a named list that wins over the role defaults
# (used by the planner, which may specify reasoning/evidence explicitly).
# ---- Built-in roles ----------------------------------------------------------
# The four roles the five-role adversarial-scrutiny panel seats BY NAME. They are
# app machinery, not library content: without them that panel silently degrades
# to generic agents stripped of the constraints that define it, and the panel is
# the default. Keeping definitions here means curating (or wiping) roles.json can
# never break it. The library still wins on a name match, so a user who wants a
# different Steelman just defines one -- these are a floor, not a lock.
.BUILTIN_ROLES <- list(
  "Proponent / Causal Advocate" = list(
    name = "Proponent / Causal Advocate", category = "Epistemic",
    expertise = "building the strongest causal case for the proposition, mechanism and pathway construction",
    reasoning = "First Principles", evidence = "Mechanistic Research",
    communication = "Structured", bias = "Advocates for the proposition",
    constraints = "Do not pretend uncertainties are resolved. State plainly which links in your causal chain are established and which are assumed or contested; overstating a weak link forfeits your case.",
    creativity = 0.6, skepticism = 0.3, risk_tolerance = 0.6),
  "Skeptic / Falsifier" = list(
    name = "Skeptic / Falsifier", category = "Epistemic",
    expertise = "falsification, failure modes, disconfirming evidence",
    reasoning = "Skeptic", evidence = "Meta-analysis",
    communication = "Blunt", bias = "Doubt-first",
    constraints = "Do not merely offer an alternative opinion. Every objection must name a specific way the proposition could be FALSE and state what observation or result would demonstrate that.",
    creativity = 0.4, skepticism = 0.9, risk_tolerance = 0.3),
  "Steelman" = list(
    name = "Steelman", category = "Epistemic",
    expertise = "reconstructing the strongest version of positions under attack, testing the fairness of critique",
    reasoning = "First Principles", evidence = "Expert Consensus",
    communication = "Structured", bias = "Charitable interpretation",
    constraints = "Do not simply defend the original position, and do not join the critics. Your object is the critique itself: test whether each objection engages the argument's strongest form; where it attacks a weaker version, restate the strongest version and require the objection to meet it.",
    creativity = 0.5, skepticism = 0.6, risk_tolerance = 0.4),
  "Empirical Evidence Auditor" = list(
    name = "Empirical Evidence Auditor", category = "Epistemic",
    expertise = "locating studies and data that directly test the claimed links",
    reasoning = "Evidence Maximizer", evidence = "Systematic Reviews",
    communication = "Structured", bias = "Demands direct evidence",
    constraints = "Do not infer evidence from plausibility. If no study directly tests a link, say so explicitly and record it as an evidence gap rather than substituting a related or analogous finding. Form your audit from the claims and citations alone: do not adopt, weigh, or be swayed by any participant's stated conclusion or confidence.",
    history_view = "claims_digest", digest_rounds = 2,
    creativity = 0.3, skepticism = 0.7, risk_tolerance = 0.3))

# Is this role name supplied by the app rather than the user's library?
is_builtin_role <- function(name) !is.null(.BUILTIN_ROLES[[as.character(name %||% "")]])

# Is this exact record the app's built-in (rather than a user definition that
# happens to share the name)? Used to label the library table.
is_builtin_record <- function(r) {
  b <- .BUILTIN_ROLES[[as.character(r$name %||% "")]]
  !is.null(b) && identical(r, b)
}

# Fill in any built-in the library does not already define. Without this the
# built-ins are resolvable inside agent_from_role but invisible everywhere else:
# not listed in the library table, not offered in the role picker, not found by
# the manual add form. A library definition of the same name always wins, so
# this only ever fills gaps.
merge_builtin_roles <- function(roles) {
  roles <- roles %||% list()
  have <- vapply(roles, function(r) as.character(r$name %||% ""), character(1))
  c(roles, unname(.BUILTIN_ROLES[setdiff(names(.BUILTIN_ROLES), have)]))
}

agent_from_role <- function(cfg, role_name, provider = NULL, overrides = list()) {
  # Library first (so a user definition overrides), then the built-in floor,
  # then the bare fallback for a role the planner invented.
  r <- cfg_find(cfg$roles, role_name) %||% .BUILTIN_ROLES[[as.character(role_name)]]
  base <- if (is.null(r)) {
    # Planner invented a role not in config -- still build a usable agent.
    list(name = role_name, category = "Custom", expertise = role_name,
         reasoning = "Pragmatist", evidence = "Expert Consensus",
         communication = "", bias = "")
  } else r
  agent_from_role_record(cfg, base, provider = provider, overrides = overrides)
}

# The critical rules a given agent actually receives:
#   - its checkbox off      -> nothing (a free hand in the debate)
#   - its own rules set     -> those, overriding the Planner tab's ruleset
#   - otherwise             -> the global ruleset it inherits
# Kept here rather than inline in the round loop so the precedence is stated
# once and the same answer is used by the run and by the Prompt Preview.
agent_rules <- function(agent, global_rules) {
  if (!isTRUE(agent$apply_rules %||% TRUE)) return("")
  own <- trimws(agent$rules %||% "")
  if (nzchar(own)) own else (global_rules %||% "")
}

# Roster names must stay distinct: they key the transcript, the analytics table
# and the moderator's attributions, so two "Skeptic / Falsifier" agents would be
# indistinguishable downstream. Appends " (2)", " (3)" ... on collision.
unique_agent_name <- function(name, agents) {
  name <- if (nzchar(name %||% "")) name else "Agent"
  taken <- vapply(agents %||% list(), function(a) a$name %||% "", character(1))
  if (!(name %in% taken)) return(name)
  i <- 2L
  repeat {
    cand <- paste0(name, " (", i, ")")
    if (!(cand %in% taken)) return(cand)
    i <- i + 1L
  }
}

# Build an agent from a role RECORD rather than a name looked up in config, so
# roles added to the library mid-session (which aren't in the loaded cfg) can
# still be seated. agent_from_role() is the by-name wrapper around this.
agent_from_role_record <- function(cfg, base, provider = NULL, overrides = list()) {
  new_agent(
    cfg,
    name = overrides$name %||% base$name,
    role = base$name, category = base$category %||% "",
    # An expertise override is how the five-role planner attaches a topic-
    # specific DOMAIN LENS to a fixed epistemic role (two-layer architecture).
    expertise = if (nzchar(overrides$expertise %||% "")) overrides$expertise else (base$expertise %||% ""),
    reasoning = if (nzchar(overrides$reasoning %||% "")) overrides$reasoning else (base$reasoning %||% "Pragmatist"),
    evidence  = if (nzchar(overrides$evidence  %||% "")) overrides$evidence  else (base$evidence  %||% "Expert Consensus"),
    communication = base$communication %||% "", bias = base$bias %||% "",
    constraints = base$constraints %||% "",
    history_view = base$history_view %||% "",
    digest_rounds = { dr <- suppressWarnings(as.integer((base$digest_rounds %||% 0)[1]))
                      if (length(dr) == 0 || is.na(dr)) 0L else dr },
    goal = overrides$goal %||% "",
    # Dials fall back to the role's presets before the 0.5 default, so a role
    # like Red-Team arrives already sceptical rather than neutral.
    creativity = overrides$creativity %||% base$creativity %||% 0.5,
    skepticism = overrides$skepticism %||% base$skepticism %||% 0.5,
    risk_tolerance = overrides$risk_tolerance %||% base$risk_tolerance %||% 0.5,
    confidence = overrides$confidence %||% 0.5,
    prompt = overrides$prompt %||% "",
    provider = provider
  )
}

# Turn a planner plan into a full agent roster. Distributes the chosen
# provider(s) round-robin across agents so a multi-provider run is easy.
agents_from_plan <- function(cfg, plan, providers) {
  if (length(providers) == 0) providers <- provider_ids(cfg)[1]
  experts <- plan$experts
  if (length(experts) == 0) return(list())
  # Built iteratively so each agent's name is uniquified against those already
  # seated -- the planner may legitimately propose the same role twice.
  agents <- list()
  for (i in seq_along(experts)) {
    e <- experts[[i]]
    prov <- providers[((i - 1) %% length(providers)) + 1]
    a <- agent_from_role(cfg, e$role, provider = prov,
                         overrides = list(name = e$name %||% NULL,
                                          expertise = e$expertise %||% "",
                                          reasoning = e$reasoning %||% "", evidence = e$evidence %||% "",
                                          goal = e$why %||% ""))
    a$name <- unique_agent_name(a$name, agents)
    agents <- c(agents, list(a))
  }
  agents
}

# A minimal default roster so the app is usable before running the planner.
# `providers` should be the set of usable providers (with keys); defaults to
# all configured providers if not supplied.
default_agents <- function(cfg, providers = NULL) {
  provs <- providers %||% provider_ids(cfg)
  if (length(provs) == 0) provs <- provider_ids(cfg)
  p1 <- provs[1]; p2 <- if (length(provs) > 1) provs[2] else provs[1]
  list(
    agent_from_role(cfg, "Optimist", provider = p1),
    agent_from_role(cfg, "Skeptic",  provider = p2)
  )
}

# Build a fresh RANDOM roster: n agents (default 2-5), each a random role with
# a random reasoning style, evidence preference, personality dials, and a
# provider drawn from `providers` (the active set). Used by the "Shuffle
# roster" button.
random_roster <- function(cfg, providers, n = NULL) {
  if (length(providers) == 0) providers <- provider_ids(cfg)
  if (length(cfg$roles) == 0) return(list())
  n <- n %||% sample(2:5, 1)
  reasoning <- cfg_names(cfg$reasoning_styles)
  evidence  <- cfg_names(cfg$evidence_types)
  # Distinct roles when the pool allows (duplicate experts add little), and
  # names uniquified regardless -- they key the transcript and analytics.
  picks <- sample(length(cfg$roles), min(n, length(cfg$roles)))
  if (length(picks) < n) picks <- c(picks, sample(length(cfg$roles), n - length(picks), replace = TRUE))
  agents <- list()
  for (i in seq_len(n)) {
    role <- cfg$roles[[picks[i]]]
    # A role with constraints is defined by its epistemic job: randomizing its
    # reasoning/evidence/dials would undermine the very boundary it enforces
    # (a Red-Team at skepticism 0.1). Shuffle only what the role leaves open.
    constrained <- nzchar(role$constraints %||% "")
    a <- agent_from_role(cfg, role$name, provider = sample(providers, 1),
      overrides = if (constrained) list(confidence = round(runif(1), 2)) else list(
        reasoning = if (length(reasoning)) sample(reasoning, 1) else "",
        evidence  = if (length(evidence))  sample(evidence, 1)  else "",
        creativity = round(runif(1), 2), skepticism = round(runif(1), 2),
        risk_tolerance = round(runif(1), 2), confidence = round(runif(1), 2)))
    a$name <- unique_agent_name(a$name, agents)
    agents <- c(agents, list(a))
  }
  agents
}

# Flatten a roster to a data.frame for the Participants table.
agents_to_df <- function(agents) {
  if (length(agents) == 0) {
    return(data.frame(Order = integer(), Name = character(), Role = character(),
                      Provider = character(), Reasoning = character(), Evidence = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(seq_along(agents), function(i) {
    a <- agents[[i]]
    data.frame(Order = i, Name = a$name, Role = a$role, Provider = a$provider,
               Reasoning = a$reasoning, Evidence = a$evidence, stringsAsFactors = FALSE)
  }))
}
