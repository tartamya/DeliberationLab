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
                      constraints = "") {
  provider <- provider %||% (if (length(cfg$providers)) cfg$providers[[1]]$id else "openai")
  list(
    id = digest::digest(paste(name, role, Sys.time(), runif(1)), algo = "crc32"),
    name = name, role = role, category = category, expertise = expertise,
    reasoning = reasoning, evidence = evidence, communication = communication, bias = bias,
    # what this role must NOT do -- emitted as a hard prohibition in the persona.
    constraints = constraints,
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
agent_from_role <- function(cfg, role_name, provider = NULL, overrides = list()) {
  r <- cfg_find(cfg$roles, role_name)
  base <- if (is.null(r)) {
    # Planner invented a role not in config -- still build a usable agent.
    list(name = role_name, category = "Custom", expertise = role_name,
         reasoning = "Pragmatist", evidence = "Expert Consensus",
         communication = "", bias = "")
  } else r
  new_agent(
    cfg,
    name = overrides$name %||% base$name,
    role = base$name, category = base$category %||% "",
    expertise = base$expertise %||% "",
    reasoning = if (nzchar(overrides$reasoning %||% "")) overrides$reasoning else (base$reasoning %||% "Pragmatist"),
    evidence  = if (nzchar(overrides$evidence  %||% "")) overrides$evidence  else (base$evidence  %||% "Expert Consensus"),
    communication = base$communication %||% "", bias = base$bias %||% "",
    constraints = base$constraints %||% "",
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
  lapply(seq_along(experts), function(i) {
    e <- experts[[i]]
    prov <- providers[((i - 1) %% length(providers)) + 1]
    agent_from_role(cfg, e$role, provider = prov,
                    overrides = list(reasoning = e$reasoning %||% "", evidence = e$evidence %||% "",
                                     goal = e$why %||% ""))
  })
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
  lapply(seq_len(n), function(i) {
    role <- cfg$roles[[sample(length(cfg$roles), 1)]]
    agent_from_role(cfg, role$name, provider = sample(providers, 1),
      overrides = list(
        reasoning = if (length(reasoning)) sample(reasoning, 1) else "",
        evidence  = if (length(evidence))  sample(evidence, 1)  else "",
        creativity = round(runif(1), 2), skepticism = round(runif(1), 2),
        risk_tolerance = round(runif(1), 2), confidence = round(runif(1), 2)))
  })
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
