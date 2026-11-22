# Agents, rule precedence, and the round -> phase mapping.
# The rule-precedence table below is the one the user designed: a participant's
# own ruleset beats the Planner tab's, and an unticked checkbox means a total
# free hand. Getting it wrong either gags an agent or lets it off the leash.

mk_agent <- function(...) new_agent(CFG, name = "A", role = "A", expertise = "x",
                                    provider = "deepseek", ...)

test_that("per-agent rules take precedence over the global ruleset", {
  G <- "GLOBAL RULES"
  expect_equal(agent_rules(mk_agent(), G), G)                       # inherits
  expect_equal(agent_rules(mk_agent(rules = "MINE"), G), "MINE")    # own wins
  expect_equal(agent_rules(mk_agent(apply_rules = FALSE), G), "")   # opted out
  # Own rules still apply when the global switch is off -- that is the point of
  # a per-participant override.
  expect_equal(agent_rules(mk_agent(rules = "MINE", apply_rules = TRUE), ""), "MINE")
})

test_that("an agent record from before the rules feature still inherits", {
  a <- mk_agent(); a$rules <- NULL; a$apply_rules <- NULL
  expect_equal(agent_rules(a, "GLOBAL"), "GLOBAL")
})

test_that("built-in roles resolve even when absent from the user library", {
  # The five-role panel binds roles by name. Storing app machinery in the
  # user's content library meant curating roles could break the panel.
  for (slot in .FIVE_ROLE_PANEL) {
    a <- agent_from_role(CFG, slot$lib, provider = "deepseek")
    expect_false(is.null(a))
    expect_true(nzchar(a$name %||% ""))
  }
  expect_true(is_builtin_role("Steelman") || !is.null(cfg_find(CFG$roles, "Steelman")))
})

test_that("starter agents are flagged so they can be cleared", {
  # Every add path appends, so unflagged starters silently survived into
  # rosters the user had built: "4 experts" was really 6.
  ag <- default_agents(CFG, c("deepseek"))
  expect_gt(length(ag), 0)
  expect_true(all(vapply(ag, function(a) isTRUE(a$seeded), logical(1))))
})

test_that("roster names are unique", {
  nm <- vapply(random_roster(CFG, c("deepseek"), 6), function(a) a$name, character(1))
  expect_equal(anyDuplicated(nm), 0L, info = paste(nm, collapse = ", "))
})

test_that("select_phase returns an instruction for every mode and round count", {
  gaps <- character(0)
  for (m in CFG$debate_modes) for (n in c(1, 2, 3, 4, 6, 8, 12)) for (r in seq_len(n)) {
    p <- tryCatch(select_phase(m, r, n), error = function(e) NA_character_)
    if (is.na(p) || !nzchar(p)) gaps <- c(gaps, paste0(m$name, " r", r, "/", n))
  }
  expect_equal(gaps, character(0), info = paste(head(gaps, 5), collapse = "; "))
})

test_that("staged modes anchor their first and last stage", {
  staged <- Filter(function(m) length(m$phase_sequence %||% list()) > 0, CFG$debate_modes)
  expect_gt(length(staged), 0)
  for (m in staged) {
    n <- length(m$phase_sequence)
    expect_match(select_phase(m, 1, 9), "Stage 1/", fixed = TRUE, info = m$name)
    expect_match(select_phase(m, 9, 9), paste0("Stage ", n, "/"), fixed = TRUE, info = m$name)
    # Every stage reachable when rounds == stages, so none is skipped.
    seen <- unique(vapply(seq_len(n), function(r) select_phase(m, r, n), character(1)))
    expect_length(seen, n)
    # Fewer rounds than stages must still advance rather than collapse.
    s2 <- vapply(1:2, function(r) select_phase(m, r, 2), character(1))
    expect_false(identical(s2[1], s2[2]), info = m$name)
  }
})

test_that("blind round 1 hides same-round peers", {
  # The engine blinds by FILTERING the history it passes (app.R hist_for_turn),
  # not by blanking the rendered text: history_override_txt = "" is a no-op
  # because nzchar("") is FALSE.
  hist <- list(fake_turn(1, "A", text = "A's view"), fake_turn(1, "B", text = "B's view"))
  ag <- mk_agent()
  flat <- function(ms) paste(vapply(ms, function(m) m$content %||% "", character(1)), collapse = "\n")
  blind <- flat(build_turn_messages(topic = "T",
    history = Filter(function(h) (h$round %||% 0) < 1, hist), kg_summary = "",
    agent = ag, cfg = CFG, phase_instruction = "go", mode_name = "Adversarial Scrutiny",
    objective_fragment = "", round_number = 1, max_tokens = 800))
  expect_false(grepl("A's view", blind, fixed = TRUE))
  expect_true(grepl("(no prior turns)", blind, fixed = TRUE))

  open <- flat(build_turn_messages(topic = "T", history = hist, kg_summary = "",
    agent = ag, cfg = CFG, phase_instruction = "go", mode_name = "Panel Discussion",
    objective_fragment = "", round_number = 2, max_tokens = 800))
  expect_true(grepl("A's view", open, fixed = TRUE))
})

test_that("the claims digest replaces the transcript and drops conclusions", {
  ag <- mk_agent()
  flat <- function(ms) paste(vapply(ms, function(m) m$content %||% "", character(1)), collapse = "\n")
  hist <- list(fake_turn(1, "A", text = "A's view"))
  d <- flat(build_turn_messages(topic = "T", history = hist, kg_summary = "", agent = ag,
    cfg = CFG, phase_instruction = "go", mode_name = "Adversarial Scrutiny",
    objective_fragment = "", round_number = 2, max_tokens = 800,
    history_override_txt = "DIGEST ONLY"))
  expect_true(grepl("DIGEST ONLY", d, fixed = TRUE))
  expect_false(grepl("A's view", d, fixed = TRUE))

  kgh <- list(fake_turn(1, "A", text = "I conclude X is true.",
                        moderator = list(nodes = list(list(label = "claim about X", type = "Claim")))))
  dg <- claims_digest(kgh)
  expect_true(grepl("claim about X", dg, fixed = TRUE))
  expect_false(grepl("I conclude X is true", dg, fixed = TRUE))
  expect_equal(claims_digest(NULL), "")
})

test_that("critical rules and problem details reach the turn prompt", {
  ag <- mk_agent()
  flat <- function(...) paste(vapply(build_turn_messages(topic = "T", history = list(),
    kg_summary = "", agent = ag, cfg = CFG, phase_instruction = "go",
    mode_name = "Panel Discussion", objective_fragment = "", round_number = 1,
    max_tokens = 800, ...), function(m) m$content %||% "", character(1)), collapse = "\n")
  expect_true(grepl("RULE ONE", flat(critical_rules = "RULE ONE"), fixed = TRUE))
  expect_true(grepl("BACKGROUND BLOB", flat(problem_details = "BACKGROUND BLOB"), fixed = TRUE))
  expect_false(grepl("CRITICAL RULES", flat(), fixed = TRUE))   # no empty block
})
