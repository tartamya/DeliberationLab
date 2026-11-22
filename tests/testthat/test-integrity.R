# The Run Integrity report. Each check here corresponds to a way the app has
# actually degraded silently, so the fixtures are deliberately shaped like
# those failures rather than like healthy runs.

agent <- function(nm, prov) new_agent(CFG, name = nm, role = "R", expertise = "e", provider = prov)

test_that("a clean run reports clean", {
  ag <- list(agent("A", "deepseek"), agent("B", "mistral"))
  h <- list(fake_turn(1, "A", "deepseek"), fake_turn(1, "B", "mistral"),
            fake_turn(2, "A", "deepseek"), fake_turn(2, "B", "mistral"))
  h <- lapply(h, function(x) { x$model <- paste0(x$provider, "-m")
                               x$citation <- citation_integrity(x$text, FALSE)
                               x$dropped <- NULL; x })
  ri <- run_integrity(h, ag, CFG, fake_kg(), list(consensus = "c"), NULL, NULL,
                      settings = list(mode = "Panel Discussion"))
  expect_true(ri$clean)
  expect_equal(ri$n_warn, 0)
  expect_gt(ri$n_ok, 4)
  expect_match(integrity_text(ri), "did what it was configured to do", fixed = TRUE)
})

test_that("provider drift is caught and named", {
  # The 29 Aug run: Steelman configured on claude, served by mistral all four
  # rounds, and nothing in the transcript said so.
  ag <- list(agent("Steelman", "claude"))
  h <- lapply(1:4, function(r) { x <- fake_turn(r, "Steelman", "mistral"); x$model <- "mistral-large"; x })
  ri <- run_integrity(h, ag, CFG, settings = list(mode = "Adversarial Scrutiny"))
  f <- Filter(function(x) x$name == "Seat fidelity", ri$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "Steelman", fixed = TRUE)
  expect_match(f$detail, "claude", fixed = TRUE)
  expect_match(f$detail, "mistral", fixed = TRUE)
  expect_false(ri$clean)
})

test_that("two seats collapsing onto one model is flagged", {
  ag <- list(agent("Proposer", "deepseek"), agent("Steelman", "claude"))
  h <- c(lapply(1:2, function(r) { x <- fake_turn(r, "Proposer", "mistral"); x$model <- "mistral-large"; x }),
         lapply(1:2, function(r) { x <- fake_turn(r, "Steelman", "mistral"); x$model <- "mistral-large"; x }))
  ri <- run_integrity(h, ag, CFG)
  f <- Filter(function(x) x$name == "Model diversity", ri$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "Proposer", fixed = TRUE)
  expect_match(f$detail, "Steelman", fixed = TRUE)
  # The point of the warning: their agreement carries less information.
  expect_match(f$detail, "less informative")
})

test_that("a refused parameter is surfaced, and absence is distinguished from silence", {
  ag <- list(agent("A", "openai"))
  h <- lapply(1:2, function(r) { x <- fake_turn(r, "A", "openai")
    x$dropped <- "temperature(0) ignored by this model"; x })
  f <- Filter(function(x) x$name == "Settings honoured",
              run_integrity(h, ag, CFG)$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "temperature", fixed = TRUE)
  expect_match(f$detail, "2 turns", fixed = TRUE)

  # A session saved before refusals were tracked must NOT report a false pass.
  h2 <- lapply(1:2, function(r) fake_turn(r, "A", "openai"))
  f2 <- Filter(function(x) x$name == "Settings honoured",
               run_integrity(h2, ag, CFG)$findings)[[1]]
  expect_equal(f2$status, "info")
  expect_match(f2$detail, "not recorded")
})

test_that("lost retrieval is caught even when the provider can retrieve", {
  # The Source Auditor had sources in rounds 1-2 and none by round 3, while
  # still emitting [1] markers.
  ag <- list(agent("Source Auditor", "perplexity"))
  h <- list(
    fake_turn(1, "Source Auditor", "perplexity", text = "finding\nSources:\n[1] https://a.gov"),
    fake_turn(2, "Source Auditor", "perplexity", text = "finding\nSources:\n[1] https://b.gov"),
    fake_turn(3, "Source Auditor", "perplexity", text = "claims [1] [2] [3] with no sources"))
  f <- Filter(function(x) x$name == "Retrieval", run_integrity(h, ag, CFG)$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "1 of 3", fixed = TRUE)
  expect_match(f$detail, "r3", fixed = TRUE)

  # No retrieval-capable seat at all is information, not a warning.
  ag2 <- list(agent("A", "claude"))
  f2 <- Filter(function(x) x$name == "Retrieval",
               run_integrity(list(fake_turn(1, "A", "claude")), ag2, CFG)$findings)[[1]]
  expect_equal(f2$status, "info")
})

test_that("running a different mode than the plan recommends is reported", {
  # input$mode stays at its default until "Apply recommended mode/moderator"
  # is clicked, which silently loses the blind first round.
  ag <- list(agent("A", "deepseek"))
  h <- list(fake_turn(1, "A", "deepseek"))
  plan <- list(recommended_mode = "Adversarial Scrutiny")
  f <- Filter(function(x) x$name == "Protocol",
              run_integrity(h, ag, CFG, plan = plan,
                            settings = list(mode = "Panel Discussion"))$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "Apply recommended", fixed = TRUE)

  f2 <- Filter(function(x) x$name == "Protocol",
               run_integrity(h, ag, CFG, plan = plan,
                             settings = list(mode = "Adversarial Scrutiny"))$findings)[[1]]
  expect_equal(f2$status, "ok")
})

test_that("a missing synthesis is a warning, not a silence", {
  ag <- list(agent("A", "deepseek")); h <- list(fake_turn(1, "A", "deepseek"))
  f <- Filter(function(x) x$name == "Synthesis", run_integrity(h, ag, CFG)$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "never run", fixed = TRUE)
  f2 <- Filter(function(x) x$name == "Synthesis",
               run_integrity(h, ag, CFG, consensus = list(consensus = "c"))$findings)[[1]]
  expect_equal(f2$status, "ok")
})

test_that("off-schema moderator vocabulary is reported", {
  kg <- empty_kg()
  kg <- kg_add_round(kg, list(list(id = "h1", type = "Hypothesis", label = "x")),
                     list(list(from = "h1", to = "h1", relation = "Challenges")), 1)
  ag <- list(agent("A", "deepseek")); h <- list(fake_turn(1, "A", "deepseek"))
  f <- Filter(function(x) x$name == "Moderator vocabulary",
              run_integrity(h, ag, CFG, kg = kg)$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "Hypothesis", fixed = TRUE)
  expect_match(f$detail, "Challenges", fixed = TRUE)
  # A clean graph passes.
  f2 <- Filter(function(x) x$name == "Moderator vocabulary",
               run_integrity(h, ag, CFG, kg = fake_kg())$findings)[[1]]
  expect_equal(f2$status, "ok")
})

test_that("a cost total containing fallback-priced calls is declared unreliable", {
  ag <- list(agent("A", "openai")); h <- list(fake_turn(1, "A", "openai"))
  ul <- list(list(model = "gpt-5.5", prompt_tokens = 100, completion_tokens = 50),
             list(model = "gpt-6.2-sol", prompt_tokens = 100, completion_tokens = 50))
  f <- Filter(function(x) x$name == "Cost reliability",
              run_integrity(h, ag, CFG, usage_log = ul)$findings)[[1]]
  # gpt-6.2-sol has no pricing entry, so the total is fiction and must say so.
  expect_equal(f$status, "warn")
  expect_match(f$detail, "NOT reliable", fixed = TRUE)
  expect_match(f$detail, "gpt-6.2-sol", fixed = TRUE)

  ul2 <- list(list(model = "gpt-5.5", prompt_tokens = 100, completion_tokens = 50))
  f2 <- Filter(function(x) x$name == "Cost reliability",
               run_integrity(h, ag, CFG, usage_log = ul2)$findings)[[1]]
  expect_equal(f2$status, "ok")
})

test_that("error turns are counted", {
  ag <- list(agent("A", "deepseek"))
  h <- list(fake_turn(1, "A", "deepseek"),
            fake_turn(2, "A", "deepseek", text = "[ERROR: provider exhausted]"))
  f <- Filter(function(x) x$name == "Turn completion", run_integrity(h, ag, CFG)$findings)[[1]]
  expect_equal(f$status, "warn")
  expect_match(f$detail, "1 of 2", fixed = TRUE)
})

test_that("the report survives an empty or degenerate run", {
  expect_silent(ri <- run_integrity(list()))
  expect_equal(ri$turns, 0)
  expect_match(integrity_text(ri), "RUN INTEGRITY", fixed = TRUE)
  expect_silent(run_integrity(NULL, NULL, NULL, NULL, NULL, NULL, NULL))
  # The renderers must tolerate a NULL report rather than erroring in renderUI.
  expect_null(integrity_html(NULL))
  expect_match(integrity_text(NULL), "Nothing to check", fixed = TRUE)
})

test_that("both renderers emit every finding", {
  ag <- list(agent("Steelman", "claude"))
  h <- lapply(1:2, function(r) { x <- fake_turn(r, "Steelman", "mistral"); x$model <- "m"; x })
  ri <- run_integrity(h, ag, CFG, settings = list(mode = "Oxford"))
  txt <- integrity_text(ri)
  html <- paste(as.character(integrity_html(ri)), collapse = "")
  for (f in ri$findings) {
    expect_match(txt, f$name, fixed = TRUE)
    expect_match(html, f$name, fixed = TRUE)
  }
  expect_match(txt, "WARN", fixed = TRUE)
  expect_match(html, "CHECK", fixed = TRUE)      # red badge for a warning
  expect_match(html, "#C0392B", fixed = TRUE)
})

test_that("replaying the real 29 Aug session reproduces its known problems", {
  f <- file.path(APP, "sessions", "Gates_AI_Warnings_India_2026-08-29_07-50.rds")
  skip_if_not(file.exists(f), "the Gates session is not on this machine")
  s <- readRDS(f)
  # Reconstruct the citation field the way the live app now would.
  h <- lapply(s$history, function(x) { x$citation <- citation_integrity(
    x$text, provider_web_capable(CFG, x$provider)); x })
  ri <- run_integrity(h, s$agents, CFG, s$kg, s$consensus, s$usage_log, s$plan,
                      settings = list(mode = "Adversarial Scrutiny"))
  expect_false(ri$clean)
  nm <- vapply(ri$findings, function(x) x$name, character(1))
  st <- setNames(vapply(ri$findings, function(x) x$status, character(1)), nm)
  # Known from that run: the Proposer and Steelman drifted off their
  # providers, and three turns had suspect citations.
  expect_equal(st[["Seat fidelity"]], "warn")
  expect_equal(st[["Citation integrity"]], "warn")
  # Synthesis WAS run on this one, and its moderator vocabulary was clean --
  # unlike the Gandhi session below.
  expect_equal(st[["Synthesis"]], "ok")
  expect_equal(st[["Moderator vocabulary"]], "ok")
})

test_that("replaying the Gandhi session catches its missing synthesis", {
  f <- file.path(APP, "sessions", "Gandhi_thought_2026-08-12_18-23.rds")
  skip_if_not(file.exists(f), "the Gandhi session is not on this machine")
  s <- readRDS(f)
  ri <- run_integrity(s$history, s$agents, CFG, s$kg, s$consensus, s$usage_log, s$plan)
  st <- setNames(vapply(ri$findings, function(x) x$status, character(1)),
                 vapply(ri$findings, function(x) x$name, character(1)))
  # This is the session that saved with no consensus at all.
  expect_equal(st[["Synthesis"]], "warn")
  expect_match(Filter(function(x) x$name == "Synthesis", ri$findings)[[1]]$detail,
               "no consensus", fixed = TRUE)
  # Its moderator invented Hypothesis / Challenges / Refutes, which the claim
  # ledger and the scorecard both drop.
  expect_equal(st[["Moderator vocabulary"]], "warn")
  expect_false(ri$clean)
})
