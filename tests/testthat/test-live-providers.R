# LIVE PROVIDER TESTS -- these spend real money and are SKIPPED by default.
#
#   Run them with:  DELIB_LIVE_TESTS=1
#
# Cost is a few cents: each call asks for a one-word reply. Run them after
# changing llm_api.R, after a provider changes its model line-up, or when a run
# starts failing for no obvious reason -- they distinguish "our bug" from
# "their API changed", which took most of a session to work out by hand.

live_key <- function(id) {
  kf <- file.path(APP, ".secrets", "keys.txt")
  if (!file.exists(kf)) return("")
  k <- read_keys_file(kf)
  p <- provider_by_id(CFG, id)
  ev <- Sys.getenv(p$key_env %||% "", "")
  if (nzchar(ev)) ev else as.character(k[[p$key_name %||% id]] %||% "")
}
say_ok <- list(list(role = "user", content = "Reply with exactly: OK"))

test_that("every configured default_model actually answers", {
  skip_unless_live()
  for (p in CFG$providers) {
    if (grepl("localhost", p$endpoint)) next
    k <- live_key(p$id)
    if (!nzchar(k)) { message("skip ", p$id, ": no key"); next }
    r <- llm_chat(CFG, p$id, say_ok, k, model = p$default_model,
                  max_tokens = 2000, temperature = 0.7, use_cache = FALSE)
    expect_true(isTRUE(r$ok),
                info = paste(p$id, "/", p$default_model, ":",
                             substr(as.character(r$error %||% ""), 1, 120)))
  }
})

test_that("every model in every list is still accepted by its provider", {
  skip_unless_live()
  # Catches deprecations: grok-2-latest returned 404, sarvam-30b and
  # sonar-reasoning reported "has been deprecated".
  dead <- character(0)
  for (p in CFG$providers) {
    if (grepl("localhost", p$endpoint)) next
    k <- live_key(p$id); if (!nzchar(k)) next
    for (m in unlist(p$models)) {
      r <- llm_chat(CFG, p$id, say_ok, k, model = m, max_tokens = 2000,
                    temperature = 0.7, use_cache = FALSE)
      e <- as.character(r$error %||% "")
      if (!isTRUE(r$ok) && grepl("deprecat|does not exist|not found", e, ignore.case = TRUE))
        dead <- c(dead, paste0(p$id, "|", m, ": ", substr(e, 1, 70)))
    }
  }
  expect_equal(dead, character(0), info = paste(dead, collapse = " | "))
})

test_that("temperature 0 works on every provider", {
  skip_unless_live()
  # Mistral returns 400 "top_p must be 1 when using greedy sampling" unless
  # top_p is sent; the slider allows temperature 0, so this path is reachable.
  for (id in c("mistral", "grok", "deepseek", "perplexity")) {
    k <- live_key(id); if (!nzchar(k)) next
    r <- llm_chat(CFG, id, say_ok, k, model = provider_by_id(CFG, id)$default_model,
                  max_tokens = 2000, temperature = 0, use_cache = FALSE)
    expect_true(isTRUE(r$ok),
                info = paste(id, "at temperature 0:", substr(as.character(r$error %||% ""), 1, 120)))
  }
})

test_that("a refused parameter is reported rather than silently dropped", {
  skip_unless_live()
  k <- live_key("openai")
  skip_if_not(nzchar(k), "no openai key")
  # gpt-5.x/6.x accept only temperature = 1. The handler drops the setting and
  # retries, which is right -- but it must say so, or a run looks reproducible
  # when it is not.
  r <- llm_chat(CFG, "openai", say_ok, k, model = "gpt-5.5",
                max_tokens = 2000, temperature = 0, use_cache = FALSE)
  expect_true(isTRUE(r$ok))
  expect_gt(length(r$dropped), 0)
  expect_true(any(grepl("temperature", r$dropped, fixed = TRUE)))
})

test_that("a reasoning model completes a realistic turn within its timeout", {
  skip_unless_live()
  k <- live_key("openai")
  skip_if_not(nzchar(k), "no openai key")
  # The real failure: gpt-5.5 needs 33-124s on a prompt of this size, so the
  # old 90s default timed it out intermittently.
  ag <- new_agent(CFG, name = "Source Auditor", role = "Empirical Evidence Auditor",
                  expertise = "cybersecurity and biosecurity risk evidence", provider = "openai")
  ag$model <- "gpt-5.5"
  tn <- run_turn(CFG, ag, "Audit the claim.", "AI displacement", list(), empty_kg(),
                 "Adversarial Scrutiny", "", "", 1, k, 6144, 0.7, "medium", use_cache = FALSE)
  expect_true(isTRUE(tn$ok), info = substr(as.character(tn$error %||% ""), 1, 150))
  expect_gt(nchar(as.character(tn$text %||% "")), 200)
})

test_that("the retrieval-capable provider returns real sources", {
  skip_unless_live()
  k <- live_key("perplexity")
  skip_if_not(nzchar(k), "no perplexity key")
  # Without retrieval the Source Auditor cannot audit a topic newer than its
  # training data -- it reconstructs plausible claims instead.
  r <- llm_chat(CFG, "perplexity", list(list(role = "user",
        content = "What did Bill Gates publish about AI in August 2026? Cite sources.")),
        k, model = "sonar", max_tokens = 2000, temperature = 0.7, use_cache = FALSE)
  expect_true(isTRUE(r$ok))
  ci <- citation_integrity(r$text, provider_web_capable(CFG, "perplexity"))
  expect_equal(ci$status, "sourced")
  expect_gt(ci$urls, 0)
})
