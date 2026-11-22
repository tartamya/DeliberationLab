# The output-token budget. Three limits stack (provider ceiling, context window
# minus prompt minus 768 headroom, required multiple) and they are applied at
# exactly one place: effective_output_tokens(). Every token bug this app has
# had came from that arithmetic or from the slider feeding it.

test_that("the budget is clamped by the provider ceiling", {
  expect_equal(effective_output_tokens(CFG, "claude", 99999, 0), 8192L)   # cap 8192
  expect_equal(effective_output_tokens(CFG, "openai", 99999, 0), 16384L)  # cap 16384
  expect_equal(effective_output_tokens(CFG, "openai", 2560, 0), 2560L)    # under the cap
})

test_that("the budget is clamped by the context window, with headroom", {
  # celeris declares context_window 8192 and token_multiple 256.
  big <- effective_output_tokens(CFG, "celeris", 4096, 0)
  tight <- effective_output_tokens(CFG, "celeris", 4096, 7000)
  expect_lt(tight, big)
  expect_equal(tight %% 256, 0)                 # rounded DOWN to the multiple
  # Never returns a non-positive budget even when the prompt exceeds the window.
  expect_gte(effective_output_tokens(CFG, "celeris", 4096, 999999), 1L)
})

test_that("an unknown provider passes the request through unchanged", {
  expect_equal(effective_output_tokens(CFG, "no-such-provider", 1234, 0), 1234L)
})

test_that("provider_max_tokens ignores the context window", {
  # The UI has no prompt in hand, so it asks for the ceiling only.
  expect_equal(provider_max_tokens(CFG, "deepseek", 99999),
               effective_output_tokens(CFG, "deepseek", 99999, 0))
})

test_that("the token slider's range is coherent and covers the measured need", {
  ln <- grep('sliderInput\\("max_tokens"', readLines(file.path(APP, "app.R"), warn = FALSE),
             value = TRUE)
  expect_length(ln, 1)
  num <- function(field) as.numeric(sub(paste0(".*", field, " = ([0-9]+).*"), "\\1", ln))
  mn <- num("min"); mx <- num("max"); val <- num("value")
  expect_lt(mn, mx)                    # the inverted range that shipped
  expect_gte(val, mn)
  expect_lte(val, mx)
  # gpt-5.5 produced output only at 4096+ on a realistic prompt, so a ceiling
  # below that cannot serve a reasoning model at all.
  expect_gte(mx, 4096)
})

test_that("the thinking-token guard fires at the slider default", {
  src <- readLines(file.path(APP, "app.R"), warn = FALSE)
  th <- as.numeric(sub(".*<-\\s*", "", grep("^THINKING_MIN_TOKENS <-", src, value = TRUE)))
  val <- as.numeric(sub('.*value = ([0-9]+).*', '\\1',
                        grep('sliderInput\\("max_tokens"', src, value = TRUE)))
  thinking <- Filter(function(p) isTRUE(p$thinking), CFG$providers)
  fires <- vapply(thinking, function(p) provider_max_tokens(CFG, p$id, val) < th, logical(1))
  # deepseek-reasoner, gpt-5.5 and sarvam-105b each burned ~2500 reasoning
  # tokens at a 2500 budget and returned EMPTY. At the old threshold of 1000
  # the guard warned about none of them.
  expect_true(all(fires),
              info = paste("guard silent for:", paste(vapply(thinking, function(p) p$id,
                           character(1))[!fires], collapse = ", ")))
})

test_that("greedy sampling sends top_p so Mistral accepts it", {
  src <- readLines(file.path(APP, "R", "llm_api.R"), warn = FALSE)
  # Mistral returns 400 "top_p must be 1 when using greedy sampling" at
  # temperature 0 unless top_p is stated. Verified harmless on the other six.
  expect_true(any(grepl("temperature == 0) body$top_p <- 1", src, fixed = TRUE)))
  # ...and clears it again if the provider rejects temperature outright,
  # otherwise top_p would be left behind with no temperature to pair with.
  expect_true(any(grepl("body$temperature <- NULL; body$top_p <- NULL", src, fixed = TRUE)))
})

test_that("a timeout is not retried three times over", {
  # safe_api_post retries RETRYABLE_STATUS_CODES only; a timeout raises and is
  # caught by the outer handler, returning at once. This is what makes a 300s
  # timeout safe -- it costs one period, not three.
  src <- paste(readLines(file.path(APP, "R", "llm_api.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("Network/timeout error:", src, fixed = TRUE))
  expect_true(grepl("status %in% RETRYABLE_STATUS_CODES && attempt <= max_retries", src, fixed = TRUE))
})

test_that("llm_cost prices from the table and falls back explicitly", {
  p <- CFG$pricing
  expect_equal(llm_cost(p, "gpt-5.5", 1e6, 0), p$models[["gpt-5.5"]]$input)
  expect_equal(llm_cost(p, "gpt-5.5", 0, 1e6), p$models[["gpt-5.5"]]$output)
  # An unknown model silently uses pricing$_default -- the reason the suite
  # reports unpriced models in test-config.R.
  expect_equal(llm_cost(p, "no-such-model", 1e6, 0), p[["_default"]]$input)
  expect_equal(llm_cost(p, "gpt-5.5", NA, NA), 0)
  expect_true(is.na(llm_cost(NULL, "gpt-5.5", 1, 1)))
  # Perplexity carries a flat per-request fee on top of tokens.
  expect_gt(llm_cost(p, "sonar", 0, 0), 0)
})
