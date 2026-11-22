# =============================================================================
# run_tests.R -- run the test suite in one command
# -----------------------------------------------------------------------------
#   Rscript --vanilla run_tests.R              offline suite, no API calls, free
#   Rscript --vanilla run_tests.R live         + live provider checks (costs a
#                                              few cents; use after changing
#                                              llm_api.R or when a provider
#                                              changes its model line-up)
#   Rscript --vanilla run_tests.R <pattern>    only files matching the pattern,
#                                              e.g. `tokens` or `citation`
#
# --vanilla matters: it skips renv's autoloader, which costs ~55s here.
# =============================================================================

.lib <- "C:/R Projects/renv/library/windows/R-4.4/x86_64-w64-mingw32"
if (dir.exists(.lib)) .libPaths(.lib)
suppressMessages(library(testthat))

args    <- commandArgs(trailingOnly = TRUE)
live    <- "live" %in% args
pattern <- setdiff(args, "live")

if (live) {
  Sys.setenv(DELIB_LIVE_TESTS = "1")
  cat("LIVE MODE: provider tests will run and WILL spend money.\n\n")
} else {
  cat("Offline suite. Live provider tests are skipped",
      "(pass `live` to include them).\n\n")
}

root <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
dir  <- file.path(root, "tests", "testthat")
if (!dir.exists(dir)) stop("cannot find tests/testthat under ", root)

t0 <- Sys.time()
res <- testthat::test_dir(
  dir,
  filter = if (length(pattern)) paste(pattern, collapse = "|") else NULL,
  reporter = "progress",
  stop_on_failure = FALSE)

df <- as.data.frame(res)
fail <- sum(df$failed) + sum(df$error)
cat(sprintf("\n%s\n  %d passed | %d failed | %d skipped | %d warnings | %.0fs\n",
            strrep("-", 62), sum(df$passed), fail, sum(df$skipped), sum(df$warning),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (fail > 0) {
  cat("  FAILING FILES:\n")
  for (i in which(df$failed + df$error > 0)) cat("    ", df$file[i], "\n")
}
quit(status = if (fail > 0) 1 else 0)
