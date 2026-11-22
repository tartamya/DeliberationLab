# Standard testthat entry point.
#
#   Rscript --vanilla tests/testthat.R          # offline suite (no API calls)
#   DELIB_LIVE_TESTS=1 Rscript --vanilla tests/testthat.R   # + live providers
#
# run_tests.R at the project root is the friendlier wrapper.

.lib <- "C:/R Projects/renv/library/windows/R-4.4/x86_64-w64-mingw32"
if (dir.exists(.lib)) .libPaths(.lib)

library(testthat)

# Resolve the directory holding this file, however it was invoked.
here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
if (is.null(here) || !dir.exists(file.path(here, "testthat"))) {
  here <- if (dir.exists("tests/testthat")) "tests" else "."
}

testthat::test_dir(file.path(here, "testthat"), stop_on_failure = TRUE)
