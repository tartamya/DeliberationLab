# =============================================================================
# helper-app.R -- shared setup for every test file
# -----------------------------------------------------------------------------
# testthat sources helper-*.R before any test-*.R, so everything here is in
# scope for all of them.
#
# TWO RULES FOR THIS SUITE:
#   1. NO LIVE API CALLS by default. Tests that need a real provider are
#      skipped unless DELIB_LIVE_TESTS=1, because they cost real money.
#   2. NEVER WRITE INTO THE PROJECT. Anything that touches disk uses a
#      per-test tempdir, so the user's config/, sessions/ and reports/ are
#      read-only as far as the suite is concerned.
# =============================================================================

# renv's autoloader costs ~55s; point straight at the library instead.
.lib <- "C:/R Projects/renv/library/windows/R-4.4/x86_64-w64-mingw32"
if (dir.exists(.lib)) .libPaths(.lib)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- locate the app -------------------------------------------------------
# testthat runs with the working directory at tests/testthat/, but the suite
# must also work when sourced from the project root.
app_dir <- function() {
  for (p in c("../..", ".", "C:/R Projects/Debate Simulator")) {
    if (file.exists(file.path(p, "app.R")) && dir.exists(file.path(p, "R"))) {
      return(normalizePath(p, winslash = "/"))
    }
  }
  stop("cannot locate the app directory from ", getwd())
}
APP <- app_dir()

# ---- load the domain modules (no Shiny server) ----------------------------
# Loaded once into the global env. app.R itself is NOT sourced here: it starts
# a Shiny app object and pulls in API keys. Tests that genuinely need the
# server use load_app_env() below.
.MODULES <- c("config_loader.R", "llm_api.R", "prompt_templates.R", "planner.R",
              "agent_factory.R", "moderator.R", "knowledge_graph.R",
              "debate_engine.R", "synthesis_engine.R", "integrity.R",
              "memory.R", "ui_helpers.R", "reports.R")
for (f in .MODULES) {
  p <- file.path(APP, "R", f)
  if (file.exists(p)) sys.source(p, envir = globalenv(), chdir = TRUE)
}

# The real configuration, read-only. If a test needs to mutate config it must
# copy this object, never the files on disk.
CFG <- load_config(file.path(APP, "config"))

# ---- the full app environment, for tests that need server-scope code ------
# Sourcing app.R defines ui/server plus the module functions in one env.
load_app_env <- function() {
  if (is.null(.test_cache$app_env)) {
    e <- new.env(parent = globalenv())
    old <- getwd(); on.exit(setwd(old), add = TRUE)
    setwd(APP)
    suppressMessages(sys.source(file.path(APP, "app.R"), envir = e, chdir = TRUE))
    .test_cache$app_env <- e
  }
  .test_cache$app_env
}
.test_cache <- new.env(parent = emptyenv())
.test_cache$app_env <- NULL

# ---- guards ---------------------------------------------------------------
# Call at the top of any test that would spend money.
skip_unless_live <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("DELIB_LIVE_TESTS"), "1"),
    "live API test -- set DELIB_LIVE_TESTS=1 to run (costs real money)")
}

# Scratch directory that cleans itself up when the calling test exits.
test_tmpdir <- function(env = parent.frame()) {
  d <- file.path(tempdir(), paste0("delib_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}

# ---- fixtures -------------------------------------------------------------
# A history turn shaped exactly like run_turn() returns one. NOTE the field is
# `text`, not `content` -- getting that wrong silently produces empty
# transcripts, which cost real debugging time in August.
fake_turn <- function(round = 1L, agent = "A", provider = "deepseek",
                      text = "a contribution", ...) {
  c(list(round = as.integer(round), agent = agent, role = "R", provider = provider,
         model = "m", text = text, ok = TRUE, confidence = 70,
         usage = list(prompt_tokens = 100, completion_tokens = 50), cached = FALSE), list(...))
}

fake_history <- function(rounds = 2, agents = c("A", "B")) {
  out <- list()
  for (r in seq_len(rounds)) for (a in agents)
    out[[length(out) + 1]] <- fake_turn(r, a, text = paste("point", r, a))
  out
}

# A knowledge graph with one claim, one evidence node and a supporting edge.
fake_kg <- function() {
  kg <- empty_kg()
  kg_add_round(kg,
    list(list(id = "c1", type = "Claim",    label = "statins reduce events"),
         list(id = "e1", type = "Evidence", label = "trial 2024")),
    list(list(from = "e1", to = "c1", relation = "Supports")), 1)
}
