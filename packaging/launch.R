# =============================================================================
# launch.R -- portable launcher for the Deliberation Lab
# -----------------------------------------------------------------------------
# Started by "Start Deliberation Lab.bat". It:
#   1. locates its own folder (robust to the current working directory),
#   2. points R at the bundled package library,
#   3. finds a free TCP port,
#   4. prints a splash and opens the browser,
#   5. runs the Shiny app.
# No assumptions about where the user unzipped the folder.
# =============================================================================

# ---- locate this launcher's folder ------------------------------------------
.args   <- commandArgs(FALSE)
.script <- sub("^--file=", "", .args[grep("^--file=", .args)])
root    <- if (length(.script)) normalizePath(dirname(.script[1])) else normalizePath(getwd())
app_dir <- file.path(root, "app")
lib_dir <- file.path(root, "library")

# ---- use the bundled package library first ----------------------------------
if (dir.exists(lib_dir)) .libPaths(c(lib_dir, .libPaths()))

pause_quit <- function(msg, code = 1L) {
  cat("\n  ERROR: ", msg, "\n\n", sep = "")
  Sys.sleep(10); quit(save = "no", status = code)
}
if (!file.exists(file.path(app_dir, "app.R")))
  pause_quit(paste0("could not find app/app.R next to this launcher.\n  Expected: ", app_dir))

# ---- splash -----------------------------------------------------------------
cat("\n",
    "  ===========================================================\n",
    "     DELIBERATION LAB  -  Multi-LLM Deliberation Laboratory\n",
    "  ===========================================================\n\n",
    "  Loading the engine (first start takes ~10-20 seconds)...\n", sep = "")

# ---- find a free port -------------------------------------------------------
free_port <- function(candidates = 7788:7830) {
  for (p in candidates) {
    con <- tryCatch(serverSocket(p), error = function(e) NULL)  # base R, no deps
    if (!is.null(con)) { close(con); return(p) }
  }
  as.integer(sample(20000:60000, 1))  # last resort
}
port <- free_port()

cat("  Opening  http://127.0.0.1:", port, "  in your browser.\n",
    "  KEEP THIS WINDOW OPEN. Close it to stop the app.\n\n", sep = "")

# ---- run --------------------------------------------------------------------
if (!suppressWarnings(suppressPackageStartupMessages(require(shiny, quietly = TRUE))))
  pause_quit(paste0("the 'shiny' package is not in the bundled library.\n  Expected: ", lib_dir))

shiny::runApp(app_dir, port = port, host = "127.0.0.1", launch.browser = TRUE, quiet = TRUE)
