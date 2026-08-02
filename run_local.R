# =============================================================================
# run_local.R -- stand-alone launcher for the Deliberation Lab
# -----------------------------------------------------------------------------
# Started by "Run Deliberation Lab.bat". Uses your INSTALLED R plus the project's
# renv package library (no RStudio needed). Run with --vanilla so the slow renv
# autoloader (.Rprofile) is skipped; the library is set manually below.
# =============================================================================

# ---- locate this launcher's own folder (= the app directory) ----------------
.args   <- commandArgs(FALSE)
.script <- sub("^--file=", "", .args[grep("^--file=", .args)])
app_dir <- if (length(.script)) normalizePath(dirname(.script[1])) else normalizePath(getwd())
setwd(app_dir)

# ---- point R at the renv package library ------------------------------------
# The renv library lives one level up (shared "C:\R Projects\renv"); also try a
# project-local one. Pick whichever actually contains the packages (has shiny).
lib_candidates <- c(
  file.path(dirname(app_dir), "renv", "library", "windows", "R-4.4", "x86_64-w64-mingw32"),
  file.path(app_dir,          "renv", "library", "windows", "R-4.4", "x86_64-w64-mingw32")
)
for (l in lib_candidates) if (dir.exists(file.path(l, "shiny"))) { .libPaths(c(normalizePath(l), .libPaths())); break }

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("\n  ERROR: could not find the 'shiny' package library.\n",
      "  Edit run_local.R and set lib_candidates to your renv library path.\n", sep = "")
  Sys.sleep(15); quit(save = "no", status = 1)
}

# ---- pick a free port (base R, no deps) -------------------------------------
free_port <- function(cands = 7788:7830) {
  for (p in cands) {
    con <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(con)) { close(con); return(p) }
  }
  7788
}
port <- free_port()

cat("\n  ===========================================================\n",
    "     DELIBERATION LAB  -  Multi-LLM Deliberation Laboratory\n",
    "  ===========================================================\n\n",
    "  Serving at:  http://127.0.0.1:", port, "\n",
    "  Your browser will open automatically.\n",
    "  KEEP THIS WINDOW OPEN.  Close it (or press Ctrl+C) to stop the app.\n\n", sep = "")

# ---- run the app (opens the browser when ready) -----------------------------
shiny::runApp(app_dir, port = port, host = "127.0.0.1", launch.browser = TRUE)
