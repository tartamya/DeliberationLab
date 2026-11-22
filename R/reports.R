# =============================================================================
# reports.R -- output report file management
# -----------------------------------------------------------------------------
# Strategy:
#   #1 Descriptive download filenames  -> handled by out_filename() in app.R.
#   #2 Per-debate BUNDLE               -> one folder per debate under reports/,
#      holding every artifact (transcript, consensus, plan, data, session .rds)
#      plus a manifest.json.
#   #3 A searchable INDEX              -> reports/index.csv, one row per bundle.
# Pure helpers only (no reactivity); the server (app.R) orchestrates content.
# =============================================================================

# Filesystem-safe, lowercase, truncated slug from arbitrary text.
report_slug <- function(text, max_chars = 50) {
  s <- tolower(trimws(as.character(text %||% "")))
  s <- gsub("[^a-z0-9]+", "-", s)          # runs of non-alphanumerics -> one hyphen
  s <- gsub("(^-+)|(-+$)", "", s)          # trim leading/trailing hyphens
  if (!nzchar(s)) s <- "debate"
  s <- substr(s, 1, max_chars)
  # Trim AGAIN after truncating: cutting at max_chars can land on a hyphen
  # (max_chars = 6 on "alpha beta" gave "alpha-"), which would show up in
  # the bundle folder name.
  gsub("-+$", "", s)
}

# Sortable timestamp for folders/filenames: YYYY-MM-DD_HHMM.
report_stamp <- function(t = Sys.time()) format(t, "%Y-%m-%d_%H%M")

# Ensure the reports root exists; return its path.
ensure_reports_dir <- function(root) {
  if (!dir.exists(root)) dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
}

# Create and return a fresh per-debate bundle folder:
#   <root>/<stamp>__<topic-slug>[ _2 _3 ... ]   (numbered if it already exists)
new_report_bundle_dir <- function(root, topic, t = Sys.time()) {
  ensure_reports_dir(root)
  base <- file.path(root, paste0(report_stamp(t), "__", report_slug(topic)))
  folder <- base; i <- 2L
  while (dir.exists(folder)) { folder <- paste0(base, "_", i); i <- i + 1L }
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  folder
}

# Append one row (a 1-row data.frame) to <root>/index.csv, writing the header
# only when the file is first created.
append_report_index <- function(root, row) {
  ensure_reports_dir(root)
  path <- file.path(root, "index.csv")
  first <- !file.exists(path)
  utils::write.table(row, path, sep = ",", row.names = FALSE, col.names = first,
                     append = !first, qmethod = "double", fileEncoding = "UTF-8")
  path
}

# Write the per-bundle manifest.json.
write_report_manifest <- function(folder, manifest) {
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             file.path(folder, "manifest.json"), useBytes = TRUE)
  invisible(file.path(folder, "manifest.json"))
}
