# =============================================================================
# config_loader.R -- loads all config/*.json into a single CONFIG list
# -----------------------------------------------------------------------------
# The whole application is configuration-driven: roles, reasoning styles,
# debate modes, dimensions, moderators, evidence types, output formats,
# objectives and providers all live in config/*.json. This module reads them
# once at startup and exposes accessor helpers. To extend the app you edit
# JSON here -- no R changes for the common cases (new provider that is
# OpenAI-compatible, new role, new mode, new dimension, new output format).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Read API keys from a plain-text `keys.txt` file: one `provider=key` per line,
# blank lines and `#` comments ignored, values trimmed. Returns a named list
# (empty if the file is missing). Keeping keys in a plain text file (in a hidden,
# git-ignored folder) is simpler and safer than an .R source file.
read_keys_file <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # Strip a leading UTF-8 BOM (Notepad often adds one) so the FIRST provider's
  # name isn't silently corrupted -- handle both the decoded char and the
  # raw-bytes (mojibake) form.
  lines <- sub("^﻿", "", lines)
  lines <- sub("^ï»¿", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  keys <- list()
  for (ln in lines) {
    eq <- regexpr("=", ln, fixed = TRUE)
    if (eq > 0) {
      k <- trimws(substr(ln, 1, eq - 1))
      v <- trimws(substr(ln, eq + 1, nchar(ln)))
      if (nzchar(k)) keys[[k]] <- v
    }
  }
  keys
}

# Read one JSON file into a nested list (simplifyVector = FALSE keeps a
# predictable list-of-lists shape regardless of how "rectangular" the data is,
# which avoids jsonlite silently turning some arrays into data.frames and
# others into lists depending on content).
.read_json <- function(path) {
  if (!file.exists(path)) stop("Missing config file: ", path)
  jsonlite::fromJSON(readLines(path, warn = FALSE, encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

# Load every config file into a single list. `config_dir` defaults to the
# config/ folder next to app.R.
load_config <- function(config_dir) {
  cfg <- list(
    providers        = .read_json(file.path(config_dir, "providers.json"))$providers,
    roles            = .read_json(file.path(config_dir, "roles.json"))$roles,
    reasoning_styles = .read_json(file.path(config_dir, "reasoning_styles.json"))$reasoning_styles,
    debate_modes     = .read_json(file.path(config_dir, "debate_modes.json"))$debate_modes,
    dimensions       = .read_json(file.path(config_dir, "domains.json"))$dimensions,
    moderators       = .read_json(file.path(config_dir, "moderators.json"))$moderators,
    evidence_types   = .read_json(file.path(config_dir, "evidence_types.json"))$evidence_types,
    output_formats   = .read_json(file.path(config_dir, "output_formats.json"))$output_formats,
    objectives       = .read_json(file.path(config_dir, "objectives.json"))$objectives,
    # Pricing is optional -- if the file is missing, costs fall back to a zero
    # default so the app still runs (just reports $0).
    pricing          = tryCatch(.read_json(file.path(config_dir, "pricing.json")),
                                error = function(e) list(`_default` = list(input = 0, output = 0), models = list())),
    # Critical rules: an editable, plain-text ruleset (config/critical_rules.txt)
    # seeded into the UI and injected -- as authoritative, persona-overriding
    # constraints -- into every agent turn and the final consensus. Missing file
    # -> empty string (feature simply inactive).
    critical_rules   = tryCatch(paste(readLines(file.path(config_dir, "critical_rules.txt"),
                                                warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                                error = function(e) "")
  )
  cfg
}

# ---- Accessors --------------------------------------------------------------
# Small helpers so the rest of the app never indexes into CONFIG by hand.

# Pull the character vector of a named field across a config list-of-lists.
cfg_names <- function(items, field = "name") {
  vapply(items, function(x) as.character(x[[field]] %||% ""), character(1))
}

# Find one item (list) in a config list-of-lists by its `name` (or another
# key field). Returns NULL if not found.
cfg_find <- function(items, value, field = "name") {
  hit <- Filter(function(x) identical(as.character(x[[field]] %||% ""), as.character(value)), items)
  if (length(hit) == 0) NULL else hit[[1]]
}

# Provider lookups by id.
provider_by_id <- function(cfg, id) cfg_find(cfg$providers, id, field = "id")
provider_ids   <- function(cfg) vapply(cfg$providers, function(p) p$id, character(1))
provider_labels <- function(cfg) {
  setNames(provider_ids(cfg), vapply(cfg$providers, function(p) p$label, character(1)))
}

# Distinct role categories (in first-seen order) for grouping the role picker.
role_categories <- function(cfg) unique(cfg_names(cfg$roles, "category"))

# Roles filtered to one category.
roles_in_category <- function(cfg, category) {
  Filter(function(r) identical(r$category, category), cfg$roles)
}

# Append one role object to config/roles.json (preserving the file's _comment
# and existing roles). `role` is a named list with the fields a roles.json
# entry uses (name, category, expertise, reasoning, evidence, communication,
# bias). Used by the "Save to library" button so custom agents become reusable
# in future sessions without hand-editing JSON.
append_role_to_library <- function(config_dir, role) {
  path <- file.path(config_dir, "roles.json")
  obj <- jsonlite::fromJSON(readLines(path, warn = FALSE, encoding = "UTF-8"), simplifyVector = FALSE)
  obj$roles <- c(obj$roles, list(role))
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
  invisible(path)
}
