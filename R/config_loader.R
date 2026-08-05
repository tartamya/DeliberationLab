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
    # Narrative/myth types for narrative-engineering runs. Optional file --
    # absent means the selector simply offers no presets (custom still works).
    narrative_types  = tryCatch(.read_json(file.path(config_dir, "narrative_types.json"))$narrative_types,
                                error = function(e) list()),
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
append_role_to_library <- function(config_dir, role) append_roles_to_library(config_dir, list(role))

# Bulk variant: one read/write for any number of roles (used by the importer).
append_roles_to_library <- function(config_dir, roles) {
  if (length(roles) == 0) return(invisible(NULL))
  path <- file.path(config_dir, "roles.json")
  obj <- jsonlite::fromJSON(readLines(path, warn = FALSE, encoding = "UTF-8"), simplifyVector = FALSE)
  obj$roles <- c(obj$roles, roles)
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
  invisible(path)
}

# ---- Role import -------------------------------------------------------------
# Header aliases -> canonical role field. Deliberately generous: roles are
# usually drafted as a markdown table or a spreadsheet, where the columns get
# called "Role", "Function", "What it must not do" rather than the JSON keys.
.ROLE_FIELD_ALIASES <- c(
  "name" = "name", "role" = "name", "title" = "name", "agent" = "name",
  "category" = "category", "group" = "category", "type" = "category",
  "expertise" = "expertise", "function" = "expertise", "specialism" = "expertise",
  "specialty" = "expertise", "domain" = "expertise", "skills" = "expertise",
  "reasoning" = "reasoning", "reasoningstyle" = "reasoning", "style" = "reasoning",
  "evidence" = "evidence", "evidencepreference" = "evidence", "evidencetype" = "evidence",
  "communication" = "communication", "communicationstyle" = "communication", "tone" = "communication",
  "bias" = "bias", "leaning" = "bias",
  "constraints" = "constraints", "constraint" = "constraints", "mustnot" = "constraints",
  "whatitmustnotdo" = "constraints", "whatitmustnot" = "constraints", "boundary" = "constraints",
  "prohibition" = "constraints", "prohibitions" = "constraints",
  "creativity" = "creativity", "skepticism" = "skepticism", "scepticism" = "skepticism",
  "risktolerance" = "risk_tolerance", "risk" = "risk_tolerance")

.canon_field <- function(h) {
  k <- tolower(gsub("[^A-Za-z]", "", h))          # drop spaces, punctuation, markdown emphasis
  unname(.ROLE_FIELD_ALIASES[k])                   # NA when unrecognized
}

# Coerce one parsed record into the role shape, keeping only known fields.
# Dials are clamped to 0-1 and omitted entirely when absent, so the agent
# factory's own defaults apply.
.normalize_imported_role <- function(r) {
  chr <- function(k) { v <- r[[k]]; if (is.null(v) || length(v) == 0) "" else trimws(as.character(v)[1]) }
  out <- list(
    name = chr("name"), category = chr("category"), expertise = chr("expertise"),
    reasoning = chr("reasoning"), evidence = chr("evidence"),
    communication = chr("communication"), bias = chr("bias"), constraints = chr("constraints"))
  if (!nzchar(out$category))  out$category  <- "Custom"
  if (!nzchar(out$reasoning)) out$reasoning <- "Pragmatist"
  if (!nzchar(out$evidence))  out$evidence  <- "Expert Consensus"
  for (d in c("creativity", "skepticism", "risk_tolerance")) {
    v <- suppressWarnings(as.numeric((r[[d]] %||% NA)[1]))
    if (length(v) == 1 && !is.na(v)) out[[d]] <- max(0, min(1, v))
  }
  out
}

# Parse pasted role definitions. Accepts a JSON array/object, a markdown table,
# or tab/comma-separated rows -- each with a header naming the fields. Returns
# list(roles = <list of role records>, error = <message or NULL>).
parse_roles_text <- function(txt) {
  txt <- trimws(txt %||% "")
  if (!nzchar(txt)) return(list(roles = list(), error = "Nothing pasted."))

  # ---- JSON ----
  if (substr(txt, 1, 1) %in% c("[", "{")) {
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return(list(roles = list(), error = "That looks like JSON but could not be parsed."))
    # Accept a bare array, a single object, or {"roles": [...]}.
    if (!is.null(parsed$roles)) parsed <- parsed$roles
    if (!is.null(parsed$name)) parsed <- list(parsed)
    return(list(roles = lapply(parsed, .normalize_imported_role), error = NULL))
  }

  # ---- Table (markdown / TSV / CSV) ----
  lines <- unlist(strsplit(txt, "\r?\n"))
  lines <- lines[nzchar(trimws(lines))]
  # Markdown separator rows (|---|---|) carry no data.
  lines <- lines[!grepl("^[[:space:]|:+-]+$", lines)]
  if (length(lines) < 2) return(list(roles = list(), error = "Need a header row and at least one role row."))
  sep <- if (any(grepl("|", lines, fixed = TRUE))) "|" else if (any(grepl("\t", lines, fixed = TRUE))) "\t" else ","
  split_row <- function(l) {
    if (sep == "|") l <- gsub("^\\s*\\||\\|\\s*$", "", l)   # strip outer pipes
    trimws(unlist(strsplit(l, sep, fixed = TRUE)))
  }
  header <- split_row(lines[1])
  fields <- vapply(header, .canon_field, character(1))
  if (!any(fields == "name", na.rm = TRUE))
    return(list(roles = list(), error = paste0(
      "No name column found. Header read as: ", paste(header, collapse = " / "),
      ". Rename one column to Name (or Role).")))
  roles <- lapply(lines[-1], function(l) {
    cells <- split_row(l)
    rec <- list()
    for (i in seq_along(fields)) {
      f <- fields[i]
      if (is.na(f) || i > length(cells)) next
      # Strip markdown emphasis from values (**bold**, *italics*).
      rec[[f]] <- gsub("\\*+", "", cells[i])
    }
    .normalize_imported_role(rec)
  })
  roles <- Filter(function(r) nzchar(r$name), roles)
  if (length(roles) == 0) return(list(roles = list(), error = "No rows with a non-empty name."))
  list(roles = roles, error = NULL)
}
