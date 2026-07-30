# =============================================================================
# memory.R -- agent memory + session persistence
# -----------------------------------------------------------------------------
# Agent memory: each agent accumulates what it said, its confidence trajectory,
# and (optionally) challenges/evidence, so later turns and the persona can
# reference the agent's own history.
# Session persistence: the entire deliberation state is saved to / loaded from
# an .rds file so a session can be reloaded and continued. API KEYS ARE NEVER
# PERSISTED.
# =============================================================================

# Append a turn to an agent's memory and confidence history. Returns the
# updated agent. `agents` is the roster; we match by agent id.
update_agent_memory <- function(agents, turn) {
  idx <- which(vapply(agents, function(a) identical(a$id, turn$agent_id), logical(1)))
  if (length(idx) != 1) return(agents)
  a <- agents[[idx]]
  a$memory <- c(a$memory, list(list(round = turn$round, text = turn$text)))
  a$current_position <- turn$text
  if (!is.na(turn$confidence %||% NA)) {
    a$confidence_history <- c(a$confidence_history, turn$confidence)
    a$confidence <- turn$confidence / 100
  }
  agents[[idx]] <- a
  agents
}

# ---- Session save / load ----------------------------------------------------
sessions_dir <- function() {
  d <- file.path(getwd(), "sessions")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# Save a full debate: `state` should carry the configuration AND results (see
# app.R). Writes the full object as <name>.rds plus a lightweight
# <name>.meta.json sidecar so the retrieval list can be shown WITHOUT loading
# every (potentially large) .rds. API keys are never persisted.
save_session <- function(state, name) {
  state$api_keys <- NULL
  state$config$api_keys <- NULL   # belt-and-suspenders: never persist keys
  state$saved_at <- Sys.time()
  safe <- gsub("[^A-Za-z0-9_-]", "_", name)
  path <- file.path(sessions_dir(), paste0(safe, ".rds"))
  saveRDS(state, path)
  rounds <- if (length(state$history)) max(vapply(state$history, function(h) h$round, numeric(1))) else 0
  cost <- sum(vapply(state$usage_log %||% list(), function(r) r$cost %||% 0, numeric(1)), na.rm = TRUE)
  meta <- list(name = name, file = paste0(safe, ".rds"),
               topic = state$topic %||% "",
               saved_at = format(state$saved_at, "%Y-%m-%d %H:%M"),
               rounds = as.integer(rounds), agents = length(state$agents %||% list()),
               cost = round(cost, 4))
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE),
             file.path(sessions_dir(), paste0(safe, ".meta.json")))
  path
}

load_session <- function(file) {
  path <- file.path(sessions_dir(), file)
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

delete_session <- function(file) {
  p <- file.path(sessions_dir(), file)
  if (file.exists(p)) file.remove(p)
  meta <- file.path(sessions_dir(), sub("\\.rds$", ".meta.json", file))
  if (file.exists(meta)) file.remove(meta)
  invisible(TRUE)
}

list_sessions <- function() list.files(sessions_dir(), pattern = "\\.rds$")

# Fast listing of saved debates from the .meta.json sidecars, newest first.
# Falls back to plain .rds names for sessions saved before sidecars existed.
list_session_meta <- function() {
  empty <- data.frame(name = character(), topic = character(), saved_at = character(),
                      rounds = integer(), agents = integer(), cost = numeric(),
                      file = character(), stringsAsFactors = FALSE)
  metas <- list.files(sessions_dir(), pattern = "\\.meta\\.json$", full.names = TRUE)
  rds <- list.files(sessions_dir(), pattern = "\\.rds$")
  rows <- lapply(metas, function(m) {
    j <- tryCatch(jsonlite::fromJSON(readLines(m, warn = FALSE)), error = function(e) NULL)
    if (is.null(j)) return(NULL)
    data.frame(name = j$name %||% "", topic = j$topic %||% "", saved_at = j$saved_at %||% "",
               rounds = j$rounds %||% NA_integer_, agents = j$agents %||% NA_integer_,
               cost = j$cost %||% NA_real_, file = j$file %||% "", stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, c(list(empty), Filter(Negate(is.null), rows)))
  # Include any .rds that lack a sidecar (legacy saves).
  legacy <- setdiff(rds, df$file)
  if (length(legacy) > 0) {
    df <- rbind(df, data.frame(name = sub("\\.rds$", "", legacy), topic = "(legacy save)",
                               saved_at = "", rounds = NA_integer_, agents = NA_integer_,
                               cost = NA_real_, file = legacy, stringsAsFactors = FALSE))
  }
  if (nrow(df) > 1) df <- df[order(df$saved_at, decreasing = TRUE), ]
  df
}
