# =============================================================================
# integrity.R -- did this run do what the user configured?
# -----------------------------------------------------------------------------
# Every check here exists because the app once degraded SILENTLY in that exact
# way and the transcript looked perfectly normal afterwards:
#
#   * a Steelman configured on claude ran all four rounds on mistral, and by
#     round 3 two of four seats were the same model -- which is why that run's
#     "critical engagement" score was Low and nothing said so;
#   * gpt-5.x accepts only temperature = 1, so the handler dropped the setting
#     and retried, making a run look reproducible when it was not;
#   * a Source Auditor on a retrieval provider stopped returning sources after
#     round 2 but kept emitting [1]..[21] citation markers;
#   * a session saved with no consensus at all, because synthesis never ran;
#   * 18 models had no pricing entry and were billed at a fictional default.
#
# The data for all of it already existed, scattered across the history, the run
# log and the usage log. This module only COLLECTS it into one place the reader
# cannot miss. It computes nothing new and calls no API.
#
# Pure functions: no reactivity, no file I/O. app.R renders the result.
# =============================================================================

# A finding is one row of the report.
#   status: "ok" | "warn" | "info"   (nothing here is fatal -- the run happened)
.finding <- function(name, status, detail) {
  list(name = name, status = status, detail = detail)
}

# ---- individual checks ------------------------------------------------------

# Which provider actually served each seat, against the roster as configured.
.check_seats <- function(history, agents) {
  if (length(agents) == 0 || length(history) == 0) return(NULL)
  conf <- setNames(vapply(agents, function(a) as.character(a$provider %||% ""), character(1)),
                   vapply(agents, function(a) as.character(a$name %||% ""), character(1)))
  drift <- character(0)
  for (nm in names(conf)) {
    turns <- Filter(function(h) identical(as.character(h$agent %||% ""), nm), history)
    if (length(turns) == 0) next
    served <- table(vapply(turns, function(h) as.character(h$provider %||% "?"), character(1)))
    off <- served[names(served) != conf[[nm]]]
    if (length(off) > 0)
      drift <- c(drift, sprintf("%s configured on %s but ran on %s", nm, conf[[nm]],
                                paste0(names(off), " x", as.integer(off), collapse = ", ")))
  }
  if (length(drift) == 0)
    .finding("Seat fidelity", "ok", "every turn ran on the provider you assigned")
  else
    .finding("Seat fidelity", "warn", paste(drift, collapse = "; "))
}

# Model-family diversity is the mechanism behind the whole panel design: four
# agents on one model share blind spots, so their agreement carries no
# information. Collapse is invisible in the transcript.
.check_diversity <- function(history) {
  if (length(history) == 0) return(NULL)
  by_agent <- split(vapply(history, function(h) as.character(h$model %||% h$provider %||% "?"),
                           character(1)),
                    vapply(history, function(h) as.character(h$agent %||% "?"), character(1)))
  if (length(by_agent) < 2) return(NULL)
  # The model that served the majority of each seat's turns.
  dom <- vapply(by_agent, function(v) names(sort(table(v), decreasing = TRUE))[1], character(1))
  shared <- table(dom)
  shared <- shared[shared > 1]
  if (length(shared) == 0)
    .finding("Model diversity", "ok",
             sprintf("%d seats on %d distinct models", length(dom), length(unique(dom))))
  else
    .finding("Model diversity", "warn",
             paste0(paste(vapply(names(shared), function(m)
               sprintf("%d seats both ran mostly on %s (%s)", as.integer(shared[[m]]), m,
                       paste(names(dom)[dom == m], collapse = " + ")), character(1)),
               collapse = "; "),
               " -- agreement between them is less informative than it looks"))
}

# Parameters the provider refused. run_turn carries these on the turn.
.check_dropped <- function(history) {
  if (length(history) == 0) return(NULL)
  d <- unlist(lapply(history, function(h) h$dropped))
  if (is.null(d)) return(.finding("Settings honoured", "info",
    "not recorded for this run (saved before refusals were tracked)"))
  if (length(d) == 0)
    .finding("Settings honoured", "ok", "no provider refused a parameter")
  else {
    t <- table(d)
    .finding("Settings honoured", "warn",
             paste0(paste(sprintf("%s (%d turns)", names(t), as.integer(t)), collapse = "; "),
                    " -- the request was retried without it"))
  }
}

# Citations with nothing behind them, or sources from a model that cannot browse.
.check_citations <- function(history) {
  cs <- citation_summary(history)
  if (is.null(cs)) return(.finding("Citation integrity", "info",
    "not recorded for this run (saved before citations were checked)"))
  if (cs$suspect == 0)
    .finding("Citation integrity", "ok",
             sprintf("%d turns checked, %d properly sourced, none unsupported",
                     cs$turns, cs$sourced))
  else {
    parts <- c(if (cs$unsourced_markers > 0)
                 sprintf("%d turns cited with nothing behind the markers", cs$unsourced_markers),
               if (cs$uncapable_sources > 0)
                 sprintf("%d turns produced sources from a provider that cannot retrieve",
                         cs$uncapable_sources))
    .finding("Citation integrity", "warn",
             sprintf("%d of %d turns suspect -- %s", cs$suspect, cs$turns,
                     paste(parts, collapse = "; ")))
  }
}

# A seat on a retrieval-capable provider that stopped returning sources mid-run
# is auditing from memory it does not have.
.check_retrieval <- function(history, cfg) {
  if (length(history) == 0 || is.null(cfg)) return(NULL)
  caps <- Filter(function(h) provider_web_capable(cfg, as.character(h$provider %||% "")), history)
  if (length(caps) == 0)
    return(.finding("Retrieval", "info", "no seat ran on a retrieval-capable provider"))
  sourced <- vapply(caps, function(h) grepl("Sources:", as.character(h$text %||% ""), fixed = TRUE),
                    logical(1))
  if (all(sourced))
    .finding("Retrieval", "ok",
             sprintf("all %d turns on a retrieval provider returned sources", length(caps)))
  else {
    miss <- vapply(caps[!sourced], function(h) sprintf("r%s %s", h$round %||% "?", h$agent %||% "?"),
                   character(1))
    .finding("Retrieval", "warn",
             sprintf("%d of %d turns on a retrieval provider returned NO sources (%s)",
                     sum(!sourced), length(caps), paste(miss, collapse = ", ")))
  }
}

# The run reads input$mode, which stays at its default until "Apply recommended
# mode/moderator" is clicked -- skipping it silently loses the blind first round.
.check_protocol <- function(settings, plan) {
  mode <- as.character(settings$mode %||% "")
  if (!nzchar(mode)) return(NULL)
  rec <- as.character(plan$recommended_mode %||% "")
  if (nzchar(rec) && !identical(rec, mode))
    .finding("Protocol", "warn",
             sprintf("ran as '%s' but the plan recommends '%s' -- 'Apply recommended mode/moderator' was not used",
                     mode, rec))
  else
    .finding("Protocol", "ok", sprintf("ran as '%s'", mode))
}

# A session saved without synthesis contains no consensus at all.
.check_synthesis <- function(consensus) {
  if (is.null(consensus))
    .finding("Synthesis", "warn",
             "never run -- there is no consensus in this debate. Run it before saving or exporting")
  else
    .finding("Synthesis", "ok", "produced")
}

# Off-schema node types and edge relations are silently dropped by the claim
# ledger, Idea Evolution and the scorecard's contradiction counts.
.check_vocabulary <- function(kg) {
  if (is.null(kg) || !is.data.frame(kg$nodes) || nrow(kg$nodes) == 0) return(NULL)
  offt <- setdiff(unique(kg$nodes$type), .KG_NODE_TYPES)
  offr <- if (is.data.frame(kg$edges) && nrow(kg$edges) > 0)
    setdiff(unique(kg$edges$relation), .KG_RELATIONS) else character(0)
  if (length(offt) == 0 && length(offr) == 0)
    .finding("Moderator vocabulary", "ok",
             sprintf("%d nodes and %d edges all inside the schema", nrow(kg$nodes),
                     if (is.data.frame(kg$edges)) nrow(kg$edges) else 0))
  else
    .finding("Moderator vocabulary", "warn",
             paste0("off-schema tokens are dropped from the claim ledger and the scorecard: ",
                    paste(c(offt, offr), collapse = ", ")))
}

# A model with no pricing entry bills at pricing$_default, so any total that
# includes it is fiction.
.check_cost <- function(usage_log, cfg) {
  if (length(usage_log) == 0 || is.null(cfg)) return(NULL)
  priced <- names(cfg$pricing$models %||% list())
  used <- vapply(usage_log, function(r) as.character(r$model %||% ""), character(1))
  used <- used[nzchar(used)]
  if (length(used) == 0) return(NULL)
  unpriced <- setdiff(unique(used), priced)
  total <- tryCatch(sum(usage_to_df(usage_log, cfg$pricing)$cost, na.rm = TRUE),
                    error = function(e) NA_real_)
  if (length(unpriced) == 0)
    .finding("Cost reliability", "ok",
             sprintf("$%.4f over %d calls, every model priced", total, length(usage_log)))
  else
    .finding("Cost reliability", "warn",
             sprintf("$%.4f is NOT reliable: %d of %d calls used a model with no pricing entry (%s) and billed at the $%s/$%s fallback",
                     total, sum(used %in% unpriced), length(used),
                     paste(unpriced, collapse = ", "),
                     cfg$pricing[["_default"]]$input %||% "?",
                     cfg$pricing[["_default"]]$output %||% "?"))
}

# Turns that failed outright still sit in the transcript as [ERROR ...].
.check_errors <- function(history) {
  if (length(history) == 0) return(NULL)
  bad <- Filter(function(h) grepl("^\\[ERROR", trimws(as.character(h$text %||% ""))), history)
  if (length(bad) == 0)
    .finding("Turn completion", "ok", sprintf("all %d turns produced content", length(history)))
  else
    .finding("Turn completion", "warn",
             sprintf("%d of %d turns failed and stand as [ERROR] in the transcript (%s)",
                     length(bad), length(history),
                     paste(vapply(bad, function(h) sprintf("r%s %s", h$round %||% "?",
                                  h$agent %||% "?"), character(1)), collapse = ", ")))
}

# ---- the report -------------------------------------------------------------

#' Compare what a run was configured to do with what it actually did.
#'
#' Returns list(findings, n_ok, n_warn, turns, clean). Safe on an empty debate
#' and on sessions saved before any of these fields existed -- those checks
#' report "info / not recorded" rather than a false pass.
run_integrity <- function(history, agents = list(), cfg = NULL, kg = NULL,
                          consensus = NULL, usage_log = NULL, plan = NULL,
                          settings = list()) {
  history <- history %||% list()
  f <- list(
    .check_seats(history, agents),
    .check_diversity(history),
    .check_dropped(history),
    .check_citations(history),
    .check_retrieval(history, cfg),
    .check_protocol(settings, plan),
    .check_synthesis(consensus),
    .check_vocabulary(kg),
    .check_cost(usage_log, cfg),
    .check_errors(history))
  f <- Filter(Negate(is.null), f)
  st <- vapply(f, function(x) x$status, character(1))
  list(findings = f, n_ok = sum(st == "ok"), n_warn = sum(st == "warn"),
       n_info = sum(st == "info"), turns = length(history),
       clean = sum(st == "warn") == 0)
}

# ---- renderers --------------------------------------------------------------

integrity_text <- function(ri) {
  if (is.null(ri) || length(ri$findings) == 0)
    return("RUN INTEGRITY\n  Nothing to check yet.\n")
  mark <- c(ok = "  ok  ", warn = "  WARN", info = "  --  ")
  lines <- c("RUN INTEGRITY",
             sprintf("  %d checks: %d ok, %d need attention%s", length(ri$findings),
                     ri$n_ok, ri$n_warn,
                     if (ri$n_info) sprintf(", %d not recorded", ri$n_info) else ""))
  for (x in ri$findings)
    lines <- c(lines, sprintf("%s %-22s %s", mark[[x$status]], x$name, x$detail))
  if (ri$clean)
    lines <- c(lines, "  This run did what it was configured to do.")
  paste(lines, collapse = "\n")
}

integrity_html <- function(ri) {
  if (is.null(ri) || length(ri$findings) == 0) return(NULL)
  col <- c(ok = "#1E8449", warn = "#C0392B", info = "#8A97A6")
  lab <- c(ok = "OK", warn = "CHECK", info = "n/a")
  rows <- lapply(ri$findings, function(x) htmltools::tags$li(
    style = "margin-bottom:4px; list-style:none;",
    htmltools::tags$span(style = paste0("display:inline-block; min-width:52px; font-weight:700;",
                                        "font-size:0.78em; color:", col[[x$status]], ";"),
                         lab[[x$status]]),
    htmltools::tags$b(x$name), " — ",
    htmltools::tags$span(class = "text-muted", x$detail)))
  htmltools::tags$div(
    class = "info-card",
    style = paste0("border-left-color:", if (ri$clean) col[["ok"]] else col[["warn"]], ";"),
    htmltools::tags$h5("Run Integrity"),
    htmltools::tags$p(style = "margin:2px 0 6px 0;",
      sprintf("%d checks · %d ok · %d need attention", length(ri$findings),
              ri$n_ok, ri$n_warn)),
    htmltools::tags$ul(style = "padding-left:0; margin-bottom:0;", rows),
    if (ri$clean) htmltools::tags$p(class = "text-muted", style = "font-size:0.85em;",
      "This run did what it was configured to do.") else NULL)
}
