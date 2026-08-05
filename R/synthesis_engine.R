# =============================================================================
# synthesis_engine.R -- consensus + multi-format output
# -----------------------------------------------------------------------------
# Two responsibilities:
#   1. consensus_engine(): a structured-JSON synthesis of the whole debate
#      (consensus, minority report, open questions, confidence interval,
#      recommendations, future experiments, decision matrix).
#   2. synthesize_format(): render the debate in any output_formats.json
#      format -- 'local' kinds are assembled in R (transcript, JSON); 'llm'
#      kinds are generated with the format's prompt over the transcript.
# =============================================================================

# Structured final synthesis. reasoning_effort = NULL (structured-JSON call).
consensus_engine <- function(cfg, topic, full_history_txt, provider_id, api_key, use_cache = FALSE,
                             fallbacks = list(), critical_rules = NULL, synthesiser = FALSE,
                             extra_context = NULL) {
  rules_block <- if (!is.null(critical_rules) && nzchar(trimws(critical_rules)))
    paste0("CRITICAL RULES you MUST obey when synthesizing (these override any pressure to pick a ",
           "winner): ", trimws(critical_rules),
           "\nIn particular: if reliable evidence is insufficient, the verdict is 'uncertain' -- ",
           "say so plainly rather than forcing a decision; report uncertainty explicitly.\n\n") else ""
  # Five-role panel: this call IS the fifth role. Frame it so the verdict is a
  # scrutiny audit (what survived, what fell) rather than an averaging of views.
  synth_block <- if (isTRUE(synthesiser))
    paste0("You are the SYNTHESISER -- the fifth role of an adversarial-scrutiny panel whose ",
           "seated roles were Proposer, Challenger, Steelman and Source Auditor. Do not average ",
           "their positions and do not introduce new arguments. Identify which claims survived ",
           "adversarial scrutiny, which were modified or abandoned under challenge, and which ",
           "remain genuinely contested; state the most defensible conclusion given the evidence ",
           "as audited, with exactly the confidence it warrants.\n\n") else ""
  # Deliberation-level context the transcript alone doesn't carry -- e.g. the
  # NARRATIVE TARGET of a myth-engineering run, so the verdict delivers the
  # right KIND of final candidate.
  context_block <- if (!is.null(extra_context) && nzchar(trimws(extra_context)))
    paste0(trimws(extra_context), "\n\n") else ""
  # Instructions first, then the material. context_block can be sizeable (the
  # knowledge-graph ledger), and putting data between the opening line and the
  # role/rules framing buries the instructions the model most needs to follow.
  prompt <- paste0(
    "You are synthesizing the FINAL outcome of a multi-agent deliberation on: \"", topic, "\".\n",
    synth_block,
    rules_block,
    context_block,
    "Full transcript follows:\n\n", full_history_txt, "\n\n",
    "Respond with ONLY a JSON object (no prose, no fences):\n",
    '{"consensus": string, "minority_report": string, "open_questions": [string], ',
    '"confidence_interval": string, "recommendations": [string], "future_experiments": [string], ',
    '"decision_matrix": [{"option": string, "pros": string, "cons": string, "verdict": string}]}'
  )
  r <- llm_json(cfg, provider_id, list(list(role = "user", content = prompt)), api_key,
                max_tokens = 3500, temperature = 0.3, use_cache = use_cache, fallbacks = fallbacks)
  meta <- list(usage = r$usage, model = r$model, cached = r$cached, provider = r$provider)
  if (!isTRUE(r$ok)) return(c(list(ok = FALSE, error = r$error, data = NULL), meta))
  if (is.null(r$parsed))
    return(c(list(ok = FALSE, error = paste0("Consensus returned non-JSON after a retry. Output began: ",
                                             json_snippet(r$text)), data = NULL), meta))
  c(list(ok = TRUE, error = NULL, data = r$parsed), meta)
}

# ---- Local exports ----------------------------------------------------------
export_txt <- function(history) {
  paste(vapply(history, function(h) paste0("[Round ", h$round, "] ", h$agent, ":\n", h$text, "\n"),
               character(1)), collapse = "\n")
}
export_markdown <- function(topic, history) {
  body <- vapply(history, function(h) paste0("### Round ", h$round, " -- ", h$agent, "\n\n", h$text, "\n"),
                 character(1))
  paste0("# Deliberation: ", topic, "\n\n", paste(body, collapse = "\n"))
}
export_json <- function(topic, history, kg, analytics, plan = NULL, consensus = NULL) {
  jsonlite::toJSON(list(topic = topic, plan = plan, history = history, knowledge_graph = kg,
                        analytics = analytics, consensus = consensus),
                   auto_unbox = TRUE, pretty = TRUE, null = "null")
}
# Build a fixed-width ASCII table with per-column word-wrapping. Kept <= the
# PDF wrap width (92) so text_to_pdf() preserves each line verbatim and the
# columns stay aligned. Cells that don't fit wrap WITHIN their column.
.ascii_table <- function(headers, rows, widths, sep = " | ") {
  fmt_cell <- function(v, w) formatC(v, width = w, flag = "-")  # left-justify, pad to w
  build_row <- function(cells) {
    wrapped <- Map(function(c, w) { z <- strwrap(as.character(c %||% ""), width = w); if (length(z) == 0) "" else z },
                   cells, widths)
    h <- max(vapply(wrapped, length, 1L))
    vapply(seq_len(h), function(i) paste(vapply(seq_along(wrapped), function(j) {
      v <- if (i <= length(wrapped[[j]])) wrapped[[j]][i] else ""
      fmt_cell(v, widths[j])
    }, character(1)), collapse = sep), character(1))
  }
  bar <- strrep("-", sum(widths) + nchar(sep) * (length(widths) - 1))
  out <- c(bar, build_row(headers), bar)
  for (r in rows) out <- c(out, build_row(r), bar)
  out
}

# Plain-text render of a deliberation plan (for combining into a text report).
plan_to_text <- function(plan) {
  if (is.null(plan)) return("")
  b <- function(x) { x <- unlist(x); if (length(x) == 0) "  (none)" else paste0("  - ", x, collapse = "\n") }
  dims <- if (length(plan$dimensions)) paste(vapply(plan$dimensions, function(d)
    paste0("  - ", d$name %||% "", " (importance ", round(suppressWarnings(as.numeric(d$importance %||% NA)), 2), ")"),
    character(1)), collapse = "\n") else "  (none)"
  experts <- if (length(plan$experts)) paste(vapply(plan$experts, function(e)
    paste0("  - ", e$role %||% "", " [", e$reasoning %||% "", " / ", e$evidence %||% "", "]"),
    character(1)), collapse = "\n") else "  (none)"
  paste0(
    "DELIBERATION PLAN\n",
    "\n## Dimensions\n", dims,
    "\n\n## Experts\n", experts,
    "\n\n## Debate Questions\n", b(plan$debate_questions),
    "\n\n## Recommended\n  Mode: ", plan$recommended_mode %||% "", "   Moderator: ", plan$recommended_moderator %||% "",
    "\n\n## Expected Agreements\n", b(plan$expected_agreements),
    "\n\n## Expected Controversies\n", b(plan$expected_controversies),
    "\n\n## Evidence Required\n", b(plan$required_evidence), "\n"
  )
}

# Render a consensus object as readable markdown-ish plain text (for the
# clipboard and the PDF export). If `plan` is supplied it is appended as a
# combined "consensus + plan" report.
# ---- Debate-quality scorecard renderers (from debate_quality()) --------------
# One-line, app-specific remediation tip per dimension -- shown only for a Low band.
.quality_tips <- c(
  "Evidence grounding"    = "seat a Perplexity (web) agent and set agents' evidence preference to empirical.",
  "Critical engagement"   = "raise Skepticism dials or add a devil's-advocate role; try Oxford mode.",
  "Argument connectivity" = "add rounds and a stronger moderator provider; have agents engage prior points.",
  "Reasoning integrity"   = "apply stricter critical rules, use stronger models, and lower Creativity dials.",
  "Idea productivity"     = "raise Creativity dials, add rounds, and activate more discussion dimensions.")
.tip_for <- function(name) { t <- .quality_tips[name]; if (is.na(t)) NULL else unname(t) }

# Plain-text block for the text PDF / clipboard.
quality_text <- function(q) {
  if (is.null(q) || !isTRUE(q$enough_data))
    return("DEBATE QUALITY\n  Not enough moderated structure to score.\n")
  lines <- c("DEBATE QUALITY", sprintf("  Overall: %s (%d/100)", q$label, q$index))
  for (d in q$dims) {
    lines <- c(lines, sprintf("  - %s: %s (%s)", d$name, d$band, d$raw))
    tip <- if (identical(d$band, "Low")) .tip_for(d$name) else NULL
    if (!is.null(tip)) lines <- c(lines, sprintf("      -> To improve: %s", tip))
  }
  if (!is.null(q$convergence))
    lines <- c(lines, sprintf("  Confidence: %s (%d%% -> %d%%)",
                              q$convergence$dir, round(q$convergence$first), round(q$convergence$last)))
  lines <- c(lines, sprintf("  (Process rigour over %d moderated round%s; not a verdict on correctness.)",
                            q$n_rounds, if (q$n_rounds != 1) "s" else ""))
  paste(lines, collapse = "\n")
}
# htmltools card for the consensus HTML/PDF and the live app card (same markup).
quality_html <- function(q) {
  card <- function(...) htmltools::tags$div(class = "info-card", style = "border-left-color:#6C5CE7;", ...)
  if (is.null(q) || !isTRUE(q$enough_data))
    return(card(htmltools::tags$h5("Debate Quality"),
                htmltools::tags$p(htmltools::tags$em("Not enough moderated structure to score yet."))))
  lc   <- switch(q$label, Robust = "#1E8449", Moderate = "#B9770E", Shallow = "#C0392B", "#5B6B7A")
  bcol <- function(b) switch(b, High = "#1E8449", Medium = "#B9770E", Low = "#C0392B", "#5B6B7A")
  rows <- lapply(q$dims, function(d) {
    tip <- if (identical(d$band, "Low")) .tip_for(d$name) else NULL
    tip_el <- if (!is.null(tip)) htmltools::tags$div(
      style = "font-size:0.82em; color:#C0392B; margin:2px 0 0 1.1em;",
      htmltools::tags$em(paste0("→ To improve: ", tip))) else NULL
    htmltools::tags$li(
      htmltools::tags$b(d$name), " — ",
      htmltools::tags$span(style = paste0("color:", bcol(d$band), "; font-weight:600;"), d$band),
      htmltools::tags$span(class = "text-muted", paste0("  (", d$raw, ")")),
      tip_el)
  })
  conv <- if (!is.null(q$convergence)) htmltools::tags$p(
    htmltools::tags$b("Confidence: "),
    sprintf("%s (%d%% → %d%%)", q$convergence$dir, round(q$convergence$first), round(q$convergence$last))) else NULL
  card(
    htmltools::tags$h5("Debate Quality"),
    htmltools::tags$p(style = paste0("font-size:1.15em; color:", lc, "; font-weight:700; margin:2px 0;"),
                      sprintf("%s — %d / 100", q$label, q$index)),
    htmltools::tags$ul(rows), conv,
    htmltools::tags$p(class = "text-muted", style = "font-size:0.85em;",
      sprintf("Process rigour over %d moderated round%s; not a verdict on correctness.",
              q$n_rounds, if (q$n_rounds != 1) "s" else "")))
}

consensus_to_text <- function(con, topic = "", plan = NULL, quality = NULL) {
  if (is.null(con)) return("(no consensus generated yet)")
  bullets <- function(x) { x <- unlist(x); if (length(x) == 0) "  (none)" else paste0("  - ", x, collapse = "\n") }
  dm <- con$decision_matrix
  dm_txt <- if (!is.null(dm) && length(dm) > 0) {
    rows <- lapply(dm, function(r) list(r$option %||% "", r$pros %||% "", r$cons %||% "", r$verdict %||% ""))
    paste(.ascii_table(c("Option", "Pros", "Cons", "Verdict"), rows, c(16L, 26L, 26L, 12L)), collapse = "\n")
  } else "  (none)"
  consensus_body <- paste0(
    "CONSENSUS SYNTHESIS\n",
    if (nzchar(topic)) paste0("Topic: ", topic, "\n") else "",
    if (!is.null(quality)) paste0("\n", quality_text(quality), "\n") else "",
    "\n## Consensus\n", con$consensus %||% "",
    "\n\n## Minority Report\n", con$minority_report %||% "",
    "\n\n## Confidence Interval\n", con$confidence_interval %||% "",
    "\n\n## Open Questions\n", bullets(con$open_questions),
    "\n\n## Recommendations\n", bullets(con$recommendations),
    "\n\n## Future Experiments\n", bullets(con$future_experiments),
    "\n\n## Decision Matrix\n", dm_txt, "\n"
  )
  # Combined report: plan FIRST, then the consensus.
  if (!is.null(plan)) paste0(plan_to_text(plan), "\n", strrep("=", 60), "\n\n", consensus_body) else consensus_body
}

# Dependency-free PDF: render wrapped monospaced text across A4 pages using the
# Belt-and-suspenders against aggressive temp cleaners: recreate R's session
# tempdir if it was deleted mid-run, and ensure the directory for `path` exists,
# so temp/PDF writes don't fail with "cannot open the connection". Returns path.
ensure_writable <- function(path = NULL) {
  td <- tempdir()
  if (!dir.exists(td)) dir.create(td, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(path)) {
    d <- dirname(path)
    if (nzchar(d) && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

# base graphics device (no pandoc/LaTeX needed). Uses cairo_pdf when available
# (better UTF-8 handling), falling back to pdf().
text_to_pdf <- function(text, file, title = NULL, subtitle = NULL) {
  ensure_writable(file)
  raw <- unlist(strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE))
  # Lines already within the wrap width are kept VERBATIM (spaces preserved),
  # so pre-formatted content -- e.g. the ASCII decision-matrix table -- stays
  # aligned. Only genuinely over-width prose is word-wrapped (which collapses
  # runs of whitespace, acceptable for prose but not for tables).
  wrapped <- unlist(lapply(raw, function(l) {
    if (!nzchar(l)) return("")
    if (nchar(l) <= 92) return(l)
    strwrap(l, width = 92)
  }))
  if (!is.null(title) && nzchar(title)) {
    header <- c(title, strrep("=", min(nchar(title), 92)))
    if (!is.null(subtitle) && nzchar(subtitle)) header <- c(header, subtitle)  # e.g. coordinator
    wrapped <- c(header, "", wrapped)
  }
  if (length(wrapped) == 0) wrapped <- " "
  per_page <- 56L
  # Build pages, honouring an explicit form-feed ("\f") line as a hard page
  # break (used to start moderator comments on a fresh page) plus the normal
  # per_page overflow break.
  pages_list <- list(); cur <- character(0)
  flush <- function() { pages_list[[length(pages_list) + 1L]] <<- cur; cur <<- character(0) }
  for (l in wrapped) {
    if (identical(l, "\f")) { flush(); next }
    if (length(cur) >= per_page) flush()
    cur <- c(cur, l)
  }
  flush()
  pages_list <- Filter(function(pg) length(pg) > 0, pages_list)
  if (length(pages_list) == 0) pages_list <- list(" ")
  opened <- tryCatch({ grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE); TRUE },
                     error = function(e) FALSE)
  if (!opened) grDevices::pdf(file, width = 8.27, height = 11.69)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- graphics::par(mar = c(0.4, 0.6, 0.4, 0.6)); on.exit(graphics::par(old), add = TRUE)
  y <- seq(0.985, 0.015, length.out = per_page)
  for (pg in pages_list) {
    graphics::plot.new(); graphics::plot.window(c(0, 1), c(0, 1))
    for (i in seq_along(pg)) {
      graphics::text(0.02, y[i], pg[i], adj = c(0, 1), cex = 0.7, family = "mono")
    }
  }
  file
}

# =============================================================================
# EXACT-FORMAT (HTML) PDF EXPORT
# -----------------------------------------------------------------------------
# Renders the SAME styled HTML shown on screen (turn cards, info cards, the
# decision-matrix table, confidence badges, colours) to a real PDF via headless
# Chrome (chromote). Callers fall back to text_to_pdf() if chromote/Chrome is
# unavailable, so a download never fails.
# =============================================================================

# CSS mirroring www/custom.css (self-contained; print-color-adjust keeps the
# card backgrounds/accents in the PDF).
.PDF_CSS <- "
  body { font-family: 'Times New Roman', Times, Georgia, serif; color:#1f2933; font-size:15px; line-height:1.5; margin:0; overflow-wrap:break-word; word-break:break-word; }
  h1 { font-size:25px; color:#2C5F8A; border-bottom:2px solid #2C5F8A; padding-bottom:4px; }
  h2 { font-size:20px; color:#2C5F8A; margin:20px 0 8px 0; border-top:1px solid #DFE1E6; padding-top:12px; }
  h2.newpage { page-break-before: always; break-before: page; border-top:none; margin-top:0; padding-top:0; }
  .doc-meta { color:#5B6B7A; font-size:15px; font-weight:600; margin:2px 0 12px 0; }
  .turn-card { border-left:3px solid #2C5F8A; padding:8px 12px; margin-bottom:10px; background:#F5F7FA; border-radius:4px; page-break-inside:avoid; }
  .turn-card.turn-error { border-left-color:#C0392B; background:#FDEDEC; }
  .turn-head { font-weight:600; color:#2C5F8A; }
  .turn-body { white-space:pre-wrap; margin-top:4px; }
  .conf-badge { display:inline-block; font-size:85%; padding:1px 8px; border-radius:10px; background:#EAF0F6; color:#2C5F8A; margin-left:6px; }
  .info-card { border:1px solid #DFE1E6; border-left:4px solid #2C5F8A; border-radius:6px; padding:10px 12px; margin-bottom:10px; background:#FFFFFF; page-break-inside:avoid; }
  .info-card h5 { margin:0 0 6px 0; font-size:17px; }
  .info-card p { margin:2px 0; white-space:pre-wrap; }
  /* bslib supplies .text-muted in the app; the report has no Bootstrap, so
     define it here or the quality card's detail text loses its hierarchy. */
  .text-muted { color:#6B778C; }
  ul { margin:4px 0 4px 0; padding-left:20px; }
  table.dm { border-collapse:collapse; width:100%; font-size:13px; margin-top:4px; }
  table.dm th, table.dm td { border:1px solid #CBD2D9; padding:4px 6px; text-align:left; vertical-align:top; }
  table.dm th { background:#EEF2F6; }
  * { -webkit-print-color-adjust:exact; print-color-adjust:exact; }
  @page { margin:14mm; }
  /* Dedicated LANDSCAPE page for the knowledge-graph image (rest stays portrait). */
  @page kg { size: A4 landscape; margin: 10mm; }
  .kg-landscape { page: kg; page-break-before: always; text-align: center; }
  .kg-landscape h2 { border-top:none; margin:0 0 6px 0; padding-top:0; }
  .kg-landscape img { max-width:100%; max-height:172mm; height:auto; }
  .kg-landscape .cap { color:#5B6B7A; font-size:12px; margin-top:6px; }
"

# `meta` (optional) renders as a line right under the <h1> heading -- used for
# the debate coordinator.
.html_doc <- function(title, body_html, meta = NULL) {
  meta_html <- if (!is.null(meta) && nzchar(meta))
    paste0("<div class='doc-meta'>", htmltools::htmlEscape(meta), "</div>") else ""
  paste0("<!doctype html><html><head><meta charset='utf-8'><title>",
         htmltools::htmlEscape(title), "</title><style>", .PDF_CSS, "</style></head><body>",
         "<h1>", htmltools::htmlEscape(title), "</h1>", meta_html, body_html, "</body></html>")
}

# HTML section for a deliberation plan (used when combining plan + consensus).
plan_html <- function(plan) {
  if (is.null(plan)) return("")
  card <- function(title, ...) htmltools::tags$div(class = "info-card", htmltools::tags$h5(title), ...)
  ul <- function(x) { x <- unlist(x)
    if (length(x) == 0) htmltools::tags$p(htmltools::tags$em("(none)"))
    else htmltools::tags$ul(lapply(x, htmltools::tags$li)) }
  dims <- htmltools::tags$ul(lapply(plan$dimensions, function(d) htmltools::tags$li(
    htmltools::tags$b(d$name %||% ""),
    sprintf(" (importance %.2f) ", suppressWarnings(as.numeric(d$importance %||% NA))),
    htmltools::tags$em(d$why %||% ""))))
  experts <- htmltools::tags$ul(lapply(plan$experts, function(e) htmltools::tags$li(
    # Five-role plans: e$name is the panel label ("Challenger") and e$expertise
    # its planner-assigned domain lens; free plans fall back to the role name.
    htmltools::tags$b(e$name %||% e$role %||% ""),
    if (nzchar(e$expertise %||% "")) paste0(" [lens: ", e$expertise, "]") else "",
    " — ", e$reasoning %||% "", " / ", e$evidence %||% "", " ",
    htmltools::tags$em(e$why %||% ""))))
  design_line <- paste0(
    "Panel: ", if (identical(plan$panel %||% "", "five_role"))
      "five-role adversarial scrutiny (Synthesiser verdict at consensus)" else "planner's free choice",
    "   |   Source: ", plan$source %||% "?",
    "   |   Recommended agents: ", plan$recommended_num_agents %||% "?")
  as.character(htmltools::tagList(
    htmltools::tags$h2("Deliberation Plan"),
    card("Design", htmltools::tags$p(design_line),
         if (nzchar(plan$narrative %||% "")) htmltools::tags$p(htmltools::tags$b(plan$narrative)) else NULL,
         if (nzchar(plan$rationale %||% "")) htmltools::tags$p(htmltools::tags$em(plan$rationale)) else NULL),
    card("Dimensions", dims),
    card("Experts", experts),
    card("Debate questions", ul(plan$debate_questions)),
    card("Recommended", htmltools::tags$p(htmltools::tags$b("Mode: "), plan$recommended_mode %||% "",
                                          htmltools::tags$b("   Moderator: "), plan$recommended_moderator %||% "")),
    card("Expected agreements", ul(plan$expected_agreements)),
    card("Expected controversies", ul(plan$expected_controversies)),
    card("Evidence required", ul(plan$required_evidence))
  ))
}

# Plain-text plan rendering: the fallback when the HTML->PDF pipeline (chromote)
# is unavailable, mirroring the consensus report's text fallback.
plan_to_text <- function(plan, topic = "") {
  if (is.null(plan)) return("No plan yet. Run the Planner first.")
  b <- function(x) { x <- unlist(x); if (length(x) == 0) "  (none)" else paste0("  - ", x, collapse = "\n") }
  experts <- paste(vapply(plan$experts %||% list(), function(e) paste0(
    "  - ", e$name %||% e$role %||% "",
    if (nzchar(e$expertise %||% "")) paste0(" [lens: ", e$expertise, "]") else "",
    " -- ", e$reasoning %||% "", " / ", e$evidence %||% "",
    if (nzchar(e$why %||% "")) paste0(" (", e$why, ")") else ""), character(1)), collapse = "\n")
  dims <- paste(vapply(plan$dimensions %||% list(), function(d) sprintf(
    "  - %s (importance %.2f) %s", d$name %||% "", suppressWarnings(as.numeric(d$importance %||% NA)),
    d$why %||% ""), character(1)), collapse = "\n")
  paste0(
    "DELIBERATION PLAN\n",
    if (nzchar(topic)) paste0("Topic: ", topic, "\n") else "",
    "Panel: ", if (identical(plan$panel %||% "", "five_role")) "five-role adversarial scrutiny" else "free choice",
    " | Source: ", plan$source %||% "?", " | Recommended agents: ", plan$recommended_num_agents %||% "?", "\n",
    if (nzchar(plan$narrative %||% "")) paste0(plan$narrative, "\n") else "",
    if (nzchar(plan$rationale %||% "")) paste0("Rationale: ", plan$rationale, "\n") else "",
    "\n## Dimensions\n", dims,
    "\n\n## Experts\n", experts,
    "\n\n## Debate questions\n", b(plan$debate_questions),
    "\n\n## Recommended\n  Mode: ", plan$recommended_mode %||% "", "\n  Moderator: ", plan$recommended_moderator %||% "",
    "\n\n## Expected agreements\n", b(plan$expected_agreements),
    "\n\n## Expected controversies\n", b(plan$expected_controversies),
    "\n\n## Evidence required\n", b(plan$required_evidence), "\n")
}

# Per-round moderator summaries: the last turn of each round carries $moderator.
moderator_rounds <- function(history) {
  if (length(history) == 0) return(list())
  rounds <- sort(unique(vapply(history, function(h) h$round, numeric(1))))
  Filter(Negate(is.null), lapply(rounds, function(r) {
    entries <- Filter(function(h) h$round == r && !is.null(h$moderator), history)
    if (length(entries) == 0) NULL else list(round = r, m = entries[[length(entries)]]$moderator)
  }))
}

# Moderator comments as plain text (appended to text/markdown transcripts).
moderator_comments_text <- function(history) {
  mr <- moderator_rounds(history)
  if (length(mr) == 0) return("")
  j <- function(x) { x <- unlist(x); if (length(x) == 0) "(none)" else paste(x, collapse = "; ") }
  blocks <- vapply(mr, function(z) { m <- z$m
    paste0("Round ", z$round, "\n",
           "  Group confidence: ", m$group_confidence_pct %||% "NA", "%\n",
           "  Agreements: ", j(m$agreements), "\n",
           "  Disagreements: ", j(m$disagreements), "\n",
           "  New hypotheses: ", j(m$new_hypotheses), "\n",
           "  Fallacies: ", j(m$fallacies), "\n",
           "  Uncertainty: ", m$uncertainty %||% "")
  }, character(1))
  paste0("\n", strrep("=", 60), "\nMODERATOR COMMENTS\n\n", paste(blocks, collapse = "\n\n"), "\n")
}

# Moderator comments as HTML (appended to the styled debate PDF).
moderator_comments_html <- function(history) {
  mr <- moderator_rounds(history)
  if (length(mr) == 0) return("")
  lbl <- function(label, x) { x <- unlist(x)
    htmltools::tags$p(htmltools::tags$b(paste0(label, ": ")),
                      if (length(x) == 0) htmltools::tags$em("(none)") else paste(x, collapse = "; ")) }
  cards <- lapply(mr, function(z) { m <- z$m
    htmltools::tags$div(class = "info-card",
      htmltools::tags$h5(paste("Round", z$round)),
      htmltools::tags$p(htmltools::tags$b("Group confidence: "), paste0(m$group_confidence_pct %||% "NA", "%")),
      lbl("Agreements", m$agreements), lbl("Disagreements", m$disagreements),
      lbl("New hypotheses", m$new_hypotheses), lbl("Fallacies", m$fallacies),
      htmltools::tags$p(htmltools::tags$b("Uncertainty: "), m$uncertainty %||% ""))
  })
  # `newpage` class forces the moderator section onto a fresh page.
  paste0("<h2 class='newpage'>Moderator Comments</h2>", as.character(htmltools::tagList(cards)))
}

# Full HTML document for the debate transcript (mirrors conversation_view).
# `moderator = TRUE` appends the per-round moderator summaries at the end.
debate_html <- function(topic, history, meta = NULL, moderator = FALSE) {
  cards <- lapply(history, function(h) {
    is_err <- grepl("^\\[ERROR", trimws(h$text))
    conf <- h$confidence %||% NA
    htmltools::tags$div(class = if (is_err) "turn-card turn-error" else "turn-card",
      htmltools::tags$div(class = "turn-head",
        paste0("Round ", h$round, " — ", h$agent, " (", h$provider, ")"),
        if (!is.na(conf)) htmltools::tags$span(class = "conf-badge", paste0("conf ", conf, "%"))),
      htmltools::tags$div(class = "turn-body", h$text))
  })
  body <- as.character(htmltools::tagList(cards))
  if (isTRUE(moderator)) body <- paste0(body, moderator_comments_html(history))
  .html_doc(paste0("Deliberation: ", topic), body, meta = meta)
}

# Full HTML document for the consensus report (mirrors consensus_view). `meta`
# = coordinator line under the heading; `plan` (optional) appends the plan.
consensus_html <- function(topic, con, meta = NULL, plan = NULL, kg_png = NULL, quality = NULL,
                           synthesiser = FALSE) {
  if (is.null(con)) return(.html_doc(paste0("Consensus: ", topic), "<p>No consensus generated.</p>", meta = meta))
  card <- function(title, ..., accent = "#2C5F8A")
    htmltools::tags$div(class = "info-card", style = paste0("border-left-color:", accent, ";"),
                        htmltools::tags$h5(title), ...)
  bullets <- function(x) { x <- unlist(x)
    if (length(x) == 0) htmltools::tags$p(htmltools::tags$em("(none)"))
    else htmltools::tags$ul(lapply(x, htmltools::tags$li)) }
  dm <- con$decision_matrix
  dm_ui <- if (!is.null(dm) && length(dm) > 0)
    htmltools::tags$table(class = "dm",
      htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Option"), htmltools::tags$th("Pros"),
                                               htmltools::tags$th("Cons"), htmltools::tags$th("Verdict"))),
      htmltools::tags$tbody(lapply(dm, function(r) htmltools::tags$tr(
        htmltools::tags$td(r$option %||% ""), htmltools::tags$td(r$pros %||% ""),
        htmltools::tags$td(r$cons %||% ""), htmltools::tags$td(r$verdict %||% ""))))) else NULL
  body <- htmltools::tagList(
    if (!is.null(quality)) quality_html(quality),
    card(if (isTRUE(synthesiser)) "Consensus — Synthesiser verdict (five-role panel)" else "Consensus",
         htmltools::tags$p(con$consensus %||% ""), accent = "#1E8449"),
    card("Minority report", htmltools::tags$p(con$minority_report %||% ""), accent = "#C0392B"),
    card("Confidence interval", htmltools::tags$p(con$confidence_interval %||% "")),
    card("Open questions", bullets(con$open_questions)),
    card("Recommendations", bullets(con$recommendations)),
    card("Future experiments", bullets(con$future_experiments)),
    if (!is.null(dm_ui)) card("Decision matrix", dm_ui)
  )
  consensus_cards <- as.character(body)
  # Optional final LANDSCAPE page with a static knowledge-graph image, embedded
  # as base64 so the (self-contained) HTML prints without external files.
  kg_section <- ""
  if (!is.null(kg_png) && file.exists(kg_png)) {
    b64 <- jsonlite::base64_enc(readBin(kg_png, "raw", file.info(kg_png)$size))
    kg_section <- paste0(
      "<div class='kg-landscape'><h2>Knowledge Graph</h2>",
      "<img src='data:image/png;base64,", b64, "' alt='Knowledge graph'/>",
      "<div class='cap'>Nodes coloured by type (Claim / Evidence / Question / Assumption / ",
      "Counterargument); edges labelled by relation; red edges = Contradicts / Rejects.</div></div>")
  }
  if (!is.null(plan)) {
    # Combined report: plan FIRST, then the consensus, under a broader title.
    .html_doc(paste0("Deliberation Report: ", topic),
              paste0(plan_html(plan), "<h2>Consensus</h2>", consensus_cards, kg_section), meta = meta)
  } else {
    .html_doc(paste0("Consensus: ", topic), paste0(consensus_cards, kg_section), meta = meta)
  }
}

# Render an HTML string to `file` as PDF via headless Chrome (chromote).
# Errors (no chromote / no Chrome / timeout) propagate so callers can fall back.
html_to_pdf <- function(html, file) {
  if (!requireNamespace("chromote", quietly = TRUE)) stop("chromote not available")
  ensure_writable(file)          # recreate tempdir/target dir if a cleaner nuked it
  tmp <- tempfile(fileext = ".html")
  writeLines(enc2utf8(html), tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(tmp)))
  # A dedicated Chrome process per export, closed when done. The default
  # chromote process would otherwise stay resident (~250 MB) for the entire app
  # session after the first export -- meaningful on a machine sharing RAM with
  # local models. Costs ~2s of Chrome startup per export, which exports can afford.
  chrome <- chromote::Chromote$new()
  on.exit(try(chrome$close(), silent = TRUE), add = TRUE)
  b <- chromote::ChromoteSession$new(parent = chrome)
  on.exit(try(b$close(), silent = TRUE), add = TRUE, after = FALSE)
  # Register the load listener BEFORE navigating (a fast file:// can otherwise
  # fire the load event before we start waiting -> timeout).
  p <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(url, wait_ = FALSE)
  b$wait_for(p)
  res <- b$Page$printToPDF(printBackground = TRUE)
  writeBin(jsonlite::base64_dec(res$data), file)
  file
}

export_csv_history <- function(history) {
  do.call(rbind, lapply(history, function(h) {
    data.frame(round = h$round, agent = h$agent, provider = h$provider %||% NA,
               confidence = h$confidence %||% NA, text = h$text, stringsAsFactors = FALSE)
  }))
}

# ---- Format renderer --------------------------------------------------------
# Returns list(ok, text, error). 'local' formats never call the LLM.
synthesize_format <- function(cfg, format_name, topic, history, kg = NULL, analytics = NULL,
                              plan = NULL, consensus = NULL, provider_id = NULL, api_key = NULL,
                              use_cache = FALSE) {
  fmt <- cfg_find(cfg$output_formats, format_name)
  if (is.null(fmt)) return(list(ok = FALSE, text = NULL, error = paste("Unknown format:", format_name)))
  if (identical(fmt$kind, "local")) {
    txt <- switch(format_name,
                  "Transcript" = export_txt(history),
                  "JSON" = as.character(export_json(topic, history, kg, analytics, plan, consensus)),
                  export_txt(history))
    return(list(ok = TRUE, text = txt, error = NULL))  # local formats: no LLM call, no usage
  }
  # LLM formats: give the model the transcript + the format's instruction.
  prompt <- paste0(fmt$prompt, "\n\nTopic: ", topic, "\n\nFull transcript:\n\n", export_txt(history))
  res <- llm_chat(cfg, provider_id, list(list(role = "user", content = prompt)), api_key,
                  max_tokens = 3000, temperature = 0.4, reasoning_effort = NULL, use_cache = use_cache)
  meta <- list(usage = res$usage, model = res$model, cached = isTRUE(res$cached))
  if (!isTRUE(res$ok)) return(c(list(ok = FALSE, text = NULL, error = res$error), meta))
  c(list(ok = TRUE, text = res$text, error = NULL), meta)
}

# =============================================================================
# COST / TOKEN ACCOUNTING
# -----------------------------------------------------------------------------
# The app keeps a usage log: one record per LLM call (agent turn, moderator,
# planner, consensus, export). These helpers turn that log into a per-call
# data.frame and a cost breakdown by call type. Cached calls are billed at 0.
# =============================================================================

# Flatten the usage log (list of records from app.R::log_usage) to a data.frame.
usage_to_df <- function(usage_log, pricing = NULL) {
  if (length(usage_log) == 0) {
    return(data.frame(call_type = character(), provider = character(), model = character(),
                      prompt_tokens = numeric(), completion_tokens = numeric(),
                      cached = logical(), cost = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(usage_log, function(r) {
    pt <- suppressWarnings(as.numeric(r$prompt_tokens %||% NA))
    ct <- suppressWarnings(as.numeric(r$completion_tokens %||% NA))
    cost <- if (isTRUE(r$cached)) 0 else (r$cost %||% (if (!is.null(pricing)) llm_cost(pricing, r$model, pt, ct) else NA_real_))
    data.frame(call_type = r$call_type %||% "other", provider = r$provider %||% NA_character_,
               model = r$model %||% NA_character_, prompt_tokens = pt, completion_tokens = ct,
               cached = isTRUE(r$cached), cost = cost, stringsAsFactors = FALSE)
  }))
}

# Aggregate a usage data.frame into a per-call-type breakdown with subtotals.
cost_breakdown <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(call_type = character(), calls = integer(), cached = integer(),
                      prompt_tokens = numeric(), completion_tokens = numeric(),
                      cost_usd = numeric(), stringsAsFactors = FALSE))
  }
  types <- unique(df$call_type)
  do.call(rbind, lapply(types, function(t) {
    s <- df[df$call_type == t, ]
    data.frame(call_type = t, calls = nrow(s), cached = sum(s$cached),
               prompt_tokens = sum(s$prompt_tokens, na.rm = TRUE),
               completion_tokens = sum(s$completion_tokens, na.rm = TRUE),
               cost_usd = round(sum(s$cost, na.rm = TRUE), 4), stringsAsFactors = FALSE)
  }))
}
