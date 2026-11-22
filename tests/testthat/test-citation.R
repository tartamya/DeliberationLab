# Citation integrity. These encode a real failure: in the 29 Aug run the Source
# Auditor cited [1]..[21] in round 3 with NO Sources block and NO URLs, while a
# Proposer on a provider that cannot browse produced 15 citations of its own.
# Formatted citations read as more authoritative than plain assertions, so an
# unsourced marker is worse than no marker.

test_that("provider_web_capable is driven by config, not hardcoded ids", {
  expect_true(provider_web_capable(CFG, "perplexity"))   # web_search: native
  expect_false(provider_web_capable(CFG, "grok"))        # optional, live_search off
  expect_false(provider_web_capable(CFG, "mistral"))     # none
  expect_false(provider_web_capable(CFG, "nope"))        # unknown
  expect_false(provider_web_capable(CFG, NULL))          # must not error

  # "optional" must honour the live_search toggle without any R change.
  cfg2 <- CFG
  for (i in seq_along(cfg2$providers))
    if (identical(cfg2$providers[[i]]$id, "grok")) cfg2$providers[[i]]$live_search$enabled <- TRUE
  expect_true(provider_web_capable(cfg2, "grok"))
})

test_that("the four citation statuses are assigned correctly", {
  st <- function(txt, cap) citation_integrity(txt, cap)$status
  expect_equal(st("no citations at all", FALSE), "clean")
  expect_equal(st("Claim [1].\nSources:\n[1] https://a.gov/x", TRUE), "sourced")
  expect_equal(st("See https://example.org/r", TRUE), "sourced")
  expect_equal(st("As shown [1] and [2].", TRUE), "unsourced_markers")
  expect_equal(st("As shown [1] and [2].", FALSE), "unsourced_markers")
  expect_equal(st("See https://example.org/report", FALSE), "uncapable_sources")
  expect_equal(st("Sources: none really", FALSE), "uncapable_sources")
})

test_that("the two failures from the 29 Aug run are both flagged", {
  r3 <- paste(sprintf("Point %d [%d].", 1:21, 1:21), collapse = " ")
  expect_equal(citation_integrity(r3, TRUE)$status, "unsourced_markers")
  expect_true(citation_integrity(r3, TRUE)$suspect)

  steel <- "Analysis. Sources:\n[1] https://a.org\n[2] https://b.org"
  expect_equal(citation_integrity(steel, FALSE)$status, "uncapable_sources")
  # The identical text from a provider that CAN retrieve is legitimate.
  expect_false(citation_integrity(steel, TRUE)$suspect)
})

test_that("citation_integrity never alters the text and never errors", {
  orig <- "Claim [1] with https://x.org and Sources: block"
  ci <- citation_integrity(orig, FALSE)
  # The agent's words are the record; flagging must not rewrite them.
  expect_setequal(names(ci),
                  c("markers", "urls", "has_block", "web_capable", "status", "suspect"))
  expect_false(any(vapply(ci, function(x) is.character(x) && identical(x, orig), logical(1))))
  for (v in list(NULL, NA, "", NA_character_, character(0), c("a", "b"), 123))
    expect_type(citation_integrity(v, TRUE), "list")
  z <- citation_integrity("", TRUE)
  expect_equal(z$markers, 0L)
  expect_equal(z$urls, 0L)
})

test_that("citation_warning names the agent, round and count", {
  expect_equal(citation_warning(citation_integrity("hi", TRUE), "A", 1, "p"), "")
  expect_equal(citation_warning(NULL, "A", 1, "p"), "")
  r3 <- paste(sprintf("P%d [%d].", 1:21, 1:21), collapse = " ")
  w <- citation_warning(citation_integrity(r3, TRUE), "Source Auditor", 3, "perplexity")
  expect_match(w, "Source Auditor", fixed = TRUE)
  expect_match(w, "Round 3", fixed = TRUE)
  expect_match(w, "21", fixed = TRUE)
  w2 <- citation_warning(citation_integrity("https://x.org", FALSE), "Steelman", 1, "mistral")
  expect_match(w2, "mistral", fixed = TRUE)
  expect_match(w2, "fabricat")
})

test_that("citation_summary rolls up and tolerates older sessions", {
  hist <- list(
    list(citation = citation_integrity("Sources:\n[1] https://a.org", TRUE)),
    list(citation = citation_integrity("markers [1][2]", TRUE)),
    list(citation = citation_integrity("https://x.org", FALSE)),
    list(citation = citation_integrity("plain prose", FALSE)))
  cs <- citation_summary(hist)
  expect_equal(cs$turns, 4)
  expect_equal(cs$sourced, 1)
  expect_equal(cs$unsourced_markers, 1)
  expect_equal(cs$uncapable_sources, 1)
  expect_equal(cs$suspect, 2)
  # Sessions saved before the feature carry no citation field at all.
  expect_null(citation_summary(list(list(agent = "X"))))
  expect_null(citation_summary(list()))
  expect_null(citation_summary(NULL))
})

test_that("the scorecard reports citations without folding them into the index", {
  # A run can score High on evidence grounding while its citations are
  # unsourced; averaging the two would hide exactly that.
  h_plain <- fake_history(3)
  kg <- fake_kg()
  q_plain <- debate_quality(lapply(h_plain, function(h) { h$moderator <- list(
      nodes = list(list(id = "c1", type = "Claim", label = "x")), edges = list(),
      disagreements = list(), fallacies = list(), new_hypotheses = list(),
      group_confidence_pct = 60); h }), kg)
  h_cit <- lapply(h_plain, function(h) {
    h$moderator <- list(nodes = list(list(id = "c1", type = "Claim", label = "x")),
      edges = list(), disagreements = list(), fallacies = list(),
      new_hypotheses = list(), group_confidence_pct = 60)
    h$citation <- citation_integrity("markers [1]", TRUE); h })
  q_cit <- debate_quality(h_cit, kg)

  expect_equal(q_plain$index, q_cit$index)          # score untouched
  expect_equal(q_plain$label, q_cit$label)
  expect_null(q_plain$citations)                    # older history: no line
  expect_false(is.null(q_cit$citations))
  expect_match(quality_text(q_cit), "Citations:", fixed = TRUE)
  expect_false(grepl("Citations:", quality_text(q_plain), fixed = TRUE))
})

test_that("the moderator's vocabulary is canonicalised on the way in", {
  # Off-schema tokens are silently dropped by the KG, the claim ledger, Idea
  # Evolution and the scorecard's contradiction counts.
  raw <- list(
    nodes = list(list(id = "n1", type = "claim", label = "lower"),
                 list(id = "n2", type = "COUNTERARGUMENT", label = "upper"),
                 list(id = "n3", type = "Speculation", label = "off-vocabulary"),
                 list(id = "n4", label = "type absent")),
    edges = list(list(from = "n1", to = "n2", relation = "contradicts"),
                 list(from = "n1", to = "n3", relation = "depends on"),
                 list(from = "n2", to = "n3", relation = "Inspires")))
  cn <- canonicalize_extraction(raw)
  ty <- vapply(cn$nodes, function(n) as.character(n$type %||% "<none>"), character(1))
  rl <- vapply(cn$edges, function(e) as.character(e$relation), character(1))
  expect_equal(ty[1], "Claim")
  expect_equal(ty[2], "Counterargument")
  expect_equal(ty[3], "Speculation")      # unknown values pass through untouched
  expect_equal(ty[4], "<none>")
  expect_equal(rl[1], "Contradicts")
  expect_equal(rl[2], "DependsOn")        # letters-only comparison
  expect_equal(rl[3], "Inspires")
  expect_true(is.list(canonicalize_extraction(list())))
  expect_null(canonicalize_extraction(NULL))
})
