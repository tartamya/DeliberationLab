# Report bundles, the searchable index, and the export helpers.
# Everything here writes into a per-test tempdir -- the project's own reports/
# and sessions/ folders are never touched.

test_that("report_slug is filesystem-safe and never ends in a hyphen", {
  expect_equal(report_slug("Hello World!"), "hello-world")
  expect_equal(report_slug("a   &&&   b"), "a-b")
  expect_equal(report_slug("--abc--"), "abc")
  expect_equal(report_slug(""), "debate")
  expect_equal(report_slug(NULL), "debate")
  expect_equal(report_slug("!!!???"), "debate")
  expect_lte(nchar(report_slug(strrep("ab ", 60))), 50)
  # Truncation used to land on a hyphen: max_chars = 6 on "alpha beta" gave
  # "alpha-", which showed up in the bundle folder name.
  for (n in c(5, 6, 7, 10, 11, 14))
    expect_false(grepl("-$", report_slug("alpha beta gamma", n)),
                 info = paste("max_chars", n))
  expect_true(nzchar(report_slug("हिंदी topic")))
})

test_that("bundle folders never collide", {
  root <- test_tmpdir()
  t0 <- as.POSIXct("2026-11-20 07:48:00", tz = "UTC")
  d <- vapply(1:3, function(i) new_report_bundle_dir(root, "Gates AI warnings", t = t0),
              character(1))
  expect_length(unique(d), 3)
  expect_true(all(dir.exists(d)))
  expect_match(d[2], "_2$")
  expect_match(d[3], "_3$")
})

test_that("index.csv appends with one header and survives awkward topics", {
  root <- test_tmpdir()
  row <- function(topic) data.frame(saved_at = "2026-11-20 07:48:00", topic = topic,
    mode = "Oxford", moderator = "Neutral", agents = 4L, rounds = 4L,
    est_cost_usd = 0.85, folder = "f", n_files = 8L, stringsAsFactors = FALSE)
  append_report_index(root, row("plain"))
  append_report_index(root, row('He said "yes, maybe", then left'))   # comma + quotes
  append_report_index(root, row("line one\nline two"))                # embedded newline
  path <- file.path(root, "index.csv")
  idx <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(idx), 3)
  expect_equal(ncol(idx), 9)
  expect_equal(sum(grepl('^"?saved_at', readLines(path))), 1)   # header written once
})

test_that("the manifest is valid JSON and keeps files as an array", {
  root <- test_tmpdir()
  d <- new_report_bundle_dir(root, "t")
  m <- write_report_manifest(d, list(topic = "T", files = as.list(c("a", "b"))))
  expect_true(file.exists(m))
  j <- jsonlite::fromJSON(m, simplifyVector = FALSE)
  expect_length(j$files, 2)
  # A one-file bundle must stay an array, not unbox to a bare string.
  m1 <- write_report_manifest(d, list(topic = "T", files = as.list("only")))
  j1 <- jsonlite::fromJSON(m1, simplifyVector = FALSE)
  expect_true(is.list(j1$files))
  expect_length(j1$files, 1)
})

test_that("export helpers tolerate an empty debate", {
  expect_no_error(export_txt(list()))
  expect_no_error(export_csv_history(list()))
  expect_no_error(export_markdown("T", list()))
  expect_no_error(export_json("T", list(), NULL, NULL, NULL, NULL))
  expect_no_error(moderator_comments_text(list()))
})

test_that("export_json emits valid JSON for a real debate", {
  h <- fake_history(2)
  j <- tryCatch(jsonlite::fromJSON(as.character(export_json("T", h, fake_kg(), NULL, NULL, NULL))),
                error = function(e) NULL)
  expect_false(is.null(j))
})

test_that("export_csv_history gives one row per turn", {
  h <- fake_history(3, c("A", "B"))
  df <- export_csv_history(h)
  expect_equal(nrow(df), 6)
  expect_true(all(c("round", "agent", "text") %in% names(df)))
})

test_that("the report renderers survive degenerate input", {
  # Called from download handlers and the save-bundle path, where a NULL plan
  # or a missing consensus is normal rather than exceptional.
  empty_plan <- .normalize_plan(list(), CFG)
  expect_true(nzchar(plan_html(empty_plan)))
  expect_true(nzchar(plan_to_text(empty_plan, "T")))
  expect_equal(plan_html(NULL), "")
  expect_match(plan_to_text(NULL, "T"), "No plan")

  con <- list(consensus = "c", minority_report = "m", confidence_interval = "ci",
              open_questions = list(), recommendations = list(), future_experiments = list())
  expect_true(nzchar(as.character(consensus_html("T", con))))
  expect_true(nzchar(consensus_to_text(con, "T")))
  expect_true(nzchar(as.character(consensus_html("T", NULL))))

  expect_false(debate_quality(NULL, NULL)$enough_data)
  expect_equal(kg_structure_text(NULL), "")
  expect_equal(kg_summary_text(NULL), "(empty)")     # nrow(NULL$nodes) is length-zero
  expect_equal(kg_summary_text(list()), "(empty)")
  expect_null(kg_to_png(NULL, tempfile(fileext = ".png")))
  expect_equal(nrow(analytics_append(NULL, list(), "t")), 0)
  expect_true(nzchar(quality_text(NULL)))
  expect_true(nzchar(as.character(quality_html(NULL))))
})

test_that("analytics_append fills every missing row", {
  # It used to be called once per ROUND while history grew per TURN, so its
  # guard never matched and analytics silently stopped updating.
  h <- fake_history(3, c("A", "B"))      # 6 turns
  a <- analytics_append(NULL, h[1:2], "t")
  expect_equal(nrow(a), 2)
  a <- analytics_append(a, h, "t")
  expect_equal(nrow(a), 6)
  expect_equal(nrow(analytics_append(a, h, "t")), 6)   # idempotent
})

test_that("every plan path produces the fields the UI and reports read", {
  need <- c("dimensions", "experts", "debate_questions", "recommended_mode",
            "recommended_moderator", "mode_rationale", "expected_agreements",
            "expected_controversies", "required_evidence", "recommended_num_agents",
            "rationale", "panel", "source")
  paths <- list(
    llm_free  = .normalize_plan(list(experts = list(list(role = "X"))), CFG),
    heur_free = planner_heuristic("t", CFG),
    llm_five  = .normalize_plan_five_role(list(experts = list()), CFG),
    heur_five = planner_heuristic_five_role("t", CFG))
  for (nm in names(paths))
    expect_equal(setdiff(need, names(paths[[nm]])), character(0), info = nm)
  # The five-role panel fixes mode and moderator; they are not free choices.
  expect_equal(paths$llm_five$recommended_mode, "Adversarial Scrutiny")
  expect_equal(paths$llm_five$panel, "five_role")
})
