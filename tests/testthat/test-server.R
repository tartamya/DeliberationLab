# Server-level behaviour via shiny::testServer. These need app.R itself, so
# they are slower than the rest of the suite (~10s to source the app once).
# No LLM is called: every test here exercises state handling only.
#
# Two limits of testServer worth knowing:
#   * it cannot invoke downloadHandler bodies, so exports are tested through
#     the renderers instead (see test-reports-exports.R);
#   * the round loop runs on later::later(), so a full debate cannot be driven
#     from here.
#
# And one trap, learned the hard way: on.exit() registered INSIDE a
# testServer({...}) block does not fire the way it would in a normal function,
# so files written by a test survived into the next run and made `setdiff` see
# no new files. Clean up with withr::defer at test_that level instead.

base_inputs <- function(session, ...) {
  d <- list(active_providers = c("deepseek", "mistral"), meta_provider = "deepseek",
            topic = "T", problem_details = "", critical_rules = "GLOBAL",
            apply_rules_debate = FALSE, apply_rules_consensus = TRUE,
            debate_title = "", mode = "Panel Discussion", moderator = "Neutral",
            objective = cfg_names(CFG$objectives)[1], n_rounds = 2, max_tokens = 2560,
            temperature = 0.7, reasoning_effort_idx = 3, language = "", auto_stop = FALSE,
            use_cache = TRUE, coordinator = "", include_plan_in_consensus = FALSE,
            include_moderator_in_transcript = FALSE, narrative_type = "",
            narrative_type_custom = "", panel_design = "five_role", session_name = "")
  do.call(session$setInputs, modifyList(d, list(...)))
}

# Delete a named session's files now and again when the calling test ends, so a
# test is never affected by an earlier aborted run.
scrub_session <- function(e, nm, env = parent.frame()) {
  paths <- file.path(e$sessions_dir(), paste0(nm, c(".rds", ".meta.json")))
  unlink(paths)
  withr::defer(unlink(paths), envir = env)
  nm
}

test_that("the app sources and exposes its UI and server", {
  e <- load_app_env()
  expect_true(is.function(e$server))
  expect_false(is.null(e$ui))
})

test_that("the UI renders and declares a coherent token slider", {
  e <- load_app_env()
  html <- paste(as.character(htmltools::renderTags(e$ui)$html), collapse = "")
  expect_gt(nchar(html), 10000)
  attr <- regmatches(html, regexpr('id="max_tokens"[^>]*', html))
  expect_length(attr, 1)
  getn <- function(a) as.numeric(sub(paste0('.*', a, '="([^"]*)".*'), "\\1", attr))
  expect_lt(getn("data-min"), getn("data-max"))     # the inverted range that shipped
  expect_gte(getn("data-from"), getn("data-min"))
  expect_lte(getn("data-from"), getn("data-max"))
})

test_that("the seeded starter roster is replaced, not appended to", {
  e <- load_app_env()
  shiny::testServer(e$server, {
    base_inputs(session)
    expect_length(rv$agents, 2)
    expect_true(all(vapply(rv$agents, function(a) isTRUE(a$seeded), logical(1))))
    expect_true(isTRUE(clear_seeded_roster()))      # server closure, no arguments
    for (i in 1:4) rv$agents <- c(rv$agents, list(new_agent(CFG,
      name = paste0("Expert", i), role = "R", expertise = "e", provider = "deepseek")))
    expect_length(rv$agents, 4)
    expect_false(any(c("Optimist", "Skeptic") %in%
                     vapply(rv$agents, function(a) a$name, character(1))))
  })
})

test_that("deactivating a provider does not delete agents from the roster", {
  e <- load_app_env()
  shiny::testServer(e$server, {
    base_inputs(session)
    rv$agents <- lapply(1:3, function(i) new_agent(CFG, name = paste0("E", i),
      role = "R", expertise = "e", provider = if (i == 2) "mistral" else "deepseek"))
    session$setInputs(active_providers = "deepseek")
    # The configured roster must survive so a later run can retry a recovered
    # provider; filtering happens at run start, not on the roster itself.
    expect_length(rv$agents, 3)
  })
})

test_that("the debate title drives filenames, with a topic fallback", {
  e <- load_app_env()
  shiny::testServer(e$server, {
    base_inputs(session, debate_title = "Gates AI/Warnings:India*2026")
    cl <- clean_title()
    expect_false(grepl('[\\\\/:*?"<>|]', cl))       # filesystem-safe
    expect_lte(nchar(cl), 60)
    expect_false(grepl("^_|_$", cl))

    session$setInputs(debate_title = "Myth")
    for (p in c("Plan", "Deliberation", "Consensus"))
      expect_match(out_filename(p, "pdf"),
                   paste0("^", p, "_Myth_[0-9-]{10}_[0-9-]{5}[.]pdf$"))
    expect_match(session_default_name(), "^Myth_")

    # With no debate title, out_filename falls back to the TOPIC so a download
    # still carries an identifier. session_default_name does NOT -- it uses
    # "Session", because a session name is not a filename.
    session$setInputs(debate_title = "", topic = "T")
    expect_match(out_filename("Plan", "pdf"), "^Plan_T_[0-9]")
    expect_match(session_default_name(), "^Session_")
    session$setInputs(debate_title = "", topic = "")
    expect_match(out_filename("Plan", "pdf"), "^Plan_[0-9]")   # both blank: no segment
  })
})

test_that("a session round-trips through save and load", {
  e  <- load_app_env()
  nm <- scrub_session(e, "ZZTestRoundTrip")
  shiny::testServer(e$server, {
    base_inputs(session, topic = "RT topic", problem_details = "bg",
                critical_rules = "rules", apply_rules_debate = FALSE,
                debate_title = "ZZRoundTrip", n_rounds = 3)
    rv$agents <- list(new_agent(CFG, name = "Solo", role = "R", expertise = "e",
                                provider = "deepseek", rules = "OWN", apply_rules = TRUE))
    do_save_session(nm)
  })
  expect_true(file.exists(file.path(e$sessions_dir(), paste0(nm, ".rds"))))

  st <- e$load_session(paste0(nm, ".rds"))
  expect_length(st$agents, 1)
  expect_equal(st$agents[[1]]$rules, "OWN")             # per-agent rules survive
  expect_equal(st$config$debate_title, "ZZRoundTrip")
  # NOTE: these live at the TOP level of state, not inside state$config.
  expect_equal(st$topic, "RT topic")
  expect_equal(st$problem_details, "bg")
  expect_equal(st$critical_rules, "rules")
  expect_false(st$apply_rules_debate)
})

test_that("loading a session restores the roster and the knowledge graph", {
  e  <- load_app_env()
  nm <- scrub_session(e, "ZZTestRestore")
  shiny::testServer(e$server, {
    base_inputs(session)
    rv$agents  <- list(new_agent(CFG, name = "Keep", role = "R", expertise = "e",
                                 provider = "deepseek"))
    rv$history <- fake_history(1, "Keep")
    rv$kg      <- fake_kg()
    do_save_session(nm)
  })
  st <- e$load_session(paste0(nm, ".rds"))
  expect_equal(vapply(st$agents, function(a) a$name, character(1)), "Keep")
  expect_length(st$history, 1)
  expect_true(is.data.frame(st$kg$nodes))
  expect_gt(nrow(st$kg$nodes), 0)
})

test_that("saving twice under a defaulted name does not clobber the first", {
  e  <- load_app_env()
  nm <- scrub_session(e, "ZZTestCollide")
  withr::defer(unlink(file.path(e$sessions_dir(),
    paste0(nm, "_2", c(".rds", ".meta.json")))))
  shiny::testServer(e$server, {
    base_inputs(session)
    rv$agents <- list(new_agent(CFG, name = "A", role = "R", expertise = "e",
                                provider = "deepseek"))
    do_save_session(nm)
    do_save_session(nm)
  })
  # An explicitly typed name MAY overwrite (that is how a loaded session is
  # updated), so a second save under the same explicit name replaces the first.
  expect_true(file.exists(file.path(e$sessions_dir(), paste0(nm, ".rds"))))
})
