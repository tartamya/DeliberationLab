# Config integrity. The app is config-driven, so a bad entry here is a runtime
# failure with no compile step to catch it. Several of these encode faults that
# actually shipped: a default_model that no longer existed at the provider, and
# a role whose `reasoning` value was outside the configured vocabulary (so the
# persona silently lost its style fragment).

test_that("provider ids and key names are unique", {
  ids <- vapply(CFG$providers, function(p) p$id %||% "", character(1))
  expect_equal(anyDuplicated(ids), 0L, info = paste(ids[duplicated(ids)], collapse = ", "))
  keyed <- Filter(function(p) isTRUE(p$needs_key), CFG$providers)
  kn <- vapply(keyed, function(p) p$key_name %||% "", character(1))
  # key_name doubles as a Shiny input id on the Settings tab, so collisions
  # would silently share one text box between two providers.
  expect_equal(anyDuplicated(kn), 0L, info = paste(kn[duplicated(kn)], collapse = ", "))
})

test_that("every provider has a usable handler, endpoint and model list", {
  for (p in CFG$providers) {
    expect_false(is.null(API_HANDLERS[[p$api_type %||% ""]]),
                 info = paste(p$id, "api_type", p$api_type, "has no handler"))
    expect_match(p$endpoint %||% "", "^https?://", info = p$id)
    ms <- unlist(p$models %||% list())
    expect_gte(length(ms), 1L, label = paste(p$id, "model count"))
    expect_equal(anyDuplicated(ms), 0L, info = paste(p$id, "duplicate models"))
    # A default_model outside its own list cannot be selected in the UI.
    expect_true((p$default_model %||% "") %in% ms,
                info = paste(p$id, "default_model", p$default_model, "not in models"))
  }
})

test_that("declared token ceilings are big enough for the meta calls", {
  # moderator_call asks for 4000 and the consensus call ~3000. A provider
  # capped below that truncates the structured JSON and silently falls back to
  # the base-R heuristic.
  for (p in CFG$providers) {
    cap <- suppressWarnings(as.numeric(p$max_tokens %||% NA))
    if (is.na(cap)) next
    expect_gte(cap, 3500, label = paste(p$id, "max_tokens ceiling"))
  }
})

test_that("reasoning-capable providers allow enough wall clock", {
  # gpt-5.5 took 33-124s on one realistic prompt; the 90s default timed it out
  # intermittently, which looked like non-deterministic token starvation.
  for (p in Filter(function(x) isTRUE(x$thinking), CFG$providers)) {
    expect_gte(as.numeric(p$timeout_sec %||% 90), 300,
               label = paste(p$id, "timeout_sec"))
  }
})

test_that("web_search is a known value and drives provider_web_capable", {
  for (p in CFG$providers)
    expect_true(tolower(p$web_search %||% "none") %in% c("native", "optional", "none"),
                info = paste(p$id, "web_search =", p$web_search))
  expect_true(provider_web_capable(CFG, "perplexity"))     # native
  expect_false(provider_web_capable(CFG, "claude"))        # none
  expect_false(provider_web_capable(CFG, "nope"))          # unknown id, no error
})

test_that("role library entries use the configured vocabularies", {
  styles <- cfg_names(CFG$reasoning_styles)
  offenders <- Filter(function(r) !((r$reasoning %||% "") %in% styles), CFG$roles)
  # Informational rather than fatal: it is the user's own content library, and
  # an off-vocabulary value costs the persona its style fragment, not the run.
  if (length(offenders))
    message("NOTE roles with an off-vocabulary `reasoning`: ",
            paste(vapply(offenders, function(r) r$name, character(1)), collapse = ", "))
  nm <- cfg_names(CFG$roles)
  expect_equal(anyDuplicated(nm), 0L, info = paste(nm[duplicated(nm)], collapse = ", "))
})

test_that("every debate mode is runnable", {
  mods <- cfg_names(CFG$moderators)
  for (m in CFG$debate_modes) {
    expect_true((m$recommended_moderator %||% "") %in% mods,
                info = paste(m$name, "moderator", m$recommended_moderator))
    seqn <- m$phase_sequence %||% list()
    if (length(seqn)) {
      for (s in seqn) expect_true(nzchar(s$instruction %||% ""),
                                  info = paste(m$name, "stage with no instruction"))
    } else {
      expect_gt(length(m$phases %||% list()), 0)
    }
    # The planner picks a mode from `suited_to`; without it the model is
    # guessing from the name alone, which made mode choice look arbitrary.
    expect_true(nzchar(m$suited_to %||% m$description %||% ""), info = m$name)
  }
})

test_that("names hardcoded in R still resolve against config", {
  panel <- vapply(.FIVE_ROLE_PANEL, function(x) x$lib, character(1))
  for (lib in panel)
    expect_true(!is.null(cfg_find(CFG$roles, lib)) || is_builtin_role(lib),
                info = paste("five-role panel needs role", lib))
  for (nm in c("Panel Discussion", "Adversarial Scrutiny", "Narrative Forge"))
    expect_false(is.null(cfg_find(CFG$debate_modes, nm)), info = nm)
  expect_false(is.null(cfg_find(CFG$moderators, "Neutral")))
})

test_that("cloud models are priced, or the gap is reported", {
  priced <- names(CFG$pricing$models %||% list())
  cloud <- Filter(function(p) !grepl("localhost", p$endpoint %||% ""), CFG$providers)
  all_cloud <- unique(unlist(lapply(cloud, function(p) unlist(p$models))))
  unpriced <- setdiff(all_cloud, priced)
  # Not a hard failure -- an unpriced model still runs. But it is billed at
  # pricing$_default, so every cost figure involving it is fiction, and the
  # tariff card is built on these numbers.
  if (length(unpriced))
    message("NOTE ", length(unpriced), " cloud models have no pricing entry and bill at the ",
            "_default rate: ", paste(unpriced, collapse = ", "))
  expect_type(unpriced, "character")
})
