# =============================================================================
# GENERIC MULTI-LLM DELIBERATION LABORATORY
# =============================================================================
# A configuration-driven Shiny platform for structured multi-agent LLM
# deliberation. Nothing here knows anything topic-specific: the Planner infers
# the shape of each deliberation, and roles / reasoning styles / debate modes /
# dimensions / moderators / evidence types / output formats / providers all
# live in config/*.json.
#
# LAYERS:   Topic -> Planner -> Debate Engine -> Synthesis Engine
#
# This app.R is the INTEGRATION layer. Domain logic lives in R/*.R:
#   config_loader . llm_api . prompt_templates . planner . agent_factory
#   moderator . knowledge_graph . debate_engine . synthesis_engine . memory
#   ui_helpers
#
# Concurrency: cooperative-sequential. Agents speak in order (each sees prior
# turns); the server yields to Shiny between rounds via later::later() so the
# UI stays live and Stop works mid-run.
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(visNetwork)
library(igraph)
library(later)
library(httr2)
library(jsonlite)
library(digest)

options(shiny.maxRequestSize = 30 * 1024^2)
options(shiny.launch.browser = TRUE)
# ---- Source modules (order matters: config -> utils -> engines) -------------
APP_DIR    <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!dir.exists(file.path(APP_DIR, "R"))) APP_DIR <- getwd()
CONFIG_DIR <- file.path(APP_DIR, "config")

for (f in c("config_loader.R", "llm_api.R", "prompt_templates.R", "planner.R",
            "agent_factory.R", "moderator.R", "knowledge_graph.R", "debate_engine.R",
            "synthesis_engine.R", "memory.R", "ui_helpers.R")) {
  # local = TRUE sources into THIS (the app's) environment. Under runApp the
  # app body runs in its own env, not globalenv -- so sourcing modules/secrets
  # here (rather than into globalenv with local = FALSE) is what lets
  # resolve_api_key() and the server see API_KEYS and the module functions.
  source(file.path(APP_DIR, "R", f), local = TRUE)
}

# ---- Load configuration + secrets ------------------------------------------
CONFIG <- load_config(CONFIG_DIR)

# API_KEYS resolution order (first hit wins):
#   1. .secrets/keys.txt        -- plain `provider=key` lines, hidden + git-ignored
#   2. config/secrets.R         -- legacy R file (still supported as a fallback)
#   3. config/secrets.example.R -- blank template (so the app still runs keyless)
# resolve_api_key() (below) additionally lets an environment variable or a key
# typed in the Settings tab override these at runtime.
API_KEYS <- list()
keys_txt <- file.path(APP_DIR, ".secrets", "keys.txt")
if (file.exists(keys_txt)) {
  API_KEYS <- read_keys_file(keys_txt)
} else if (file.exists(file.path(CONFIG_DIR, "secrets.R"))) {
  source(file.path(CONFIG_DIR, "secrets.R"), local = TRUE)
} else if (file.exists(file.path(CONFIG_DIR, "secrets.example.R"))) {
  source(file.path(CONFIG_DIR, "secrets.example.R"), local = TRUE)
}

# Resolve a provider's key: env var > runtime UI key > secrets file.
resolve_api_key <- function(cfg, provider_id, ui_keys = list()) {
  prov <- provider_by_id(cfg, provider_id)
  if (is.null(prov)) return("")
  env_val <- if (nzchar(prov$key_env %||% "")) Sys.getenv(prov$key_env) else ""
  if (nzchar(env_val)) return(env_val)
  kn <- prov$key_name %||% provider_id
  if (!is.null(ui_keys[[kn]]) && nzchar(ui_keys[[kn]])) return(ui_keys[[kn]])
  if (exists("API_KEYS") && !is.null(API_KEYS[[kn]]) && nzchar(API_KEYS[[kn]])) return(API_KEYS[[kn]])
  ""
}

# Providers usable at startup (needs no key, or a key resolves). Drives the
# default Active Providers selection.
available_providers <- function(cfg, ui_keys = list()) {
  Filter(function(id) {
    prov <- provider_by_id(cfg, id)
    !isTRUE(prov$needs_key) || nzchar(resolve_api_key(cfg, id, ui_keys))
  }, provider_ids(cfg))
}

REASONING_EFFORT_LEVELS <- c("minimal", "low", "medium", "high")
THINKING_MIN_TOKENS <- 1000

# Providers enabled by default: those with a usable key, EXCLUDING `local`
# (needs_key = FALSE, but points at a localhost server that may not be
# running -- defaulting agents onto it would make every turn error) and also
# EXCLUDING `openai` and `claude` (left unchecked by default -- opt in when
# wanted). Each exclusion falls back gracefully if it would leave nothing:
# if only local / only openai+claude are available, they are used so the app
# always has a sensible default selection.
DEFAULT_ACTIVE <- local({
  av   <- available_providers(CONFIG)
  pref <- setdiff(av, c("local", "gpt4all"))   # local servers: opt-in (may be off)
  # openai/claude (premium) and perplexity (per-request web-search fee) are left
  # unchecked by default -- opt in when wanted.
  pref_no_premium <- setdiff(pref, c("openai", "claude", "perplexity"))
  if (length(pref_no_premium)) pref_no_premium
  else if (length(pref)) pref
  else if (length(av)) av
  else provider_ids(CONFIG)[1]
})

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "Deliberation Lab",
  id = "navbar",
  theme = app_theme(),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    # Client-side clipboard copy. Triggered from the server via
    # session$sendCustomMessage("clipboard_copy", <text>). Uses the async
    # Clipboard API in a secure context (localhost counts) and falls back to
    # a hidden-textarea execCommand for older/non-secure contexts.
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('clipboard_copy', function(text){",
      "  if (navigator.clipboard && window.isSecureContext){",
      "    navigator.clipboard.writeText(text);",
      "  } else {",
      "    var ta=document.createElement('textarea'); ta.value=text;",
      "    ta.style.position='fixed'; ta.style.top='-1000px'; ta.style.opacity='0';",
      "    document.body.appendChild(ta); ta.focus(); ta.select();",
      "    try{document.execCommand('copy');}catch(e){}",
      "    document.body.removeChild(ta);",
      "  }",
      "});"
    ))
  ),
  fillable = FALSE,

  # ---- Persistent sidebar: core run controls, shared by every tab ---------
  sidebar = sidebar(
    width = 300,
    h5("Providers"),
    checkboxGroupInput("active_providers", "Active providers",
                       choices = provider_choices(CONFIG),
                       selected = DEFAULT_ACTIVE),
    actionButton("test_keys_sidebar", "Test keys (disable failing)",
                 class = "btn-sm btn-outline-secondary", width = "100%"),
    uiOutput("key_test_result_sidebar"),
    selectInput("meta_provider", "Planner / Moderator / Synthesis provider",
                # Perplexity is omitted here: its web models aren't tuned for the
                # strict-JSON meta calls (it stays selectable as an agent provider).
                choices = local({ ch <- provider_choices(CONFIG); ch[ch != "perplexity"] }),
                # Moderator (and planner/synthesis) default to Sarvam when it has
                # a key -- cheap and steady for the structured-JSON meta calls --
                # otherwise the first available provider.
                selected = if ("sarvam" %in% DEFAULT_ACTIVE) "sarvam" else DEFAULT_ACTIVE[1]),
    hr(),
    h5("Run settings"),
    selectInput("mode", "Debate mode", choices = vocab_choices(CONFIG$debate_modes),
                selected = "Panel Discussion"),
    selectInput("moderator", "Moderator", choices = vocab_choices(CONFIG$moderators),
                selected = "Neutral"),
    selectInput("objective", "Objective", choices = vocab_choices(CONFIG$objectives),
                selected = "Truth Seeking"),
    sliderInput("n_rounds", "Rounds", min = 1, max = 20, value = 3),
    sliderInput("max_tokens", "Max tokens", min = 256, max = 4096, value = 2560, step = 256),
    uiOutput("token_warning"),
    sliderInput("temperature", "Temperature", min = 0, max = 1, value = 0.7, step = 0.05),
    sliderInput("reasoning_effort_idx", "Reasoning effort", min = 1,
                max = length(REASONING_EFFORT_LEVELS), value = 3, step = 1, ticks = FALSE),
    textOutput("reasoning_effort_label"),
    textInput("language", "Response language (optional)", value = ""),
    checkboxInput("auto_stop", "Auto-stop on novelty plateau", value = FALSE),
    checkboxInput("use_cache", "Cache LLM responses", value = TRUE),
    actionButton("shuffle_settings", "🎲 Shuffle settings", class = "btn-sm btn-outline-secondary", width = "100%"),
    hr(),
    actionButton("run_discussion", "Run Deliberation", class = "btn-primary", width = "100%"),
    br(), br(),
    actionButton("stop_discussion", "Stop", width = "100%")
  ),

  # ---- Home ---------------------------------------------------------------
  nav_panel(
    "Home", icon = icon("house"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Generic Multi-LLM Deliberation Laboratory"),
        card_body(
          p("Run structured intellectual deliberations among multiple LLM agents ",
            "(OpenAI, Claude, Grok, Sarvam, Gemini, or any OpenAI-compatible endpoint)."),
          tags$ol(
            tags$li(strong("Planner:"), " enter a topic; the planner infers dimensions, experts, questions, a debate mode and a moderator."),
            tags$li(strong("Participants:"), " build the roster from the plan, or design agents by hand."),
            tags$li(strong("Debate Setup / sidebar:"), " choose mode, moderator, objective, rounds and models."),
            tags$li(strong("Prompt Preview:"), " inspect the exact prompt any agent will receive."),
            tags$li(strong("Live Debate:"), " run it; watch turns, confidence and moderation stream in."),
            tags$li(strong("Knowledge Graph / Consensus / Export:"), " analyze and export the outcome.")
          ),
          p(class = "text-muted",
            "Everything is config-driven: add a provider, role, mode, dimension or output format ",
            "by editing config/*.json -- no R changes for the common cases.")
        )
      ),
      card(
        card_header("Status"),
        card_body(uiOutput("home_status"))
      )
    ),
    # Saved debates lives here rather than in Settings: loading a past debate is
    # a starting action, not a configuration one. fillable=FALSE so a long
    # sessions table pushes the buttons down instead of flexing over them.
    card(
      card_header("Saved debates"),
      card_body(
        fillable = FALSE,
        p(class = "text-muted", "Saves the full configuration (providers, mode, moderator, ",
          "objective, rounds, tokens, dimensions, models) AND all results (agents, plan, ",
          "transcript, knowledge graph, analytics, consensus, cost)."),
        textInput("session_name", "Name", value = "",
                  placeholder = "blank = <debate title>_<date-time>"),
        actionButton("btn_save_session", "Save current debate", class = "btn-sm btn-primary"),
        hr(),
        helpText("Select a saved debate, then Load or Delete."),
        DTOutput("sessions_table"),
        br(),
        actionButton("btn_load_session", "Load selected", class = "btn-sm"),
        actionButton("btn_delete_session", "Delete selected", class = "btn-sm btn-danger")
      )
    )
  ),

  # ---- Planner ------------------------------------------------------------
  nav_panel(
    "Planner", icon = icon("brain"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Topic"),
        card_body(
          textAreaInput("topic", "Topic / question", rows = 2, width = "100%",
                        value = "Are statins overprescribed for primary prevention?"),
          textAreaInput("problem_details", "Problem details / background (optional)", rows = 8, width = "100%",
                        placeholder = paste("Paste the full case, background, constraints, data...",
                                            "The Planner and every agent see this as context.",
                                            "(Longer text = more tokens per turn.)")),
          conditionalPanel("input.mode == 'Narrative Forge'",
            selectInput("narrative_type", "Narrative / myth type (optional)",
                        choices = c("None" = "",
                                    setNames(cfg_names(CONFIG$narrative_types), cfg_names(CONFIG$narrative_types)),
                                    "Custom..." = "__custom__")),
            conditionalPanel("input.narrative_type == '__custom__'",
              textAreaInput("narrative_type_custom", "Describe the narrative/myth type to engineer", rows = 3,
                            placeholder = paste("e.g. A lullaby-cycle that encodes water discipline",
                                                "for children of a drought region..."))),
            helpText("Every agent (and the Planner) receives this as the NARRATIVE TARGET for the ",
                     "8-stage myth-engineering loop. Seat the Narrative roles from the participant library.")),
          conditionalPanel("input.mode != 'Narrative Forge'",
            helpText(class = "text-muted",
                     "To engineer narratives/myths: pick the 'Narrative Forge' debate mode on the ",
                     "Debate Setup tab -- a narrative/myth type selector then appears here.")),
          tags$label(class = "control-label", "Apply critical rules"),
          checkboxInput("apply_rules_debate", "During deliberation (every agent turn)", value = TRUE),
          checkboxInput("apply_rules_consensus", "At consensus (final verdict)", value = TRUE),
          textAreaInput("critical_rules", "Critical rules (the ruleset applied above)",
                        value = CONFIG$critical_rules %||% "", rows = 10, width = "100%",
                        placeholder = paste("Epistemic ground rules injected into every turn as",
                                            "authoritative, overriding persuasion. Type freely --",
                                            "one rule per line. Edit, add, or clear as needed.")),
          actionButton("reset_critical_rules", "Reset to default rules",
                       class = "btn-sm btn-outline-secondary"),
          br(), br(),
          textInput("debate_title", "Debate title", value = "",
                    placeholder = "Short name used in all output filenames, e.g. Myth"),
          helpText("Files are named <type>_<title>_<date-time>: Plan_Myth_..., ",
                   "Deliberation_Myth_..., Consensus_Myth_... Leave blank to omit the title part."),
          textInput("coordinator", "Debate coordinator", value = "",
                    placeholder = "Name shown under the heading of every exported PDF"),
          radioButtons("panel_design", "Panel design",
                       choices = c("Five-role adversarial scrutiny (Proposer / Challenger / Steelman / Source Auditor + Synthesiser verdict)" = "five_role",
                                   "Planner's free choice of experts" = "free"),
                       selected = "five_role"),
          helpText("Five-role: the four debating roles are fixed; the Planner assigns each a ",
                   "topic-specific domain lens, and the consensus step acts as the Synthesiser. ",
                   "The Source Auditor speaks first each round and, for the first two rounds, ",
                   "sees only a claims-and-evidence digest -- not the other participants' ",
                   "conclusions -- so its audit is formed independently. ",
                   "Free choice: the Planner designs the panel from the full role library (previous behaviour)."),
          conditionalPanel("input.panel_design == 'free'",
            numericInput("planner_n_hint", "Target number of agents (hint)", value = 4, min = 2, max = 20)),
          actionButton("run_planner", "Run Planner", class = "btn-primary"),
          br(), br(),
          actionButton("apply_plan_agents", "Build participants from plan", class = "btn-success"),
          actionButton("apply_plan_setup", "Apply recommended mode/moderator"),
          br(), br(),
          downloadButton("dl_plan_pdf", "Save plan as PDF", class = "btn-sm btn-outline-secondary"),
          helpText("The PDF includes the debate topic, the debate coordinator named above, ",
                   "and the full plan (panel, lenses, dimensions, questions, recommendations)."),
          uiOutput("planner_msg")
        )
      ),
      card(
        card_header("Deliberation plan"),
        card_body(uiOutput("planner_view"))
      )
    )
  ),

  # ---- Participants -------------------------------------------------------
  nav_panel(
    "Participants", icon = icon("users"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Add / edit agent"),
        card_body(
          textInput("ag_name", "Name", value = "New Agent"),
          selectInput("ag_role", "Role (from roles.json)",
                      choices = c("Custom", cfg_names(CONFIG$roles))),
          textInput("ag_expertise", "Expertise", value = ""),
          selectInput("ag_reasoning", "Reasoning style", choices = vocab_choices(CONFIG$reasoning_styles)),
          selectInput("ag_evidence", "Evidence preference", choices = vocab_choices(CONFIG$evidence_types)),
          selectInput("ag_provider", "Provider", choices = provider_choices(CONFIG)),
          textAreaInput("ag_goal", "Goal (optional)", rows = 2),
          sliderInput("ag_creativity", "Creativity", 0, 1, 0.5),
          sliderInput("ag_skepticism", "Skepticism", 0, 1, 0.5),
          sliderInput("ag_risk", "Risk tolerance", 0, 1, 0.5),
          sliderInput("ag_confidence", "Starting confidence", 0, 1, 0.5),
          textAreaInput("ag_prompt", "Custom instructions (optional)", rows = 2),
          actionButton("ag_add", "Add Agent", class = "btn-success"),
          helpText("Select a row to edit it (button becomes Save). Click it again to deselect.")
        )
      ),
      card(
        card_header("Current roster"),
        # fillable=FALSE for the same reason as the library card below: a tall
        # roster table must push the buttons down, not flex over them.
        card_body(
          fillable = FALSE,
          DTOutput("agents_table"),
          br(),
          div(
            actionButton("shuffle_roster", "🎲 Shuffle roster", class = "btn-sm btn-outline-secondary"),
            actionButton("ag_remove", "Remove", class = "btn-danger btn-sm"),
            actionButton("ag_duplicate", "Duplicate", class = "btn-sm"),
            actionButton("ag_up", "Move up", class = "btn-sm"),
            actionButton("ag_down", "Move down", class = "btn-sm"),
            actionButton("ag_randomize", "Randomize dials", class = "btn-sm"),
            actionButton("ag_reset_dials", "Reset dials", class = "btn-sm"),
            actionButton("save_agent_to_library", "💾 Save to library", class = "btn-sm btn-outline-secondary")
          ),
          helpText("Shuffle roster replaces all participants with a fresh random set on the active providers. ",
                   "Save to library adds the selected agent's role to config/roles.json so it's reusable in future sessions ",
                   "(saves expertise/reasoning/evidence/communication/bias, any role constraints, and the dials as presets; ",
                   "goal and prompt stay per-agent).")
        )
      )
    ),
    card(
      card_header("Participant library"),
      # fillable=FALSE: let the body flow top-to-bottom. The default fill layout
      # flexes the DT against the button row, which overlaps them once the
      # table is taller than the allotted space.
      card_body(
        fillable = FALSE,
        p(class = "text-muted",
          "Every role in roles.json. Tick one or more and add them straight to the roster, ",
          "or load a single one into the editor on the left to adjust it before adding. ",
          "Search and filter with the boxes under the headers."),
        DTOutput("library_table"),
        br(),
        div(
          actionButton("lib_add", "➕ Add selected to roster", class = "btn-success btn-sm"),
          actionButton("lib_load", "✎ Load into editor", class = "btn-sm btn-primary"),
          span(class = "text-muted", style = "margin-left:8px;",
               "Rules ✓ = the role carries constraints (what it must not do).")
        ),
        hr(),
        tags$h6("Generate roles"),
        p(class = "text-muted",
          "Have the Planner/Moderator provider design new library roles. Name a field to ",
          "stay inside it, or leave it blank for a broad mix. Generated roles land in the ",
          "same preview below -- nothing is written until you press Import."),
        layout_columns(
          col_widths = c(7, 2, 3),
          textInput("gen_roles_field", NULL, value = "",
                    placeholder = "Field, e.g. automotive engineering (blank = broad mix)"),
          numericInput("gen_roles_n", NULL, value = 5, min = 1, max = 20),
          actionButton("gen_roles", "Generate", class = "btn-sm btn-primary")
        ),
        hr(),
        tags$h6("Import roles"),
        p(class = "text-muted",
          "The box starts with a ready-made prompt: copy it into any LLM, then replace it ",
          "with what comes back. It also accepts a markdown table or spreadsheet rows with a ",
          "header naming the columns (Name / Category / Expertise / Reasoning / Evidence / ",
          "Communication / Bias / Constraints, plus optional Creativity, Skepticism, Risk ",
          "tolerance). Preview first; importing appends to config/roles.json and the roles ",
          "are usable immediately."),
        # Pre-filled with the generation prompt: copy it into any LLM, then
        # replace it with the JSON that comes back. Built from the live config,
        # so it always names the real vocabulary and the real taken names.
        textAreaInput("lib_import_text", NULL, rows = 8, width = "100%",
                      value = role_prompt_text(CONFIG, 5, cfg_names(CONFIG$roles))),
        div(
          actionButton("lib_import_preview", "Preview", class = "btn-sm"),
          actionButton("lib_import_do", "Import", class = "btn-sm btn-success"),
          actionButton("lib_import_reset", "↺ Restore prompt", class = "btn-sm btn-outline-secondary")
        ),
        uiOutput("lib_import_report")
      )
    )
  ),

  # ---- Discussion Dimensions ---------------------------------------------
  nav_panel(
    "Dimensions", icon = icon("layer-group"),
    card(
      card_header("Discussion dimensions"),
      card_body(
        p("Dimensions from the plan (or the config library). Checked dimensions are ",
          "emphasized to every agent during the debate."),
        uiOutput("dimensions_view")
      )
    )
  ),

  # ---- Debate Setup -------------------------------------------------------
  nav_panel(
    "Debate Setup", icon = icon("sliders"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Model per active provider"),
           card_body(
             p(class = "text-muted", "Overrides the provider default from providers.json for this run."),
             uiOutput("model_selectors"))),
      card(card_header("Run summary"),
           card_body(uiOutput("setup_summary"),
                     uiOutput("ram_status"),
                     uiOutput("local_server_status"),
                     br(),
                     actionButton("run_discussion2", "Run Deliberation", class = "btn-primary"),
                     hr(),
                     textInput("session_name2", "Save as", value = "",
                               placeholder = "blank = <debate title>_<date-time>"),
                     actionButton("btn_save_session2", "Save current debate", class = "btn-sm"),
                     helpText("Saves the full configuration + results (same as Home → Saved debates).")))
    ),
    card(
      card_header(
        div(style = "display:flex; justify-content:space-between; align-items:center;",
            span("Run log — errors & warnings during deliberation (newest first)"),
            actionButton("clear_run_log", "Clear", class = "btn-sm"))
      ),
      card_body(
        uiOutput("run_log_summary"),
        div(class = "run-log", uiOutput("run_log"))
      )
    )
  ),

  # ---- Prompt Preview -----------------------------------------------------
  nav_panel(
    "Prompt Preview", icon = icon("eye"),
    card(
      card_header("Exact prompt an agent will receive (no API call)"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          selectInput("preview_agent", "Agent", choices = NULL),
          numericInput("preview_round", "Round", value = 1, min = 1, max = 20)
        ),
        h6("SYSTEM"), verbatimTextOutput("preview_system"),
        h6("USER"), verbatimTextOutput("preview_user")
      )
    )
  ),

  # ---- Live Debate --------------------------------------------------------
  nav_panel(
    "Live Debate", icon = icon("comments"),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header(textOutput("live_header")), card_body(uiOutput("conversation_view"))),
      card(card_header("Moderator (per round)"), card_body(uiOutput("moderator_view")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Confidence"), card_body(plotlyOutput("plot_confidence", height = 260))),
      card(card_header("Novelty"), card_body(plotlyOutput("plot_novelty", height = 260)))
    ),
    card(
      card_header("Token usage & estimated cost"),
      card_body(
        uiOutput("cost_summary"),
        div(actionButton("reset_cost", "Reset cost meter", class = "btn-sm"),
            span(class = "text-muted", style = "font-size:85%; margin-left:8px;",
                 "Estimates from config/pricing.json — update those rates for accuracy.")),
        br(),
        DTOutput("cost_table")
      )
    )
  ),

  # ---- Knowledge Graph ----------------------------------------------------
  nav_panel(
    "Knowledge Graph", icon = icon("diagram-project"),
    layout_columns(
      col_widths = c(9, 3),
      card(card_header("Claim / evidence graph"), card_body(visNetworkOutput("kg_plot", height = "560px"))),
      card(card_header("Export"), card_body(
        downloadButton("dl_graphml", "GraphML"), br(), br(),
        downloadButton("dl_kg_csv", "Nodes+Edges CSV")))
    ),
    card(card_header("Idea evolution"), card_body(DTOutput("idea_evolution_table")))
  ),

  # ---- Consensus ----------------------------------------------------------
  nav_panel(
    "Consensus", icon = icon("handshake"),
    card(
      card_header("Final synthesis"),
      card_body(
        div(
          actionButton("run_consensus", "Generate Consensus", class = "btn-primary"),
          actionButton("copy_consensus", "Copy to clipboard", icon = icon("clipboard")),
          downloadButton("dl_consensus_pdf", "PDF")
        ),
        checkboxInput("include_plan_in_consensus",
                      "Include the deliberation plan in the consensus report (PDF & clipboard)", value = FALSE),
        checkboxInput("include_kg_consensus",
                      "Append the knowledge graph as a landscape page (PDF only; adds a few seconds)", value = FALSE),
        uiOutput("debate_quality_card"),
        uiOutput("consensus_view")
      )
    )
  ),

  # ---- Export -------------------------------------------------------------
  nav_panel(
    "Export", icon = icon("file-export"),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Generate formatted output"), card_body(
        selectInput("export_format", "Format", choices = vocab_choices(CONFIG$output_formats)),
        actionButton("gen_export", "Generate", class = "btn-primary"),
        br(), br(),
        downloadButton("dl_formatted", "Download generated"),
        hr(),
        h6("Debate transcript"),
        checkboxInput("include_moderator_in_transcript",
                      "Append moderator comments at the end of the transcript", value = FALSE),
        actionButton("copy_debate", "Copy to clipboard", icon = icon("clipboard")),
        downloadButton("dl_debate_pdf", "PDF"),
        hr(),
        h6("Raw downloads"),
        downloadButton("dl_txt", "TXT"), downloadButton("dl_md", "Markdown"),
        downloadButton("dl_json", "JSON"), downloadButton("dl_csv", "CSV"))),
      card(card_header("Preview"), card_body(verbatimTextOutput("export_preview")))
    )
  ),

  # ---- Settings -----------------------------------------------------------
  nav_panel(
    "Settings", icon = icon("gear"),
    layout_columns(
      col_widths = c(6),
      card(card_header("API keys (session only)"), card_body(
        p(class = "text-muted", "Env vars and config/secrets.R are used first; keys typed here override for this session only and are never saved."),
        uiOutput("key_inputs"),
        actionButton("save_keys", "Use these keys", class = "btn-sm"),
        actionButton("test_keys", "Test keys (disable failing)", class = "btn-sm"),
        uiOutput("key_test_result"),
        hr(),
        actionButton("clear_cache", "Clear response cache", class = "btn-sm"),
        textOutput("cache_status")))
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  cfg <- CONFIG

  rv <- reactiveValues(
    agents = default_agents(cfg, DEFAULT_ACTIVE),
    plan = NULL, plan_msg = NULL,
    history = list(), kg = empty_kg(), analytics = NULL, consensus = NULL,
    running = FALSE, stop_requested = FALSE,
    current_speaker = NULL, formatted_output = "",
    ui_keys = list(),
    usage_log = list(),  # one record per LLM call, for token/cost accounting
    run_log = list(),    # activity log: run clicks, API calls, warnings/errors (Debate Setup tab)
    loaded_providers = NULL  # one-shot: providers a just-loaded debate needs active
  )

  reasoning_effort <- reactive(REASONING_EFFORT_LEVELS[input$reasoning_effort_idx])
  output$reasoning_effort_label <- renderText(paste0("Level: ", tools::toTitleCase(reasoning_effort())))

  # Provider used for planner / moderator / consensus meta-calls.
  meta_key <- reactive(resolve_api_key(cfg, input$meta_provider, rv$ui_keys))

  # Ordered fallback providers (with keys) for the meta-calls, so a failed/timed-
  # out meta provider (e.g. Sarvam) hands off to a working one before dropping to
  # the heuristic. Same priority as agent failover (DeepSeek first = cheapest),
  # excluding the primary and `local` (slow/unreliable for structured JSON).
  meta_fallbacks <- function(primary) {
    keyed  <- available_providers(cfg, rv$ui_keys)
    pri    <- c("deepseek", "mistral", "celeris", "claude", "openai")
    # Exclude local and perplexity: neither is suited to the structured-JSON meta
    # calls (local is slow; perplexity's web models aren't tuned for strict JSON
    # and bill a per-request search fee).
    ranked <- c(intersect(pri, keyed),
                setdiff(intersect(provider_ids(cfg), keyed), c(pri, "local", "gpt4all", "perplexity")))
    ranked <- setdiff(ranked, c(primary, "local", "gpt4all", "perplexity"))
    lapply(ranked, function(p) list(provider = p, key = resolve_api_key(cfg, p, rv$ui_keys)))
  }

  # Model override map from the Debate Setup selectors (provider_id -> model).
  model_for <- function(provider_id) {
    v <- input[[paste0("model_", provider_id)]]
    if (is.null(v) || !nzchar(v)) NULL else v
  }

  # Append one call's token usage + estimated cost to the ledger. Called after
  # every LLM call (agent turn, moderator, planner, consensus, export). Cached
  # calls are billed at 0. Priced via config/pricing.json (cfg$pricing).
  log_usage <- function(call_type, provider, model, usage, cached, agent = NA, round = NA) {
    u  <- usage %||% list()
    pt <- suppressWarnings(as.numeric(u$prompt_tokens %||% NA))
    ct <- suppressWarnings(as.numeric(u$completion_tokens %||% NA))
    cost <- if (isTRUE(cached)) 0 else llm_cost(cfg$pricing, model, pt, ct)
    rv$usage_log <- c(rv$usage_log, list(list(
      call_type = call_type, agent = agent, round = round, provider = provider %||% NA_character_,
      model = model %||% NA_character_, prompt_tokens = pt, completion_tokens = ct,
      cached = isTRUE(cached), cost = cost)))
    # Also surface every API call in the Run log (level "API" -- not counted as an
    # error/warning). Shows what ran, on which provider/model, tokens and cost.
    det  <- paste0(call_type,
                   if (!is.na(round)) paste0(" R", round) else "",
                   if (!is.na(agent) && nzchar(as.character(agent))) paste0(" [", agent, "]") else "")
    toks <- if (is.na(pt) && is.na(ct)) "" else
      sprintf(" | %s+%s tok", ifelse(is.na(pt), "?", round(pt)), ifelse(is.na(ct), "?", round(ct)))
    money <- if (isTRUE(cached)) " | cached" else if (!is.na(cost) && cost > 0) sprintf(" | $%.4f", cost) else ""
    log_event("API", paste0(det, " | ", provider %||% "?", "/", model %||% "?", toks, money))
  }

  # Append an entry to the Run log (Debate Setup tab). level is
  # "ERROR" | "WARN" | "INFO" | "API" (API = per-call trace, not counted as an
  # issue in the summary). Bounded to the most recent 400 entries.
  log_event <- function(level, msg) {
    entry <- list(time = format(Sys.time(), "%H:%M:%S"), level = level, msg = as.character(msg))
    rv$run_log <- c(rv$run_log, list(entry))
    if (length(rv$run_log) > 400) rv$run_log <- utils::tail(rv$run_log, 400)
  }

  # ---- Home status --------------------------------------------------------
  output$home_status <- renderUI({
    avail <- available_providers(cfg, rv$ui_keys)
    tags$div(
      labelled_list("Providers with keys", vapply(avail, function(id) provider_by_id(cfg, id)$label, character(1))),
      tags$p(tags$b("Agents: "), length(rv$agents)),
      tags$p(tags$b("Rounds completed: "),
             if (length(rv$history) == 0) 0 else as.integer(max(vapply(rv$history, function(h) h$round, numeric(1))))),
      tags$p(tags$b("Estimated cost this debate: "),
             sprintf("~$%.4f", sum(usage_to_df(rv$usage_log, cfg$pricing)$cost, na.rm = TRUE))),
      tags$p(tags$b("Cache entries: "), llm_cache_size())
    )
  })

  output$token_warning <- renderUI({
    if (is.null(input$max_tokens)) return(NULL)
    thinking <- Filter(function(p) isTRUE(provider_by_id(cfg, p)$thinking), input$active_providers)
    # Warn per provider using its EFFECTIVE budget (slider clamped by the
    # provider's max_tokens cap) -- so a thinking provider capped below the
    # threshold is flagged even when the slider itself is high enough.
    low <- Filter(function(p) provider_max_tokens(cfg, p, input$max_tokens) < THINKING_MIN_TOKENS, thinking)
    if (length(low) == 0) return(NULL)
    labels <- vapply(low, function(p) paste0(provider_by_id(cfg, p)$label, " (",
                                             provider_max_tokens(cfg, p, input$max_tokens), ")"), character(1))
    tags$div(style = "color:#8a6d00;background:#fff8e1;border:1px solid #ffe082;border-radius:4px;padding:6px 8px;font-size:88%;",
             paste0(paste(labels, collapse = ", "),
                    if (length(low) > 1) " use thinking mode" else " uses thinking mode",
                    " with an effective budget below ", THINKING_MIN_TOKENS,
                    " and may return empty turns. Raise Max Tokens, or the provider's max_tokens cap."))
  })

  # =========================================================================
  # PLANNER
  # =========================================================================
  # Restore the shipped default ruleset (config/critical_rules.txt) after free edits.
  observeEvent(input$reset_critical_rules, {
    updateTextAreaInput(session, "critical_rules", value = CONFIG$critical_rules %||% "")
    showNotification("Critical rules reset to the default set.", type = "message")
  })

  # The selected narrative/myth type as a prompt line, or NULL when unset.
  # Preset -> its configured fragment; Custom -> the user's own description.
  # Gated on the Narrative Forge mode to match the control's visibility: the
  # selector only shows in that mode, and a value chosen there must not keep
  # injecting a NARRATIVE TARGET into other modes from its hidden state.
  narrative_fragment <- function() {
    if (!identical(input$mode, "Narrative Forge")) return(NULL)
    sel <- input$narrative_type %||% ""
    if (!nzchar(sel)) return(NULL)
    if (identical(sel, "__custom__")) {
      d <- trimws(input$narrative_type_custom %||% "")
      return(if (nzchar(d)) paste0("NARRATIVE TARGET (custom type): ", d) else NULL)
    }
    nt <- cfg_find(cfg$narrative_types, sel)
    if (is.null(nt)) return(NULL)
    paste0("NARRATIVE TARGET -- ", nt$name, ": ", nt$prompt_fragment %||% nt$description %||% "")
  }

  observeEvent(input$run_planner, {
    req(nzchar(input$topic))
    key <- meta_key()
    # numericInput becomes NA when cleared; NA would break head()/messaging.
    n_hint <- if (is.null(input$planner_n_hint) || is.na(input$planner_n_hint)) NULL else input$planner_n_hint
    withProgress(message = "Planning deliberation...", value = 0.5, {
      # The planner sees the narrative target as part of the brief, so it
      # designs dimensions/lenses (or experts) for that kind of story.
      pd <- input$problem_details
      nf <- narrative_fragment()
      if (!is.null(nf)) pd <- trimws(paste(pd %||% "", nf, sep = "\n\n"))
      out <- run_planner(input$topic, cfg, input$meta_provider, key,
                         n_agents_hint = n_hint, use_cache = input$use_cache,
                         problem_details = pd,
                         fallbacks = meta_fallbacks(input$meta_provider),
                         panel = input$panel_design %||% "five_role")
    })
    # Stamp the narrative target onto the plan so the plan PDF (the design
    # record) shows what kind of story the deliberation was planned around.
    # Also pin the plan's recommendation to Narrative Forge: the target only
    # operates in that mode, so a plan recommending any other mode (five-role
    # plans always recommend Adversarial Scrutiny) would make "Apply
    # recommended mode" silently disable the target the user just configured.
    if (!is.null(out$plan) && !is.null(nf)) {
      out$plan$narrative <- nf
      out$plan$recommended_mode <- "Narrative Forge"
      out$plan$recommended_moderator <- "Facilitator"
    }
    rv$plan <- out$plan
    rv$plan_msg <- out$error
    if (!is.null(out$error)) log_event("WARN", paste("Planner:", out$error))
    if (is.null(out$error) && !is.null(out$provider) && !identical(out$provider, input$meta_provider))
      log_event("WARN", paste0("Planner failed over from '", input$meta_provider, "' to '", out$provider, "'."))
    log_usage("planner", out$provider %||% input$meta_provider, out$model, out$usage, out$cached)
  })

  output$planner_msg <- renderUI({
    if (is.null(rv$plan_msg)) return(NULL)
    tags$div(class = "text-warning", style = "margin-top:8px;", rv$plan_msg)
  })

  # Save the current plan as a PDF (topic in the title, coordinator underneath,
  # same pattern as the consensus report: HTML->PDF with a plain-text fallback).
  output$dl_plan_pdf <- downloadHandler(
    filename = function() out_filename("Plan", "pdf"),
    content = function(file) {
      ensure_writable(file)
      title <- paste0("Deliberation Plan: ", trimws(input$topic %||% ""))
      meta <- coord_meta()
      ok <- if (is.null(rv$plan)) FALSE else
        tryCatch({ html_to_pdf(.html_doc(title, plan_html(rv$plan), meta = meta), file); TRUE },
                 error = function(e) FALSE)
      if (!ok) text_to_pdf(plan_to_text(rv$plan, input$topic %||% ""), file,
                           title = title, subtitle = meta)
    })

  output$planner_view <- renderUI({
    p <- rv$plan
    if (is.null(p)) return(p("Run the planner to design a deliberation."))
    dim_items <- lapply(p$dimensions, function(d)
      tags$li(strong(d$name), sprintf(" (importance %.2f) ", d$importance %||% NA), em(d$why)))
    expert_items <- lapply(p$experts, function(e) {
      # Five-role plans: e$name is the panel label ("Challenger") and
      # e$expertise its planner-assigned domain lens.
      label <- e$name %||% e$role
      lens <- if (nzchar(e$expertise %||% "")) paste0(" [lens: ", e$expertise, "]") else ""
      tags$li(strong(label), lens, " -- ", e$reasoning, " / ", e$evidence, " ", em(e$why))
    })
    tags$div(
      info_card(paste0("Source: ", p$source, " | recommended agents: ", p$recommended_num_agents),
                tags$p(em(p$rationale))),
      info_card("Dimensions", tags$ul(dim_items)),
      info_card("Experts", tags$ul(expert_items)),
      info_card("Debate questions", tags$ul(lapply(unlist(p$debate_questions), tags$li))),
      layout_columns(col_widths = c(6, 6),
        info_card("Recommended", tags$p(tags$b("Mode: "), p$recommended_mode),
                  tags$p(tags$b("Moderator: "), p$recommended_moderator)),
        info_card("Evidence required", tags$ul(lapply(unlist(p$required_evidence), tags$li)))),
      layout_columns(col_widths = c(6, 6),
        info_card("Expected agreements", tags$ul(lapply(unlist(p$expected_agreements), tags$li)), accent = "#1E8449"),
        info_card("Expected controversies", tags$ul(lapply(unlist(p$expected_controversies), tags$li)), accent = "#C0392B"))
    )
  })

  observeEvent(input$apply_plan_agents, {
    req(rv$plan)
    provs <- input$active_providers
    rv$agents <- agents_from_plan(cfg, rv$plan, provs)
    showNotification(paste("Built", length(rv$agents), "participants from the plan."), type = "message")
    nav_select("navbar", "Participants")
  })

  observeEvent(input$apply_plan_setup, {
    req(rv$plan)
    if (rv$plan$recommended_mode %in% cfg_names(cfg$debate_modes))
      updateSelectInput(session, "mode", selected = rv$plan$recommended_mode)
    if (rv$plan$recommended_moderator %in% cfg_names(cfg$moderators))
      updateSelectInput(session, "moderator", selected = rv$plan$recommended_moderator)
    showNotification("Applied recommended mode and moderator.", type = "message")
  })

  # =========================================================================
  # PARTICIPANTS
  # =========================================================================
  editing_idx <- reactiveVal(NULL)
  agents_proxy <- dataTableProxy("agents_table")

  # Keep the agent provider dropdown limited to active providers.
  observeEvent(input$active_providers, {
    cur <- input$ag_provider %||% ""   # NULL on the very first flush -> "" (avoids if(logical(0)))
    sel <- if (cur %in% input$active_providers) cur else
      if (length(input$active_providers)) input$active_providers[1] else character(0)
    updateSelectInput(session, "ag_provider",
                      choices = provider_choices(cfg)[provider_choices(cfg) %in% input$active_providers],
                      selected = sel)
    # Once the checkbox reflects a real selection (incl. the post-load update
    # arriving, or any manual change), drop the one-shot loaded-provider set so
    # it can't later re-enable a provider the user has since disabled.
    rv$loaded_providers <- NULL
  }, ignoreNULL = FALSE)

  # Loading an existing agent into the form also changes ag_role, which re-fires
  # the prefill observer below on the next flush -- after the row observer has
  # written the agent's own values. Left unguarded it overwrites the agent's
  # customized expertise and dials with the role's defaults. The row observer
  # parks the role name here; the prefill skips that one firing.
  skip_role_prefill <- reactiveVal(NULL)

  # The live participant library. Starts as the roles loaded from roles.json and
  # grows when "Save to library" appends one, so a role saved this session is
  # immediately pickable and seatable -- previously only its NAME reached the
  # dropdown, so selecting it prefilled nothing.
  role_lib <- reactiveVal(CONFIG$roles)

  # Selecting a role prefills expertise/reasoning/evidence and any dial presets.
  observeEvent(input$ag_role, {
    if (identical(skip_role_prefill(), input$ag_role)) { skip_role_prefill(NULL); return() }
    skip_role_prefill(NULL)
    r <- cfg_find(role_lib(), input$ag_role)
    if (!is.null(r)) {
      updateTextInput(session, "ag_expertise", value = r$expertise %||% "")
      if ((r$reasoning %||% "") %in% cfg_names(cfg$reasoning_styles))
        updateSelectInput(session, "ag_reasoning", selected = r$reasoning)
      if ((r$evidence %||% "") %in% cfg_names(cfg$evidence_types))
        updateSelectInput(session, "ag_evidence", selected = r$evidence)
      # Optional per-role dial presets (Red-Team arrives sceptical, not neutral).
      for (d in list(c("ag_creativity", "creativity"), c("ag_skepticism", "skepticism"),
                     c("ag_risk", "risk_tolerance"))) {
        v <- suppressWarnings(as.numeric(r[[d[2]]] %||% NA))
        if (!is.na(v)) updateSliderInput(session, d[1], value = v)
      }
    }
  })

  reset_agent_form <- function() {
    skip_role_prefill(NULL)
    updateTextInput(session, "ag_name", value = "New Agent")
    updateSelectInput(session, "ag_role", selected = "Custom")
    updateTextInput(session, "ag_expertise", value = "")
    updateTextAreaInput(session, "ag_goal", value = "")
    updateTextAreaInput(session, "ag_prompt", value = "")
    for (s in c("ag_creativity", "ag_skepticism", "ag_risk", "ag_confidence"))
      updateSliderInput(session, s, value = 0.5)
  }

  observeEvent(input$agents_table_rows_selected, {
    sel <- input$agents_table_rows_selected
    if (length(sel) == 1 && sel <= length(rv$agents)) {
      a <- rv$agents[[sel]]
      editing_idx(sel)
      updateTextInput(session, "ag_name", value = a$name)
      role_sel <- if (a$role %in% cfg_names(role_lib())) a$role else "Custom"
      # Keep the agent's own values, not the role defaults -- but only park the
      # skip if the dropdown will actually change (no change = no event = the
      # parked skip would go stale and swallow the next genuine selection).
      if (!identical(input$ag_role, role_sel)) skip_role_prefill(role_sel)
      updateSelectInput(session, "ag_role", selected = role_sel)
      updateTextInput(session, "ag_expertise", value = a$expertise)
      if (a$reasoning %in% cfg_names(cfg$reasoning_styles)) updateSelectInput(session, "ag_reasoning", selected = a$reasoning)
      if (a$evidence %in% cfg_names(cfg$evidence_types)) updateSelectInput(session, "ag_evidence", selected = a$evidence)
      updateSelectInput(session, "ag_provider", choices = union(input$active_providers, a$provider), selected = a$provider)
      updateTextAreaInput(session, "ag_goal", value = a$goal)
      updateSliderInput(session, "ag_creativity", value = a$creativity)
      updateSliderInput(session, "ag_skepticism", value = a$skepticism)
      updateSliderInput(session, "ag_risk", value = a$risk_tolerance)
      updateSliderInput(session, "ag_confidence", value = a$confidence)
      updateTextAreaInput(session, "ag_prompt", value = a$prompt)
      updateActionButton(session, "ag_add", label = "Save Changes")
    } else {
      editing_idx(NULL); updateActionButton(session, "ag_add", label = "Add Agent")
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$ag_add, {
    a <- new_agent(cfg, name = input$ag_name,
                   role = if (identical(input$ag_role, "Custom")) input$ag_name else input$ag_role,
                   category = { r <- cfg_find(role_lib(), input$ag_role); if (is.null(r)) "Custom" else r$category },
                   expertise = input$ag_expertise, reasoning = input$ag_reasoning, evidence = input$ag_evidence,
                   communication = { r <- cfg_find(role_lib(), input$ag_role); if (is.null(r)) "" else r$communication %||% "" },
                   bias = { r <- cfg_find(role_lib(), input$ag_role); if (is.null(r)) "" else r$bias %||% "" },
                   constraints = { r <- cfg_find(role_lib(), input$ag_role); if (is.null(r)) "" else r$constraints %||% "" },
                   history_view = { r <- cfg_find(role_lib(), input$ag_role); if (is.null(r)) "" else r$history_view %||% "" },
                   digest_rounds = { r <- cfg_find(role_lib(), input$ag_role)
                                     dr <- suppressWarnings(as.integer((if (is.null(r)) 0 else r$digest_rounds %||% 0)[1]))
                                     if (length(dr) == 0 || is.na(dr)) 0L else dr },
                   goal = input$ag_goal, creativity = input$ag_creativity, skepticism = input$ag_skepticism,
                   risk_tolerance = input$ag_risk, confidence = input$ag_confidence,
                   prompt = input$ag_prompt, provider = input$ag_provider)
    idx <- editing_idx()
    if (!is.null(idx) && idx <= length(rv$agents)) {
      # Names key the transcript/analytics; uniquify against the OTHER agents
      # so an edit can keep its own name without being renamed to "(2)".
      a$name <- unique_agent_name(a$name, rv$agents[-idx])
      a$id <- rv$agents[[idx]]$id; rv$agents[[idx]] <- a
      showNotification(paste("Saved:", a$name), type = "message")
    } else {
      a$name <- unique_agent_name(a$name, rv$agents)
      rv$agents <- c(rv$agents, list(a)); showNotification(paste("Added:", a$name), type = "message")
    }
    editing_idx(NULL); updateActionButton(session, "ag_add", label = "Add Agent")
    selectRows(agents_proxy, NULL); reset_agent_form()
  })

  output$agents_table <- renderDT({
    datatable(agents_to_df(rv$agents), selection = "single", rownames = FALSE,
              options = list(pageLength = 10, dom = "tp"))
  })

  observeEvent(input$ag_remove, {
    sel <- input$agents_table_rows_selected
    if (length(sel) == 1 && length(rv$agents) >= sel) rv$agents <- rv$agents[-sel]
    editing_idx(NULL); selectRows(agents_proxy, NULL); reset_agent_form()
  })
  observeEvent(input$ag_duplicate, {
    sel <- input$agents_table_rows_selected
    if (length(sel) == 1 && length(rv$agents) >= sel) {
      a <- rv$agents[[sel]]; a$id <- digest::digest(paste(a$name, Sys.time(), runif(1)), algo = "crc32")
      a$name <- paste0(a$name, " (copy)")
      rv$agents <- append(rv$agents, list(a), after = sel)
    }
  })
  move_agent <- function(dir) {
    sel <- input$agents_table_rows_selected
    if (length(sel) != 1) return()
    j <- sel + dir
    if (j < 1 || j > length(rv$agents)) return()
    tmp <- rv$agents[[sel]]; rv$agents[[sel]] <- rv$agents[[j]]; rv$agents[[j]] <- tmp
    selectRows(agents_proxy, j)
  }
  observeEvent(input$ag_up, move_agent(-1))
  observeEvent(input$ag_down, move_agent(1))
  # Randomize / reset the three personality dials by moving the FORM sliders,
  # exactly like every other field on this form: the change is visible and is
  # applied on Save/Add. Deliberately does NOT write rv$agents -- doing so would
  # re-render the roster table, drop the row selection, revert the button to
  # "Add Agent", and risk adding a duplicate on the next click.
  apply_to_agent_hint <- function()
    if (!is.null(editing_idx())) "Click 'Save Changes' to apply." else "Click 'Add Agent' to apply."
  observeEvent(input$ag_randomize, {
    cr <- round(runif(1), 2); sk <- round(runif(1), 2); rk <- round(runif(1), 2)
    updateSliderInput(session, "ag_creativity", value = cr)
    updateSliderInput(session, "ag_skepticism", value = sk)
    updateSliderInput(session, "ag_risk", value = rk)
    showNotification(sprintf("Randomized dials -- creativity %.2f, skepticism %.2f, risk %.2f. %s",
                             cr, sk, rk, apply_to_agent_hint()), type = "message")
  })
  observeEvent(input$ag_reset_dials, {
    updateSliderInput(session, "ag_creativity", value = 0.5)
    updateSliderInput(session, "ag_skepticism", value = 0.5)
    updateSliderInput(session, "ag_risk", value = 0.5)
    showNotification(sprintf("Reset dials to default (0.50). %s", apply_to_agent_hint()), type = "message")
  })
  observeEvent(input$shuffle_roster, {
    if (isTRUE(rv$running)) {
      showNotification("Stop the running deliberation before shuffling the roster.", type = "warning"); return()
    }
    # Shuffle draws from the live library, so roles saved this session count.
    cfg_live <- cfg; cfg_live$roles <- role_lib()
    roster <- random_roster(cfg_live, input$active_providers)
    if (length(roster) == 0) { showNotification("Could not build a roster.", type = "error"); return() }
    rv$agents <- roster
    rv$loaded_providers <- NULL   # a fresh random roster, not a loaded debate's
    editing_idx(NULL); updateActionButton(session, "ag_add", label = "Add Agent")
    selectRows(agents_proxy, NULL); reset_agent_form()
    showNotification(paste0("Shuffled ", length(roster), " participants."), type = "message")
  })

  # Names saved to the library this session (so the role picker shows them and
  # a double-save can't duplicate the file entry -- without relying on mutating
  # the shared `cfg` from inside an observer).

  # ---- Participant library (browse / seat / edit-then-seat) ----------------
  # Flat view of role_lib() for the table. `Rules` marks roles that carry
  # constraints, since that is what distinguishes otherwise similar roles.
  library_df <- reactive({
    lib <- role_lib()
    if (length(lib) == 0)
      return(data.frame(Role = character(), Category = character(),
                        Expertise = character(), Rules = character()))
    data.frame(
      Role      = vapply(lib, function(r) r$name %||% "", character(1)),
      Category  = vapply(lib, function(r) r$category %||% "", character(1)),
      Expertise = vapply(lib, function(r) r$expertise %||% "", character(1)),
      Rules     = vapply(lib, function(r) if (nzchar(r$constraints %||% "")) "✓" else "", character(1)),
      stringsAsFactors = FALSE)
  })

  output$library_table <- renderDT({
    datatable(library_df(), rownames = FALSE, selection = "multiple", filter = "top",
              options = list(pageLength = 8, lengthMenu = c(5, 8, 15, 30), dom = "ltip",
                             columnDefs = list(list(width = "46px", targets = 3))))
  })

  # The library table is rebuilt whenever role_lib() changes, which clears the
  # selection; keep a proxy so handlers can deselect deliberately.
  library_proxy <- dataTableProxy("library_table")

  # Seat every ticked library role directly on the roster.
  observeEvent(input$lib_add, {
    if (isTRUE(rv$running)) {
      showNotification("Stop the running deliberation before changing the roster.", type = "warning"); return()
    }
    sel <- input$library_table_rows_selected
    lib <- role_lib()
    if (length(sel) == 0) { showNotification("Tick one or more roles in the library first.", type = "warning"); return() }
    provs <- input$active_providers
    if (length(provs) == 0) provs <- provider_ids(cfg)
    if (length(provs) == 0) { showNotification("No active providers to assign.", type = "error"); return() }
    added <- character(0)
    for (i in seq_along(sel)) {
      r <- lib[[sel[i]]]
      # Providers are dealt round-robin so a multi-role add spreads across them
      # rather than stacking every new agent on one provider.
      a <- agent_from_role_record(cfg, r, provider = provs[[((i - 1) %% length(provs)) + 1]])
      a$name <- unique_agent_name(r$name %||% "Agent", rv$agents)
      rv$agents <- c(rv$agents, list(a))
      added <- c(added, a$name)
    }
    selectRows(library_proxy, NULL)
    showNotification(paste0("Added ", length(added), " participant",
                            if (length(added) != 1) "s" else "", ": ",
                            paste(added, collapse = ", ")), type = "message")
  })

  # Load one library role into the editor on the left so it can be adjusted
  # before being added. Deliberately leaves the roster untouched.
  observeEvent(input$lib_load, {
    sel <- input$library_table_rows_selected
    if (length(sel) != 1) { showNotification("Tick exactly one role to load into the editor.", type = "warning"); return() }
    r <- role_lib()[[sel]]
    # Drop any roster row selection first, or "Add Agent" would still be in
    # Save-Changes mode and overwrite the selected agent instead of adding.
    editing_idx(NULL); selectRows(agents_proxy, NULL)
    updateActionButton(session, "ag_add", label = "Add Agent")
    nm <- r$name %||% "New Agent"
    role_sel <- if (nm %in% cfg_names(role_lib())) nm else "Custom"
    # We set the fields ourselves; don't let the prefill observer race us. Park
    # the skip ONLY if the dropdown will actually change -- updating a select to
    # its current value fires no event, so an unconditional park would go stale
    # and swallow the user's next genuine selection of this role.
    if (!identical(input$ag_role, role_sel)) skip_role_prefill(role_sel)
    updateSelectInput(session, "ag_role", selected = role_sel)
    updateTextInput(session, "ag_name", value = unique_agent_name(nm, rv$agents))
    updateTextInput(session, "ag_expertise", value = r$expertise %||% "")
    if ((r$reasoning %||% "") %in% cfg_names(cfg$reasoning_styles))
      updateSelectInput(session, "ag_reasoning", selected = r$reasoning)
    if ((r$evidence %||% "") %in% cfg_names(cfg$evidence_types))
      updateSelectInput(session, "ag_evidence", selected = r$evidence)
    for (d in list(c("ag_creativity", "creativity"), c("ag_skepticism", "skepticism"),
                   c("ag_risk", "risk_tolerance"))) {
      v <- suppressWarnings(as.numeric(r[[d[2]]] %||% NA))
      updateSliderInput(session, d[1], value = if (is.na(v)) 0.5 else v)
    }
    updateTextAreaInput(session, "ag_goal", value = "")
    updateTextAreaInput(session, "ag_prompt", value = "")
    showNotification(paste0("Loaded '", nm, "' into the editor -- adjust it, then click Add Agent."),
                     type = "message")
  })

  # ---- Import roles (paste JSON / markdown table / spreadsheet rows) --------
  # Validated on Preview and held here, so Import can only ever write exactly
  # what was shown -- a bad paste cannot silently corrupt the library.
  import_staged <- reactiveVal(NULL)

  # Check a parsed batch against the live library and the config vocabularies.
  # Returns the roles that may be imported, plus per-role notes.
  validate_import <- function(roles) {
    existing <- cfg_names(role_lib())
    rs <- cfg_names(cfg$reasoning_styles); ev <- cfg_names(cfg$evidence_types)
    seen <- character(0); ok <- list(); rejected <- list(); notes <- list()
    for (r in roles) {
      why <- character(0)
      if (r$name %in% existing) why <- c(why, "a role with this name already exists")
      if (r$name %in% seen)     why <- c(why, "duplicated within this paste")
      if (length(why) > 0) { rejected[[length(rejected) + 1]] <- list(role = r, why = why); next }
      n <- character(0)
      # Unknown vocabulary is allowed (custom roles already do it) but the
      # persona then carries the raw text with no configured style fragment.
      if (!(r$reasoning %in% rs)) n <- c(n, paste0("reasoning '", r$reasoning, "' is not a configured style"))
      if (!(r$evidence %in% ev))  n <- c(n, paste0("evidence '", r$evidence, "' is not a configured type"))
      if (!nzchar(r$constraints)) n <- c(n, "no constraints -- the role has no enforced boundary")
      seen <- c(seen, r$name); ok[[length(ok) + 1]] <- r
      notes[[length(notes) + 1]] <- n
    }
    list(ok = ok, notes = notes, rejected = rejected)
  }

  # Put the prompt back (refreshed with the current library's taken names).
  observeEvent(input$lib_import_reset, {
    updateTextAreaInput(session, "lib_import_text",
                        value = role_prompt_text(cfg, 5, cfg_names(role_lib())))
    import_staged(NULL)
  })

  observeEvent(input$lib_import_preview, {
    txt <- input$lib_import_text %||% ""
    # The box ships pre-filled with the prompt; previewing it would otherwise
    # fail with a parser message that explains nothing about what to do next.
    if (grepl("Respond with ONLY a JSON object", txt, fixed = TRUE)) {
      import_staged(list(error = paste0(
        "That is still the prompt. Copy it into an LLM, then paste the JSON it returns ",
        "here -- or use Generate above to have the app do it.")))
      return()
    }
    res <- parse_roles_text(txt)
    if (!is.null(res$error)) {
      import_staged(list(error = res$error)); return()
    }
    v <- validate_import(res$roles)
    import_staged(c(v, list(error = NULL)))
  })

  # Ask the meta provider to design new roles. The result goes through the same
  # staging/preview/validate path as a paste, so generation is never a direct
  # write -- the model proposes, the preview shows, the user commits.
  observeEvent(input$gen_roles, {
    n <- suppressWarnings(as.integer(input$gen_roles_n))
    if (is.na(n) || n < 1) n <- 5
    n <- min(n, 20L)
    field <- trimws(input$gen_roles_field %||% "")
    prov <- input$meta_provider
    log_event("INFO", paste0("Generate roles clicked: n=", n,
                             if (nzchar(field)) paste0(", field='", field, "'") else ", no field",
                             ", provider=", prov, "."))
    msgs <- build_role_gen_messages(cfg, field, n, existing = cfg_names(role_lib()))
    r <- withProgress(message = "Designing roles...", value = 0.5, {
      # Warmer than the other meta calls: role design wants variety, and the
      # schema is simple enough that temperature does not threaten the JSON.
      llm_json(cfg, prov, msgs, meta_key(), max_tokens = 3500, temperature = 0.7,
               use_cache = FALSE, fallbacks = meta_fallbacks(prov))
    })
    log_usage("role generation", r$provider %||% prov, r$model, r$usage, r$cached)
    if (!isTRUE(r$ok) || is.null(r$parsed)) {
      msg <- r$error %||% "the model did not return usable JSON"
      log_event("ERROR", paste("Role generation failed:", msg))
      import_staged(list(error = paste("Role generation failed:", msg)))
      return()
    }
    raw <- r$parsed
    if (!is.null(raw$roles)) raw <- raw$roles      # tolerate a {"roles": [...]} wrapper
    if (!is.null(raw$name)) raw <- list(raw)       # ...or a single object
    v <- validate_import(lapply(raw, .normalize_imported_role))
    if (length(v$ok) == 0 && length(v$rejected) == 0) {
      import_staged(list(error = "The model returned no usable roles.")); return()
    }
    import_staged(c(v, list(error = NULL)))
    showNotification(paste0("Designed ", length(v$ok), " role(s) -- review below, then Import."),
                     type = "message")
  })

  output$lib_import_report <- renderUI({
    s <- import_staged()
    if (is.null(s)) return(NULL)
    if (!is.null(s$error))
      return(tags$div(class = "text-danger", style = "margin-top:8px;", s$error))
    items <- lapply(seq_along(s$ok), function(i) {
      r <- s$ok[[i]]; n <- s$notes[[i]]
      tags$li(tags$b(r$name), tags$span(class = "text-muted",
        paste0(" [", r$category, "] ", r$reasoning, " / ", r$evidence)),
        if (length(n)) tags$div(class = "text-warning", style = "font-size:0.85em;",
                                paste("⚠", paste(n, collapse = "; "))) else NULL)
    })
    rej <- lapply(s$rejected, function(x)
      tags$li(tags$b(x$role$name), tags$span(class = "text-danger",
                                             paste0(" -- skipped: ", paste(x$why, collapse = "; ")))))
    tags$div(style = "margin-top:10px;",
      tags$p(tags$b(paste0(
        sprintf("%d role%s ready to import", length(s$ok), if (length(s$ok) == 1) "" else "s"),
        if (length(s$rejected)) sprintf(", %d skipped", length(s$rejected)) else ""))),
      if (length(items)) tags$ul(items) else NULL,
      if (length(rej)) tags$ul(rej) else NULL)
  })

  observeEvent(input$lib_import_do, {
    s <- import_staged()
    if (is.null(s) || !is.null(s$error) || length(s$ok %||% list()) == 0) {
      showNotification("Nothing staged -- press Preview first.", type = "warning"); return()
    }
    ok <- tryCatch({ append_roles_to_library(CONFIG_DIR, s$ok); TRUE },
                   error = function(e) { showNotification(paste("Import failed:", conditionMessage(e)),
                                                          type = "error"); FALSE })
    if (!ok) return()
    role_lib(c(role_lib(), s$ok))
    updateSelectInput(session, "ag_role", choices = c("Custom", cfg_names(role_lib())))
    import_staged(NULL)
    # Restore the prompt rather than blanking the box, and refresh it so the
    # just-imported names are listed as taken next time.
    updateTextAreaInput(session, "lib_import_text",
                        value = role_prompt_text(cfg, 5, cfg_names(role_lib())))
    log_event("INFO", paste0("Imported ", length(s$ok), " role(s) into the library: ",
                             paste(vapply(s$ok, function(r) r$name, character(1)), collapse = ", "), "."))
    showNotification(paste0("Imported ", length(s$ok), " role(s) into the library."), type = "message")
  })

  # Save the selected agent's role into config/roles.json so it becomes a
  # reusable, pickable role in future sessions (and this one's role picker).
  observeEvent(input$save_agent_to_library, {
    sel <- input$agents_table_rows_selected
    if (length(sel) != 1 || sel > length(rv$agents)) {
      showNotification("Select an agent in the roster first.", type = "warning"); return()
    }
    a <- rv$agents[[sel]]
    role_name <- trimws(a$name %||% "")
    if (!nzchar(role_name)) { showNotification("The agent needs a name first.", type = "warning"); return() }
    if (!is.null(cfg_find(role_lib(), role_name))) {
      showNotification(paste0("A library role named '", role_name, "' already exists -- rename the agent first."),
                       type = "warning"); return()
    }
    # constraints are part of the role's identity, not per-agent trim -- dropping
    # them here would silently save e.g. a Red-Team stripped of its prohibition.
    # Dials go too, so a saved role is re-seated with the temperament it had.
    role <- list(name = role_name,
                 category = if (nzchar(a$category %||% "")) a$category else "Custom",
                 expertise = a$expertise %||% "", reasoning = a$reasoning %||% "Pragmatist",
                 evidence = a$evidence %||% "Expert Consensus",
                 communication = a$communication %||% "", bias = a$bias %||% "",
                 constraints = a$constraints %||% "",
                 history_view = a$history_view %||% "",
                 digest_rounds = a$digest_rounds %||% 0,
                 creativity = a$creativity, skepticism = a$skepticism,
                 risk_tolerance = a$risk_tolerance)
    ok <- tryCatch({ append_role_to_library(CONFIG_DIR, role); TRUE },
                   error = function(e) { showNotification(paste("Could not save:", conditionMessage(e)), type = "error"); FALSE })
    if (!ok) return()
    # Append to the live library so the role is immediately usable this session:
    # it appears in the library table, the picker, and prefills when selected.
    role_lib(c(role_lib(), list(role)))
    updateSelectInput(session, "ag_role", choices = c("Custom", cfg_names(role_lib())))
    showNotification(paste0("Saved '", role_name, "' to the participant library (config/roles.json)."), type = "message")
  })

  # Randomize the run settings (mode, moderator, objective, rounds, tokens,
  # temperature, reasoning effort). Providers/keys are left alone on purpose.
  observeEvent(input$shuffle_settings, {
    updateSelectInput(session, "mode", selected = sample(cfg_names(cfg$debate_modes), 1))
    updateSelectInput(session, "moderator", selected = sample(cfg_names(cfg$moderators), 1))
    updateSelectInput(session, "objective", selected = sample(cfg_names(cfg$objectives), 1))
    updateSliderInput(session, "n_rounds", value = sample(2:6, 1))
    updateSliderInput(session, "max_tokens", value = sample(seq(1500, 3500, by = 50), 1))
    updateSliderInput(session, "temperature", value = round(runif(1, 0.3, 1.0) / 0.05) * 0.05)
    updateSliderInput(session, "reasoning_effort_idx", value = sample(seq_along(REASONING_EFFORT_LEVELS), 1))
    showNotification("Shuffled run settings.", type = "message")
  })

  # =========================================================================
  # DIMENSIONS
  # =========================================================================
  output$dimensions_view <- renderUI({
    dims <- if (!is.null(rv$plan)) rv$plan$dimensions else
      lapply(cfg$dimensions, function(d) list(name = d$name, importance = d$importance, why = d$description))
    if (length(dims) == 0) return(p("No dimensions."))
    default_sel <- vapply(dims, function(d) d$name, character(1))
    tagList(
      checkboxGroupInput("active_dimensions", NULL, choices = default_sel, selected = default_sel),
      hr(),
      lapply(dims, function(d) info_card(d$name, tags$p(em(d$why)),
                                         tags$span(class = "phase-tag", sprintf("importance %.2f", d$importance %||% NA))))
    )
  })
  dimensions_txt <- reactive({
    # input$active_dimensions only exists once the Dimensions tab has rendered
    # its checkbox group. Until then it is NULL -- fall back to the plan's
    # dimensions so a debate still gets dimension guidance without requiring a
    # visit to that tab. (Deselecting all -> character(0), which we honor as "".)
    if (!is.null(input$active_dimensions)) return(paste(input$active_dimensions, collapse = ", "))
    if (!is.null(rv$plan) && length(rv$plan$dimensions)) {
      return(paste(vapply(rv$plan$dimensions, function(d) as.character(d$name %||% ""), character(1)),
                   collapse = ", "))
    }
    ""
  })

  # =========================================================================
  # DEBATE SETUP
  # =========================================================================
  output$model_selectors <- renderUI({
    provs <- input$active_providers
    if (length(provs) == 0) return(p("No active providers."))
    lapply(provs, function(id) {
      prov <- provider_by_id(cfg, id)
      selectInput(paste0("model_", id), prov$label, choices = unlist(prov$models),
                  selected = prov$default_model)
    })
  })
  output$setup_summary <- renderUI({
    tags$div(
      tags$p(tags$b("Mode: "), input$mode, tags$b("  Moderator: "), input$moderator),
      tags$p(tags$b("Objective: "), input$objective),
      tags$p(tags$b("Rounds: "), input$n_rounds, tags$b("  Max tokens: "), input$max_tokens,
             tags$b("  Temp: "), input$temperature),
      tags$p(tags$b("Effective max tokens/turn: "),
             if (length(input$active_providers) == 0) "-" else
               paste(vapply(input$active_providers, function(id)
                 paste0(provider_by_id(cfg, id)$label, "=", provider_max_tokens(cfg, id, input$max_tokens)),
                 character(1)), collapse = "  •  ")),
      tags$p(tags$b("Active providers: "), paste(input$active_providers, collapse = ", ")),
      tags$p(tags$b("Agents on active providers: "),
             length(Filter(function(a) a$provider %in% input$active_providers, rv$agents)))
    )
  })

  # System RAM read (fast, no subprocess) via the ps package; NA if unavailable.
  system_ram <- function() {
    if (requireNamespace("ps", quietly = TRUE)) {
      m <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
      if (!is.null(m)) return(list(avail = m$avail / 1024^3, total = m$total / 1024^3))
    }
    list(avail = NA_real_, total = NA_real_)
  }
  # Live RAM gauge with an adequacy verdict (local 7-8B models need ~5 GB each,
  # so headroom matters). Refreshes every 5s.
  output$ram_status <- renderUI({
    invalidateLater(5000, session)
    r <- system_ram()
    if (is.na(r$avail))
      return(tags$p(class = "text-muted", style = "margin:6px 0;", "System RAM: unavailable."))
    a <- r$avail; tot <- r$total
    v <- if (a >= 6)
           list(col = "#2E7D32", tag = "Adequate",
                msg = "comfortable for a local 7-8B model plus the app, or any cloud debate.")
         else if (a >= 3)
           list(col = "#B9770E", tag = "Tight",
                msg = "cloud debates are fine; a local 7-8B model may be slow -- close heavy apps for local use.")
         else
           list(col = "#C0392B", tag = "Low",
                msg = "risk of slowdowns/instability -- close other apps, especially before running LOCAL models.")
    tags$div(style = paste0("border-left:4px solid ", v$col,
                            "; padding:6px 10px; margin:6px 0; background:rgba(0,0,0,0.03); border-radius:3px;"),
      tags$div(tags$b("Available RAM: "), sprintf("%.1f GB", a),
               tags$span(class = "text-muted", sprintf(" of %.1f GB", tot))),
      tags$div(style = paste0("color:", v$col, "; font-weight:600; margin-top:2px;"),
               v$tag, tags$span(style = "font-weight:400; color:inherit;", paste0(" — ", v$msg))))
  })

  # Quick reachability check for the local (Ollama) server: any HTTP response on
  # its host:port = up; connection refused/timeout = down. Bounded to ~2s.
  local_server_up <- function() {
    prov <- provider_by_id(cfg, "local")
    if (is.null(prov) || is.null(prov$endpoint)) return(NA)
    base <- sub("(https?://[^/]+).*", "\\1", prov$endpoint)   # http://localhost:11434
    resp <- tryCatch(
      httr2::req_perform(httr2::req_error(httr2::req_timeout(httr2::request(base), 2),
                                          is_error = function(r) FALSE)),
      error = function(e) NULL)
    !is.null(resp)
  }
  # Live "is the local model server running?" badge. Refreshes every 8s.
  output$local_server_status <- renderUI({
    invalidateLater(8000, session)
    up <- tryCatch(local_server_up(), error = function(e) NA)
    if (is.na(up)) return(NULL)
    active <- "local" %in% (input$active_providers %||% character(0))
    if (isTRUE(up)) {
      tags$div(style = "margin:4px 0; font-size:0.92em;",
        tags$span(style = "color:#2E7D32; font-weight:700;", "● "),
        tags$b("Local server (Ollama): up"),
        if (!active) tags$span(class = "text-muted", " — tick “Local” in Active providers to use it"))
    } else {
      tags$div(style = "margin:4px 0; font-size:0.92em;",
        tags$span(style = "color:#C0392B; font-weight:700;", "● "),
        tags$b(style = "color:#C0392B;", "Local server (Ollama): down"),
        tags$span(class = "text-muted", " — start Ollama to use local models"))
    }
  })

  # ---- Run log (Debate Setup tab) ----------------------------------------
  observeEvent(input$clear_run_log, { rv$run_log <- list(); showNotification("Run log cleared.", type = "message") })
  output$run_log_summary <- renderUI({
    log <- rv$run_log
    if (length(log) == 0) return(tags$p(class = "text-muted", "No issues logged. Run a deliberation to populate."))
    levels <- vapply(log, function(e) e$level, character(1))
    n_err <- sum(levels == "ERROR"); n_warn <- sum(levels == "WARN")
    tags$p(
      tags$span(class = if (n_err > 0) "text-danger" else "text-muted",
                style = "font-weight:600;", paste0(n_err, " error", if (n_err != 1) "s" else "")),
      "  •  ",
      tags$span(class = if (n_warn > 0) "text-warning" else "text-muted",
                style = "font-weight:600;", paste0(n_warn, " warning", if (n_warn != 1) "s" else "")),
      tags$span(class = "text-muted", paste0("  •  ", length(log), " entries"))
    )
  })
  output$run_log <- renderUI({
    log <- rv$run_log
    if (length(log) == 0) return(NULL)
    colour <- function(lvl) switch(lvl, ERROR = "#C0392B", WARN = "#B9770E", API = "#2C7A7B", "#5B6B7A")
    # newest first
    tags$div(lapply(rev(log), function(e) {
      tags$div(class = "run-log-line",
        tags$span(class = "run-log-time", e$time), " ",
        tags$span(style = paste0("color:", colour(e$level), "; font-weight:600;"),
                  sprintf("%-5s", e$level)), " ",
        tags$span(e$msg))
    }))
  })

  # =========================================================================
  # PROMPT PREVIEW
  # =========================================================================
  observe({
    updateSelectInput(session, "preview_agent",
                      choices = setNames(seq_along(rv$agents), vapply(rv$agents, function(a) a$name, character(1))))
  })
  preview_msgs <- reactive({
    req(length(rv$agents) > 0, input$preview_agent, input$preview_round)
    req(!is.na(input$preview_round))   # numericInput is NA when cleared -> select_phase would error
    idx <- as.integer(input$preview_agent); req(idx <= length(rv$agents))
    a <- rv$agents[[idx]]
    mode_cfg <- cfg_find(cfg$debate_modes, input$mode)
    phase <- select_phase(mode_cfg, input$preview_round, input$n_rounds, idx)
    obj <- cfg_find(cfg$objectives, input$objective)
    build_turn_messages(input$topic, rv$history, kg_summary_text(rv$kg), a, cfg,
                        phase_instruction = phase, mode_name = input$mode,
                        objective_fragment = trimws(paste(obj$prompt_fragment %||% "",
                                                          narrative_fragment() %||% "", sep = "\n")),
                        round_number = input$preview_round, max_tokens = input$max_tokens,
                        dimensions_txt = dimensions_txt(),
                        language = if (nzchar(input$language)) input$language else NULL,
                        problem_details = input$problem_details,
                        critical_rules = if (!identical(input$apply_rules_debate, FALSE)) input$critical_rules else NULL)
  })
  output$preview_system <- renderText(preview_msgs()[[1]]$content)
  output$preview_user   <- renderText(preview_msgs()[[2]]$content)

  # =========================================================================
  # DEBATE ENGINE (cooperative-sequential round loop)
  # =========================================================================
  observeEvent(input$stop_discussion, { rv$stop_requested <- TRUE })

  start_run <- function(source = "manual") {
    if (isTRUE(rv$running)) {
      log_event("WARN", paste0("Run deliberation clicked (", source, ") while a run is in progress -- ignored."))
      showNotification("A deliberation is already running. Click Stop first.", type = "warning"); return()
    }
    rv$run_log <- list()   # fresh diagnostics log for this run attempt
    log_event("INFO", paste0("Run deliberation clicked (", source, ")."))
    # ---- Free memory before a (potentially heavy) run -----------------------
    # Reclaim R's own heap and, when caching is off, drop the response cache
    # (which can hold every prior reply's full text). NOTE: gc() frees only
    # memory held by THIS R process -- it cannot reclaim RAM used by other
    # processes such as Ollama's loaded models. Wrapped so a failure here can
    # never block the run.
    tryCatch({
      if (!isTRUE(input$use_cache)) llm_cache_clear()
      g0 <- gc(full = TRUE)  # full collection across generations
      freed_mb <- tryCatch(round(sum(as.numeric(g0[, "(Mb)"]))), error = function(e) NA_real_)
      log_event("INFO", paste0("Reclaimed R memory before run (gc",
                               if (!isTRUE(input$use_cache)) " + cache purge" else "",
                               "); R heap now ~", freed_mb, " Mb."))
    }, error = function(e) log_event("WARN", paste("Pre-run gc skipped:", conditionMessage(e))))
    # Right after loading a saved debate, the active-providers checkbox update
    # may not have round-tripped from the client yet -- union in the loaded
    # debate's providers (one-shot) so a re-run never fails with a spurious
    # "no agents". Cleared here so subsequent manual runs respect the checkbox.
    active_set <- input$active_providers
    if (!is.null(rv$loaded_providers)) {
      active_set <- union(active_set, rv$loaded_providers); rv$loaded_providers <- NULL
    }
    if (length(active_set) == 0) {
      log_event("ERROR", "No active provider selected.")
      showNotification("Select at least one active provider.", type = "error"); return()
    }
    active_agents <- Filter(function(a) a$provider %in% active_set, rv$agents)
    if (length(active_agents) == 0) {
      log_event("ERROR", "No agents use an active provider (check each agent's provider vs. the active set).")
      showNotification("No agents use an active provider.", type = "error"); return()
    }
    # Fresh run: clear prior state so round numbering and the KG start clean.
    # usage_log is reset too, so the cost meter reflects THIS debate (planner
    # cost from before the run is intentionally not carried in).
    rv$history <- list(); rv$kg <- empty_kg(); rv$analytics <- NULL; rv$consensus <- NULL
    rv$usage_log <- list()
    rv$stop_requested <- FALSE; rv$running <- TRUE
    log_event("INFO", paste0("Started: mode=", input$mode, ", moderator=", input$moderator,
                             ", ", length(active_agents), " agents, ", input$n_rounds, " rounds."))
    topic <- input$topic; mode_name <- input$mode; moderator_name <- input$moderator
    pdetails <- input$problem_details
    # Critical rules apply to agent turns unless "During deliberation" is
    # explicitly unticked. (Default-ON: an unset/NULL value must still count as
    # ON, so we test against FALSE rather than isTRUE, which would treat NULL as off.)
    crules <- if (!identical(input$apply_rules_debate, FALSE)) input$critical_rules else ""
    n_rounds <- input$n_rounds; mode_cfg <- cfg_find(cfg$debate_modes, mode_name)
    # Staged protocols (phase_sequence modes like Narrative Forge) ignore the
    # novelty auto-stop: revision/testing stages rework existing material, so
    # novelty naturally plateaus -- stopping there would skip the remaining
    # stages, including the final candidate the whole loop builds toward.
    staged_mode <- length(mode_cfg$phase_sequence %||% list()) > 0
    if (staged_mode && isTRUE(input$auto_stop))
      log_event("INFO", paste0("Auto-stop ignored: '", mode_name,
                               "' is a staged protocol and always completes its stages."))
    obj_fragment <- (cfg_find(cfg$objectives, input$objective)$prompt_fragment) %||% ""
    # Narrative target rides with the objective so every turn engineers the
    # same kind of story (captured once at run start, like the objective).
    nf <- narrative_fragment()
    if (!is.null(nf)) obj_fragment <- trimws(paste(obj_fragment, nf, sep = "\n"))
    dims_txt <- dimensions_txt()
    meta_prov <- input$meta_provider; meta_k <- meta_key()
    meta_fb <- meta_fallbacks(meta_prov)   # meta-call provider failover list
    # Free local (Ollama) RAM when THIS debate uses no local model -- neither any
    # participant nor the meta (planner/moderator/consensus) provider. Skipped
    # entirely if a local model is in play, so it never unloads a model we need.
    uses_local <- identical(meta_prov, "local") ||
      any(vapply(active_agents, function(a) identical(a$provider, "local"), logical(1)))
    if (!uses_local) {
      unloaded <- ollama_unload_all(ollama_base_url(cfg))
      if (length(unloaded) > 0)
        log_event("INFO", paste0("Freed RAM: unloaded ", length(unloaded),
                                 " idle Ollama model(s): ", paste(unloaded, collapse = ", "), "."))
    }
    max_tokens <- input$max_tokens; temperature <- input$temperature
    reff <- reasoning_effort(); lang <- if (nzchar(input$language)) input$language else NULL
    use_cache <- input$use_cache

    # ---- Provider failover --------------------------------------------------
    # If an agent's (provider, model) errors mid-run, reassign it to a working
    # one and retry, so one flaky provider can't kill participants' turns.
    #
    # Failover works on SLOTS = (provider, model) pairs, so a failed LOCAL model
    # can fall back to another LOCAL model before touching paid cloud.
    #  - CLOUD agent fails  -> cloud slots only, in fixed priority: DeepSeek,
    #    Mistral, Celeris, Claude, OpenAI, then any other keyed provider. `local`
    #    is never an auto-target for a cloud agent.
    #  - LOCAL agent fails  -> other LOCAL models FIRST (prefer-local policy),
    #    then the cloud priority list as a last resort.
    # Within either order we first try slots NOT already used by another agent
    # (diversity); only if every fresh slot is exhausted do we allow a repeat.
    # `run_failed_keys` accumulates slots that errored so a known-bad one is
    # never re-picked. Slots are keyed "provider|model" (model resolved to the
    # provider default when unset) so tracking is consistent.
    keyed_pool <- available_providers(cfg, rv$ui_keys)
    realloc_priority <- c("deepseek", "mistral", "celeris", "claude", "openai")
    cloud_ranked <- {
      pref  <- intersect(realloc_priority, keyed_pool)              # preferred, in order
      other <- setdiff(intersect(provider_ids(cfg), keyed_pool),        # remaining keyed, config
                       c(realloc_priority, "local", "gpt4all", "perplexity"))  # order; no local servers/perplexity
      c(pref, other)
    }
    local_models <- unlist(provider_by_id(cfg, "local")$models %||% list())
    resolve_model <- function(p, m) m %||% (provider_by_id(cfg, p)$default_model)
    slot_key   <- function(p, m) paste0(p, "|", resolve_model(p, m))
    cloud_slots <- lapply(cloud_ranked, function(p) list(provider = p, model = model_for(p)))
    local_slots <- lapply(local_models, function(m) list(provider = "local", model = m))
    run_failed_keys <- character(0)
    used_slot_keys <- function()
      vapply(active_agents, function(x) slot_key(x$provider, x$model), character(1))
    pick_replacement <- function(cur_p, cur_m, failed_keys, in_use_keys = character(0)) {
      cands <- if (identical(cur_p, "local")) c(local_slots, cloud_slots) else cloud_slots
      ck <- slot_key(cur_p, cur_m)
      cands <- Filter(function(s) {
        k <- slot_key(s$provider, s$model); !identical(k, ck) && !(k %in% failed_keys)
      }, cands)
      if (length(cands) == 0) return(NULL)
      fresh <- Filter(function(s) !(slot_key(s$provider, s$model) %in% in_use_keys), cands)
      pool  <- if (length(fresh) > 0) fresh else cands
      pool[[1]]
    }

    progress <- shiny::Progress$new(session, min = 0, max = n_rounds)
    progress$set(message = "Running deliberation...", value = 0)
    finish_run <- function() { try(progress$close(), silent = TRUE); rv$running <- FALSE; rv$current_speaker <- NULL }

    run_round <- function(r) {
      isolate({
        if (r > n_rounds || rv$stop_requested) {
          if (isTRUE(rv$stop_requested)) log_event("INFO", paste0("Stopped by user before round ", r, "."))
          finish_run(); return(invisible(NULL))
        }
        ok <- tryCatch({
          round_texts <- list()
          for (i in seq_along(active_agents)) {
            a <- active_agents[[i]]
            # Resolve the concrete model up front so slot keys are consistent.
            # Keep an already-set model (e.g. a prior round's reallocation);
            # otherwise take the UI selector, else the provider default. Without
            # the `a$model %||%` guard, a reallocated local agent would be reset
            # to the UI/default model each round and flip-flop between models.
            a$model <- resolve_model(a$provider, a$model %||% model_for(a$provider))
            # Pre-emptive swap: if this agent's (provider|model) already failed
            # earlier this run, move it before wasting a guaranteed error.
            if (slot_key(a$provider, a$model) %in% run_failed_keys) {
              rp <- pick_replacement(a$provider, a$model, run_failed_keys, used_slot_keys())
              if (!is.null(rp)) {
                old <- slot_key(a$provider, a$model)
                a$provider <- rp$provider; a$model <- resolve_model(rp$provider, rp$model)
                log_event("INFO", paste0("Round ", r, ": moved ", a$name, " off failed '",
                                         old, "' to '", slot_key(a$provider, a$model), "'."))
              }
            }
            rv$current_speaker <- a$name
            phase <- select_phase(mode_cfg, r, n_rounds, i)
            # Try the turn; on error, remember the bad slot, reallocate to a fresh
            # one (local-first for local agents), and retry until one works or the
            # pool is exhausted (then the [ERROR:] placeholder stands, as before).
            # "Commit before you converse": in a mode flagged independent_first_round
            # (Adversarial Scrutiny), round-1 turns are formed blind -- each agent
            # sees the topic and problem details but no same-round peers, so first
            # positions are independent judgments, not reactions to whoever spoke
            # first. From round 2 the full history flows as usual.
            blind <- r == 1 && isTRUE(mode_cfg$independent_first_round)
            hist_for_turn <- if (blind) Filter(function(h) (h$round %||% 0) < r, rv$history) else rv$history
            # Independent-audit window (Source Auditor): within the agent's
            # digest_rounds window it sees a conclusion-free claims digest
            # instead of the verbatim transcript. Empty digest (moderator
            # produced nothing yet) falls back to the full transcript, so the
            # auditor never audits from nothing.
            digest_txt <- if (!blind && identical(a$history_view %||% "", "claims_digest") &&
                              r <= (a$digest_rounds %||% 0)) claims_digest(hist_for_turn) else ""
            audit_view <- nzchar(digest_txt)
            turn <- NULL
            repeat {
              # A blind turn gets NO same-round signals: history is filtered above,
              # and group confidence / provisional consensus are withheld too --
              # analytics update per turn, so they would leak the peers' positions
              # into a round that is supposed to produce independent judgments.
              # The audit view withholds the same signals: both exist to keep the
              # turn's judgment independent of the group's stated conclusions.
              turn <- run_turn(cfg, a, phase, topic, hist_for_turn, rv$kg, mode_name, obj_fragment,
                               dims_txt, r, resolve_api_key(cfg, a$provider, rv$ui_keys),
                               max_tokens, temperature, reff,
                               history_override_txt = if (audit_view) digest_txt else NULL,
                               current_confidence = if (!blind && !audit_view && !is.null(rv$analytics) && nrow(rv$analytics) > 0)
                                 paste0(round(mean(rv$analytics$confidence, na.rm = TRUE)), "%") else NULL,
                               current_consensus = if (!blind && !audit_view && !is.null(rv$consensus)) rv$consensus$consensus else NULL,
                               language = lang, use_cache = use_cache,
                               problem_details = pdetails, critical_rules = crules)
              if (isTRUE(turn$ok)) break
              run_failed_keys <<- union(run_failed_keys, slot_key(a$provider, a$model))
              rp <- pick_replacement(a$provider, a$model, run_failed_keys, used_slot_keys())
              if (is.null(rp)) break   # no working alternative -- keep the error turn
              old  <- slot_key(a$provider, a$model)
              a$provider <- rp$provider; a$model <- resolve_model(rp$provider, rp$model)
              newk <- slot_key(a$provider, a$model)
              log_event("WARN", paste0("Round ", r, " ", a$name, ": '", old, "' failed (",
                                       turn$error %||% "error", ") -- reallocating to '", newk, "'."))
              # Carry a short reason in the toast too: the full error goes to the
              # Run log, but the toast is what the user actually sees, and
              # "X failed" without a cause sends them hunting for it later.
              why <- gsub("[[:space:]]+", " ", trimws(turn$error %||% ""))
              if (nchar(why) > 90) why <- paste0(substr(why, 1, 90), "...")
              showNotification(paste0(a$name, ": ", old, " failed -- switched to ", newk,
                                      if (nzchar(why)) paste0("\n", why) else ""),
                               type = "warning", duration = 8)
            }
            # Keep the reassignment for the rest of THIS run (so a reallocated
            # agent stays on its working provider next round instead of reverting
            # and re-erroring). Deliberately NOT written back to rv$agents: the
            # configured roster stays intact so the next run starts fresh from the
            # user's chosen providers (and retries a recovered provider). The
            # actual provider used each turn is recorded in the transcript/log.
            active_agents[[i]] <<- a
            rv$history <- c(rv$history, list(turn))
            rv$agents <- update_agent_memory(rv$agents, turn)
            round_texts[[a$name]] <- turn$text
            log_usage("agent turn", turn$provider, turn$model, turn$usage, turn$cached,
                      agent = turn$agent, round = r)
            if (!isTRUE(turn$ok)) {
              log_event("ERROR", paste0("Round ", r, " ", turn$agent, " (", turn$provider, "): ",
                                        turn$error %||% "unknown error (no working provider left to reallocate)"))
            }
          }
          mod <- moderator_call(cfg, topic, round_texts, r, meta_prov, meta_k, mode_name, moderator_name,
                                fallbacks = meta_fb)
          if (isTRUE(mod$ok) && !is.null(mod$provider) && !identical(mod$provider, meta_prov))
            log_event("WARN", paste0("Round ", r, " moderator failed over from '", meta_prov, "' to '", mod$provider, "'."))
          log_usage("moderator", mod$provider %||% meta_prov, mod$model, mod$usage, mod$cached, round = r)
          if (!isTRUE(mod$ok)) {
            log_event("WARN", paste0("Round ", r, " moderator fell back to heuristic: ", mod$error))
            showNotification(paste0("Round ", r, " moderator fell back to heuristic: ", mod$error),
                             type = "warning", duration = 6)
          }
          rv$kg <- kg_add_round(rv$kg, mod$data$nodes %||% list(), mod$data$edges %||% list(), r)
          rv$history[[length(rv$history)]]$moderator <- mod$data
          rv$analytics <- analytics_append(rv$analytics, rv$history, topic)
          progress$set(value = r, detail = paste("Round", r, "of", n_rounds))
          TRUE
        }, error = function(e) {
          log_event("ERROR", paste0("Deliberation aborted on round ", r, ": ", conditionMessage(e)))
          showNotification(paste0("Deliberation aborted on round ", r, ": ", conditionMessage(e)),
                           type = "error", duration = 10); FALSE
        })
        if (!isTRUE(ok)) { finish_run(); return(invisible(NULL)) }
        stop_now <- isTRUE(input$auto_stop) && !staged_mode && r >= 3 &&
          auto_stop_reached(rv$analytics, length(active_agents))
        if (stop_now) {
          log_event("INFO", paste0("Auto-stopped at round ", r, " (novelty plateaued)."))
          showNotification(paste("Auto-stopped at round", r, "-- novelty plateaued."), type = "message")
        }
        if (r >= n_rounds || stop_now) { log_event("INFO", paste0("Deliberation finished at round ", r, ".")); finish_run() }
        else later::later(function() shiny::withReactiveDomain(session, run_round(r + 1)), delay = 0)
      })
    }
    run_round(1)
  }
  # Every "Run deliberation" click is logged (from either button). A rejected
  # click (no panel yet) is logged here; an accepted one is logged as the first
  # line of the fresh run log inside start_run() (which clears the log per run).
  handle_run_click <- function(source) {
    if (length(rv$agents) == 0) {
      log_event("WARN", paste0("Run deliberation clicked (", source, ") but no panel built yet -- aborted."))
      showNotification("Build a panel first (Planner or Debate Setup).", type = "warning")
      return(invisible())
    }
    start_run(source)
  }
  observeEvent(input$run_discussion,  handle_run_click("Debate Setup"), ignoreInit = TRUE)
  observeEvent(input$run_discussion2, handle_run_click("Live Debate"),  ignoreInit = TRUE)

  # ---- Live views ---------------------------------------------------------
  output$live_header <- renderText({
    if (isTRUE(rv$running)) paste0("Running... current speaker: ", rv$current_speaker %||% "-")
    else if (length(rv$history) > 0) "Deliberation complete" else "Not started"
  })
  output$conversation_view <- renderUI({
    if (length(rv$history) == 0) return(p("No deliberation yet. Configure and click Run Deliberation."))
    tags$div(lapply(rv$history, function(h) {
      is_err <- grepl("^\\[ERROR", trimws(h$text))
      conf <- h$confidence %||% NA
      tags$div(class = if (is_err) "turn-card turn-error" else "turn-card",
        tags$div(class = "turn-head",
                 paste0("Round ", h$round, " -- ", h$agent, " (", h$provider, ")"),
                 if (!is.na(conf)) tags$span(class = "conf-badge", paste0("conf ", conf, "%"))),
        tags$div(class = "turn-body", h$text))
    }))
  })
  output$moderator_view <- renderUI({
    if (length(rv$history) == 0) return(p("No moderator summaries yet."))
    rounds <- sort(unique(vapply(rv$history, function(h) h$round, numeric(1))))
    cards <- lapply(rounds, function(r) {
      entries <- Filter(function(h) h$round == r && !is.null(h$moderator), rv$history)
      if (length(entries) == 0) return(NULL)
      m <- entries[[length(entries)]]$moderator
      info_card(paste("Round", r),
        tags$p(tags$b("Group confidence: "), paste0(m$group_confidence_pct %||% "NA", "%")),
        labelled_list("Agreements", m$agreements),
        labelled_list("Disagreements", m$disagreements),
        labelled_list("New hypotheses", m$new_hypotheses),
        labelled_list("Fallacies", m$fallacies),
        tags$p(tags$b("Uncertainty: "), m$uncertainty %||% ""))
    })
    tags$div(Filter(Negate(is.null), cards))
  })
  # Per-agent line colours. plotly's default qualitative palette is Set2, which
  # caps at 8 -- a 9+ agent roster (one click via the participant library) made
  # every render warn and interpolate. Same Set2 look up to 8 agents; beyond
  # that, distinct hues from base R's hcl palette, which scales to any n.
  agent_palette <- function(agents) {
    n <- length(unique(agents))
    set2 <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
              "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    if (n <= length(set2)) set2[seq_len(max(n, 1))] else grDevices::hcl.colors(n, "Dark 3")
  }
  output$plot_confidence <- renderPlotly({
    df <- rv$analytics; req(df, nrow(df) > 0)
    plot_ly(df, x = ~round, y = ~confidence, color = ~agent, colors = agent_palette(df$agent),
            type = "scatter", mode = "lines+markers") |>
      plotly::layout(yaxis = list(title = "Confidence %"), xaxis = list(title = "Round"))
  })
  output$plot_novelty <- renderPlotly({
    df <- rv$analytics; req(df, nrow(df) > 0)
    plot_ly(df, x = ~round, y = ~novelty, color = ~agent, colors = agent_palette(df$agent),
            type = "scatter", mode = "lines+markers") |>
      plotly::layout(yaxis = list(title = "Novelty"), xaxis = list(title = "Round"))
  })

  # ---- Cost / token accounting -------------------------------------------
  usage_df <- reactive(usage_to_df(rv$usage_log, cfg$pricing))
  observeEvent(input$reset_cost, {
    rv$usage_log <- list()
    showNotification("Cost meter reset.", type = "message")
  })
  output$cost_summary <- renderUI({
    df <- usage_df()
    if (nrow(df) == 0) return(p(class = "text-muted", "No LLM calls billed yet. Run a deliberation."))
    total <- sum(df$cost, na.rm = TRUE)
    pt <- sum(df$prompt_tokens, na.rm = TRUE); ct <- sum(df$completion_tokens, na.rm = TRUE)
    tags$div(
      tags$span(style = "font-size:150%; font-weight:600; color:#2C5F8A;",
                sprintf("~$%.4f", total)),
      tags$span(class = "text-muted", style = "margin-left:10px;",
                sprintf("estimated  •  %s calls (%s cached)  •  %s prompt + %s completion tokens",
                        nrow(df), sum(df$cached), format(pt, big.mark = ","), format(ct, big.mark = ",")))
    )
  })
  output$cost_table <- renderDT({
    bd <- cost_breakdown(usage_df())
    if (nrow(bd) > 0) bd$cost_usd <- sprintf("$%.4f", bd$cost_usd)
    datatable(bd, rownames = FALSE, options = list(dom = "t"),
              colnames = c("Call type", "Calls", "Cached", "Prompt tok", "Completion tok", "Cost (USD)"))
  })

  # =========================================================================
  # KNOWLEDGE GRAPH
  # =========================================================================
  output$kg_plot <- renderVisNetwork(render_kg_visnetwork(rv$kg))
  output$idea_evolution_table <- renderDT({
    current_round <- if (length(rv$history) == 0) 1L else as.integer(max(vapply(rv$history, function(h) h$round, numeric(1))))
    datatable(idea_evolution_table(rv$kg, current_round), rownames = FALSE, options = list(dom = "tp"))
  })
  output$dl_graphml <- downloadHandler(
    filename = function() out_filename("KnowledgeGraph", "graphml"),
    content = function(file) { ensure_writable(file); export_graphml(rv$kg, file) })
  output$dl_kg_csv <- downloadHandler(
    filename = function() out_filename("KnowledgeGraph", "csv"),
    content = function(file) {
      ensure_writable(file)
      nodes_out <- if (nrow(rv$kg$nodes) > 0) data.frame(record_type = "NODE", id = rv$kg$nodes$id,
        label = rv$kg$nodes$label, type_or_relation = rv$kg$nodes$type, from = NA, to = NA,
        round = rv$kg$nodes$round, stringsAsFactors = FALSE) else NULL
      edges_out <- if (nrow(rv$kg$edges) > 0) data.frame(record_type = "EDGE", id = NA, label = NA,
        type_or_relation = rv$kg$edges$relation, from = rv$kg$edges$from, to = rv$kg$edges$to,
        round = rv$kg$edges$round, stringsAsFactors = FALSE) else NULL
      out <- rbind(nodes_out, edges_out)  # both NULL when the graph is empty
      if (is.null(out)) out <- data.frame(record_type = character(), id = character(),
        label = character(), type_or_relation = character(), from = character(),
        to = character(), round = integer(), stringsAsFactors = FALSE)
      write.csv(out, file, row.names = FALSE)
    })

  # =========================================================================
  # CONSENSUS
  # =========================================================================
  observeEvent(input$run_consensus, {
    req(length(rv$history) > 0)
    withProgress(message = "Synthesizing consensus...", value = 0.5, {
      res <- consensus_engine(cfg, input$topic, export_txt(rv$history), input$meta_provider, meta_key(),
                              use_cache = input$use_cache,
                              fallbacks = meta_fallbacks(input$meta_provider),
                              critical_rules = if (!identical(input$apply_rules_consensus, FALSE)) input$critical_rules else NULL,
                              synthesiser = identical(rv$plan$panel %||% "", "five_role"),
                              extra_context = narrative_fragment())
    })
    if (!isTRUE(res$ok)) {
      log_event("ERROR", paste("Consensus failed:", res$error))
      showNotification(paste("Consensus failed:", res$error), type = "error")
    } else rv$consensus <- res$data
    if (isTRUE(res$ok) && !is.null(res$provider) && !identical(res$provider, input$meta_provider))
      log_event("WARN", paste0("Consensus failed over from '", input$meta_provider, "' to '", res$provider, "'."))
    log_usage("consensus", res$provider %||% input$meta_provider, res$model, res$usage, res$cached)
  })
  # Coordinator line shown under the heading of every exported PDF.
  coord_meta <- function() {
    c0 <- trimws(input$coordinator %||% "")
    if (nzchar(c0)) paste0("Debate coordinator: ", c0) else NULL
  }
  # Debate title sanitized for the filesystem ("" when blank/unusable).
  # Truncated so a paragraph-length title cannot push paths past Windows'
  # 260-character limit when combined with the sessions/Downloads directory.
  clean_title <- function() {
    t <- trimws(input$debate_title %||% "")
    t <- gsub("[^A-Za-z0-9_-]+", "_", t)
    t <- gsub("^_+|_+$", "", gsub("_+", "_", t))
    substr(t, 1, 60)
  }
  # Uniform output filenames: <prefix>_<debate title>_<date-time>.<ext>.
  # A blank title drops that segment.
  out_filename <- function(prefix, ext) {
    t <- clean_title()
    paste0(prefix, if (nzchar(t)) paste0("_", t) else "", "_",
           format(Sys.time(), "%Y-%m-%d_%H-%M"), ".", ext)
  }
  # Default saved-session name: <debate title>_<date-time> (Session_... when
  # no title is set), matching the download-filename scheme.
  session_default_name <- function() {
    t <- clean_title()
    paste0(if (nzchar(t)) t else "Session", "_", format(Sys.time(), "%Y-%m-%d_%H-%M"))
  }
  # The value last auto-filled into the save-name boxes. Tracked so we can
  # (a) refresh them when the debate title changes WITHOUT clobbering a name the
  # user typed, and (b) still treat an auto-filled name as "defaulted" at save
  # time, so it keeps the never-overwrite protection a blank field would get.
  session_name_auto <- reactiveVal("")

  # Debate title (Planner tab) -> the "Save as" boxes, so the name you will save
  # under is visible up front instead of hiding behind a placeholder. Clearing
  # the title clears the boxes again (restoring the placeholder hint).
  observeEvent(input$debate_title, {
    nm <- if (nzchar(clean_title())) session_default_name() else ""
    prev <- session_name_auto()
    for (id in c("session_name", "session_name2")) {
      cur <- trimws(input[[id]] %||% "")
      # Only touch a field the user has not customized (blank, or still holding
      # exactly what we put there last time).
      if (!nzchar(cur) || identical(cur, prev)) updateTextInput(session, id, value = nm)
    }
    session_name_auto(nm)
  }, ignoreInit = TRUE)
  # Plan to fold into the consensus report, when the checkbox is ticked.
  consensus_plan <- function() if (isTRUE(input$include_plan_in_consensus)) rv$plan else NULL
  # Debate-quality scorecard from the moderator/KG data (NULL until there's a run).
  dq <- reactive(if (length(rv$history) == 0) NULL else debate_quality(rv$history, rv$kg))
  # Live card on the Consensus tab (same markup used in the report).
  output$debate_quality_card <- renderUI({ q <- dq(); if (is.null(q)) return(NULL); quality_html(q) })

  observeEvent(input$copy_consensus, {
    if (is.null(rv$consensus)) { showNotification("No consensus to copy yet.", type = "warning"); return() }
    session$sendCustomMessage("clipboard_copy",
                              consensus_to_text(rv$consensus, input$topic, plan = consensus_plan(), quality = dq()))
    showNotification("Consensus copied to clipboard.", type = "message")
  })
  output$dl_consensus_pdf <- downloadHandler(
    filename = function() out_filename("Consensus", "pdf"),
    content = function(file) {
      # Exact on-screen format via headless Chrome; fall back to the plain-text
      # PDF if chromote/Chrome is unavailable so the download never fails.
      # chromote pumps the later loop, so skip it while a deliberation is
      # running (it could re-enter the round loop mid-download).
      ensure_writable(file)
      meta <- coord_meta(); plan <- consensus_plan()
      # Optional landscape knowledge-graph page: render the graph to a static PNG
      # (skipped while a run is active, or if the graph is empty / render fails).
      kg_png <- NULL
      if (isTRUE(input$include_kg_consensus) && !isTRUE(rv$running) &&
          !is.null(rv$kg) && isTRUE(nrow(rv$kg$nodes) > 0)) {
        kg_png <- tryCatch(kg_to_png(rv$kg, tempfile(fileext = ".png")), error = function(e) NULL)
        if (is.null(kg_png)) showNotification("Could not render the knowledge graph; PDF made without it.",
                                              type = "warning", duration = 5)
      }
      quality <- dq()
      ok <- if (isTRUE(rv$running)) FALSE else
        tryCatch({ html_to_pdf(consensus_html(input$topic, rv$consensus, meta = meta, plan = plan,
                                              kg_png = kg_png, quality = quality,
                                              synthesiser = identical(rv$plan$panel %||% "", "five_role")), file); TRUE },
                 error = function(e) FALSE)
      if (!ok) text_to_pdf(consensus_to_text(rv$consensus, input$topic, plan = plan, quality = quality), file,
                           title = paste("Consensus:", input$topic), subtitle = meta)
      if (!is.null(kg_png)) unlink(kg_png)
    })
  output$consensus_view <- renderUI({
    con <- rv$consensus
    if (is.null(con)) return(p("No consensus yet. Run a deliberation, then generate."))
    dm <- con$decision_matrix
    dm_ui <- if (!is.null(dm) && length(dm) > 0) {
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Option"), tags$th("Pros"), tags$th("Cons"), tags$th("Verdict"))),
        tags$tbody(lapply(dm, function(row) tags$tr(
          tags$td(row$option %||% ""), tags$td(row$pros %||% ""),
          tags$td(row$cons %||% ""), tags$td(row$verdict %||% "")))))
    } else NULL
    tags$div(
      info_card("Consensus", tags$p(con$consensus %||% ""), accent = "#1E8449"),
      info_card("Minority report", tags$p(con$minority_report %||% ""), accent = "#C0392B"),
      info_card("Confidence interval", tags$p(con$confidence_interval %||% "")),
      info_card("Open questions", tags$ul(lapply(unlist(con$open_questions), tags$li))),
      info_card("Recommendations", tags$ul(lapply(unlist(con$recommendations), tags$li))),
      info_card("Future experiments", tags$ul(lapply(unlist(con$future_experiments), tags$li))),
      if (!is.null(dm_ui)) info_card("Decision matrix", dm_ui)
    )
  })

  # =========================================================================
  # EXPORT
  # =========================================================================
  observeEvent(input$gen_export, {
    req(length(rv$history) > 0)
    withProgress(message = "Generating output...", value = 0.5, {
      res <- synthesize_format(cfg, input$export_format, input$topic, rv$history, rv$kg, rv$analytics,
                               rv$plan, rv$consensus, input$meta_provider, meta_key(), use_cache = input$use_cache)
    })
    if (!isTRUE(res$ok)) {
      log_event("ERROR", paste("Export failed:", res$error))
      showNotification(paste("Export failed:", res$error), type = "error"); return()
    }
    rv$formatted_output <- res$text
    if (!is.null(res$model)) log_usage("export", input$meta_provider, res$model, res$usage, res$cached)
  })
  output$export_preview <- renderText({
    if (!nzchar(rv$formatted_output)) "Generate a format to preview it here." else
      substr(rv$formatted_output, 1, 6000)
  })
  output$dl_formatted <- downloadHandler(
    filename = function() {
      fmt <- cfg_find(cfg$output_formats, input$export_format)
      out_filename(paste0("Deliberation_", gsub("[^A-Za-z0-9]", "_", input$export_format)),
                   fmt$extension %||% "txt")
    },
    content = function(file) { ensure_writable(file); writeLines(rv$formatted_output, file) })
  # Moderator-comments block for text/markdown transcripts, when requested.
  mod_comments <- function() if (isTRUE(input$include_moderator_in_transcript)) moderator_comments_text(rv$history) else ""
  incl_mod     <- function() isTRUE(input$include_moderator_in_transcript)

  observeEvent(input$copy_debate, {
    if (length(rv$history) == 0) { showNotification("No debate to copy yet.", type = "warning"); return() }
    session$sendCustomMessage("clipboard_copy",
                              paste0(export_markdown(input$topic, rv$history), mod_comments()))
    showNotification("Debate transcript copied to clipboard.", type = "message")
  })
  output$dl_debate_pdf <- downloadHandler(
    filename = function() out_filename("Deliberation", "pdf"),
    content = function(file) {
      meta <- coord_meta()
      ok <- if (isTRUE(rv$running)) FALSE else
        tryCatch({ html_to_pdf(debate_html(input$topic, rv$history, meta = meta, moderator = incl_mod()), file); TRUE },
                 error = function(e) FALSE)
      if (!ok) {
        body_txt <- export_txt(rv$history)
        # "\f" starts the moderator comments on a fresh page in the text PDF.
        if (incl_mod()) body_txt <- paste0(body_txt, "\n\f\n", moderator_comments_text(rv$history))
        text_to_pdf(body_txt, file, title = paste("Deliberation:", input$topic), subtitle = meta)
      }
    })
  output$dl_txt <- downloadHandler(function() out_filename("Deliberation", "txt"), function(file) {
    ensure_writable(file)
    writeLines(paste0(export_txt(rv$history), mod_comments()), file) })
  output$dl_md  <- downloadHandler(function() out_filename("Deliberation", "md"), function(file) {
    ensure_writable(file)
    writeLines(paste0(export_markdown(input$topic, rv$history), mod_comments()), file) })
  output$dl_json <- downloadHandler(function() out_filename("Deliberation", "json"), function(file) {
    ensure_writable(file)
    writeLines(as.character(export_json(input$topic, rv$history, rv$kg, rv$analytics, rv$plan, rv$consensus)), file) })
  output$dl_csv <- downloadHandler(function() out_filename("Deliberation", "csv"), function(file) {
    ensure_writable(file)
    write.csv(export_csv_history(rv$history), file, row.names = FALSE) })

  # =========================================================================
  # SETTINGS: keys, cache, sessions
  # =========================================================================
  output$key_inputs <- renderUI({
    lapply(cfg$providers, function(p) {
      if (!isTRUE(p$needs_key)) return(NULL)
      passwordInput(paste0("key_", p$key_name), p$label, value = "")
    })
  })
  observeEvent(input$save_keys, {
    keys <- list()
    for (p in cfg$providers) {
      if (!isTRUE(p$needs_key)) next
      v <- input[[paste0("key_", p$key_name)]]
      if (!is.null(v) && nzchar(v)) keys[[p$key_name]] <- v
    }
    rv$ui_keys <- keys
    updateCheckboxGroupInput(session, "active_providers", selected = available_providers(cfg, keys))
    showNotification("Using entered keys for this session.", type = "message")
  })

  key_test <- reactiveVal(NULL)

  # Ping every active provider; then DESELECT any whose key failed so a broken
  # provider can't silently poison a run. Shared by the sidebar button and the
  # Settings button.
  test_and_prune_keys <- function() {
    provs <- input$active_providers
    if (length(provs) == 0) {
      key_test(list(list(id = NA_character_, label = "", ok = FALSE, msg = "No active providers.")))
      return(invisible())
    }
    log_event("INFO", paste0("Test keys clicked: pinging ", length(provs), " active provider(s)."))
    ping <- list(list(role = "user", content = "Reply with just: OK"))
    res <- withProgress(message = "Testing keys...", value = 0, {
      lapply(seq_along(provs), function(i) {
        p <- provs[i]; incProgress(1 / length(provs), detail = provider_by_id(cfg, p)$label)
        r <- llm_chat(cfg, p, ping, resolve_api_key(cfg, p, rv$ui_keys),
                      max_tokens = 768, temperature = 0, reasoning_effort = NULL, use_cache = FALSE)
        log_event("API", paste0("key test | ", p, "/", r$model %||% "?", " | ",
                                if (isTRUE(r$ok)) "OK" else paste0("FAILED: ", r$error %||% "unknown error")))
        list(id = p, label = provider_by_id(cfg, p)$label, ok = isTRUE(r$ok),
             msg = if (isTRUE(r$ok)) "working" else (r$error %||% "unknown error"))
      })
    })
    key_test(res)
    # Prune providers whose key did not work.
    failing <- vapply(res, function(x) if (isTRUE(x$ok)) NA_character_ else (x$id %||% NA_character_), character(1))
    failing <- failing[!is.na(failing)]
    if (length(failing) > 0) {
      updateCheckboxGroupInput(session, "active_providers", selected = setdiff(provs, failing))
      # A failing provider used for meta calls is a problem too -- move it to a
      # working provider if one remains.
      working <- setdiff(provs, failing)
      if (input$meta_provider %in% failing && length(working) > 0) {
        updateSelectInput(session, "meta_provider", selected = working[1])
      }
      lbls <- vapply(failing, function(id) provider_by_id(cfg, id)$label, character(1))
      showNotification(paste0("Disabled non-working provider(s): ", paste(lbls, collapse = ", "), "."),
                       type = "warning", duration = 8)
    } else {
      showNotification("All active providers are working.", type = "message")
    }
    invisible()
  }
  observeEvent(input$test_keys, test_and_prune_keys())
  observeEvent(input$test_keys_sidebar, test_and_prune_keys())

  # Shared renderer for the test results (used in Settings and the sidebar).
  render_key_test <- function() {
    res <- key_test(); if (is.null(res)) return(NULL)
    tags$div(style = "margin-top:6px;", lapply(res, function(r) {
      if (isTRUE(r$ok)) tags$div(style = "color:#1a7f37;font-size:88%;", paste0("✓ ", r$label, ": ", r$msg))
      else tags$div(style = "color:#b00020;font-size:88%;", title = r$msg, paste0("✗ ", r$label, ": ", substr(r$msg, 1, 140)))
    }))
  }
  output$key_test_result <- renderUI(render_key_test())
  output$key_test_result_sidebar <- renderUI(render_key_test())
  observeEvent(input$clear_cache, { llm_cache_clear(); showNotification("Cache cleared.", type = "message") })
  output$cache_status <- renderText(paste0("Cache entries: ", llm_cache_size()))

  # Bumped after save/delete so the sessions table refreshes.
  sessions_refresh <- reactiveVal(0)
  sessions_meta <- reactive({ sessions_refresh(); list_session_meta() })

  output$sessions_table <- renderDT({
    m <- sessions_meta()
    if (nrow(m) == 0) {
      return(datatable(data.frame(Message = "No saved debates yet."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    disp <- data.frame(Name = m$name, Topic = substr(m$topic, 1, 60), Saved = m$saved_at,
                       Rounds = m$rounds, Agents = m$agents,
                       `Cost $` = ifelse(is.na(m$cost), "", sprintf("%.4f", m$cost)),
                       check.names = FALSE, stringsAsFactors = FALSE)
    datatable(disp, selection = "single", rownames = FALSE,
              options = list(dom = "tp", pageLength = 6))
  })

  # Shared save routine (used by the Home button and the Debate Setup copy).
  do_save_session <- function(name) {
    # Blank name -> the default scheme <debate title>_<date-time>, so one click
    # saves under a meaningful name. Overwrite semantics: an EXPLICIT name may
    # overwrite (that is how a loaded session is updated -- selecting a row
    # copies its name into this field), but a DEFAULTED name never does -- two
    # quick saves in the same minute get a numbered suffix instead of the
    # second silently clobbering the first.
    name <- trimws(name %||% "")
    # "Defaulted" = blank, or still exactly the value we auto-filled from the
    # debate title. Both get collision protection; only a name the user actually
    # typed may overwrite an existing session.
    defaulted <- !nzchar(name) || identical(name, trimws(session_name_auto()))
    if (!nzchar(name)) name <- session_default_name()
    if (defaulted) {
      taken <- function(n) file.exists(file.path(sessions_dir(),
                                                 paste0(gsub("[^A-Za-z0-9_-]", "_", n), ".rds")))
      if (taken(name)) {
        i <- 2L
        while (taken(paste0(name, "_", i))) i <- i + 1L
        name <- paste0(name, "_", i)
      }
    }
    # Capture per-provider model overrides (may be NULL if Debate Setup unseen).
    models <- setNames(lapply(provider_ids(cfg), function(id) input[[paste0("model_", id)]]),
                       provider_ids(cfg))
    state <- list(
      # ---- results ----
      topic = input$topic, problem_details = input$problem_details,
      critical_rules = input$critical_rules,
      apply_rules_debate = input$apply_rules_debate, apply_rules_consensus = input$apply_rules_consensus,
      plan = rv$plan, agents = rv$agents, history = rv$history,
      kg = rv$kg, analytics = rv$analytics, consensus = rv$consensus,
      usage_log = rv$usage_log, formatted_output = rv$formatted_output,
      # ---- full configuration ----
      config = list(
        active_providers = input$active_providers, meta_provider = input$meta_provider,
        mode = input$mode, moderator = input$moderator, objective = input$objective,
        n_rounds = input$n_rounds, max_tokens = input$max_tokens, temperature = input$temperature,
        reasoning_effort_idx = input$reasoning_effort_idx, language = input$language,
        auto_stop = input$auto_stop, use_cache = input$use_cache,
        active_dimensions = input$active_dimensions, models = models,
        coordinator = input$coordinator, include_plan_in_consensus = input$include_plan_in_consensus,
        include_moderator_in_transcript = input$include_moderator_in_transcript,
        narrative_type = input$narrative_type, narrative_type_custom = input$narrative_type_custom,
        panel_design = input$panel_design, debate_title = input$debate_title
      )
    )
    save_session(state, name)
    sessions_refresh(sessions_refresh() + 1)
    showNotification(paste0("Saved debate '", name, "'."), type = "message")
  }
  observeEvent(input$btn_save_session,  do_save_session(input$session_name))
  observeEvent(input$btn_save_session2, do_save_session(input$session_name2))

  observeEvent(input$btn_load_session, {
    if (isTRUE(rv$running)) {
      showNotification("Stop the running deliberation before loading a saved debate.", type = "warning"); return()
    }
    m <- sessions_meta(); sel <- input$sessions_table_rows_selected
    if (length(sel) != 1) { showNotification("Select a saved debate first.", type = "warning"); return() }
    state <- load_session(m$file[sel])
    if (is.null(state)) { showNotification("Could not load debate.", type = "error"); return() }
    # ---- restore results ----
    rv$agents <- state$agents %||% rv$agents
    rv$plan <- state$plan
    rv$history <- state$history %||% list()
    rv$kg <- state$kg %||% empty_kg()
    rv$analytics <- state$analytics
    rv$consensus <- state$consensus
    rv$usage_log <- state$usage_log %||% list()
    rv$formatted_output <- state$formatted_output %||% ""
    updateTextAreaInput(session, "topic", value = state$topic %||% "")
    updateTextAreaInput(session, "problem_details", value = state$problem_details %||% "")
    if (!is.null(state$critical_rules))
      updateTextAreaInput(session, "critical_rules", value = state$critical_rules)
    if (!is.null(state$apply_rules_debate))
      updateCheckboxInput(session, "apply_rules_debate", value = isTRUE(state$apply_rules_debate))
    if (!is.null(state$apply_rules_consensus))
      updateCheckboxInput(session, "apply_rules_consensus", value = isTRUE(state$apply_rules_consensus))
    # ---- restore configuration (update the actual UI controls) ----
    cf <- state$config %||% list()
    # The active set the loaded debate needs = its saved active_providers PLUS
    # every provider its agents actually use (guards against the async checkbox
    # update not arriving before a re-run, and against an agent whose provider
    # was not in the saved active set).
    agent_provs <- unique(vapply(rv$agents, function(a) a$provider %||% "", character(1)))
    agent_provs <- agent_provs[nzchar(agent_provs)]
    loaded_provs <- union(cf$active_providers %||% character(0), agent_provs)
    rv$loaded_providers <- if (length(loaded_provs)) loaded_provs else NULL
    if (length(loaded_provs)) updateCheckboxGroupInput(session, "active_providers", selected = loaded_provs)
    if (!is.null(cf$meta_provider))    updateSelectInput(session, "meta_provider", selected = cf$meta_provider)
    if (!is.null(cf$mode))             updateSelectInput(session, "mode", selected = cf$mode)
    if (!is.null(cf$moderator))        updateSelectInput(session, "moderator", selected = cf$moderator)
    if (!is.null(cf$objective))        updateSelectInput(session, "objective", selected = cf$objective)
    if (!is.null(cf$n_rounds))         updateSliderInput(session, "n_rounds", value = cf$n_rounds)
    if (!is.null(cf$max_tokens))       updateSliderInput(session, "max_tokens", value = cf$max_tokens)
    if (!is.null(cf$temperature))      updateSliderInput(session, "temperature", value = cf$temperature)
    if (!is.null(cf$reasoning_effort_idx)) updateSliderInput(session, "reasoning_effort_idx", value = cf$reasoning_effort_idx)
    if (!is.null(cf$language))         updateTextInput(session, "language", value = cf$language)
    if (!is.null(cf$auto_stop))        updateCheckboxInput(session, "auto_stop", value = cf$auto_stop)
    if (!is.null(cf$use_cache))        updateCheckboxInput(session, "use_cache", value = cf$use_cache)
    if (!is.null(cf$coordinator))      updateTextInput(session, "coordinator", value = cf$coordinator)
    if (!is.null(cf$include_plan_in_consensus)) updateCheckboxInput(session, "include_plan_in_consensus", value = cf$include_plan_in_consensus)
    if (!is.null(cf$include_moderator_in_transcript)) updateCheckboxInput(session, "include_moderator_in_transcript", value = cf$include_moderator_in_transcript)
    if (!is.null(cf$narrative_type))        updateSelectInput(session, "narrative_type", selected = cf$narrative_type)
    if (!is.null(cf$narrative_type_custom)) updateTextAreaInput(session, "narrative_type_custom", value = cf$narrative_type_custom)
    if (!is.null(cf$panel_design))          updateRadioButtons(session, "panel_design", selected = cf$panel_design)
    if (!is.null(cf$debate_title))          updateTextInput(session, "debate_title", value = cf$debate_title)
    if (!is.null(cf$active_dimensions)) updateCheckboxGroupInput(session, "active_dimensions", selected = cf$active_dimensions)
    for (id in names(cf$models %||% list())) {
      v <- cf$models[[id]]
      if (!is.null(v) && nzchar(v)) updateSelectInput(session, paste0("model_", id), selected = v)
    }
    updateTextInput(session, "session_name", value = m$name[sel])
    updateTextInput(session, "session_name2", value = m$name[sel])  # keep both save fields in sync
    showNotification(paste0("Loaded debate '", m$name[sel], "' (config + results)."), type = "message")
  })

  observeEvent(input$btn_delete_session, {
    m <- sessions_meta(); sel <- input$sessions_table_rows_selected
    if (length(sel) != 1) { showNotification("Select a saved debate first.", type = "warning"); return() }
    delete_session(m$file[sel])
    sessions_refresh(sessions_refresh() + 1)
    showNotification(paste0("Deleted '", m$name[sel], "'."), type = "message")
  })
}

shinyApp(ui, server)
