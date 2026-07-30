# =============================================================================
# ui_helpers.R -- theme + small UI building blocks
# -----------------------------------------------------------------------------
# Keeps presentational helpers out of app.R. The app uses bslib for a modern
# dashboard look; these helpers wrap repeated patterns (info cards, labelled
# lists, provider/vocab choice builders).
# =============================================================================

app_theme <- function() {
  bslib::bs_theme(
    version = 5, bootswatch = "flatly",
    primary = "#2C5F8A", secondary = "#57708A",
    base_font = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Inter", local = FALSE)
  )
}

# A titled bordered card block for the info-style tabs.
info_card <- function(title, ..., accent = "#2C5F8A") {
  shiny::tags$div(
    style = paste0("border:1px solid #DFE1E6; border-left:4px solid ", accent,
                   "; border-radius:6px; padding:12px 14px; margin-bottom:12px; background:#FFFFFF;"),
    if (!is.null(title)) shiny::tags$h5(style = "margin-top:0;", title),
    ...
  )
}

# Render a character vector / list as a labelled paragraph or bullet list.
labelled_list <- function(label, items) {
  items <- unlist(items)
  if (length(items) == 0) return(shiny::tags$p(shiny::tags$b(paste0(label, ": ")), shiny::em("(none)")))
  shiny::tags$div(
    shiny::tags$b(paste0(label, ":")),
    shiny::tags$ul(lapply(items, function(x) shiny::tags$li(x)))
  )
}

# Choice vector for a provider selectInput: value = id, name = label.
provider_choices <- function(cfg) provider_labels(cfg)

# Named-choice helper for config vocabularies (value == label == name field).
vocab_choices <- function(items, field = "name") {
  nm <- cfg_names(items, field)
  setNames(nm, nm)
}
