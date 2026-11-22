# Static integrity of the source tree and the config files. These are the
# cheapest tests in the suite and have caught the most: a duplicate input id,
# an output rendered but never placed, and the `max = 8,192` thousands-comma
# that silently made the token slider's range run 256..8.

test_that("every R file parses", {
  files <- c(file.path(APP, "app.R"), list.files(file.path(APP, "R"), "[.]R$", full.names = TRUE))
  expect_gt(length(files), 5)
  for (f in files) expect_no_error(parse(f), message = paste("parse failed:", basename(f)))
})

test_that("no duplicate Shiny input ids", {
  ui <- paste(readLines(file.path(APP, "app.R"), warn = FALSE), collapse = "\n")
  ids <- gsub('"', '', regmatches(ui,
    gregexpr('(?<=(Input|Button|Panel)\\()"[a-zA-Z0-9_]+"', ui, perl = TRUE))[[1]])
  dup <- names(which(table(ids) > 1))
  expect_equal(dup, character(0),
               info = paste("duplicate input ids:", paste(dup, collapse = ", ")))
})

test_that("every rendered output is placed, and every placed output is rendered", {
  lines <- readLines(file.path(APP, "app.R"), warn = FALSE)
  ui <- paste(lines, collapse = "\n")
  rendered <- unique(gsub('.*output\\$([a-zA-Z0-9_]+).*', '\\1',
                          grep("output\\$[a-zA-Z0-9_]+\\s*<-", lines, value = TRUE)))
  placers <- paste0("(uiOutput|textOutput|verbatimTextOutput|plotOutput|DTOutput|",
                    "plotlyOutput|visNetworkOutput|imageOutput|downloadButton)")
  placed <- unique(gsub('"', '', regmatches(ui,
    gregexpr(paste0('(?<=', placers, '\\()"[a-zA-Z0-9_]+"'), ui, perl = TRUE))[[1]]))
  expect_equal(setdiff(rendered, placed), character(0),
               info = "rendered but never placed in the UI")
  expect_equal(setdiff(placed, rendered), character(0),
               info = "placed in the UI but never rendered")
})

test_that("no numeric literal carries a thousands comma", {
  # `max = 8,192` is two arguments in R: max = 8 and a stray positional 192
  # that lands on `round`. It produced an inverted slider range (min 256 >
  # max 8) and a budget of 256 tokens, which starved every reasoning model.
  files <- c(file.path(APP, "app.R"), list.files(file.path(APP, "R"), "[.]R$", full.names = TRUE))
  offenders <- character(0)
  for (f in files) {
    ln <- readLines(f, warn = FALSE)
    ln <- ln[!grepl("^\\s*#", ln)]                      # skip comment lines
    hit <- grep("[=(,]\\s*[0-9]+,[0-9]{3}\\b", ln, value = TRUE)
    if (length(hit)) offenders <- c(offenders, paste0(basename(f), ": ", trimws(hit)))
  }
  expect_equal(offenders, character(0),
               info = paste(offenders, collapse = " | "))
})

test_that("every config JSON is valid and non-empty", {
  js <- list.files(file.path(APP, "config"), "[.]json$", full.names = TRUE)
  expect_gt(length(js), 10)
  for (f in js) {
    parsed <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    expect_false(is.null(parsed), info = paste("invalid JSON:", basename(f)))
    expect_gt(length(parsed), 0)
  }
})

test_that("no function is defined twice across the source tree", {
  defs <- list()
  files <- c(file.path(APP, "app.R"), list.files(file.path(APP, "R"), "[.]R$", full.names = TRUE))
  for (f in files) {
    ln <- readLines(f, warn = FALSE)
    ln <- ln[!grepl("^\\s*#", ln)]                      # comment banners are not definitions
    for (h in grep("^[._A-Za-z][._A-Za-z0-9]*\\s*<-\\s*function", ln, value = TRUE)) {
      nm <- sub("\\s*<-\\s*function.*", "", h)
      defs[[nm]] <- c(defs[[nm]], basename(f))
    }
  }
  dups <- Filter(function(x) length(x) > 1, defs)
  expect_equal(names(dups), character(0),
               info = paste(names(dups), collapse = ", "))
})
