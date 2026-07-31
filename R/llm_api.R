# =============================================================================
# llm_api.R -- provider abstraction layer
# -----------------------------------------------------------------------------
# One contract for every provider. The rest of the app calls llm_chat() and
# never knows which provider answered.
#
#   llm_chat(cfg, provider_id, messages, api_key, ...) -> list(
#       ok = TRUE/FALSE, text = "...", error = NULL/"...", raw = <parsed>,
#       usage = list(prompt_tokens=, completion_tokens=), cached = TRUE/FALSE)
#
# `messages` is always in OpenAI chat format: list(list(role=,content=), ...)
# with roles system/user/assistant. Each api_type handler converts to its own
# wire format. Three handlers cover everything:
#   - openai_chat : OpenAI, Grok, Sarvam, Ollama/LM Studio, any OpenAI-compat
#   - anthropic   : Claude
#   - gemini      : Google Gemini
# Adding an OpenAI-compatible provider is a config-only change. A genuinely
# new API shape means a new handler + a new api_type, registered in
# API_HANDLERS at the bottom.
# =============================================================================

# ---- Defensive HTTP POST ----------------------------------------------------
# Centralizes timeouts, retry-on-transient-error, and turning any failure into
# the common ok/error contract. Ported and generalized from the CIRL app.
# 429 = rate-limited, 529 = provider overloaded (Anthropic's "try again shortly");
# both are pacing signals answered with a longer, exponential backoff below. The
# 5xx codes are transient server faults retried with a gentler linear backoff.
RETRYABLE_STATUS_CODES <- c(429L, 500L, 502L, 503L, 504L, 529L)

safe_api_post <- function(url, headers, body, timeout_sec = 90, max_retries = 3) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    result <- tryCatch({
      req <- httr2::request(url)
      for (h in names(headers)) req <- httr2::req_headers(req, !!h := headers[[h]])
      req <- httr2::req_body_json(req, body)
      req <- httr2::req_timeout(req, timeout_sec)
      req <- httr2::req_error(req, is_error = function(resp) FALSE) # we handle errors
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      parsed <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                         error = function(e) NULL)

      # NOTE: branches YIELD a value (no return()) -- a return() inside this
      # tryCatch would exit safe_api_post() and kill the retry loop. A 429 with
      # code "insufficient_quota" is billing, not pacing, so it is NOT retried.
      err_code <- tryCatch(parsed$error$code, error = function(e) NULL)
      quota_exhausted <- identical(err_code, "insufficient_quota")
      if (status %in% RETRYABLE_STATUS_CODES && attempt <= max_retries && !quota_exhausted) {
        # Rate-limit / overloaded (429, 529): exponential backoff (2, 4, 8, ...s,
        # capped at 30) + up to 1s jitter so retries don't all fire in lockstep.
        # Other transient 5xx: gentler linear backoff.
        wait_s <- if (status %in% c(429L, 529L)) min(2^attempt, 30) + stats::runif(1, 0, 1) else 1.5 * attempt
        Sys.sleep(wait_s)
        list(retry = TRUE, last_status = status)
      } else if (status >= 400) {
        msg <- if (!is.null(parsed) && !is.null(parsed$error)) {
          if (is.list(parsed$error)) parsed$error$message %||% as.character(parsed$error) else parsed$error
        } else if (!is.null(parsed) && !is.null(parsed$message)) {
          parsed$message
        } else paste("HTTP", status)
        retry_note <- if (status %in% RETRYABLE_STATUS_CODES) paste0(" (gave up after ", attempt, " attempts)") else ""
        list(ok = FALSE, error = paste0("API error (", status, "): ", msg, retry_note), raw = parsed)
      } else if (is.null(parsed)) {
        list(ok = FALSE, error = "Invalid/unparseable JSON response from provider.", raw = NULL)
      } else {
        list(ok = TRUE, raw = parsed)
      }
    }, error = function(e) {
      list(ok = FALSE, error = paste("Network/timeout error:", conditionMessage(e)), raw = NULL)
    })
    if (!is.null(result$retry) && isTRUE(result$retry)) next
    return(result)
  }
}

# ---- Response cache ---------------------------------------------------------
# In-memory cache keyed by a hash of the full request. Cuts cost/latency when
# the same prompt is issued twice (e.g. re-running a planner on the same
# topic, or the Prompt Preview tab). Cleared when the R session ends.
.LLM_CACHE <- new.env(parent = emptyenv())

.cache_key <- function(provider_id, model, messages, max_tokens, temperature, reasoning_effort) {
  digest::digest(list(provider_id, model, messages, max_tokens, temperature, reasoning_effort),
                 algo = "sha1")
}
llm_cache_clear <- function() rm(list = ls(.LLM_CACHE), envir = .LLM_CACHE)
llm_cache_size  <- function() length(ls(.LLM_CACHE))

# ---- Rough token counter ----------------------------------------------------
# Provider-agnostic heuristic (~4 chars/token). Good enough for budgeting and
# the UI; not a substitute for a real tokenizer.
llm_count_tokens <- function(text) {
  if (is.null(text) || length(text) == 0) return(0L)
  as.integer(ceiling(nchar(paste(text, collapse = " ")) / 4))
}

# ---- Handler: OpenAI-compatible chat ---------------------------------------
# Covers OpenAI, Grok (xAI), Sarvam, and any OpenAI-compatible endpoint
# (Ollama, LM Studio, vLLM). Differences are all data in the provider spec:
# endpoint, whether the key is needed, which reasoning_effort levels are legal,
# and which max-tokens field name to use.
.handler_openai_chat <- function(prov, messages, api_key, model, max_tokens, temperature, reasoning_effort) {
  if (isTRUE(prov$needs_key) && (is.null(api_key) || nchar(trimws(api_key)) == 0)) {
    return(list(ok = FALSE, text = NULL, error = paste0("Missing API key for ", prov$label, ".")))
  }
  # OpenAI's newer models require max_completion_tokens; most compatible
  # servers still use max_tokens. Choose by endpoint, fall back on error.
  use_completion_field <- grepl("openai.com", prov$endpoint, fixed = TRUE)
  body <- list(model = model, messages = messages, temperature = temperature)
  if (use_completion_field) body$max_completion_tokens <- max_tokens else body$max_tokens <- max_tokens

  legal_levels <- unlist(prov$reasoning_effort_levels %||% list())
  if (!is.null(reasoning_effort) && nchar(trimws(reasoning_effort)) > 0 &&
      reasoning_effort %in% legal_levels) {
    body$reasoning_effort <- reasoning_effort
  }

  headers <- list(`Content-Type` = "application/json")
  if (!is.null(api_key) && nchar(trimws(api_key)) > 0) {
    headers$Authorization <- paste("Bearer", api_key)
  }
  # Local providers (CPU inference) need a much longer timeout than cloud APIs;
  # honour a per-provider `timeout_sec` (default 90s).
  tmo <- suppressWarnings(as.numeric(prov$timeout_sec %||% 90)); if (is.na(tmo)) tmo <- 90
  post <- function(b) safe_api_post(prov$endpoint, headers, b, timeout_sec = tmo)
  res <- post(body)

  # Error-driven parameter fallbacks (learned the hard way in the CIRL app).
  if (!isTRUE(res$ok) && grepl("max_tokens", res$error %||% "", fixed = TRUE)) {
    body$max_completion_tokens <- NULL; body$max_tokens <- max_tokens; res <- post(body)
  }
  if (!isTRUE(res$ok) && grepl("temperature", res$error %||% "", fixed = TRUE)) {
    body$temperature <- NULL; res <- post(body)  # reasoning models accept only default temp
  }
  if (!isTRUE(res$ok) && grepl("reasoning_effort", res$error %||% "", fixed = TRUE)) {
    body$reasoning_effort <- NULL; res <- post(body)
  }
  if (!isTRUE(res$ok)) return(list(ok = FALSE, text = NULL, error = res$error))

  msg <- tryCatch(res$raw$choices[[1]]$message, error = function(e) NULL)
  finish_reason <- tryCatch(res$raw$choices[[1]]$finish_reason, error = function(e) NULL)
  txt <- NULL
  if (!is.null(msg)) {
    if (is.character(msg$content) && length(msg$content) == 1) {
      txt <- msg$content
    } else if (is.list(msg$content) && length(msg$content) > 0) {
      parts <- vapply(msg$content, function(p) {
        if (is.list(p) && !is.null(p$text)) p$text else if (is.character(p)) p else ""
      }, character(1))
      txt <- paste(parts, collapse = "")
    } else if (!is.null(msg$refusal) && nchar(trimws(msg$refusal)) > 0) {
      return(list(ok = FALSE, text = NULL, error = paste("Model refused:", msg$refusal)))
    }
  }
  # Thinking/reasoning models can spend the whole budget on hidden reasoning
  # and return empty content with finish_reason == "length". Surface that
  # clearly instead of a silently empty turn.
  if (is.null(txt) || nchar(trimws(txt)) == 0) {
    reasoning_tok <- tryCatch(res$raw$usage$completion_tokens_details$reasoning_tokens,
                              error = function(e) NULL)
    hint <- if (identical(finish_reason, "length")) {
      paste0(" (finish_reason=length",
             if (!is.null(reasoning_tok)) paste0(", reasoning_tokens=", reasoning_tok) else "",
             " -- the token budget was likely consumed by hidden reasoning. Raise Max Tokens ",
             "to ~1500-2500 for thinking-mode models.)")
    } else paste0(" (finish_reason=", finish_reason %||% "unknown", ")")
    return(list(ok = FALSE, text = NULL, error = paste0(prov$label, " returned empty content", hint)))
  }
  usage <- tryCatch(list(prompt_tokens = res$raw$usage$prompt_tokens %||% NA,
                         completion_tokens = res$raw$usage$completion_tokens %||% NA),
                    error = function(e) NULL)
  list(ok = TRUE, text = txt, error = NULL, raw = res$raw, usage = usage)
}

# ---- Handler: Anthropic (Claude) -------------------------------------------
# Claude's Messages API: system is a top-level string (not a message), only
# user/assistant roles are allowed in `messages`, and max_tokens is required.
.handler_anthropic <- function(prov, messages, api_key, model, max_tokens, temperature, reasoning_effort) {
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) {
    return(list(ok = FALSE, text = NULL, error = "Missing Anthropic (Claude) API key."))
  }
  sys_txt <- paste(vapply(Filter(function(m) m$role == "system", messages),
                          function(m) m$content, character(1)), collapse = "\n\n")
  convo <- Filter(function(m) m$role %in% c("user", "assistant"), messages)
  # Claude requires the first message to be from the user.
  if (length(convo) == 0 || convo[[1]]$role != "user") {
    convo <- c(list(list(role = "user", content = "(begin)")), convo)
  }
  body <- list(model = model, max_tokens = max_tokens, temperature = temperature,
               messages = lapply(convo, function(m) list(role = m$role, content = m$content)))
  if (nchar(trimws(sys_txt)) > 0) body$system <- sys_txt

  headers <- list(`x-api-key` = api_key, `anthropic-version` = "2023-06-01",
                  `Content-Type` = "application/json")
  tmo <- suppressWarnings(as.numeric(prov$timeout_sec %||% 90)); if (is.na(tmo)) tmo <- 90
  post <- function(b) safe_api_post(prov$endpoint, headers, b, timeout_sec = tmo)
  res <- post(body)
  # Some newer Claude models reject `temperature` ("temperature is deprecated
  # for this model"). Drop it and retry rather than failing the turn -- mirrors
  # the OpenAI handler's parameter-fallback behavior.
  if (!isTRUE(res$ok) && grepl("temperature", res$error %||% "", fixed = TRUE)) {
    body$temperature <- NULL
    res <- post(body)
  }
  if (!isTRUE(res$ok)) return(list(ok = FALSE, text = NULL, error = res$error))
  # content is an array of blocks; concatenate the text blocks.
  blocks <- tryCatch(res$raw$content, error = function(e) NULL)
  txt <- if (is.list(blocks)) {
    paste(vapply(blocks, function(b) if (!is.null(b$text)) b$text else "", character(1)), collapse = "")
  } else NULL
  if (is.null(txt) || nchar(trimws(txt)) == 0) {
    return(list(ok = FALSE, text = NULL, error = "Claude returned empty content."))
  }
  usage <- tryCatch(list(prompt_tokens = res$raw$usage$input_tokens %||% NA,
                         completion_tokens = res$raw$usage$output_tokens %||% NA),
                    error = function(e) NULL)
  list(ok = TRUE, text = txt, error = NULL, raw = res$raw, usage = usage)
}

# ---- Handler: Google Gemini -------------------------------------------------
# generateContent API. NOTE: Gemini normally accepts the key as a ?key= query
# param, but the app's privacy rules forbid keys in URLs -- so we send it in
# the x-goog-api-key HEADER instead. Roles map: system->systemInstruction,
# assistant->model, user->user.
.handler_gemini <- function(prov, messages, api_key, model, max_tokens, temperature, reasoning_effort) {
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) {
    return(list(ok = FALSE, text = NULL, error = "Missing Google Gemini API key."))
  }
  sys_txt <- paste(vapply(Filter(function(m) m$role == "system", messages),
                          function(m) m$content, character(1)), collapse = "\n\n")
  contents <- lapply(Filter(function(m) m$role %in% c("user", "assistant"), messages), function(m) {
    list(role = if (m$role == "assistant") "model" else "user",
         parts = list(list(text = m$content)))
  })
  body <- list(contents = contents,
               generationConfig = list(maxOutputTokens = max_tokens, temperature = temperature))
  if (nchar(trimws(sys_txt)) > 0) body$systemInstruction <- list(parts = list(list(text = sys_txt)))

  url <- paste0(prov$endpoint, "/", model, ":generateContent")
  headers <- list(`x-goog-api-key` = api_key, `Content-Type` = "application/json")
  tmo <- suppressWarnings(as.numeric(prov$timeout_sec %||% 90)); if (is.na(tmo)) tmo <- 90
  res <- safe_api_post(url, headers, body, timeout_sec = tmo)
  if (!isTRUE(res$ok)) return(list(ok = FALSE, text = NULL, error = res$error))
  txt <- tryCatch({
    parts <- res$raw$candidates[[1]]$content$parts
    paste(vapply(parts, function(p) p$text %||% "", character(1)), collapse = "")
  }, error = function(e) NULL)
  if (is.null(txt) || nchar(trimws(txt)) == 0) {
    return(list(ok = FALSE, text = NULL, error = "Gemini returned empty content."))
  }
  usage <- tryCatch(list(prompt_tokens = res$raw$usageMetadata$promptTokenCount %||% NA,
                         completion_tokens = res$raw$usageMetadata$candidatesTokenCount %||% NA),
                    error = function(e) NULL)
  list(ok = TRUE, text = txt, error = NULL, raw = res$raw, usage = usage)
}

# api_type -> handler. Register a new API shape here.
API_HANDLERS <- list(
  openai_chat = .handler_openai_chat,
  anthropic   = .handler_anthropic,
  gemini      = .handler_gemini
)

# ---- Public dispatch --------------------------------------------------------
# The single entry point. Handles caching, provider/model resolution, and
# dispatch to the correct api_type handler.
llm_chat <- function(cfg, provider_id, messages, api_key,
                     model = NULL, max_tokens = 1200, temperature = 0.7,
                     reasoning_effort = NULL, use_cache = TRUE) {
  prov <- provider_by_id(cfg, provider_id)
  if (is.null(prov)) return(list(ok = FALSE, text = NULL, error = paste("Unknown provider:", provider_id)))
  model <- model %||% prov$default_model
  handler <- API_HANDLERS[[prov$api_type]]
  if (is.null(handler)) {
    return(list(ok = FALSE, text = NULL, error = paste("No handler for api_type:", prov$api_type)))
  }
  # Clamp the OUTPUT budget to the provider's limits (output cap, context
  # window relative to this prompt's size, and required multiple). This is what
  # stops small-context models (e.g. Celeris, 8192 total) from rejecting a
  # request when a long prompt + a big max_tokens would overflow the window.
  input_tok  <- llm_count_tokens(vapply(messages, function(m) as.character(m$content %||% ""), character(1)))
  # If the prompt itself is bigger than the provider's context window, no output
  # size can rescue it -- fail early with an actionable message rather than a
  # cryptic provider 400 (and don't waste an API call).
  prov_ctx <- suppressWarnings(as.numeric(prov$context_window %||% NA))
  if (!is.na(prov_ctx) && input_tok > prov_ctx - 128) {
    return(list(ok = FALSE, text = NULL, provider = provider_id, model = model, cached = FALSE,
                error = paste0("Prompt (~", input_tok, " tokens) exceeds ", prov$label, "'s ",
                               prov_ctx, "-token context window. Use a larger-context provider, or ",
                               "shorten the topic / reduce rounds.")))
  }
  max_tokens <- effective_output_tokens(cfg, provider_id, max_tokens, input_tok)
  key <- .cache_key(provider_id, model, messages, max_tokens, temperature, reasoning_effort)
  if (use_cache && exists(key, envir = .LLM_CACHE, inherits = FALSE)) {
    cached <- get(key, envir = .LLM_CACHE, inherits = FALSE)
    cached$cached <- TRUE
    return(cached)
  }
  res <- handler(prov, messages, api_key, model, max_tokens, temperature, reasoning_effort)
  res$cached <- FALSE
  # Record the resolved provider/model so callers can attribute token usage and
  # cost without having to re-derive which model actually ran.
  res$provider <- provider_id
  res$model <- model
  if (use_cache && isTRUE(res$ok)) assign(key, res, envir = .LLM_CACHE)
  res
}

# Convenience: the model list for a provider (for UI dropdowns).
llm_model_list <- function(cfg, provider_id) {
  prov <- provider_by_id(cfg, provider_id)
  if (is.null(prov)) character(0) else unlist(prov$models)
}

# Effective output-token budget: `requested` clamped to the provider's optional
# `max_tokens` ceiling from providers.json (if that field is set and numeric).
# No field -> returns `requested` unchanged. Used by llm_chat() and the UI.
# Effective OUTPUT token budget for a request, honouring three provider limits:
#   - max_tokens     : the provider's output-token ceiling
#   - context_window : total input+output must fit (so output <= window - input)
#   - token_multiple : output must be a multiple of N (e.g. Celeris = 256)
# `input_tokens` is the estimated size of the prompt; pass 0 when unknown (the
# UI does this). Rounds DOWN to the multiple so the window is never exceeded.
effective_output_tokens <- function(cfg, provider_id, requested, input_tokens = 0) {
  prov <- provider_by_id(cfg, provider_id)
  if (is.null(prov)) return(as.integer(requested))
  out  <- requested
  cap  <- suppressWarnings(as.numeric(prov$max_tokens %||% NA))
  ctx  <- suppressWarnings(as.numeric(prov$context_window %||% NA))
  mult <- suppressWarnings(as.numeric(prov$token_multiple %||% NA))
  if (!is.na(cap)) out <- min(out, cap)
  # 768-token headroom absorbs role/formatting tokens and the input estimate's
  # imprecision, so input + output stays safely under the window.
  if (!is.na(ctx)) out <- min(out, ctx - input_tokens - 768)
  if (!is.na(mult) && mult >= 1) {
    out <- mult * floor(out / mult)   # round DOWN so we never exceed cap/window
    if (out < mult) out <- mult       # but never below one multiple
  }
  as.integer(max(1L, out))
}

# UI helper (no prompt in hand -> ignore the context window).
provider_max_tokens <- function(cfg, provider_id, requested) {
  effective_output_tokens(cfg, provider_id, requested, input_tokens = 0)
}

# ---- Cost estimation --------------------------------------------------------
# Dollar cost of one call given a pricing config (see config/pricing.json),
# the model, and token counts. Unknown/NA token counts contribute 0; a model
# not in the table falls back to pricing$`_default`. Rates are per 1e6 tokens.
llm_cost <- function(pricing, model, prompt_tokens, completion_tokens) {
  if (is.null(pricing)) return(NA_real_)
  rate <- NULL
  if (!is.null(model) && !is.na(model)) rate <- pricing$models[[model]]
  rate <- rate %||% pricing[["_default"]] %||% list(input = 0, output = 0)
  pt <- if (is.null(prompt_tokens) || is.na(prompt_tokens)) 0 else as.numeric(prompt_tokens)
  ct <- if (is.null(completion_tokens) || is.na(completion_tokens)) 0 else as.numeric(completion_tokens)
  (pt * (rate$input %||% 0) + ct * (rate$output %||% 0)) / 1e6
}
