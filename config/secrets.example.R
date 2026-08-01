# =============================================================================
# secrets.example.R -- TEMPLATE (safe to commit)
# -----------------------------------------------------------------------------
# Copy this file to config/secrets.R and fill in your keys. config/secrets.R
# is git-ignored (see .gitignore) so real keys never enter version control.
#
# Key resolution order in the app (see app.R::resolve_api_key):
#   1. Environment variable named by each provider's `key_env` in
#      config/providers.json (e.g. OPENAI_API_KEY). Preferred.
#   2. This API_KEYS list, keyed by each provider's `key_name`.
#   3. Whatever you type into the app's Settings tab at runtime.
#
# Keys are held only in memory and are NEVER written into saved sessions.
# =============================================================================

API_KEYS <- list(
  openai = "",   # https://platform.openai.com/api-keys
  claude = "",   # https://console.anthropic.com/  (Anthropic)
  grok   = "",   # https://console.x.ai/
  sarvam = "",   # https://docs.sarvam.ai/  (starts "sk_")
  gemini = "",   # https://aistudio.google.com/app/apikey
  celeris = "",  # Celeris key (also set its endpoint in config/providers.json)
  deepseek = "", # https://platform.deepseek.com/api_keys
  mistral = "",  # https://console.mistral.ai/
  perplexity = "", # https://www.perplexity.ai/settings/api  (web-connected Sonar models)
  local  = ""    # usually blank for Ollama / LM Studio
)

