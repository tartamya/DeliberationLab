# Generic Multi-LLM Deliberation Laboratory

A configuration-driven Shiny (base R) platform for structured multi-agent LLM
deliberation. Nothing in the code is topic-specific: a **Planner** infers the
shape of each deliberation, and every vocabulary (roles, reasoning styles,
debate modes, dimensions, moderators, evidence types, output formats,
providers) lives in `config/*.json`.

```
Topic  →  Planner  →  Debate Engine  →  Synthesis Engine
```

## Run it

```bash
Rscript -e "shiny::runApp('C:/R Projects/Debate Simulator', launch.browser = TRUE)"
```

Or open `app.R` in RStudio and click **Run App**. API keys are read from (in
order): environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`,
`GROK_API_KEY`, `SARVAM_API_KEY`, `GEMINI_API_KEY`), then `config/secrets.R`
(git-ignored, pre-filled for local use), then keys typed into the **Settings**
tab. Add a Claude or Gemini key in `config/secrets.R` to enable those.

## Layout

```
app.R                      Integration layer: UI (bslib navbar) + server wiring
R/
  config_loader.R          Loads config/*.json into CONFIG; accessor helpers
  llm_api.R                Provider abstraction (openai_chat / anthropic / gemini) + cache
  prompt_templates.R       Persona, turn, planner prompts; robust JSON extractor
  planner.R                Intelligence layer: topic → deliberation design (LLM + heuristic fallback)
  agent_factory.R          Build agents from attributes / roles / plan
  moderator.R              Per-round structured moderation + KG extraction
  knowledge_graph.R        Claim/evidence graph (igraph + visNetwork) + idea evolution
  debate_engine.R          Phase-protocol selection, turn execution, analytics, auto-stop
  synthesis_engine.R       Consensus engine + multi-format output rendering
  memory.R                 Agent memory + session save/load (keys never persisted)
  ui_helpers.R             bslib theme + small UI building blocks
config/
  providers.json roles.json reasoning_styles.json debate_modes.json
  domains.json moderators.json evidence_types.json output_formats.json objectives.json
  secrets.R (git-ignored)  secrets.example.R (template)
www/custom.css
sessions/                  Saved deliberations (.rds; created on first save)
```

## Extending — config only, no R changes

| Add a…                    | Edit only…                          |
|---------------------------|-------------------------------------|
| OpenAI-compatible provider| `providers.json` (api_type `openai_chat`) |
| Expert role               | `roles.json`                        |
| Reasoning style           | `reasoning_styles.json`             |
| Debate mode / protocol    | `debate_modes.json`                 |
| Discussion dimension      | `domains.json`                      |
| Moderator personality     | `moderators.json`                   |
| Output format             | `output_formats.json`               |

A genuinely new API *shape* (not OpenAI-compatible) needs one new handler +
`api_type` in `R/llm_api.R::API_HANDLERS`; everything else stays config.

## Notes

- **Concurrency:** cooperative-sequential — agents speak in order (each sees
  prior turns) and the server yields to Shiny between rounds via
  `later::later()`, so the UI stays live and **Stop** works mid-run.
- **Thinking-mode providers** (OpenAI reasoning models, Grok, Sarvam) can spend
  the whole token budget on hidden reasoning; keep **Max tokens** ≥ ~1500. The
  sidebar warns when it is too low.
- **Structured calls** (planner, moderator, synthesis) send `reasoning_effort =
  NULL` on purpose so the budget goes to JSON output, not hidden reasoning.
- **Caching** is in-memory per session; clear it from Settings.
