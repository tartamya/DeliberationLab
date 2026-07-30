DELIBERATION LAB  -  portable edition
=====================================

WHAT THIS IS
  A self-contained copy of the Multi-LLM Deliberation Laboratory.
  Nothing to install - it brings its own copy of R.

HOW TO RUN
  1. Double-click   "Start Deliberation Lab.bat"
  2. Wait ~10-20 seconds. Your web browser opens automatically.
  3. Use the app in the browser.
  4. To STOP the app: close the black console window.

FIRST-TIME SETUP  -  API KEYS  (required)
  The app talks to LLM providers (OpenAI, Claude, Grok, Sarvam, Gemini,
  Celeris). You must supply your own key(s):
    - In the app, open the "Settings" tab, paste your key(s), and click
      "Use these keys".  (Held only in memory; never written to disk.)
    - OR create the file   app\config\secrets.R   with your keys
      (copy app\config\secrets.example.R and fill it in).

NOTES
  - Styled PDF export uses Microsoft Edge / Chrome (already present on
    Windows). If none is found, PDFs fall back to a plain-text layout.
  - Saved debates are written to   app\sessions\
  - 64-bit Windows 10 / 11 required.

TROUBLESHOOTING
  - "Could not find Rscript.exe" : the R-Portable folder is missing or was
    not extracted next to the .bat.
  - Browser did not open : manually visit the  http://127.0.0.1:PORT
    address printed in the console window.
  - A provider shows errors : check the key on the Settings tab, and use
    the "Test keys" button.
