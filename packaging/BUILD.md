# Building the portable "Deliberation Lab" (Windows)

Produces a self-contained folder anyone can unzip and run — no R install, no
Rtools, no admin rights. Do this once on a machine with internet.

## Final folder layout (this is what you zip)

```
DeliberationLab\                       <- ZIP this whole folder
├─ Start Deliberation Lab.bat          <- (from packaging\)  the launcher
├─ launch.R                            <- (from packaging\)  R startup script
├─ README.txt                          <- (from packaging\)  user instructions
├─ R-Portable\                         <- a portable R 4.4.x (see step 1)
│   └─ App\R-Portable\bin\Rscript.exe  <- (found automatically by the .bat)
├─ app\                                <- a copy of the "Debate Simulator" app
│   ├─ app.R
│   ├─ R\ ...
│   ├─ config\   (secrets.example.R only -- NO real keys)
│   ├─ www\
│   └─ sessions\  (created at runtime)
└─ library\                            <- the app's R packages (see step 3)
    ├─ shiny\  bslib\  DT\  plotly\  ...
```

## Steps

### 1. Get a portable R that MATCHES the packages (R 4.4.x)
Download **R-Portable 4.4.x** (e.g. from sourceforge.net/projects/rportable/)
and extract it into `DeliberationLab\R-Portable\`.
IMPORTANT: it must be R **4.4.x** — the packages below are compiled for R 4.4.

### 2. Copy the app
Copy the `Debate Simulator` folder to `DeliberationLab\app`.
Then, in `app\config\`, **delete `secrets.R`** (your real keys) and keep only
`secrets.example.R`. Users add their own keys (see README).

### 3. Install the app's packages into `library\` (binaries, no Rtools)
From a Command Prompt, run R-Portable's Rscript to install the dependencies as
CRAN **binaries** straight into the bundled library (this is the robust way —
it avoids copying renv's cache links):

```bat
cd /d "C:\...\DeliberationLab"
"R-Portable\App\R-Portable\bin\Rscript.exe" -e "dir.create('library',showWarnings=FALSE); install.packages(c('shiny','bslib','DT','plotly','visNetwork','igraph','later','httr2','jsonlite','digest','chromote','htmltools'), lib='library', type='binary', repos='https://cran.r-project.org')"
```

`install.packages` pulls every transitive dependency (rlang, cli, httpuv, …)
automatically. No Rtools needed because these are prebuilt binaries.

### 4. Drop in the launcher files
Copy `launch.R`, `Start Deliberation Lab.bat`, and `README.txt` from this
`packaging\` folder into `DeliberationLab\` (the root, next to `app\`).

### 5. Test
Double-click **Start Deliberation Lab.bat**. A console appears, the browser
opens to `http://127.0.0.1:<port>`, and the app loads. Enter an API key on the
Settings tab and run a short debate to confirm.

### 6. Zip and distribute
Right-click `DeliberationLab` → Send to → Compressed (zipped) folder.
Recipients unzip anywhere and double-click the .bat. (Expect ~400-700 MB.)

## Notes
- **Chrome/Edge**: the styled PDF uses headless Edge/Chrome (present on Windows).
  If absent, PDFs fall back to plain text automatically — nothing to bundle.
- **Per-OS**: this build is Windows-only. Build separately on macOS/Linux.
- **Updating the app**: replace the contents of `app\` and re-zip; the R-Portable
  and library\ folders can stay.
```
