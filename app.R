# ============================================================================
# 50 States of Permitting
# A Shiny app surfacing state-level permitting reforms, reform categories,
# project types, and individual action tools.
# Created by Brent Efron w/ help from Claude & Gabe 
# Last Edited: April 16 2026
# Created: April 16 2026
# ============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(googlesheets4)
library(purrr)
library(htmltools)
library(commonmark)

# ---- Configuration ---------------------------------------------------------

gs4_deauth()

SHEET_URL    <- "https://docs.google.com/spreadsheets/d/1NK1UMXMBYbJOSKN6QEYwbgfZeveMKgROiKqf977jD2s/edit?gid=0#gid=0"
GEOJSON_PATH <- "www/states_ak_hi_v3.geojson"
CSV_FALLBACK <- "www/fifty_states_permitting_application_data.csv"

# Brand palette + supporting tones (darker base)
PAL <- list(
  green    = "#50a623",
  navy     = "#012169",
  orange   = "#b25712",
  blue     = "#0f53a6",
  # app chrome
  bg       = "#1f2744",   # deep navy app background
  panel    = "#f4f2ec",   # warm off-white panel
  panel_2  = "#ebe8de",   # slightly deeper panel variant
  ink      = "#1a1f36",
  muted    = "#5a6478",
  border   = "#d6d2c4",
  # map tones (tuned for light-grey map background)
  no_data      = "#b4bac8",  # soft slate, clearly visible on light grey
  no_data_line = "#8a92a6",
  dim          = "#c9ccd6"
)

# Emoji per reform category (visual flair)
REFORM_EMOJI <- list(
  "Promote Certainty in Permitting Process"           = "✅",
  "Consolidate or Eliminate Steps"                    = "🪢",
  "Shift Culture"                                     = "🌱",
  "Change Decision-Making Level"                      = "⚖️",
  "Set Projects up for Success"                       = "🚀",
  "Utilize Data and Technology"                       = "💻",
  "Benefits for Nature/Communities"                   = "🌳",
  "Eliminate Permits/Permitting"                      = "✂️",
  "Building the Permitting Workforce"                 = "👷"
)

PROJECT_EMOJI <- list(
  "General"                                 = "🧭",
  "Clean Energy"                            = "⚡",
  "Water"                                   = "💧",
  "Housing and Other Building Construction" = "🏗️",
  "Data Centers"                            = "🖥️",
  "Transportation"                          = "🚊",
  "Fossil Fuels"                            = "🛢️",
  "Ecological Restoration"                  = "🌿",
  "Mining and Critical Minerals"            = "⛏️",
  "Broadband"                               = "📶",
  "Other"                                   = "📦"
)

emoji_for <- function(lookup, key, fallback = "•") {
  v <- lookup[[key]]
  if (is.null(v)) fallback else v
}

# ---- Data loading ----------------------------------------------------------

load_data <- function() {
  df <- NULL
  for (attempt in 1:2) {
    df <- tryCatch(
      read_sheet(SHEET_URL, sheet = 1, col_types = "c"),
      error = function(e) {
        message(sprintf("Google Sheets attempt %d failed: %s", attempt, e$message))
        NULL
      }
    )
    if (!is.null(df)) break
  }
  if (is.null(df)) {
    message("Falling back to local CSV: ", CSV_FALLBACK)
    df <- readr::read_csv(CSV_FALLBACK, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  }
  df |> mutate(across(everything(),
                      ~ ifelse(is.na(.) | trimws(.) == "", NA_character_, trimws(.))))
}

split_multi <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  str_trim(str_split(x, ",")[[1]])
}

# ---- Markdown rendering -----------------------------------------------------
# Spreadsheet text columns (bolding, bullet lists, links) are authored in
# markdown. These helpers turn that into real HTML instead of showing the
# raw ** and - characters.

# Block-level: keeps paragraphs/lists as-is. Use for long-form prose fields.
md_block <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  HTML(commonmark::markdown_html(x, hardbreaks = TRUE))
}

# Inline: strips the wrapping <p> commonmark always adds, so short fields
# (chips, table cells, single-line meta values) stay inline instead of
# becoming block elements.
md_to_inline_html <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  html <- commonmark::markdown_html(x, hardbreaks = TRUE)
  html <- sub("^<p>", "", html)
  html <- sub("</p>\\n?$", "", html)
  html
}
md_inline <- function(x) HTML(md_to_inline_html(x))

# For fields displayed with a "—" fallback when empty.
md_or_dash <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x)) return("—")
  md_inline(x)
}

# ---- Prep ------------------------------------------------------------------

raw <- load_data()

long_reform <- raw |>
  mutate(val = map(reform_category, split_multi)) |>
  unnest(val) |> filter(!is.na(val), nzchar(val)) |>
  select(action_tool_name, state, reform_category_single = val)

long_project <- raw |>
  mutate(val = map(project_type, split_multi)) |>
  unnest(val) |> filter(!is.na(val), nzchar(val)) |>
  select(action_tool_name, state, project_type_single = val)

states_with_data <- sort(unique(raw$state))
reform_choices   <- sort(unique(long_reform$reform_category_single))
project_choices  <- sort(unique(long_project$project_type_single))

state_counts <- raw |> count(state, name = "n_tools")

# Reform categories present per state (for tooltip emoji grid)
state_reform_cats <- long_reform |>
  group_by(state) |>
  summarise(cats = list(unique(reform_category_single)), .groups = "drop")
state_reform_lookup <- setNames(state_reform_cats$cats, state_reform_cats$state)

# Ordered reform names matching sidebar (alphabetical = reform_choices order)
REFORM_ORDERED <- sort(names(REFORM_EMOJI))

make_state_tooltip <- function(state_nm, n_tools_val, has_data_flag) {
  if (!has_data_flag) {
    return(paste0(
      "<div style='min-width:130px;'>",
      "<strong>", state_nm, "</strong>",
      "<br/><span style='font-size:11px;color:#aaa;'>No data yet</span>",
      "</div>"
    ))
  }
  present <- state_reform_lookup[[state_nm]]
  if (is.null(present)) present <- character(0)
  cells <- vapply(REFORM_ORDERED, function(cat) {
    em  <- REFORM_EMOJI[[cat]]
    sty <- if (cat %in% present) "font-size:16px;" else "font-size:16px;opacity:0.2;filter:grayscale(1);"
    sprintf("<span style='%s' title='%s'>%s</span>", sty, cat, em)
  }, character(1))
  grid <- paste0(
    "<div style='display:grid;grid-template-columns:repeat(3,1fr);",
    "gap:4px;margin-top:6px;text-align:center;'>",
    paste(cells, collapse = ""),
    "</div>"
  )
  paste0(
    "<div style='min-width:130px;'>",
    "<strong>", state_nm, "</strong>",
    "<br/><span style='font-size:11px;opacity:0.75;'>",
    n_tools_val, ifelse(n_tools_val == 1, " Action or Tool", " Actions or Tools"),
    "</span>",
    grid,
    "</div>"
  )
}

# ---- Geojson ---------------------------------------------------------------

states_sf <- tryCatch(st_read(GEOJSON_PATH, quiet = TRUE),
                     error = function(e) { warning("Geojson missing: ", e$message); NULL })

if (!is.null(states_sf)) {
  states_sf <- states_sf |>
    mutate(
      state_name = NAME,
      n_tools  = state_counts$n_tools[match(NAME, state_counts$state)],
      n_tools  = ifelse(is.na(n_tools), 0L, n_tools),
      has_data = n_tools > 0
    )
}

# ---- CSS -------------------------------------------------------------------
# Grid:
#   cols: 300px (sidebar) | 1.8fr (map) | 1fr (detail)
#   rows: 1fr (top)       | 280px (table)
# Sidebar is top-only. Table spans all columns at the bottom.

app_css_template <- "
:root {
  --brand-green: {green};
  --brand-navy:  {navy};
  --brand-orange:{orange};
  --brand-blue:  {blue};
  --bg:          {bg};
  --panel:       {panel};
  --panel-2:     {panel_2};
  --ink:         {ink};
  --muted:       {muted};
  --border:      {border};
  --no-data:     {no_data};
}

html, body { height: 100%; margin: 0; background: var(--bg);
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
  color: var(--ink); }

.app-shell {
  display: grid;
  grid-template-columns: 361px 2.4fr 0.85fr;
  grid-template-rows: minmax(0, 1fr) 260px;
  gap: 0;
  padding: 0;
  height: 100vh;
  box-sizing: border-box;
  background: var(--panel);
  transition: grid-template-columns 0.25s ease, grid-template-rows 0.25s ease;
}

/* Collapsed states — sidebar hidden, detail hidden, table hidden, or any combo */
.app-shell.sidebar-collapsed {
  grid-template-columns: 0 2.4fr 0.85fr;
}
.app-shell.detail-collapsed {
  grid-template-columns: 361px 2.4fr 0;
}
.app-shell.sidebar-collapsed.detail-collapsed {
  grid-template-columns: 0 2.4fr 0;
}
.app-shell.table-collapsed {
  grid-template-rows: minmax(0, 1fr) 0;
}
.app-shell.sidebar-collapsed .sidebar-card,
.app-shell.detail-collapsed  .detail-card,
.app-shell.table-collapsed   .table-card {
  visibility: hidden;
  pointer-events: none;
}

.card {
  background: var(--panel);
  border-radius: 0;
  box-shadow: none;
  overflow: hidden;
  position: relative;
}

/* Subtle internal dividers instead of floating cards */
.sidebar-card { grid-column: 1 / 2; grid-row: 1 / 2; display: flex; flex-direction: column;
                border-right: 1px solid var(--border); }
.map-card     { grid-column: 2 / 3; grid-row: 1 / 2; }
.detail-card  {
  grid-column: 3 / 4; grid-row: 1 / 2;
  display: flex; flex-direction: column;
  border-left: 1px solid var(--border);
  overflow: hidden;
}
#detail_panel {
  display: flex; flex-direction: column;
  flex: 1; min-height: 0;
}
.table-card   {
  grid-column: 1 / 4; grid-row: 2 / 3;
  padding: 12px 18px; display: flex; flex-direction: column; min-height: 0;
  border-top: 1px solid var(--border);
}

/* Floating toggle chips in bottom-left of map */
.panel-toggles {
  position: absolute; left: 12px; bottom: 12px; z-index: 600;
  display: flex; gap: 6px;
}
.panel-toggle {
  background: white; color: var(--brand-navy);
  border: 1px solid var(--border); border-radius: 999px;
  padding: 5px 12px; font-size: 11px; font-weight: 600;
  cursor: pointer; box-shadow: 0 2px 6px rgba(0,0,0,0.10);
  transition: all 0.15s ease;
  display: inline-flex; align-items: center; gap: 6px;
  user-select: none;
}
.panel-toggle:hover { background: var(--brand-navy); color: white; border-color: var(--brand-navy); }
.panel-toggle .tog-icon { font-size: 13px; line-height: 1; }

/* ---- Sidebar ---- */
.sidebar-header {
  padding: 18px 20px 14px 20px;
  border-bottom: 1px solid var(--border);
  background: transparent;
}
.app-title {
  font-size: 20px; font-weight: 800; color: var(--brand-navy);
  letter-spacing: -0.01em; line-height: 1.15; margin: 0;
}
.app-subtitle {
  font-size: 11px; color: var(--muted); text-transform: uppercase;
  letter-spacing: 0.1em; margin-top: 4px; font-weight: 600;
}
.btn-show-intro {
  margin-top: 10px; font-size: 11px; font-weight: 600; color: var(--brand-navy);
  background: var(--panel-2); border: 1px solid var(--border); border-radius: 999px;
  padding: 4px 12px;
}
.btn-show-intro:hover { background: var(--brand-navy); color: white; border-color: var(--brand-navy); }
.sidebar-body {
  padding: 16px 18px 18px 18px; overflow-y: auto; flex: 1; min-height: 0;
}
.filter-group { margin-bottom: 18px; }
.filter-group-title {
  display: flex; align-items: center; justify-content: space-between;
  font-size: 11px; font-weight: 700; color: var(--brand-navy);
  text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 8px;
}
.filter-group-title .group-count {
  background: var(--panel-2); color: var(--muted);
  padding: 1px 8px; border-radius: 999px; font-size: 10px; letter-spacing: 0.04em;
}
.collapsible-title { cursor: pointer; user-select: none; }
.collapsible-title .title-left { display: flex; align-items: center; gap: 5px; }
.collapse-arrow {
  display: inline-block; font-size: 9px; color: #aaa;
  transform: rotate(90deg); transition: transform 0.2s ease;
}
.collapse-arrow.open { transform: rotate(270deg); }
.filter-group-body { overflow: hidden; transition: max-height 0.25s ease; max-height: 0; }
.filter-group-body:not(.collapsed) { max-height: 2000px; }

.sidebar-body .selectize-input,
.sidebar-body .form-control {
  border-radius: 12px !important; border: 1px solid var(--border) !important;
  background: white !important; font-size: 13px !important;
  min-height: 36px !important; box-shadow: none !important;
}
.sidebar-body .selectize-input.focus { border-color: var(--brand-blue) !important; }

.opt-list {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
}
.opt-list.single-col {
  grid-template-columns: 1fr;
}
.opt-row {
  display: flex; align-items: center; gap: 6px;
  padding: 7px 9px; border-radius: 10px;
  background: white; border: 1px solid var(--border);
  cursor: pointer; transition: all 0.12s ease;
  font-size: 12px; user-select: none;
  min-width: 0;  /* allow flex children to shrink */
}
.opt-row:hover:not(.disabled) { border-color: var(--brand-blue); background: #fafbff; }
.opt-row.active {
  background: var(--brand-navy); border-color: var(--brand-navy);
  color: white; font-weight: 600;
}
.opt-row.active .opt-count { background: rgba(255,255,255,0.2); color: white; }
.opt-row.disabled {
  background: var(--panel-2); color: #a6abbd;
  cursor: not-allowed; opacity: 0.75;
}
.opt-row.disabled .opt-count { background: #e4e0d4; color: #a6abbd; }
.opt-emoji { font-size: 14px; flex-shrink: 0; line-height: 1; }
.opt-label {
  flex: 1; line-height: 1.2; min-width: 0;
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
.opt-count {
  font-size: 10px; background: var(--panel-2); color: var(--muted);
  padding: 1px 6px; border-radius: 999px; font-weight: 700; flex-shrink: 0;
}

.clear-btn {
  width: 100%; background: transparent; color: var(--brand-navy);
  border: 1px solid var(--border); border-radius: 10px;
  padding: 7px 10px; font-size: 12px; font-weight: 600;
  cursor: pointer; margin-top: 6px; transition: all 0.15s ease;
}
.clear-btn:hover { background: var(--brand-navy); color: white; border-color: var(--brand-navy); }

/* ---- Map ---- */
.map-wrap { position: relative; width: 100%; height: 100%; }
#map { width: 100%; height: 100%; background: #e8eaf0; }

.selected-pill {
  position: absolute; top: 14px; left: 14px; z-index: 500;
  background: var(--brand-navy); color: white;
  padding: 7px 14px; border-radius: 999px;
  font-size: 12px; font-weight: 600;
  box-shadow: 0 2px 10px rgba(0,0,0,0.25);
}
.download-btn {
  position: absolute; top: 14px; right: 14px; z-index: 500;
  background: white; color: var(--brand-navy);
  border: 1px solid var(--border); border-radius: 999px;
  padding: 5px 12px; font-size: 11px; font-weight: 600;
  cursor: pointer; box-shadow: 0 2px 6px rgba(0,0,0,0.10);
  transition: all 0.15s ease;
  display: inline-flex; align-items: center; gap: 6px;
  user-select: none; text-decoration: none;
}

.map-legend {
  position: absolute; bottom: 14px; right: 14px; z-index: 500;
  background: rgba(255, 255, 255, 0.94); backdrop-filter: blur(6px);
  border-radius: 12px; padding: 8px 12px;
  font-size: 11px; color: var(--muted);
  display: flex; gap: 14px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.10);
  border: 1px solid var(--border);
}
.legend-item { display: flex; align-items: center; gap: 6px; }
.legend-swatch { width: 12px; height: 12px; border-radius: 3px; }

/* ---- Detail panel ---- */
.detail-header {
  padding: 16px 18px 12px 18px; border-bottom: 1px solid var(--border);
  background: transparent;
}
.detail-nav {
  display: flex; align-items: center; justify-content: space-between;
  margin-bottom: 8px;
}
.nav-btn {
  background: white; border: 1px solid var(--border); border-radius: 50%;
  width: 28px; height: 28px; font-size: 15px; line-height: 1;
  cursor: pointer; color: var(--brand-navy);
  transition: all 0.15s ease;
  display: flex; align-items: center; justify-content: center;
}
.nav-btn:hover:not(:disabled) { background: var(--brand-navy); color: white; border-color: var(--brand-navy); }
.nav-btn:disabled { opacity: 0.3; cursor: not-allowed; }
.nav-count { font-size: 11px; color: var(--muted); font-weight: 600; letter-spacing: 0.04em; }

.detail-title {
  font-size: 15px; font-weight: 700; color: var(--brand-navy);
  line-height: 1.3; margin: 2px 0 5px 0;
}
.detail-state {
  display: inline-block; font-size: 10px; font-weight: 700;
  color: var(--brand-green); text-transform: uppercase; letter-spacing: 0.08em;
}

.detail-body { padding: 14px 18px 20px 18px; overflow-y: auto; flex: 1; min-height: 0; }
.detail-section { margin-bottom: 14px; }
.detail-section-label {
  font-size: 10px; font-weight: 700; color: var(--muted);
  text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 5px;
}
.detail-desc { font-size: 13px; line-height: 1.5; color: var(--ink); }
.detail-desc p { margin: 0 0 8px 0; }
.detail-desc p:last-child { margin-bottom: 0; }
.detail-desc ul, .detail-desc ol { margin: 4px 0 8px 22px; padding: 0; }
.detail-desc li { margin-bottom: 4px; }
.detail-desc a { color: var(--brand-blue); }

.impl-badge {
  display: inline-block; padding: 4px 12px; border-radius: 999px;
  font-size: 12px; font-weight: 600;
}
.impl-fully   { background: #e2efd5; color: var(--brand-green); }
.impl-partial { background: #fae3cc; color: var(--brand-orange); }
.impl-not     { background: #fad6d6; color: #b91c1c; }
.impl-unknown { background: #e4e0d4; color: var(--muted); }

.chip {
  display: inline-block; padding: 3px 10px; border-radius: 999px;
  background: #e0e6f2; color: var(--brand-blue);
  font-size: 11px; font-weight: 600; margin: 2px 4px 2px 0;
}
.chip-navy   { background: #dde2ee; color: var(--brand-navy); }
.chip-green  { background: #e2efd5; color: var(--brand-green); }
.chip-orange { background: #fae3cc; color: var(--brand-orange); }

.detail-link {
  display: block; font-size: 12.5px; color: var(--brand-blue);
  text-decoration: none; margin-bottom: 4px; word-break: break-all;
}
.detail-link:hover { text-decoration: underline; color: var(--brand-navy); }

.meta-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
.meta-item-value { font-size: 13px; color: var(--ink); }

.empty-state { text-align: center; padding: 40px 20px; color: var(--muted); font-size: 13px; }

/* ---- Table ---- */
.table-card h3 {
  margin: 0 0 10px 0; font-size: 13px; color: var(--brand-navy);
  font-weight: 700; letter-spacing: 0.04em; text-transform: uppercase;
}
.table-card .dataTables_wrapper { flex: 1; overflow: auto; font-size: 13px; }
table.dataTable thead th {
  background: var(--panel-2) !important; color: var(--brand-navy) !important;
  font-weight: 700 !important; border-bottom: 2px solid var(--border) !important;
  font-size: 11px !important; text-transform: uppercase; letter-spacing: 0.06em;
}
table.dataTable tbody tr { cursor: pointer; }
table.dataTable td.dt-wrap {
  white-space: normal !important; word-break: break-word; vertical-align: top;
}
table.dataTable tbody tr.selected,
table.dataTable tbody tr.selected td {
  background: #e0e9f5 !important; color: var(--brand-navy) !important;
}
table.dataTable tbody tr:hover { background: #efece2 !important; }

/* ---- Leaflet ---- */
.leaflet-container { background: #e8eaf0 !important; font-family: inherit; }
.leaflet-tooltip {
  background: var(--brand-navy); color: white; border: none; border-radius: 10px;
  padding: 8px 12px; font-size: 12px; font-weight: 500;
  box-shadow: 0 2px 8px rgba(0,0,0,0.25); min-width: 130px;
}
.leaflet-tooltip:before { display: none; }

/* ---- Intro modal ---- */
.intro-modal p { margin: 0 0 10px 0; }

.intro-modal p { margin: 0 0 10px 0; }
.intro-how { display: grid; grid-template-columns: auto 1fr; gap: 10px 14px; margin-top: 12px; }
.intro-how-emoji { font-size: 20px; line-height: 1.2; }
.intro-how-text { font-size: 13px; color: var(--ink); }
.intro-how-text strong { color: var(--brand-navy); }

/* Scrollbar polish */
::-webkit-scrollbar { width: 8px; height: 8px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: #c9c4b3; border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: #a8a290; }

/* Responsive */
   
@media (max-width: 1100px) {
  .app-shell,
  .app-shell.sidebar-collapsed,
  .app-shell.detail-collapsed,
  .app-shell.sidebar-collapsed.detail-collapsed {
    grid-template-columns: 260px 1fr;
  }
  .app-shell,
  .app-shell.table-collapsed {
    grid-template-rows: auto auto auto;
  }
  .sidebar-card { grid-column: 1; grid-row: 1 / 3; }
  .map-card     { grid-column: 2; grid-row: 1; min-height: 50vh; }
  .detail-card  { grid-column: 2; grid-row: 2; min-height: 40vh; max-height: none; }
  .table-card   { grid-column: 1 / 3; grid-row: 3; }

  .app-shell.sidebar-collapsed .sidebar-card,
  .app-shell.detail-collapsed  .detail-card,
  .app-shell.table-collapsed   .table-card {
    display: none;
  }
}
@media (max-width: 700px) {
  .app-shell,
  .app-shell.sidebar-collapsed,
  .app-shell.detail-collapsed,
  .app-shell.sidebar-collapsed.detail-collapsed,
  .app-shell.table-collapsed {
    grid-template-columns: 1fr;
    grid-template-rows: auto;
    height: auto;
  }
  .sidebar-card, .map-card, .detail-card, .table-card { grid-column: 1; grid-row: auto; }
  .map-card { height: 50vh; }
  .opt-list { grid-template-columns: 1fr; }
}
"

# Inject palette values via simple token replacement (no sprintf = no % escaping)
app_css <- app_css_template
for (tok in names(PAL)) {
  app_css <- gsub(paste0("\\{", tok, "\\}"), PAL[[tok]], app_css)
}

# ---- UI builders (modular) -------------------------------------------------

sidebar_ui <- function() {
  div(
    class = "card sidebar-card",
    div(
      class = "sidebar-header",
      div(
        style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
        h1(class = "app-title", "50 States of Permitting"),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          tags$img(src = "EPIC_logo_small.png", height = "35px", width = "35px"),
          tags$img(src = "L4GG_logo_small.png", height = "35px", width = "23px")
        )
      ),
      actionButton("btn_show_intro", "About this tool", icon = icon("circle-info"),
                   class = "btn-show-intro")
    ),
    div(
      class = "sidebar-body",
      div(
        class = "filter-group",
        div(class = "filter-group-title",
            span("📍 State"),
            span(class = "group-count", length(states_with_data))),
        selectizeInput("flt_state", label = NULL,
                       choices = c("All states" = "", states_with_data),
                       selected = "", width = "100%", multiple = TRUE,
                       options = list(placeholder = "All states"))
      ),
      div(
        class = "filter-group",
        div(
          class = "filter-group-title collapsible-title",
          onclick = "toggleFilterGroup(this)",
          tags$span(class = "title-left",
            span("🔧 Project Type"),
            span(class = "collapse-arrow open", HTML("&#9654;"))
          ),
          span(class = "group-count", length(project_choices))
        ),
        div(class = "filter-group-body", uiOutput("project_options"))
      ),
      div(
        class = "filter-group",
        div(
          class = "filter-group-title collapsible-title",
          onclick = "toggleFilterGroup(this)",
          tags$span(class = "title-left",
            span("🏛️ Reform Category"),
            span(class = "collapse-arrow open", HTML("&#9654;"))
          ),
          span(class = "group-count", length(reform_choices))
        ),
        div(class = "filter-group-body", uiOutput("reform_options"))
      ),
      actionButton("clear_filters", "Clear all filters", class = "clear-btn")
    )
  )
}

map_ui <- function() {
  div(
    class = "card map-card",
    div(
      class = "map-wrap",
      leafletOutput("map", width = "100%", height = "100%"),
      uiOutput("selected_pill_ui"),
      downloadButton("download_data", label = "Download data",
                     class = "download-btn"),
      # Floating show/hide chips
      div(
        class = "panel-toggles",
        tags$button(
          id = "toggle_sidebar", class = "panel-toggle",
          onclick = "togglePanel('sidebar');",
          span(class = "tog-icon", HTML("☰")),
          span(id = "toggle_sidebar_label", "Hide filters")
        ),
        tags$button(
          id = "toggle_table", class = "panel-toggle",
          onclick = "togglePanel('table');",
          span(class = "tog-icon", HTML("▦")),
          span(id = "toggle_table_label", "Show table")
        ),
        tags$button(
          id = "toggle_detail", class = "panel-toggle",
          onclick = "togglePanel('detail');",
          span(class = "tog-icon", HTML("▶")),
          span(id = "toggle_detail_label", "Show detail")
        )
      ),
      div(
        class = "map-legend",
        div(class = "legend-item",
            div(class = "legend-swatch", style = paste0("background:", PAL$green)),
            "Selected"),
        div(class = "legend-item",
            div(class = "legend-swatch", style = paste0("background:", PAL$blue)),
            "Has matches"),
        div(class = "legend-item",
            div(class = "legend-swatch", style = paste0("background:", PAL$no_data)),
            "No data yet")
      )
    )
  )
}

detail_ui <- function() {
  div(class = "card detail-card", uiOutput("detail_panel"))
}

table_ui <- function() {
  div(
    class = "card table-card",
    uiOutput("table_header"),
    DTOutput("tbl")
  )
}

intro_modal <- function() {
  modalDialog(
    title = "Welcome to 50 States of Permitting",
    easyClose = TRUE,
    fade = TRUE,
    size = "m",
    footer = modalButton("Get started"),
    div(
      class = "intro-modal",
      tags$p(
        "An interactive explorer for state-level permitting reforms across ",
        "the United States. Browse reform categories, filter by project ",
        "type, and drill into individual actions and tools states are using to ",
        "streamline how permits get issued."
      ),
      tags$p("All reforms in this tool represent what states have pursued 
             between roughly 2022 and July 2026."),
      
      tags$div(class = "detail-section-label",
               style = "margin-top:18px;", "To learn more"),
      tags$p(
        "Check out EPIC's",
        tags$a(href = "https://www.policyinnovation.org/permitting",
               target = "_blank", "Permitting Innovation Hub")
      ),
      tags$p(
        "See EPIC's ",
        tags$a(href = "https://www.policyinnovation.org/insights/9-types",
               target = "_blank", "Nine Types of Permitting Reform")
      ),
      
      
      

      
      div(class = "detail-section-label",
               style = "margin-top:18px;", "How to use"),
      div(
        class = "intro-how",
        span(class = "intro-how-emoji", "🗺️"),
        div(class = "intro-how-text",
            tags$strong("Click a state"), " on the map to focus on its reforms. Click again to clear."),
        span(class = "intro-how-emoji", "🏛️"),
        div(class = "intro-how-text",
            tags$strong("Toggle reform categories and project types"),
            " in the left panel. Greyed-out options have no matches given your current filters."),
        span(class = "intro-how-emoji", "📋"),
        div(class = "intro-how-text",
            tags$strong("Browse the table"), " across the bottom and click a row to see its details."),
        span(class = "intro-how-emoji", "➡️"),
        div(class = "intro-how-text",
            tags$strong("Cycle through tools"), " one at a time using the arrows in the detail panel on the right."),
        span(class = "intro-how-emoji", "👁️"),
        div(class = "intro-how-text",
            tags$strong("Show or hide"),
            " the filter bar and table using the chips on the map.")
      ),
      
      tags$div(
        style = "margin-top:18px;",
        tags$div(class = "detail-section-label", "Created By"),
        tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$img(src = "EPIC_logo_full.png", height = "60px", width = "200px"),
          tags$div(
            style = "padding: 5px; position: relative; top: -10px;",
            tags$img(src = "L4GG_logo_full.png", height = "75px", width = "150px")
          )
        )
      )
      )
    )
}

# ---- Root UI ---------------------------------------------------------------

ui <- bootstrapPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css)),
    tags$script(HTML("
      // Toggle sidebar / table visibility by flipping classes on .app-shell.
      // Re-invalidate leaflet size after the CSS transition so the map repaints.
      window.toggleFilterGroup = function(titleEl) {
        var body = titleEl.nextElementSibling;
        var arrow = titleEl.querySelector('.collapse-arrow');
        if (!body) return;
        var isOpen = !body.classList.contains('collapsed');
        body.classList.toggle('collapsed', isOpen);
        if (arrow) arrow.classList.toggle('open', isOpen ? false : true);
      };
      window.togglePanel = function(which) {
        var shell = document.getElementById('app-shell');
        if (!shell) return;
        var clsMap = { sidebar: 'sidebar-collapsed', detail: 'detail-collapsed', table: 'table-collapsed' };
        var labelMap = { sidebar: 'filters', detail: 'detail', table: 'table' };
        var cls = clsMap[which];
        var collapsed = shell.classList.toggle(cls);
        var labelEl = document.getElementById('toggle_' + which + '_label');
        if (labelEl) labelEl.textContent = (collapsed ? 'Show ' : 'Hide ') + labelMap[which];
        // Resize leaflet after the transition ends
        setTimeout(function() {
          if (window.HTMLWidgets && HTMLWidgets.find) {
            var w = HTMLWidgets.find('#map');
            if (w && w.getMap) {
              try { w.getMap().invalidateSize(); } catch (e) {}
            }
          }
          window.dispatchEvent(new Event('resize'));
        }, 280);
      };
      // Called from server to reveal a panel (one-way: only shows, never hides)
      Shiny.addCustomMessageHandler('show_panel', function(which) {
        var shell = document.getElementById('app-shell');
        if (!shell) return;
        var clsMap   = { table: 'table-collapsed', detail: 'detail-collapsed' };
        var labelMap = { table: 'table', detail: 'detail' };
        shell.classList.remove(clsMap[which]);
        var labelEl = document.getElementById('toggle_' + which + '_label');
        if (labelEl) labelEl.textContent = 'Hide ' + labelMap[which];
        setTimeout(function() {
          if (window.HTMLWidgets && HTMLWidgets.find) {
            var w = HTMLWidgets.find('#map');
            if (w && w.getMap) { try { w.getMap().invalidateSize(); } catch(e) {} }
          }
          window.dispatchEvent(new Event('resize'));
        }, 280);
      });
    "))
  ),
  div(
    id = "app-shell",
    class = "app-shell table-collapsed detail-collapsed",
    sidebar_ui(),
    map_ui(),
    detail_ui(),
    table_ui()
  )
)

# ---- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Show intro modal on first load
  observe({
    showModal(intro_modal())
  }) |> bindEvent(session$clientData$url_search, once = TRUE, ignoreInit = FALSE)

  # Reopen intro modal from sidebar button
  observeEvent(input$btn_show_intro, {
    showModal(intro_modal())
  })


  # Selections for the pill-style filter groups
  sel_reforms  <- reactiveVal(character(0))
  sel_projects <- reactiveVal(character(0))

  # --- Progressive reveal ---------------------------------------------------
  # Show table when any filter or state is first used
  observeEvent(list(input$flt_state, sel_reforms(), sel_projects()), {
    if (isTruthy(input$flt_state) || length(sel_reforms()) > 0 || length(sel_projects()) > 0)
      session$sendCustomMessage("show_panel", "table")
  }, ignoreInit = TRUE)

  # Show detail panel only when user physically clicks a table row
  observeEvent(input$tbl_cell_clicked, {
    if (length(input$tbl_cell_clicked) > 0)
      session$sendCustomMessage("show_panel", "detail")
  }, ignoreInit = TRUE)

  # --- Filtered dataset -----------------------------------------------------
  filtered <- reactive({
    df <- raw
    if (isTruthy(input$flt_state)) df <- df |> filter(state %in% input$flt_state)
    if (length(sel_reforms()) > 0) {
      names_match <- long_reform |>
        filter(reform_category_single %in% sel_reforms()) |>
        pull(action_tool_name) |> unique()
      df <- df |> filter(action_tool_name %in% names_match)
    }
    if (length(sel_projects()) > 0) {
      names_match <- long_project |>
        filter(project_type_single %in% sel_projects()) |>
        pull(action_tool_name) |> unique()
      df <- df |> filter(action_tool_name %in% names_match)
    }
    df
  })

  # --- Counts for greying out options ---------------------------------------
  # Reform counts = how many rows per reform, given state + project filters
  rows_under_state_and_projects <- reactive({
    df <- raw
    if (isTruthy(input$flt_state)) df <- df |> filter(state %in% input$flt_state)
    if (length(sel_projects()) > 0) {
      names_match <- long_project |>
        filter(project_type_single %in% sel_projects()) |>
        pull(action_tool_name) |> unique()
      df <- df |> filter(action_tool_name %in% names_match)
    }
    df
  })
  rows_under_state_and_reforms <- reactive({
    df <- raw
    if (isTruthy(input$flt_state)) df <- df |> filter(state %in% input$flt_state)
    if (length(sel_reforms()) > 0) {
      names_match <- long_reform |>
        filter(reform_category_single %in% sel_reforms()) |>
        pull(action_tool_name) |> unique()
      df <- df |> filter(action_tool_name %in% names_match)
    }
    df
  })

  reform_counts <- reactive({
    base <- rows_under_state_and_projects()
    long_reform |>
      filter(action_tool_name %in% base$action_tool_name) |>
      count(reform_category_single, name = "n")
  })
  project_counts <- reactive({
    base <- rows_under_state_and_reforms()
    long_project |>
      filter(action_tool_name %in% base$action_tool_name) |>
      count(project_type_single, name = "n")
  })

  # --- Pill-style filter group renderer ------------------------------------
  render_pill_options <- function(choices, counts_df, key_col, sel,
                                  input_prefix, emoji_lookup, extra_class = NULL) {
    # Build a safe lookup: named numeric vector; missing keys -> 0 via match()
    if (nrow(counts_df) > 0) {
      keys <- counts_df[[key_col]]
      vals <- counts_df$n
    } else {
      keys <- character(0)
      vals <- integer(0)
    }
    safe_count <- function(opt) {
      i <- match(opt, keys)
      if (is.na(i)) 0L else as.integer(vals[i])
    }

    div(
      class = paste(c("opt-list", extra_class), collapse = " "),
      lapply(choices, function(opt) {
        n <- safe_count(opt)
        is_selected <- opt %in% sel
        is_disabled <- !is_selected && n == 0
        cls <- "opt-row"
        if (is_selected) cls <- paste(cls, "active")
        if (is_disabled) cls <- paste(cls, "disabled")
        # JS escape single-quotes in label (fixed = TRUE, literal replacement)
        safe_opt <- gsub("'", "\\'", opt, fixed = TRUE)
        onclick_js <- sprintf(
          "if(!this.classList.contains('disabled')){Shiny.setInputValue('%s_toggle', {opt: '%s', ts: Date.now()}, {priority: 'event'});}",
          input_prefix, safe_opt
        )
        div(
          class = cls, onclick = onclick_js, title = opt,
          span(class = "opt-emoji", emoji_for(emoji_lookup, opt)),
          span(class = "opt-label", opt),
          span(class = "opt-count", n)
        )
      })
    )
  }

  output$reform_options <- renderUI({
    render_pill_options(reform_choices, reform_counts(), "reform_category_single",
                        sel_reforms(), "reform", REFORM_EMOJI, extra_class = "single-col")
  })
  output$project_options <- renderUI({
    render_pill_options(project_choices, project_counts(), "project_type_single",
                        sel_projects(), "project", PROJECT_EMOJI, extra_class = "single-col")
  })

  # Toggles
  observeEvent(input$reform_toggle, {
    opt <- input$reform_toggle$opt
    cur <- sel_reforms()
    sel_reforms(if (opt %in% cur) setdiff(cur, opt) else c(cur, opt))
  })
  observeEvent(input$project_toggle, {
    opt <- input$project_toggle$opt
    cur <- sel_projects()
    sel_projects(if (opt %in% cur) setdiff(cur, opt) else c(cur, opt))
  })

  observeEvent(input$clear_filters, {
    updateSelectizeInput(session, "flt_state", selected = "")
    sel_reforms(character(0))
    sel_projects(character(0))
  })

  # --- Map ------------------------------------------------------------------
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(
      zoomControl = FALSE, attributionControl = FALSE,
      minZoom = 3, maxZoom = 7, zoomSnap = 0.25
    )) |> setView(lng = -96, lat = 40, zoom = 4)

    if (!is.null(states_sf)) {
      m <- m |>
        addPolygons(
          data = states_sf, layerId = ~state_name,
          fillColor = ~ifelse(has_data, PAL$blue, PAL$no_data),
          fillOpacity = ~ifelse(has_data, 0.65, 0.70),
          color = ~ifelse(has_data, "#ffffff", PAL$no_data_line),
          weight = 1.2, smoothFactor = 0.3,
          label = lapply(seq_len(nrow(states_sf)), function(i)
            HTML(make_state_tooltip(states_sf$state_name[i], states_sf$n_tools[i], states_sf$has_data[i]))),
          highlightOptions = highlightOptions(
            weight = 2.5, color = PAL$navy, fillOpacity = 0.85, bringToFront = TRUE
          )
        )
    }
    m
  })

  # Recolor reactively
  observe({
    if (is.null(states_sf)) return()

    df <- filtered()
    counts <- df |> count(state, name = "n_filt")
    sel_state <- if (isTruthy(input$flt_state)) input$flt_state else character(0)

    sf_upd <- states_sf |>
      mutate(
        n_filt   = counts$n_filt[match(state_name, counts$state)],
        n_filt   = ifelse(is.na(n_filt), 0L, n_filt),
        is_sel   = state_name %in% sel_state
      )

    fill_col <- ifelse(
      sf_upd$is_sel, PAL$green,
      ifelse(sf_upd$n_filt > 0, PAL$blue,
             ifelse(sf_upd$has_data, "#6b7ba6", PAL$no_data))
    )
    fill_op <- ifelse(
      sf_upd$is_sel, 0.9,
      ifelse(sf_upd$n_filt > 0, 0.7,
             ifelse(sf_upd$has_data, 0.5, 0.7))
    )
    border_col <- ifelse(
      sf_upd$is_sel, PAL$navy,
      ifelse(sf_upd$has_data, "#ffffff", PAL$no_data_line)
    )
    border_wt <- ifelse(sf_upd$is_sel, 2.5, 1.2)

    leafletProxy("map") |>
      removeShape(layerId = states_sf$state_name) |>
      addPolygons(
        data = sf_upd, layerId = ~state_name,
        fillColor = fill_col, fillOpacity = fill_op,
        color = border_col, weight = border_wt, smoothFactor = 0.3,
        label = lapply(seq_len(nrow(sf_upd)), function(i)
          HTML(make_state_tooltip(sf_upd$state_name[i], sf_upd$n_tools[i], sf_upd$has_data[i]))),
        highlightOptions = highlightOptions(
          weight = 2.5, color = PAL$navy, fillOpacity = 0.9, bringToFront = TRUE
        )
      )
  })

  # Click to filter
  observeEvent(input$map_shape_click, {
    clicked <- input$map_shape_click$id
    if (is.null(clicked)) return()
    if (!(clicked %in% states_with_data)) return()
    current <- if (isTruthy(input$flt_state)) input$flt_state else character(0)
    new_sel <- if (clicked %in% current) setdiff(current, clicked) else union(current, clicked)
    updateSelectizeInput(session, "flt_state", selected = new_sel)
  })

  output$selected_pill_ui <- renderUI({
    if (isTruthy(input$flt_state)) {
      label <- if (length(input$flt_state) == 1) {
        paste0("📍 ", input$flt_state, " · click to deselect")
      } else {
        paste0("📍 ", length(input$flt_state), " states selected · click a state to deselect")
      }
      div(class = "selected-pill", label)
    }
  })

  # --- Download -------------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() "50-states-permitting.csv",
    content  = function(file) write.csv(raw, file, row.names = FALSE)
  )

  # --- Table ----------------------------------------------------------------
  output$table_header <- renderUI({
    n <- nrow(filtered())
    tags$h3(paste0("State Permitting Actions and Tools · ", n))
  })

  output$tbl <- renderDT({
    df <- filtered() |>
      select(
        `Action and Tool`     = action_tool_name,
        State             = state,
        `Reform Category` = reform_category,
        `Project Type`    = project_type,
        `Type`            = action_tool_type,
        Implementation    = implementation
      ) |>
      mutate(across(everything(), ~ map_chr(.x, md_to_inline_html)))

    datatable(
      df,
      selection = list(mode = "single", selected = if (nrow(df)) 1 else NULL),
      rownames = FALSE, escape = FALSE,
      options = list(
        dom = "tip", pageLength = 6, scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-left"),
          list(targets = 0, width = "240px", className = "dt-left dt-wrap"),
          list(targets = 2, width = "200px", className = "dt-left dt-wrap")
        ),
        language = list(
          emptyTable = "No action tools match the current filters.",
          info = "_START_ – _END_ of _TOTAL_",
          infoEmpty = "", infoFiltered = ""
        )
      ),
      class = "display compact"
    )
  }, server = FALSE)

  # --- Selection drives detail panel ---------------------------------------
  selected_idx <- reactiveVal(1)

  observeEvent(filtered(), {
    if (nrow(filtered()) > 0) {
      selected_idx(1)
      selectRows(dataTableProxy("tbl"), 1)
    } else {
      selected_idx(NA_integer_)
    }
  })

  observeEvent(input$tbl_rows_selected, {
    sel <- input$tbl_rows_selected
    if (length(sel) && !is.na(sel)) selected_idx(sel)
  })

  observeEvent(input$prev_tool, {
    n <- nrow(filtered()); if (n == 0) return()
    cur <- selected_idx(); if (is.na(cur)) cur <- 1
    new_idx <- if (cur <= 1) n else cur - 1
    selected_idx(new_idx); selectRows(dataTableProxy("tbl"), new_idx)
  })
  observeEvent(input$next_tool, {
    n <- nrow(filtered()); if (n == 0) return()
    cur <- selected_idx(); if (is.na(cur)) cur <- 1
    new_idx <- if (cur >= n) 1 else cur + 1
    selected_idx(new_idx); selectRows(dataTableProxy("tbl"), new_idx)
  })

  # --- Detail panel ---------------------------------------------------------
  render_section <- function(label, content) {
    div(class = "detail-section",
        div(class = "detail-section-label", label),
        content)
  }

  render_chips <- function(vals, class = "chip", emoji_lookup = NULL) {
    if (length(vals) == 0) return(span(style = "color: var(--muted); font-size: 13px;", "—"))
    tagList(lapply(vals, function(v) {
      prefix <- if (!is.null(emoji_lookup)) paste0(emoji_for(emoji_lookup, v), " ") else ""
      span(class = class, HTML(paste0(prefix, md_to_inline_html(v))))
    }))
  }

  impl_class_for <- function(impl) {
    if (is.na(impl)) return("impl-unknown")
    dplyr::case_when(
      impl == "Fully Implemented"     ~ "impl-fully",
      impl == "Partially Implemented" ~ "impl-partial",
      impl == "Not Started"           ~ "impl-not",
      TRUE                            ~ "impl-unknown"
    )
  }

  extract_urls <- function(x) {
    if (is.na(x)) return(character(0))
    u <- unlist(str_split(x, "[,\\s]+"))
    u[nzchar(u) & str_detect(u, "^https?://")]
  }

  output$detail_panel <- renderUI({
    df <- filtered(); n <- nrow(df); idx <- selected_idx()

    if (n == 0 || is.na(idx)) {
      return(tagList(
        div(class = "detail-header",
            div(class = "detail-nav",
                tags$button("‹", class = "nav-btn", disabled = NA),
                span(class = "nav-count", "0 of 0"),
                tags$button("›", class = "nav-btn", disabled = NA)),
            div(class = "detail-title", "No selection"),
            span(class = "detail-state", "Adjust filters to see results")),
        div(class = "detail-body",
            div(class = "empty-state",
                "No action tools match the current filters. Try clearing filters or selecting a different state."))
      ))
    }

    idx <- max(1, min(idx, n))
    row <- df[idx, ]

    impl <- row$implementation
    impl_txt <- if (is.na(impl)) "Unknown" else impl

    reforms   <- split_multi(row$reform_category)
    projects  <- split_multi(row$project_type)
    tooltypes <- split_multi(row$action_tool_type)
    urls      <- extract_urls(row$relevant_urls)

    tagList(
      div(class = "detail-header",
          div(class = "detail-nav",
              tags$button("‹", class = "nav-btn action-button",
                          onclick = "Shiny.setInputValue('prev_tool', Date.now(), {priority: 'event'});"),
              span(class = "nav-count", sprintf("%d of %d", idx, n)),
              tags$button("›", class = "nav-btn action-button",
                          onclick = "Shiny.setInputValue('next_tool', Date.now(), {priority: 'event'});")),
          div(class = "detail-title", row$action_tool_name),
          span(class = "detail-state", row$state)),

      div(class = "detail-body",

          if (!is.na(row$description))
            render_section("Description", div(class = "detail-desc", md_block(row$description))),

          render_section("Implementation Status",
                         span(class = paste("impl-badge", impl_class_for(impl)), impl_txt)),

          if (length(urls) > 0)
            render_section("Relevant Links",
                           tagList(lapply(urls, function(u)
                             tags$a(href = HTML(u), target = "_blank",
                                    class = "detail-link", u)))),

          render_section("Reform Categories",
                         render_chips(reforms, "chip chip-navy", REFORM_EMOJI)),

          render_section("Project Types",
                         render_chips(projects, "chip chip-green", PROJECT_EMOJI)),

          render_section("Action Tool Type",
                         render_chips(tooltypes, "chip chip-orange")),

          div(class = "meta-grid",
              div(class = "detail-section",
                  div(class = "detail-section-label", "Announced / Passed"),
                  div(class = "meta-item-value", md_or_dash(row$data_announced_passed))),
              div(class = "detail-section",
                  div(class = "detail-section-label", "Effective Date"),
                  div(class = "meta-item-value", md_or_dash(row$effective_date)))),

          if (!is.na(row$EO_Leg_ID))
            render_section("Executive Orders / Legislation",
                           div(class = "meta-item-value", md_inline(row$EO_Leg_ID)))
      )
    )
  })
}

shinyApp(ui, server)
