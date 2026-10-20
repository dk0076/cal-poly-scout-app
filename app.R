# ---------------------------------------------------------------------------
# scoutR -- opponent and self scouting off VolleyStation (.vsm) and DataVolley
# (.dvw) coded match files.
#
# Shiny port of the scout_vsm single-file tool. The panels are rendered as HTML
# strings rather than built from Shiny widgets: the tables and courts are read
# off paper as often as off a screen, and raw HTML is what keeps the print
# stylesheet, the repeating table headers and the per-shot SVG tooltips intact.
# One delegated JS click handler (www/app.js) feeds every control back in.
#
# Run:  shiny::runApp("scoutR")
# ---------------------------------------------------------------------------

library(shiny)
suppressMessages(library(data.table))

for (f in c("core.R", "parse_vsm.R", "parse_dvw.R", "model.R", "svg.R",
            "tables.R", "tab_attack.R", "tab_serve.R", "tab_set.R",
            "tab_setter.R", "tab_passing.R", "tab_defense.R", "tab_rotation.R",
            "tab_last6.R",
            "tab_plays.R"))
  source(file.path("R", f))

# A merged multi-match .vsm runs well past Shiny's 5 MB default, and a
# truncated upload presents as a parsing error rather than as a size problem.
options(shiny.maxRequestSize = 300 * 1024^2)

TABS <- list(c("attack", "Attack direction"), c("serve", "Serve direction"),
             c("setting", "Set origin"), c("setter", "Setter"),
             c("passing", "Pass quality"), c("defense", "Defense"),
             c("rotation", "Rotations"), c("last6", "Last 6"),
             c("plays", "Plays &amp; export"))

DEFAULT_OPTS <- list(
  aView = "arrows", aBins = 3, aColor = "volume", aIncErr = TRUE, tend = TRUE,
  sView = "arrows", sBins = 3, srvType = "all", sPass = FALSE,
  eView = "points", eBins = 3, setSetter = "", setZones = character(),
  setterWho = "main", setterEx = character(), lastN = 6,
  mono = FALSE, phases = character(), sys = character()
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

# Stamp the static assets with their own modification time. Shiny serves www/
# with a Last-Modified header and no Cache-Control, so a browser is free to
# keep serving an old stylesheet after an edit -- which it did, and the app
# then looks broken in a way that has nothing to do with the code. The stamp
# changes whenever the file does, so the browser cannot reuse a stale copy.
asset <- function(f) {
  p <- file.path("www", f)
  if (!file.exists(p)) return(f)
  sprintf("%s?v=%d", f, as.integer(file.info(p)$mtime))
}

ui <- fluidPage(
  tags$head(
    tags$title("Cal Poly Scout App"),
    tags$link(rel = "stylesheet", href = asset("styles.css")),
    tags$script(src = asset("app.js"))
  ),
  tags$header(
    class = "appbar",
    div(class = "brand",
        span(class = "mark"),
        h1(HTML("Cal Poly <span>Scout</span>")),
        span(class = "v", "v1")),
    div(style = "min-width:260px", uiOutput("uploader", inline = TRUE)),
    div(class = "spacer"),
    uiOutput("focus_pick", inline = TRUE),
    uiOutput("header_tools", inline = TRUE)
  ),
  uiOutput("body")
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  S <- reactiveValues(
    matches = list(), focus_name = "", focus = list(),
    sel = list(matches = character(), sets = integer(), players = integer(),
               rots = character(), passers = integer()),
    o = DEFAULT_OPTS, tab = "attack", errors = character(), warnings = character()
  )

  # ------------------------------------------------------------ file intake

  # One file input, always in the DOM. On the opening screen it is visually
  # hidden and the drop zone forwards to it; once files are loaded it becomes
  # the way to add more. Hidden with the clip pattern rather than display:none,
  # because a display:none input cannot be opened with .click().
  output$uploader <- renderUI(
    div(class = if (length(S$matches)) "" else "upload-quiet",
        fileInput("files", NULL, multiple = TRUE,
                  accept = c(".vsm", ".dvw", ".json"),
                  placeholder = "Add .vsm / .dvw files", width = "100%"))
  )

  # The bundled sample is loaded only on request, never on startup. One of the
  # two files under data/ is a generated fixture: it exists so the coordinate
  # charts have something to draw, and a number read off it would be a number
  # about nothing. Making it opt-in, and banner-ing it once loaded, is what
  # keeps that distinction visible.
  # Only the .vsm. data/ also holds the .dvw it was generated from, and loading
  # both would pool one match with a copy of itself -- every total doubled,
  # every rate unchanged, which is exactly the kind of wrong that looks right.
  # tests/verify.R loads both on purpose, to exercise each parser.
  observeEvent(input$load_sample, {
    f <- Sys.glob(file.path("data", "fixture_coords.vsm"))
    if (!length(f)) {
      S$errors <- "data/fixture_coords.vsm not found. Generate it with: Rscript data-raw/make_fixture.R data/sample_match.dvw data/fixture_coords.vsm"
      return()
    }
    ingest(data.frame(name = basename(f), datapath = f, stringsAsFactors = FALSE),
           sample = TRUE)
  })

  observeEvent(input$files, ingest(input$files))

  ingest <- function(files, sample = FALSE) {
    errs <- character(); warns <- character()
    for (i in seq_len(nrow(files))) {
      nm <- files$name[i]
      m <- tryCatch({
        txt <- paste(readLines(files$datapath[i], warn = FALSE,
                               encoding = "UTF-8"), collapse = "\n")
        txt <- sub("^﻿", "", txt)
        # .dvw is DataVolley's plain text; .vsm is JSON. Sniff the content too,
        # since a file renamed on the way out of VolleyMetrics is common.
        is_dvw <- grepl("\\.dvw$", nm, ignore.case = TRUE) ||
          grepl("^\\s*\\[3DATAVOLLEYSCOUT\\]", txt)
        if (is_dvw) parse_dvw(txt, nm) else parse_vsm(txt, nm)
      }, error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
      if (is.null(m)) next
      m <- finalize_match(m)
      warns <- c(warns, m$warnings)
      if (!nrow(m$plays))
        warns <- c(warns, sprintf("%s: loaded, no coded rallies yet.", nm))
      S$matches[[m$match_id]] <- m
    }
    S$errors <- errs
    if (length(S$matches)) {
      ms <- S$matches
      ms <- ms[order(vapply(ms, function(m) paste0(m$date %||% "", "|", m$file_name), ""))]
      ms <- classify_systems(ms)
      ms <- add_season_codes(ms)
      S$matches <- ms
      idx <- team_index(ms)
      # The focus team is the one appearing in the most files; ties break
      # alphabetically and are fully overridable from the header picker.
      if (!nzchar(S$focus_name) || !(S$focus_name %in% idx$name))
        S$focus_name <- idx$name[1]
      S$focus <- focus_sides(ms, S$focus_name)
      S$sel$players <- integer()
    }
    if (sample)
      warns <- c("Bundled sample loaded. fixture_coords.vsm is SYNTHETIC, its events are real but its coordinates are generated, so read it as a demonstration of the charts, never as a measurement.",
                 warns)
    S$warnings <- unique(warns)
  }

  # ------------------------------------------------------- control handlers

  observeEvent(input$js_tab, S$tab <- input$js_tab$tab)

  observeEvent(input$js_opt, {
    k <- input$js_opt$key; v <- input$js_opt$val
    v <- if (identical(v, "true")) TRUE else if (identical(v, "false")) FALSE
         else if (!is.na(suppressWarnings(as.numeric(v))) && nzchar(v)) as.numeric(v)
         else v
    S$o[[k]] <- v
  })

  observeEvent(input$js_sel, {
    k <- input$js_sel$key; v <- input$js_sel$val
    if (k %in% c("phases", "sys")) {
      cur <- S$o[[k]]
      S$o[[k]] <- if (v %in% cur) setdiff(cur, v) else c(cur, v)
      return()
    }
    cur <- S$sel[[k]]
    v2 <- if (k %in% c("sets", "players", "passers")) as.integer(v) else v
    S$sel[[k]] <- if (v2 %in% cur) setdiff(cur, v2) else c(cur, v2)
  })

  observeEvent(input$js_clear, {
    k <- input$js_clear$key
    if (k %in% c("phases", "sys")) S$o[[k]] <- character()
    else S$sel[[k]] <- if (k %in% c("sets", "players", "passers")) integer() else character()
  })

  observeEvent(input$js_match, {
    id <- input$js_match$val
    cur <- S$sel$matches
    # "nothing selected" means "all on", so the first click has to materialise
    # the full set before removing one from it
    if (!length(cur)) cur <- names(S$matches)
    cur <- if (id %in% cur) setdiff(cur, id) else c(cur, id)
    if (length(cur) == length(S$matches)) cur <- character()
    S$sel$matches <- cur
  })

  observeEvent(input$js_side, {
    S$focus[[input$js_side$id]] <- input$js_side$val
    S$sel$players <- integer()
  })

  observeEvent(input$js_setx, {
    v <- input$js_setx$val; cur <- S$o$setterEx
    S$o$setterEx <- if (v %in% cur) setdiff(cur, v) else c(cur, v)
  })

  observeEvent(input$js_zone, {
    v <- input$js_zone$val; cur <- S$o$setZones
    S$o$setZones <- if (v %in% cur) setdiff(cur, v) else c(cur, v)
  })
  observeEvent(input$js_zoneclear, S$o$setZones <- character())

  observeEvent(input$focus_team, {
    req(input$focus_team)
    S$focus_name <- input$focus_team
    S$focus <- focus_sides(S$matches, input$focus_team)
    S$sel$players <- integer()
  })

  # ------------------------------------------------------------- reactives

  F <- reactive(focus_plays(S$matches, S$sel, S$focus))
  F_nosys <- reactive(focus_plays(S$matches, S$sel, S$focus, ignore_sys = TRUE))
  coords_ok <- reactive(coords_available(S$matches, S$sel))

  # ------------------------------------------------------------------- body

  output$body <- renderUI({
    if (!length(S$matches)) return(intro_ui(S$errors))
    div(class = "wrap",
        tags$aside(class = "rail", uiOutput("rail")),
        tags$main(class = "panel", uiOutput("tabbar"), uiOutput("panel")))
  })

  output$focus_pick <- renderUI({
    idx <- team_index(S$matches)
    if (!nrow(idx)) return(NULL)
    tagList(
      tags$label("Scouting"),
      tags$select(id = "focus_team", class = "shiny-input-select",
                  onchange = "Shiny.setInputValue('focus_team', this.value)",
                  lapply(seq_len(nrow(idx)), function(i)
                    tags$option(value = idx$name[i],
                                selected = if (identical(idx$name[i], S$focus_name)) "selected",
                                sprintf("%s · %s/%s files", idx$name[i], idx$files[i],
                                        length(S$matches)))))
    )
  })

  output$header_tools <- renderUI({
    if (!length(S$matches)) return(NULL)
    tagList(
      tags$button(class = "chip", `data-o` = "mono",
                  `data-v` = if (isTRUE(S$o$mono)) "false" else "true",
                  if (isTRUE(S$o$mono)) "Colour" else "Mono (print)"),
      tags$button(class = "chip", onclick = "window.print()", "Print"),
      downloadButton("csv", "CSV", class = "chip")
    )
  })

  output$tabbar <- renderUI(HTML(paste0(vapply(TABS, function(t)
    sprintf('<button class="tab %s" data-tab="%s">%s</button>',
            if (identical(S$tab, t[1])) "on" else "", t[1], t[2]), ""),
    collapse = "") |> (\(x) sprintf('<div class="tabs">%s</div>', x))()))

  output$rail <- renderUI(HTML(rail_html(S)))

  output$panel <- renderUI({
    o <- S$o
    body <- tryCatch({
      if (S$tab == "attack")       tab_attack(F(), o, coords_ok())
      else if (S$tab == "serve")   tab_serve(F(), o, coords_ok())
      else if (S$tab == "setting") tab_set(F(), o, coords_ok())
      else if (S$tab == "setter")  tab_setter(F(), o, coords_ok())
      else if (S$tab == "passing")
        tab_passing(F(), o, coords_ok(),
                    sys_rows = system_card(F_nosys(),
                                           convert_rallies(S$matches, S$sel, S$focus)))
      # the rotation chips are dropped for this tab: it compares the six
      # against each other, so filtering to one would empty it
      else if (S$tab == "last6") tab_last6(F(), o)
      else if (S$tab == "defense") {
        sel_all <- S$sel; sel_all$rots <- character()
        tab_defense(F(), opponent_plays(S$matches, S$sel, S$focus),
                    convert_rallies(S$matches, S$sel, S$focus), o, coords_ok())
      }
      else if (S$tab == "rotation") {
        sel_all <- S$sel; sel_all$rots <- character()
        # an opponent play already carries the opponent's own rotation in
        # `rot`; `opp_rot` on it is ours
        Op <- opponent_plays(S$matches, sel_all, S$focus)
        tab_rotation(rally_outcomes(S$matches, sel_all, S$focus),
                     focus_plays(S$matches, sel_all, S$focus),
                     rally_outcomes(S$matches, sel_all, S$focus, opp = TRUE),
                     Op, o)
      }
      else tab_plays(F(), o)
    }, error = function(e)
      empty(sprintf("Nothing coded yet on this tab. It fills in as the match is scouted. (%s)",
                    esc(conditionMessage(e)))))
    HTML(paste0(coverage_warning(S), body))
  })

  output$csv <- downloadHandler(
    filename = function()
      sprintf("scoutR_%s_%s.csv", gsub("[^A-Za-z0-9]+", "-", S$focus_name),
              format(Sys.Date(), "%Y%m%d")),
    content = function(path) {
      rows <- F()
      keep <- intersect(CSV_COLS, names(rows))
      data.table::fwrite(rows[, keep, with = FALSE], path)
    }
  )
}

# ---------------------------------------------------------------------------
# Static chrome
# ---------------------------------------------------------------------------

#' The opening screen: a title and somewhere to put the files.
#'
#' The drop zone is not its own file input -- there is one `fileInput` in the
#' header and this forwards to it -- because two inputs sharing an id is how
#' uploads start going to the wrong one.
intro_ui <- function(errors) {
  div(id = "intro",
      h2("Cal Poly Scout App"),
      div(class = "lede", "Opponent & self scout"),
      div(class = "byline", "Andrew King"),
      div(class = "affil", "Statistics & Data Science, Cal Poly"),
      div(id = "drop", tabindex = "0", role = "button",
          `aria-label` = "Select or drag match files",
          HTML('<svg class="ico" viewBox="0 0 24 24" aria-hidden="true"><path d="M12 16V4m0 0L7.5 8.5M12 4l4.5 4.5" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/><path d="M4 15v3.5A1.5 1.5 0 0 0 5.5 20h13a1.5 1.5 0 0 0 1.5-1.5V15" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>'),
          div(class = "big", "Select or drag match files"),
          div(class = "small", HTML("<code>.vsm</code> or <code>.dvw</code> · as many as you like"))),
      # Only offered when a sample is actually present. data/ is not in the
      # repository -- match files are not ours to publish -- so a fresh clone
      # has none, and a link that only produces an error is worse than no link.
      if (length(Sys.glob(file.path("data", "fixture_coords.vsm"))))
        div(class = "introfoot",
            actionLink("load_sample", "Load the bundled sample")) else NULL,
      HTML(paste0(vapply(errors, function(e) sprintf('<div class="err">%s</div>', esc(e)), ""),
                  collapse = ""))
  )
}

#' Per-file coding depth, surfaced rather than buried: pooling files coded to
#' different depths biases every rate on every tab.
coverage_warning <- function(S) {
  bad <- character()
  for (m in S$matches) {
    if (length(S$sel$matches) && !(m$match_id %in% S$sel$matches)) next
    side <- S$focus[[m$match_id]]
    if (is.null(side) || is.na(side)) next
    cv <- coverage(m, side)
    if (!is.na(cv$pct) && cv$pct < 0.85)
      bad <- c(bad, sprintf("%s (%s of receive rallies have a coded attack)",
                            esc(m$file_name), pct(cv$pct)))
  }
  w <- S$warnings
  out <- character()
  if (length(bad))
    out <- c(out, sprintf('<div class="warn">Thinly coded next to the others, so pooled rates are pulled toward the fuller files: %s</div>',
                          paste(bad, collapse = "; ")))
  if (length(w))
    out <- c(out, sprintf('<div class="warn">%s</div>',
                          paste(vapply(w, esc, ""), collapse = "<br>")))
  paste0(out, collapse = "")
}

#' The filter rail. Rendered as one HTML string for the same reason the panels
#' are: ~60 chips whose counts change with every other chip.
rail_html <- function(S) {
  F <- focus_plays(S$matches, S$sel, S$focus)
  sets_avail <- sort(unique(unlist(lapply(S$matches, function(m)
    vapply(m$sets, function(s) s$set_no, 0)))))
  roster <- focus_roster(S$matches, S$focus)
  H <- character()

  H <- c(H, '<div class="grp"><div class="lbl">Matches <button data-clr="matches">all</button></div>')
  for (m in S$matches) {
    on <- !length(S$sel$matches) || m$match_id %in% S$sel$matches
    has_focus <- identical(m$teams$home$name, S$focus_name) ||
                 identical(m$teams$away$name, S$focus_name)
    side <- S$focus[[m$match_id]]
    intra <- identical(m$teams$home$name, m$teams$away$name)
    opp <- if (has_focus && !is.na(side))
      m$teams[[if (side == "home") "away" else "home"]]$name else NA_character_
    H <- c(H, sprintf('<div class="matchrow" style="%s%s">
        <div class="top">
          <div class="nm" title="%s">%s</div>
          <button class="chip %s" data-mt="%s">%s</button>
        </div>%s
        <select data-focus="%s">
          <option value="home"%s>%s</option>
          <option value="away"%s>%s</option>
        </select>
        <div class="meta">%s · %s sets · %s rallies%s</div>
      </div>',
      if (on) "" else "opacity:.45",
      if (has_focus) "" else ";border-color:var(--blocked)",
      esc(m$file_name),
      if (intra) sprintf("intrasquad · %s side", if (is.na(side)) "?" else side)
        else if (has_focus) paste("vs", esc(opp))
        else paste(esc(m$teams$home$name), "vs", esc(m$teams$away$name)),
      if (on) "on" else "", esc(m$match_id), if (on) "on" else "off",
      if (has_focus) "" else sprintf('<div class="meta" style="color:var(--blocked)">no "%s" in this file, contributes nothing until you switch focus team</div>',
                                     esc(S$focus_name)),
      esc(m$match_id),
      # An intrasquad practice file has the same team name on both sides, so
      # the two options would be indistinguishable. Say which side each is.
      if (identical(side, "home")) " selected" else "",
      esc(paste0(m$teams$home$name, if (intra) ", home side" else "")),
      if (identical(side, "away")) " selected" else "",
      esc(paste0(m$teams$away$name, if (intra) ", away side" else "")),
      esc(m$date %||% "no date"), length(m$sets),
      sum(vapply(m$sets, function(s) s$rallies, 0)),
      if (m$no_coords) " · no coordinates" else ""))
  }
  H <- c(H, "</div>")

  H <- c(H, chip_group("Sets", "sets", lapply(sets_avail, function(n)
    list(v = n, l = paste("Set", n))), S$sel$sets))
  H <- c(H, chip_group("Rotation · setter at", "rots", lapply(1:6, function(n)
    list(v = paste0("R", n), l = paste0("R", n))), S$sel$rots))

  G <- focus_plays(S$matches, S$sel, S$focus, ignore_sys = TRUE)
  seen <- vapply(c("5-1", "6-2", "?"), function(k)
    length(unique(paste(G$match_id, G$set_no, G$rally_no)[!is.na(G$sys) & G$sys == k])), 0)
  H <- c(H, sprintf('<div class="grp"><div class="lbl">System <button data-clr="sys">all</button></div><div class="chips">%s</div>%s</div>',
    paste0(vapply(c("5-1", "6-2", "?"), function(k)
      sprintf('<button class="chip wide %s" data-set="sys" data-v="%s">%s<span class="n">%s r</span></button>',
              if (k %in% S$o$sys) "on" else "", k, SYS_LABEL[[k]], seen[[k]]), ""),
      collapse = ""),
    tiny("Per rally. Read from R4-R6: setter in the slot = 5-1, opposite in the slot with a setter across from her = 6-2. R1-R3 take the label of the surrounding R4-R6 stretch.")))

  H <- c(H, chip_group("Attack phase", "phases",
    lapply(names(PHASE), function(k) list(v = k, l = PHASE[[k]], wide = TRUE)), S$o$phases))

  # Passers, so "what happened off her pass" is one click
  pass_counts <- list()
  for (m in S$matches) {
    if (length(S$sel$matches) && !(m$match_id %in% S$sel$matches)) next
    side <- S$focus[[m$match_id]]
    if (is.null(side) || is.na(side)) next
    p <- m$plays
    keep <- p$side == side & p$skill == "A" & !is.na(p$passer) & p$reportable
    if (length(S$sel$sets)) keep <- keep & p$set_no %in% S$sel$sets
    if (!any(keep)) next
    for (n in unique(p$passer[keep])) {
      nm <- m$rosters[[side]]$last[match(n, m$rosters[[side]]$num)]
      k <- paste(n, if (is.na(nm)) paste0("#", n) else nm, sep = "|")
      pass_counts[[k]] <- (pass_counts[[k]] %||% 0L) + sum(keep & p$passer == n)
    }
  }
  if (length(pass_counts)) {
    ord <- order(-unlist(pass_counts))
    H <- c(H, '<div class="grp"><div class="lbl">Passed by <button data-clr="passers">all</button></div><div class="chips">')
    for (k in names(pass_counts)[ord]) {
      parts <- strsplit(k, "|", fixed = TRUE)[[1]]
      H <- c(H, sprintf('<button class="chip wide %s" data-set="passers" data-v="%s">%s %s<span class="n">%s atk</span></button>',
                        if (as.integer(parts[1]) %in% S$sel$passers) "on" else "",
                        parts[1], parts[1], esc(parts[2]), pass_counts[[k]]))
    }
    H <- c(H, "</div>",
           tiny("Shows only attacks off that player's serve-receive pass."), "</div>")
  }

  H <- c(H, '<div class="grp"><div class="lbl">Players <button data-clr="players">all</button></div><div class="chips">')
  for (i in seq_len(nrow(roster))) {
    r <- roster[i, ]
    H <- c(H, sprintf('<button class="chip wide %s" data-set="players" data-v="%s">%s %s<span class="n">%sa %ss %se</span></button>',
                      if (r$num %in% S$sel$players) "on" else "", r$num, r$num,
                      esc(r$name), r$atk, r$srv, r$set))
  }
  H <- c(H, "</div></div>")

  H <- c(H, sprintf('<div class="grp"><div class="lbl">Filtered</div><div class="tiny">%s attacks · %s serves · %s sets · %s receptions</div></div>',
                    nrow(attacks_of(F, S$o$phases)), sum(F$skill == "S"),
                    sum(F$skill == "E"), sum(F$skill == "R")))

  paste0(H, collapse = "")
}

chip_group <- function(label, key, items, sel) {
  body <- paste0(vapply(items, function(it)
    sprintf('<button class="chip %s %s" data-set="%s" data-v="%s">%s</button>',
            if (isTRUE(it$wide)) "wide" else "",
            if (it$v %in% sel) "on" else "", key, it$v, esc(it$l)), ""),
    collapse = "")
  sprintf('<div class="grp"><div class="lbl">%s <button data-clr="%s">all</button></div><div class="chips">%s</div></div>',
          label, key, body)
}

shinyApp(ui, server)
