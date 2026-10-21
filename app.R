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

for (f in c("core.R", "net.R", "qr.R", "positions.R", "next_server.R", "parse_vsm.R", "parse_dvw.R", "model.R", "svg.R",
            "tables.R", "tab_attack.R", "tab_serve.R", "tab_set.R",
            "tab_setter.R", "tab_passing.R", "tab_defense.R", "tab_rotation.R",
            "tab_last6.R", "tab_roster.R", "tab_nextserver.R",
            "tab_plays.R"))
  source(file.path("R", f))

# A merged multi-match .vsm runs well past Shiny's 5 MB default, and a
# truncated upload presents as a parsing error rather than as a size problem.
options(shiny.maxRequestSize = 300 * 1024^2)

# ---------------------------------------------------------------------------
# Version
# ---------------------------------------------------------------------------

#' A version that moves without anyone remembering to move it
#'
#' Read from git once at startup, because a number kept by hand is a number
#' that is wrong by the second week. The commit count is the part worth
#' reading, since it only goes up; the short hash is what pins a screenshot
#' from the bench to an exact state of the code. A copy without git, or
#' without git installed, still runs and just says v1.
app_version <- local({
  git <- function(...) tryCatch(suppressWarnings(
    system2("git", c(...), stdout = TRUE, stderr = FALSE)), error = function(e) character())
  n <- git("rev-list", "--count", "HEAD")
  sha <- git("rev-parse", "--short", "HEAD")
  day <- git("log", "-1", "--format=%ad", "--date=short")
  if (!length(n) || !nzchar(n[1]))
    list(label = "v1", build = "")
  else
    list(label = sprintf("v1.%s", n[1]),
         # The corner has room for a number, not a number and a hash. The
         # hash is what pins a screenshot from the bench to a state of the
         # code, so it goes where it can be found without taking space.
         build = sprintf("%s%s", if (length(sha)) sha[1] else "",
                         if (length(day) && nzchar(day[1])) paste0(", ", day[1]) else ""))
})

# ---------------------------------------------------------------------------
# Reading one file
# ---------------------------------------------------------------------------

#' Read and finalize one match file, or return a condition
#'
#' Both formats go through here, and so does every caller: an upload, the
#' bundled sample, and the shared poller below. A file caught mid-write is
#' truncated rather than corrupt, so the caller is handed the condition and
#' decides whether to keep what it already had.
read_match_file <- function(path, name = basename(path)) {
  tryCatch({
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    txt <- sub("^\ufeff", "", txt)
    # .dvw is DataVolley's plain text; .vsm is JSON. Sniff the content too,
    # since a file renamed on the way out of VolleyMetrics is common.
    is_dvw <- grepl("[.]dvw$", name, ignore.case = TRUE) ||
      grepl("^\\s*\\[3DATAVOLLEYSCOUT\\]", txt)
    finalize_match(if (is_dvw) parse_dvw(txt, name) else parse_vsm(txt, name))
  }, error = function(e) e)
}

# ---------------------------------------------------------------------------
# The shared watch
# ---------------------------------------------------------------------------
# One file, one poller, however many people are looking at it. At a match the
# laptop codes and the bench reads it off iPads on the same router, so the
# alternative is N tablets costing N pollers and N parses of the same bytes on
# the one machine that is also running VolleyStation.
#
# So the watched file is a property of the app, not of a viewer. Anyone can
# start it or stop it and everyone sees the same match, which is what the room
# means by watching the game. Uploads stay per viewer: an iPad can open last
# week's file without taking the bench off the live one.
#
# An upload cannot be followed either way. Shiny copies it to a temp path, so
# the original can grow all it likes and the app never sees it.
#
# Ten polls a second while the file is moving. The stat() itself is free; what
# it costs is a Shiny timer tick, measured at about 13% of one core at this
# rate against 0.8% for the app sitting still. That is a fair price during a
# match and a silly one for an app left open overnight, so the poll drops to
# twice a second after ten seconds of nothing and picks straight back up on
# the next change. The re-parse is the same work either way: at full match
# length, 795 plays, 0.11s to read and parse, finalize included.
#
# A change has to hold still for one tick before it is read. VolleyStation
# writes in bursts, and at this rate a poll lands inside a burst often rather
# than rarely, which would mean parsing half-written files all match. Waiting
# one tick costs 100ms and removes the whole class of problem.
WATCH_POLL  <- 100
WATCH_SLOW  <- 500
WATCH_QUIET <- 100L                  # ticks of no change before backing off

# `gen` counts successful parses. Sessions watch it rather than `match`,
# because a data.table that compares equal to the last one would not wake them.
LIVE <- reactiveValues(path = "", name = "", at = "", match = NULL, gen = 0L)

# ---------------------------------------------------------------------------
# What belongs to everyone, and what belongs to a device
# ---------------------------------------------------------------------------
# Two kinds of setting, and putting one in the wrong place is a bug either
# way. Which tabs a coach wants is theirs: it lives in their browser, and two
# iPads watching the same match can show different things. Who plays where is
# not theirs. A correction made on the laptop is a correction to the match,
# so it has to reach every screen already looking at it rather than waiting
# for each one to be reloaded.
#
# So the roster sits here, beside the watch, and sessions follow `gen` the
# same way. The file on disk is what survives a restart; this is what carries
# a change across the room.
ROSTER <- reactiveValues(ov = read_positions(), gen = 0L)

local({
  w_sig <- ""        # the file as it was when last parsed
  w_seen <- ""       # the file as it looked on the previous tick
  w_idle <- 0L

  # domain = NULL: this observer belongs to the app, not to whoever happened
  # to press Watch, so it keeps running after that tab closes.
  observe({
    path <- LIVE$path
    if (!nzchar(path)) { w_sig <<- ""; w_seen <<- ""; w_idle <<- 0L; return() }
    sig <- file_sig(path)                       # "" if gone or briefly locked
    changed <- nzchar(sig) && !identical(sig, w_sig)
    w_idle <<- if (changed) 0L else w_idle + 1L
    # Scheduled after the stat, not before it, so the tick that confirms a
    # change is always a fast one. Otherwise the first touch after a timeout
    # would be read at the slow cadence it arrived on.
    invalidateLater(if (w_idle > WATCH_QUIET) WATCH_SLOW else WATCH_POLL,
                    session = NULL)
    if (!changed) return()
    # Seen once but not twice: a write is in progress. Come back in a tick.
    if (!identical(sig, w_seen)) { w_seen <<- sig; return() }
    m <- read_match_file(path, isolate(LIVE$name))
    # The signature only moves on a parse that landed. A read caught mid-write
    # otherwise consumes the change and the new rallies wait for the next one.
    if (inherits(m, "condition")) return()
    w_sig <<- sig
    LIVE$match <- m
    LIVE$at <- format(Sys.time(), "%H:%M:%S")
    LIVE$gen <- LIVE$gen + 1L
  }, domain = NULL)
})

# Tabs that describe one team's play, and so can be shown for both at once.
# Rotations and Defense already compare the two by construction, and Plays is
# a wide table where a second copy would only cost width.
PAIRED_TABS <- c("attack", "serve", "setting", "setter", "passing", "last6")

TABS <- list(c("attack", "Attacking"), c("serve", "Serve direction"),
             c("setting", "Set origin"), c("setter", "Setter"),
             c("passing", "Pass quality"), c("defense", "Defense"),
             c("rotation", "Rotations"), c("last6", "Last 6"),
             c("nextserver", "Next server"),
             c("plays", "Plays &amp; export"), c("roster", "Roster"))

DEFAULT_OPTS <- list(
  aView = "arrows", aBins = 3, aColor = "volume", aIncErr = TRUE, tend = TRUE,
  sView = "arrows", sBins = 3, srvType = "all", sPass = FALSE,
  eView = "points", eBins = 3, setSetter = "", setZones = character(),
  setterWho = "main", setterEx = character(), lastN = 6,
  mono = FALSE, phases = character(), sys = character(), showshare = FALSE,
  pair = TRUE, pinServer = ""
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
    # The brand doubles as the way back. Loading one file and then wanting a
    # different one is the common case, and the top left corner is where
    # everyone presses to go home.
    tags$button(class = "brand", `data-home` = "1", type = "button",
        title = "Start over",
        span(class = "mark"),
        h1(HTML("Cal Poly <span>Scout</span>")),
        span(class = "v", title = app_version$build, app_version$label)),
    div(class = "uploadwrap", style = "min-width:260px",
        uiOutput("uploader", inline = TRUE)),
    div(class = "spacer"),
    uiOutput("focus_pick", inline = TRUE),
    uiOutput("header_tools", inline = TRUE),
    # Only on a narrow screen. Everything above folds behind it, so a tablet
    # opens on numbers rather than on controls.
    tags$button(class = "chip hidechrome", `data-chrome` = "1", "Tools")
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
    o = DEFAULT_OPTS, tab = "attack", errors = character(), warnings = character(),
    live_id = ""
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

  # ---- following the shared watch -------------------------------------
  # The poller lives at app scope; a session only says which file to follow
  # and takes what comes back.

  observeEvent(input$watch_go, {
    p <- trimws(input$watch_path %||% "")
    p <- gsub('^"|"$', "", p)                     # pasted paths arrive quoted
    if (!nzchar(p)) return()
    if (!file.exists(p)) {
      S$errors <- sprintf("No file at %s", p)
      return()
    }
    # Arm the watch only once the file has parsed. A path that is not a match
    # file should say so and stay off, rather than everyone else's screen
    # being handed a file the app cannot read.
    m <- read_match_file(p)
    if (inherits(m, "condition")) {
      S$errors <- conditionMessage(m)
      return()
    }
    S$errors <- character()
    LIVE$name <- basename(p)
    LIVE$path <- p
    LIVE$match <- m
    LIVE$at <- format(Sys.time(), "%H:%M:%S")
    LIVE$gen <- LIVE$gen + 1L
  })

  observeEvent(input$watch_stop, {
    LIVE$path <- ""; LIVE$at <- ""
  })

  # Back to the opening screen, with nothing loaded. The watch stops too: it
  # is app-wide, so leaving it on would re-adopt the match on its next
  # refresh and the button would look broken. That does mean a tablet going
  # home takes the bench's feed with it, which is why it asks first.
  observeEvent(input$js_home, {
    LIVE$path <- ""; LIVE$at <- ""; LIVE$name <- ""; LIVE$match <- NULL
    S$matches <- list()
    S$focus_name <- ""; S$focus <- list()
    S$sel <- list(matches = character(), sets = integer(), players = integer(),
                  rots = character(), passers = integer())
    S$o <- DEFAULT_OPTS
    S$tab <- "attack"
    S$errors <- character(); S$warnings <- character()
  })

  # Every refresh, and once on connect, so a tablet that joins at the end of
  # the second set does not sit empty until the next touch is coded.
  observeEvent(LIVE$gen, {
    m <- LIVE$match
    if (is.null(m)) return()
    S$matches[[m$match_id]] <- m
    S$live_id <- m$match_id
    settle(keep_view = TRUE)
  }, ignoreInit = FALSE)

  # Stopping the watch does not unload the file, but it does stop it being
  # tonight's match, so it drops back to being one more file on the shelf.
  observeEvent(LIVE$path, {
    if (!nzchar(LIVE$path)) S$live_id <- ""
  }, ignoreInit = TRUE)

  # `keep_view` is set on a live refresh. A re-parse every few seconds must not
  # reset the focus team or clear the player chips, or the rail resets itself
  # out from under whoever is reading it mid-rally.
  # Order, label and focus whatever is now loaded. Called after an upload and
  # after every live refresh, so it has to be safe to run ten times a minute.
  settle <- function(keep_view = FALSE) {
    if (!length(S$matches)) return(invisible(FALSE))
    # Re-applied on every refresh: a live re-parse rebuilds the plays from
    # the file, which would otherwise undo every correction mid-match.
    ms <- apply_positions(S$matches, ROSTER$ov)
    ms <- ms[order(vapply(ms, function(m) paste0(m$date %||% "", "|", m$file_name), ""))]
    ms <- classify_systems(ms)
    ms <- add_season_codes(ms)
    S$matches <- ms
    idx <- team_index(ms)
    # The default is the team appearing in the most files, ties breaking
    # alphabetically. That is right until a scouting library is loaded:
    # three films of the opponent against one match of ours and the opponent
    # is in all four, so the app would decide we are Grand Canyon. A
    # remembered choice therefore wins over the count.
    if (!nzchar(S$focus_name) || !(S$focus_name %in% idx$name)) {
      saved <- read_our_team()
      S$focus_name <- if (nzchar(saved) && saved %in% idx$name) saved else idx$name[1]
    }
    S$focus <- focus_sides(ms, S$focus_name)
    if (!keep_view) S$sel$players <- integer()
    invisible(TRUE)
  }

  ingest <- function(files, sample = FALSE, keep_view = FALSE) {
    errs <- character(); warns <- character(); got <- 0L
    for (i in seq_len(nrow(files))) {
      m <- read_match_file(files$datapath[i], files$name[i])
      # A file caught mid-write is truncated, not corrupt. Keep the last good
      # parse and try again on the next poll rather than blanking the screen.
      if (inherits(m, "condition")) {
        errs <- c(errs, conditionMessage(m))
        next
      }
      warns <- c(warns, m$warnings)
      if (!nrow(m$plays))
        warns <- c(warns, sprintf("%s: loaded, no coded rallies yet.", files$name[i]))
      S$matches[[m$match_id]] <- m
      got <- got + 1L
    }
    # Mid-match, a torn read is routine and self-correcting. Painting its error
    # over a screen that still holds the last good parse would mean a banner
    # flashing every few seconds for something nobody can act on.
    if (!keep_view || !length(S$matches)) S$errors <- errs
    settle(keep_view)
    if (sample)
      warns <- c("Bundled sample loaded. fixture_coords.vsm is SYNTHETIC, its events are real but its coordinates are generated, so read it as a demonstration of the charts, never as a measurement.",
                 warns)
    S$warnings <- unique(warns)
    invisible(got > 0L)
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

  # team|jersey|position, with an empty position meaning "back to the file"
  observeEvent(input$js_pos, {
    parts <- strsplit(input$js_pos$val, "|", fixed = TRUE)[[1]]
    if (length(parts) < 2) return()
    team <- parts[1]; num <- parts[2]
    pos <- if (length(parts) >= 3) parts[3] else ""
    ov <- set_position(ROSTER$ov, team, num, pos)
    # To disk first. A correction that reached the screens but not the file
    # would come back wrong after the next restart, which is worse than one
    # that never appeared to take.
    saved <- tryCatch(write_positions(ov), error = function(e) {
      S$errors <- sprintf("Could not save positions: %s", conditionMessage(e))
      FALSE
    })
    if (!isTRUE(saved)) return()
    ROSTER$ov <- ov
    ROSTER$gen <- ROSTER$gen + 1L
  })

  # Every session re-settles when the roster changes, including the one that
  # made the change, so a correction on the laptop lands on the bench iPads
  # without anyone reloading.
  observeEvent(ROSTER$gen, {
    if (!length(S$matches)) return()
    settle(keep_view = TRUE)
  }, ignoreInit = TRUE)

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
    # Written down, so loading the opponent's films later cannot move it.
    tryCatch(write_our_team(input$focus_team), error = function(e) NULL)
    S$focus_name <- input$focus_team
    S$focus <- focus_sides(S$matches, input$focus_team)
    S$sel$players <- integer()
  })

  # ------------------------------------------------------------- reactives

  # ---- what each tab is allowed to see --------------------------------
  # Scouting files are loaded before a match so the Next server tab has a
  # history to read: where their #12 serves, which of their passers to go
  # at. Every other tab is about the match being played, and pooling last
  # month's film into tonight's hitting percentage would be wrong in the
  # quiet way, producing a number that looks like tonight and is not.
  #
  # So once a file is being followed, everything except Next server is
  # scoped to it. Picking matches by hand in the rail overrides this: an
  # explicit choice is an explicit choice.
  sel_live <- reactive({
    sel <- S$sel
    if (length(sel$matches)) return(sel)
    id <- S$live_id
    if (!nzchar(id) || !(id %in% names(S$matches))) return(sel)
    sel$matches <- id
    sel
  })

  F <- reactive(focus_plays(S$matches, sel_live(), S$focus))
  F_nosys <- reactive(focus_plays(S$matches, sel_live(), S$focus, ignore_sys = TRUE))
  coords_ok <- reactive(coords_available(S$matches, sel_live()))

  # ------------------------------------------------------------------- body

  output$body <- renderUI({
    if (!length(S$matches)) return(intro_ui(S$errors))
    div(
      if (isTRUE(S$o$showshare)) div(class = "sharebar", uiOutput("share")) else NULL,
      div(class = "wrap",
        tags$aside(class = "rail", uiOutput("rail")),
        tags$main(class = "panel", uiOutput("tabbar"), uiOutput("panel"))))
  })

  output$focus_pick <- renderUI({
    idx <- team_index(S$matches)
    if (!nrow(idx)) return(NULL)
    tagList(
      tags$label("Our team"),
      tags$select(id = "focus_team", class = "shiny-input-select",
                  onchange = "Shiny.setInputValue('focus_team', this.value)",
                  lapply(seq_len(nrow(idx)), function(i)
                    tags$option(value = idx$name[i],
                                selected = if (identical(idx$name[i], S$focus_name)) "selected",
                                sprintf("%s · %s/%s files", idx$name[i], idx$files[i],
                                        length(S$matches)))))
    )
  })

  # The address to hand a tablet, with a code to point it at. Built from the
  # port this browser connected on and the host names this machine answers
  # to, never from the browser's own address: on the laptop that reads
  # localhost, which is no use to anyone else in the gym.
  share_urls <- reactive({
    port <- session$clientData$url_port
    if (is.null(port) || !nzchar(as.character(port))) port <- "7815"
    lan_urls(port)
  })

  output$share <- renderUI({
    u <- share_urls()
    if (!length(u)) return(div(class = "share",
      div(class = "sharenote", "No network address. Check the laptop is on the router.")))
    div(class = "share",
        HTML(qr_svg(u[1], px = 150, alt = paste("QR code for", u[1]))),
        div(class = "sharetxt",
            div(class = "sharelbl", "Point an iPad at this"),
            div(class = "shareurl", u[1]),
            if (length(u) > 1)
              div(class = "sharealt", sprintf("or type %s", u[2])) else NULL))
  })

  output$header_tools <- renderUI({
    if (!length(S$matches)) return(NULL)
    tagList(
      # Outside the fold. A frozen feed is the one thing worth seeing on a
      # tablet without asking for it, and Filters is how the rail is reached
      # once the rail itself is off screen.
      # The watch is app-wide, so the badge names the file: on a tablet across
      # the gym, "Live" alone does not say live off what.
      if (nzchar(LIVE$path))
        div(class = "livebox", title = LIVE$path,
            span(class = "dot"),
            span(class = "lbl", sprintf("Live %s", LIVE$at)),
            span(class = "who", LIVE$name),
            actionButton("watch_stop", "Stop", class = "chip ghost")) else NULL,
      # The same control as the opening screen, because the order of a match
      # day is load the scouting files, then start following tonight's. Once
      # a file is loaded the opening screen is gone, and with it the only way
      # to point the watch at a different file. The two never render at the
      # same time, so they can share their ids and their observers.
      if (!nzchar(LIVE$path))
        div(class = "watchrow tight",
            textInput("watch_path", NULL, width = "260px",
                      placeholder = "path of the file being coded"),
            actionButton("watch_go", "Watch", class = "chip")) else NULL,
      tags$button(class = "chip railbtn", `data-rail` = "1", "Filters"),
      # Everything a coach does not need mid-rally folds behind Tools.
      div(class = "toolset",
        tags$button(class = "chip", `data-o` = "showshare",
                    `data-v` = if (isTRUE(S$o$showshare)) "false" else "true",
                    if (isTRUE(S$o$showshare)) "Hide code" else "Connect"),
        tags$button(class = "chip", `data-o` = "mono",
                    `data-v` = if (isTRUE(S$o$mono)) "false" else "true",
                    if (isTRUE(S$o$mono)) "Colour" else "Mono (print)"),
        # Only where there are two sides to show. On Rotations and Defense
        # both teams are already on the screen.
        if (S$tab %in% PAIRED_TABS)
          tags$button(class = "chip", `data-o` = "pair",
                      `data-v` = if (isTRUE(S$o$pair)) "false" else "true",
                      if (isTRUE(S$o$pair)) "One team" else "Both teams") else NULL,
        tags$button(class = "chip", `data-tabpick` = "1", "Tabs"),
        tags$button(class = "chip", onclick = "window.print()", "Print"),
        downloadButton("csv", "CSV", class = "chip")
      )
    )
  })

  output$tabbar <- renderUI(HTML(paste0(vapply(TABS, function(t)
    sprintf('<button class="tab %s" data-tab="%s">%s</button>',
            if (identical(S$tab, t[1])) "on" else "", t[1], t[2]), ""),
    collapse = "") |> (\(x) sprintf('<div class="tabs">%s</div>', x))()))

  output$rail <- renderUI(HTML(rail_html(S)))
  # Shiny stops rendering an output whose element is hidden, and below the
  # tablet breakpoint the rail is display:none until someone presses Filters.
  # Left to itself that means the rail is never built on a tablet, so the
  # button opens an empty panel. The cost of keeping it current is one small
  # block of HTML.
  outputOptions(output, "rail", suspendWhenHidden = FALSE)

  # One team's plays through whichever tab is showing. Called once for us and
  # once for them, so the two columns can only differ by their input.
  one_side <- function(P, o, sys_rows = NULL) {
    if (S$tab == "attack")       tab_attack(P, o, coords_ok())
    else if (S$tab == "serve")   tab_serve(P, o, coords_ok())
    else if (S$tab == "setting") tab_set(P, o, coords_ok())
    else if (S$tab == "setter")  tab_setter(P, o, coords_ok())
    else if (S$tab == "passing") tab_passing(P, o, coords_ok(), sys_rows = sys_rows)
    # the rotation chips are dropped for this tab: it compares the six
    # against each other, so filtering to one would empty it
    else if (S$tab == "last6")   tab_last6(P, o)
  }

  output$panel <- renderUI({
    o <- S$o
    body <- tryCatch({
      if (S$tab %in% PAIRED_TABS) {
        ours <- one_side(F(), o,
                         sys_rows = system_card(F_nosys(),
                                                convert_rallies(S$matches, sel_live(), S$focus)))
        if (!isTRUE(o$pair)) ours
        # The opponent's own plays, read in their own frame: their attacks
        # from their side, not mirrored into ours. Mirroring is for the
        # Defense tab, where the question is what we see coming.
        else two_up(ours, one_side(opponent_plays(S$matches, sel_live(), S$focus), o),
                    side_labels(S)$us, side_labels(S)$them)
      }
      else if (S$tab == "nextserver") {
        # The two halves of this tab come from different places on purpose.
        # Who serves next is a fact about the match being played, so it is
        # read from the live file alone, and from both sides, since it is
        # not a fact about whichever team is in focus. What that player
        # does with the serve is a tendency, so it reads everything loaded,
        # which is why the scouting files were loaded in the first place.
        ns <- next_server(all_plays(S$matches, sel_live()))
        ours <- !is.null(ns) && !is.na(ns$side) &&
          any(vapply(S$matches, function(m)
            identical(S$focus[[m$match_id]], ns$side), TRUE))
        # Every file against tonight's opponent, and no others. A scouting
        # library holds several teams, and more history is only better when
        # it is history of the same people.
        # By team name, not through the focus team. A scouting film is the
        # opponent playing somebody else, with us not in it at all, so
        # asking for "our opponent in that file" finds nothing and throws
        # away the whole library.
        opp_name <- live_opponent(LIVE$match, S$focus, S$focus_name)
        sel_hist <- if (length(S$sel$matches)) S$sel
                    else sel_with_team(S$matches, S$sel, opp_name)
        tab_nextserver(focus_plays(S$matches, sel_live(), S$focus),
                       team_plays(S$matches, sel_hist, opp_name), ns, o,
                       coords_available(S$matches, sel_hist), side_labels(S), ours,
                       n_files = if (length(sel_hist$matches)) length(sel_hist$matches)
                                 else length(S$matches),
                       live_only = nzchar(LIVE$path) && !length(S$sel$matches),
                       opponent = opp_name)
      }
      else if (S$tab == "roster")
        tab_roster(position_table(S$matches, ROSTER$ov), S$focus_name)
      else if (S$tab == "defense") {
        tab_defense(F(), opponent_plays(S$matches, sel_live(), S$focus),
                    convert_rallies(S$matches, sel_live(), S$focus), o, coords_ok())
      }
      else if (S$tab == "rotation") {
        sel_all <- sel_live(); sel_all$rots <- character()
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
      # Two slots, in the order a match day happens, because the two kinds of
      # file are treated differently and guessing which is which from a
      # single drop zone is how they end up pooled.
      div(class = "slot",
          div(class = "slotn", "1"),
          div(class = "slotbody",
              div(class = "slott", "The match being coded"),
              div(class = "slotd", "Every tab counts this file, and it alone."),
              # Watching needs a real path: an upload is a snapshot of the
              # file as it was when picked, and never changes again.
              div(class = "watchrow",
                  textInput("watch_path", NULL, width = "100%",
                            placeholder = "paste the path of the .dvw being coded"),
                  actionButton("watch_go", "Watch", class = "chip")))),
      div(class = "slot",
          div(class = "slotn", "2"),
          div(class = "slotbody",
              div(class = "slott", "Scouting films, optional"),
              div(class = "slotd", HTML("Previous matches of tonight's opponent, including ones where they played somebody else. They feed the <b>Next server</b> tab: serve charts and passer history. Nothing else counts them.")),
              div(id = "drop", tabindex = "0", role = "button",
                  `aria-label` = "Select or drag scouting films",
                  HTML('<svg class="ico" viewBox="0 0 24 24" aria-hidden="true"><path d="M12 16V4m0 0L7.5 8.5M12 4l4.5 4.5" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/><path d="M4 15v3.5A1.5 1.5 0 0 0 5.5 20h13a1.5 1.5 0 0 0 1.5-1.5V15" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>'),
                  div(class = "big", "Select or drag films"),
                  div(class = "small", HTML("<code>.vsm</code> or <code>.dvw</code> · as many as you like"))))),
      uiOutput("share"),
      # Only offered when a sample is actually present. data/ is not in the
      # repository, match files are not ours to publish, so a fresh clone has
      # none, and a link that only produces an error is worse than no link.
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

  # Only ever seen on a tablet, where the rail opens over the panel rather
  # than beside it and so needs a way out. CSS hides it on a wide screen.
  H <- c(H, '<div class="railtop"><button class="chip" data-rail="1">Done</button></div>')

  # Two kinds of file, kept apart on the screen because they are kept apart
  # everywhere else. The match being coded is what every tab counts. The
  # scouting films are the opponent playing somebody else, and they feed the
  # Next server tab alone.
  live_id <- S$live_id %||% ""
  is_live <- vapply(S$matches, function(m) identical(m$match_id, live_id), TRUE)
  groups <- list(
    list(lbl = "Tonight", which = which(is_live),
         note = "Every tab counts this file."),
    list(lbl = "Scouting films", which = which(!is_live),
         note = "Serve charts and passer history on the Next server tab. Counted nowhere else."))

  for (g in groups) {
    if (!length(g$which)) next
    H <- c(H, sprintf('<div class="grp"><div class="lbl">%s <button data-clr="matches">all</button></div><div class="meta grpnote">%s</div>',
                      g$lbl, g$note))
  for (m in S$matches[g$which]) {
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
      # A scouting film has neither of our teams in it, which is normal and
      # not a problem: it is read by the team being scouted, by name. Only
      # say something when the file is in the group that is supposed to be
      # ours and is not.
      if (has_focus || !identical(m$match_id, live_id)) ""
        else sprintf('<div class="meta" style="color:var(--blocked)">no "%s" in this file, so the match tabs have nothing to count. Switch the team above.</div>',
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
  }

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
