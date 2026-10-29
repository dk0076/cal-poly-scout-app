# ---------------------------------------------------------------------------
# The scout sheet
#
# The one page a staff carries onto the court. It is not a tab to read on a
# screen: it is laid out in millimetres for Letter paper, printed, and written
# on by hand, which is why it keeps a blank box beside every player and why
# the whole thing is black on white whatever the app's theme is.
#
# Two sheets, both taken from the HTML tool this app replaced, and both
# already drawn by this app's own chart layer rather than pasted in as
# pictures: every attacker, one strip each with their calls laid left to
# right, and every server, nine to a page with where the ball lands and where
# their passers start.
#
# Charts here are drawn flipped, net at the top and the ball travelling down
# the page, because that is the way the staff's printed sheet has always read.
# On screen the app keeps its own orientation; the flip is scoped to this file.
# ---------------------------------------------------------------------------

# Nine players is the page. Beyond that the strips are too short to carry a
# court, so the rest are named under the sheet rather than squeezed onto it.
SHEET_MAX <- 9L
# Four calls across a strip, which is what the staff's own printed sheet
# carries at most. Six fitted, in the sense that six boxes drew without
# overlapping, and the courts inside them came out too small to read a shot
# off.
SHEET_CALLS <- 4L

# The printable area, in millimetres, so a cell can be sized from the page
# rather than from a guess. 199 x 261mm less 4mm of padding on every side,
# less the 16mm header and the 3mm under it.
SHEET_INNER_H <- 261 - 8 - 16 - 3
# What is left across a strip once the name column, the strip's border, its
# gap and its padding are taken off a 191mm inner width.
SHEET_INNER_W <- 141
# A court is 1140 units wide by 1080 tall in the window these charts use, so
# a box of that shape is filled by it exactly, with nothing left over on any
# side. Anything wider leaves white margins, which is what made the courts
# look small on a page that had the room for them.
COURT_AR <- 1140 / 1080

#' Every attacker, or every server, on every team in the files loaded.
#'
#' Keyed on team and jersey, never jersey alone: numbers collide between
#' teams, and a sheet that mixes an opponent in has to keep them apart.
#' Only charted plays count, because the sheet is charts.
sheet_people <- function(P, kind) {
  if (is.null(P) || !nrow(P)) return(list())
  sk <- if (identical(kind, "servers")) "S" else "A"
  rows <- P[P$skill == sk & P$has_xy & !is.na(P$num), ]
  if (!nrow(rows)) return(list())
  by_size(crosstab(rows, paste(rows$team_name, rows$num, rows$player_short, sep = "|"),
                   if (sk == "S") serve_line else attack_line),
          if (sk == "S") "n" else "ta")
}

#' Sets each player was on court for, counted from every touch she made.
#'
#' Per set, not per match: a hitter's attempts per set is the number a staff
#' reads, and dividing by matches flatters anyone who played three sets of a
#' five set match.
sheet_sets <- function(P) {
  if (is.null(P) || !nrow(P) || !nrow(P[!is.na(P$num), ])) return(setNames(integer(0), character(0)))
  q <- P[!is.na(P$num), ]
  key <- paste(q$team_name, q$num, q$player_short, sep = "|")
  st <- paste(q$match_id, q$set_no, sep = "|")
  vapply(split(st, key), function(x) length(unique(x)), 0L)
}

#' The printed header: the mark, the sheet's name, and two fields to fill in.
#'
#' Typed rather than filled in by the app, because the opponent a sheet is
#' about is not always the team whose numbers are on it, and a field the app
#' wrote is a field somebody has to check. What is typed is kept by the
#' browser, like the tab picker: it has to survive every redraw of the panel,
#' and it is nobody else's business.
sheet_header <- function(what) {
  sprintf('<div class="rowHeader">
    <div class="title-box"><img class="cp" src="cp-mark.png" alt=""><span>CAL POLY SCOUT</span></div>
    <div class="fill-box">
      <div class="fill-row"><span>%s:</span><input class="f" type="text" data-sheetf="who" aria-label="%s"></div>
      <div class="fill-row"><span>Date:</span><input class="f" type="text" data-sheetf="date" aria-label="Date"></div>
    </div>
  </div>', esc(what), esc(what))
}

#' Who is on the sheet: the ones picked by hand, in the order they were
#' picked, or the busiest nine when nobody has picked anybody.
#'
#' A sheet is a page about the players a staff has decided to talk about, and
#' the app's idea of the busiest nine is a starting point rather than the
#' answer. Picks are kept per key, so switching between the two sheets or
#' between teams leaves each one's choice alone.
sheet_pick <- function(people, picked) {
  if (!length(people)) return(people)
  keys <- vapply(people, function(o) o$key, "")
  want <- intersect(as.character(picked %||% character()), keys)
  if (length(want)) people[match(want, keys)]
  else people[seq_len(min(SHEET_MAX, length(people)))]
}

#' The picker: every player on this team who charted anything, biggest first.
sheet_pick_bar <- function(people, picked, kind) {
  if (!length(people)) return("")
  on <- intersect(as.character(picked %||% character()),
                  vapply(people, function(o) o$key, ""))
  chips <- paste0(vapply(people, function(o) {
    parts <- strsplit(o$key, "|", fixed = TRUE)[[1]]
    n <- if (identical(kind, "servers")) o$stat$n else o$stat$ta
    sprintf('<button class="chip %s" data-o="sheetPick" data-v="%s">%s <span class="thin">%s</span><span class="n">%d</span></button>',
            if (o$key %in% on) "on" else "", esc(o$key), esc(parts[2]), esc(parts[3]), n)
  }, ""), collapse = "")
  sprintf('<div class="viewbar pickbar"><button class="chip %s" data-o="sheetPick" data-v="">%s</button>%s<span class="tiny pickn">%s</span></div>',
          if (!length(on)) "on" else "", "Busiest nine", chips,
          if (length(on) > SHEET_MAX)
            sprintf("%d picked, and a page holds %d", length(on), SHEET_MAX)
          else sprintf("%d of %d on the sheet",
                       if (length(on)) min(length(on), SHEET_MAX)
                       else min(SHEET_MAX, length(people)), length(people)))
}

#' The blank box beside a player, for a photo or a note in pen.
sheet_box <- function() '<div class="atk-box"></div>'

#' One attacker per strip, her calls left to right, biggest first.
attacker_strips <- function(P, team, coords_ok, mono = FALSE, picked = NULL) {
  all_of_them <- sheet_team_people(P, "attackers", team)
  if (!length(all_of_them))
    return(list(html = '<div class="atk-empty">Nobody on this team has a charted attack in the files loaded.</div>',
                shown = 0L, total = 0L, pool = all_of_them))
  total <- length(all_of_them)
  people <- sheet_pick(all_of_them, picked)
  people <- people[seq_len(min(SHEET_MAX, length(people)))]
  sp <- sheet_sets(P)

  # A cell is as wide as its court is at the height the page gives it, not as
  # wide as the room divides into. Sizing by width was the mistake: it made a
  # box far wider than the court it holds, so every chart sat in the middle of
  # its own white margin and looked half the size it could be. Here the height
  # comes first, the width follows the court's shape, and whatever is left
  # over stays at the right hand end of the strip, as it does on the staff's
  # own sheet.
  n_rows <- length(people)
  row_h <- (SHEET_INNER_H - (n_rows - 1) * 1.6) / n_rows
  # The strip's padding, the call name, the line under it, and the ruled box
  # around the chart: what is left of the row is the court's.
  box_h <- max(6, row_h - 2 - 3.4 - 3.1 - 1.2)
  widest <- max(1L, max(vapply(people, function(o)
    min(SHEET_CALLS, length(crosstab(o$rows, o$rows$season_call))), 0L)))
  cell_w <- min(box_h * COURT_AR, (SHEET_INNER_W - (widest - 1) * 2) / widest)
  cell_w <- floor(cell_w * 10) / 10

  rows <- vapply(seq_along(people), function(pi) {
    o <- people[[pi]]
    parts <- strsplit(o$key, "|", fixed = TRUE)[[1]]
    per_set <- sp[o$key]
    # Whatever she does not run often enough to make the strip is left off
    # it rather than counted at the end: the page is the four calls a staff
    # will talk about, and a "+3 more" in the margin is not one of them.
    calls <- by_size(crosstab(o$rows, o$rows$season_call))
    calls <- calls[seq_len(min(SHEET_CALLS, length(calls)))]
    cells <- vapply(seq_along(calls), function(gi) {
      g <- calls[[gi]]
      cid <- sprintf("shA%d_%d", pi, gi)
      svg <- court_svg(cid, x_min = -120, x_max = 1020, y_min = 820, y_max = 1900,
                       halves = "opp", scale = 2.6, mono = mono, coords_ok = coords_ok,
                       marks = arrow_layer(g$rows, cid, 2.6, mono),
                       alt = sprintf("%s, %s", parts[3],
                                     call_label(g$key, g$rows$call_name[1])))
      # The call's own three numbers, the way the staff's sheet reads them:
      # how many she took on it, how many were kills, and what she hit.
      sprintf('<div class="atk-cell" style="flex:0 0 %smm;max-width:%smm">
        <div class="ct">%s</div><div class="cs">%d · %sk · %s</div>
        <div class="cv">%s</div></div>',
              cell_w, cell_w, esc(call_label(g$key, g$rows$call_name[1])),
              g$stat$ta, pct(g$stat$kpct), eff(g$stat$eff), svg)
    }, "")
    sprintf('<div class="atk-row"><div class="atk-name">%s<div class="atk-txt">
        <b>#%s %s</b><span>%s att%s</span><span>%s kill · %s hit</span></div></div>
      <div class="atk-charts">%s%s</div></div>',
            sheet_box(), esc(parts[2]), esc(parts[3]), o$stat$ta,
            if (!is.na(per_set) && per_set > 0)
              sprintf(" · %.1f/set", o$stat$ta / per_set) else "",
            pct(o$stat$kpct), eff(o$stat$eff),
            paste(cells, collapse = ""), "")
  }, "")
  list(html = sprintf('<div class="atk-flex">%s</div>', paste(rows, collapse = "")),
       shown = length(people), total = total, pool = all_of_them)
}

#' Nine servers to a page: where she serves, and where her passers stand.
server_tiles <- function(P, team, coords_ok, mono = FALSE, picked = NULL) {
  all_of_them <- sheet_team_people(P, "servers", team)
  if (!length(all_of_them))
    return(list(html = '<div class="atk-empty">Nobody on this team has a charted serve in the files loaded.</div>',
                shown = 0L, total = 0L, pool = all_of_them))
  total <- length(all_of_them)
  people <- sheet_pick(all_of_them, picked)
  people <- people[seq_len(min(SHEET_MAX, length(people)))]
  sp <- sheet_sets(P)

  tiles <- vapply(seq_along(people), function(pi) {
    o <- people[[pi]]
    parts <- strsplit(o$key, "|", fixed = TRUE)[[1]]
    cid <- sprintf("shS%d", pi)
    ps <- passer_start(o$rows)
    svg <- court_svg(cid, x_min = -120, x_max = 1020, y_min = 820, y_max = 1900,
                     halves = "opp", scale = 2.6, mono = mono, coords_ok = coords_ok,
                     marks = paste0(arrow_layer(o$rows, cid, 2.6, mono),
                                    passer_start_layer(ps, 2.6, mono)),
                     alt = sprintf("where %s serves", parts[3]))
    pass <- if (length(ps$spots))
      paste0(vapply(seq_along(ps$spots), function(i) {
        s <- ps$spots[[i]]
        sprintf('<span class="%s">%s %s %s m <i>%s</i></span>',
                if (i == 1L) "main" else "", if (i == 1L) "&#9679;" else "&#9675;",
                LANE_NAME[[s$k]], m1(s$y - COURT$NET), pct(s$share))
      }, ""), collapse = "")
    else sprintf('<span class="mut">%s</span>', esc(passer_start_text(ps)))
    per_set <- sp[o$key]
    sprintf('<div class="srv-tile"><div class="srv-head">%s<div class="atk-txt">
        <b>#%s %s</b><span>%s srv%s</span><span>%s ace · %s eff · %s err</span></div></div>
      <div class="srv-court">%s</div>
      <div class="srv-pass"><b>Pass from</b>%s</div></div>',
            sheet_box(), esc(parts[2]), esc(parts[3]), o$stat$n,
            if (!is.na(per_set) && per_set > 0)
              sprintf(" · %.1f/set", o$stat$n / per_set) else "",
            o$stat$ace, eff(o$stat$eff), o$stat$err, svg, pass)
  }, "")
  # Three across, and only as many rows as there are servers: a fixed three
  # by three leaves a third of the page blank when a team has six, and a
  # printed page has no spare thirds.
  list(html = sprintf('<div class="srv-grid" style="grid-template-rows:repeat(%d,minmax(0,1fr))">%s</div>',
                      ceiling(length(people) / 3), paste(tiles, collapse = "")),
       shown = length(people), total = total, pool = all_of_them)
}

#' One team's attackers, or servers, biggest first.
sheet_team_people <- function(P, kind, team) {
  people <- sheet_people(P, kind)
  people[vapply(people, function(o)
    identical(strsplit(o$key, "|", fixed = TRUE)[[1]][1], team), TRUE)]
}

#' Which team the sheet is about. Chips rather than a dropdown, because this
#' is read on a tablet and a dropdown there is a modal.
sheet_team_bar <- function(P, team) {
  if (is.null(P) || !nrow(P)) return("")
  tab <- sort(table(P$team_name[!is.na(P$team_name) & nzchar(P$team_name)]),
              decreasing = TRUE)
  if (length(tab) < 2L) return("")
  paste0(vapply(names(tab), function(t)
    vbtn("sheetTeam", t, esc(t), team), ""), collapse = "")
}

#' The printable page, with its controls above it.
#'
#' `P` is every play in the files loaded, both teams: a scout sheet is usually
#' about the other side, and the focus team is only the default.
tab_sheet <- function(P, o, coords_ok, focus_name = "", opponent = "") {
  kind <- if (identical(o$sheet %||% "", "servers")) "servers" else "attackers"
  teams <- if (is.null(P) || !nrow(P)) character()
           else unique(P$team_name[!is.na(P$team_name) & nzchar(P$team_name)])
  # The sheet a staff prints is nearly always about the other side, so that is
  # the default when the files say who they are.
  want <- as.character(o$sheetTeam %||% "")
  team <- if (nzchar(want) && want %in% teams) want
          else if (nzchar(opponent) && opponent %in% teams) opponent
          else if (nzchar(focus_name) && focus_name %in% teams) focus_name
          else if (length(teams)) teams[1] else ""

  H <- hdr("Scout sheet",
           sprintf("%s · %s", if (nzchar(team)) esc(team) else "no team",
                   if (kind == "servers") "every server" else "every attacker"))
  H <- c(H, '<div class="tiny nssrc">One Letter page, drawn from the files loaded and printed to write on. The blank box beside each player is for a photo or a note in pen.</div>')

  bar <- viewbar(
    vbtn("sheet", "attackers", "All attackers", kind),
    vbtn("sheet", "servers", "All servers", kind),
    sheet_team_bar(P, team),
    '<button class="chip print" data-print="1">Print</button>')

  if (!coords_ok)
    return(paste0(c(H, bar, caveat("These files carry no ball coordinates, so there is nothing to draw. The sheet is charts; the numbers behind it are on the Attacking and Serving tabs.")),
                  collapse = ""))

  picked <- as.character(o$sheetPick %||% character())
  body <- with_flip(
    if (kind == "servers") server_tiles(P, team, coords_ok, isTRUE(o$mono), picked)
    else attacker_strips(P, team, coords_ok, isTRUE(o$mono), picked))

  paste0(c(H, bar, sheet_pick_bar(body$pool, picked, kind),
           sprintf('<div class="sheetwrap"><div class="sheet %s">%s<div class="rowCourts">%s</div></div></div>',
                   kind, sheet_header("Player"),
                   body$html),
           ""),
         collapse = "")
}
