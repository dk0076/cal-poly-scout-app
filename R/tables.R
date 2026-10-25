# ---------------------------------------------------------------------------
# The HTML furniture: KPI strips, cards, and the one table builder every tab
# uses. Hand-rolled rather than handed to a datatable widget, because these
# tables are read on a printed sheet as often as on screen -- they need to
# repeat their headers across a page break, keep numerals tabular, and carry a
# totals row and an explanatory note under the body. A sortable JS grid gives
# none of that and costs a dependency.
# ---------------------------------------------------------------------------

#' One table. `head` is a character vector of column labels; `rows` is a list
#' whose elements are either a character vector of cells or
#' list(cells = <character>, tot = TRUE) for a totals row.
#'
#' `cls` optionally names a class per column, carried on the header cell and
#' every cell under it. "coord" marks a column that only exists because
#' somebody charted the ball: Simplify takes those out, since on match day
#' nobody is charting and they are a row of blanks.
tbl_generic <- function(head, rows, cls = NULL) {
  if (!length(rows))
    return('<div class="empty">Nothing to show here yet.</div>')
  cl <- function(i, base) {
    extra <- if (!is.null(cls) && i <= length(cls) && nzchar(cls[i]))
      paste0(" c-", cls[i]) else ""
    paste0(base, extra)
  }
  th <- paste0(vapply(seq_along(head), function(i)
    sprintf('<th class="%s">%s</th>', cl(i, ""), head[i]), ""), collapse = "")
  body <- vapply(rows, function(r) {
    cells <- if (is.list(r) && !is.null(r$cells)) r$cells else r
    tot <- is.list(r) && isTRUE(r$tot)
    cells <- vapply(cells, function(c0)
      if (length(c0) != 1 || is.na(c0)) "" else as.character(c0), "")
    tds <- paste0(vapply(seq_along(cells), function(i)
      sprintf('<td class="%s">%s</td>',
              cl(i, if (i == 1L) "" else "num"), cells[i]), ""), collapse = "")
    sprintf('<tr class="%s">%s</tr>', if (tot) "tot" else "", tds)
  }, "")
  paste0('<div class="scroll"><table><thead><tr>', th, "</tr></thead><tbody>",
         paste(body, collapse = ""), "</tbody></table></div>")
}

tot_row <- function(cells) list(cells = cells, tot = TRUE)

eff_td <- function(v) sprintf('<span class="%s">%s</span>', eff_cls(v), eff(v))

card <- function(title, body)
  sprintf('<div class="card"><h3>%s</h3>%s</div>', title, paste0(body, collapse = ""))

#' A card with no heading -- for a chart that titles itself.
plain_card <- function(body) sprintf('<div class="card">%s</div>', paste0(body, collapse = ""))

# The footnotes under a table and the paragraph under a tab. Both say true
# and useful things, and both are in the way: a coach between rallies is
# reading numbers, and five lines of prose under each card is five lines
# they scroll past to reach the next one.
#
# So they fold. Not deleted, because the first time someone asks what KO%
# counts, or why a lane is blank, the answer has to be on the page rather
# than in somebody's memory of a conversation. One press of More in the tab
# heading and every explanation on that screen is back.
#
# This is also what gives More something to do on the tabs where no card was
# folded: the button appears wherever a panel has anything behind it, and
# now that includes anywhere with a footnote.
tiny <- function(...)
  sprintf('<div class="tiny extra" style="margin-top:6px">%s</div>',
          paste0(..., collapse = ""))

note <- function(...)
  sprintf('<div class="note extra">%s</div>', paste0(..., collapse = ""))

#' A line that is not an explanation: a caveat attached to the number beside
#' it, which has to be read whether or not anyone pressed More. Used where
#' leaving it out would let someone read a figure as something it is not.
caveat <- function(...)
  sprintf('<div class="tiny caveat" style="margin-top:6px">%s</div>',
          paste0(..., collapse = ""))

thin <- function(x) sprintf('<span class="thin">%s</span>', x)

#' The KPI strip at the top of a tab. `items` is a list of
#' list(k = label, v = value, s = optional sub-label).
kpis <- function(items) {
  body <- vapply(items, function(i)
    sprintf('<div class="kpi"><div class="k">%s</div><div class="v">%s%s</div></div>',
            i$k, i$v, if (!is.null(i$s)) sprintf(" <small>%s</small>", i$s) else ""), "")
  paste0('<div class="kpis">', paste(body, collapse = ""), "</div>")
}

#' Every panel's heading, and the one control that acts on that panel.
#'
#' The More button expands the secondary breakdowns -- what the strip used
#' to call Detail, from a row where it sat between a printer setting and a
#' tab picker and explained itself to nobody. Here it is beside the title it
#' expands, so its subject is the thing above it rather than a word.
#'
#' It is written into every panel, which means two of it in the side-by-side
#' view. That is deliberate: the toggle is page-wide and the browser keeps
#' every copy's label in step, so whichever panel a coach is reading has the
#' control at hand instead of one of the two having it. The label is left
#' empty here and written by the browser, because the state lives in that
#' device and the server never learns it.
hdr <- function(title, cap = NULL)
  sprintf(paste0('<div class="hdr"><h2>%s</h2>%s',
                 '<button type="button" class="chip detailbtn" ',
                 'data-detail="1" aria-expanded="false"></button></div>'),
          title,
          if (!is.null(cap)) sprintf('<span class="cap">%s</span>', cap) else "")

#' Two-column panel layout. `left` is the chart column, `right` the tables.
#' A fixed rail beside a flexible panel.
#'
#' `left_class` marks the rail for the stylesheet. "chartcol" says the rail is
#' a chart: side by side it stays on the left, and wherever the two columns
#' stack, which is every tablet and both panels of the side-by-side view, it
#' drops below the numbers. A chart is worth a scroll; a table of numbers a
#' coach is reading between rallies is not worth scrolling past a court.
cols2 <- function(left, right, cw = 460, left_class = "")
  sprintf('<div class="cols" style="--cw:%spx"><div class="%s">%s</div><div>%s</div></div>',
          cw, left_class, paste0(left, collapse = ""), paste0(right, collapse = ""))

#' The switch for a tab's charts, and whether they are on
#'
#' On by default, because the charts are the reason most of this exists. Off
#' is for the bench: a landing grid of a whole team's serves takes the screen
#' and tells a coach between rallies less than one row of the table under it.
#' Per viewer, like the other view switches, so one iPad turning it off does
#' not take the charts off everybody else's.
charts_on <- function(o) !identical(o$charts, FALSE)
charts_toggle <- function(o) viewbar(vtoggle("charts", "Shot chart", charts_on(o)))

#' A card, dropped when it has nothing in it to read
#'
#' Most of this app's cards are built on coordinates: where the ball landed,
#' which third, how deep, how far off the net. A file being coded live has
#' none of that, because coding with lines during a match is not realistic,
#' so those cards come out as a frame of zeros. Half a dozen of them stack up
#' and push the numbers a coach does need off the screen, between rallies,
#' which is the one moment the screen has to be read in a glance.
#'
#' So they are not drawn at all rather than drawn empty. Nothing is lost: a
#' card with no rows was telling nobody anything, and it comes back by itself
#' the moment the data does.
card_if <- function(cond, label, body, ...) {
  if (!isTRUE(cond)) return("")
  card(label, body, ...)
}

#' A card that is interesting rather than necessary
#'
#' There is a difference between a number a coach acts on and a number worth
#' knowing, and the second kind was crowding out the first. These are folded
#' away behind the Detail button rather than deleted: nothing is lost, and a
#' staff that wants the breakdown is one tap from it.
#'
#' Folded in the browser, not here, so the choice belongs to the device the
#' way the tab picker does. One iPad showing the detail does not put it on
#' everybody else's.
card_extra <- function(cond, label, body, ...) {
  inner <- card_if(cond, label, body, ...)
  if (!nzchar(inner)) return("")
  sprintf('<div class="extra">%s</div>', inner)
}

#' Fold a piece of a page away behind Detail, but only when asked
#'
#' The same mechanism as card_extra, for the cards that are worth reading
#' before a match and in the way during one. The condition is the context,
#' not the content.
fold <- function(cond, html) {
  html <- paste0(html, collapse = "")
  if (!isTRUE(cond) || !nzchar(html)) return(html)
  sprintf('<div class="extra">%s</div>', html)
}

#' TRUE when at least one of these plays carries a position on the court
has_lines <- function(rows, col = "has_xy") {
  if (is.null(rows) || !nrow(rows) || !col %in% names(rows)) return(FALSE)
  any(rows[[col]], na.rm = TRUE)
}

#' The same tab for both teams, beside each other
#'
#' A coach between rallies should not be choosing whose numbers to look at,
#' and switching loses the comparison that makes either side mean anything.
#' So the panel renders twice and the two sit side by side, the way the
#' Rotations tab already reads.
#'
#' Colour carries the separation rather than a label alone: green is ours and
#' red is theirs, the same pairing the rest of the app uses. The heading
#' stays with each column while it scrolls, because the two halves are the
#' same shape and lose their identity the moment the names leave the screen.
two_up <- function(us, them, us_label, them_label) {
  sprintf(paste0('<div class="twoup">',
                 '<section class="side us"><h2 class="sidehdr">%s</h2>%s</section>',
                 '<section class="side them"><h2 class="sidehdr">%s</h2>%s</section>',
                 '</div>'),
          esc(us_label), paste0(us, collapse = ""),
          esc(them_label), paste0(them, collapse = ""))
}

legend <- function(..., trailing = NULL) {
  items <- list(...)
  body <- vapply(items, function(i)
    sprintf('<span><i style="border-color:%s"></i>%s</span>', i[[1]], i[[2]]), "")
  paste0('<div class="legend">', paste(body, collapse = ""),
         if (!is.null(trailing)) sprintf('<span style="color:var(--ink-3)">%s</span>', trailing) else "",
         "</div>")
}

swatch_legend <- function(..., trailing = NULL) {
  items <- list(...)
  body <- vapply(items, function(i)
    sprintf('<span><span class="sw" style="background:%s"></span>%s</span>', i[[1]], i[[2]]), "")
  paste0('<div class="legend">', paste(body, collapse = ""),
         if (!is.null(trailing)) sprintf('<span style="color:var(--ink-3)">%s</span>', trailing) else "",
         "</div>")
}

empty <- function(msg) sprintf('<div class="empty">%s</div>', msg)

#' The small per-player mini courts under a chart.
small_multiples <- function(rows, kind, coords_ok, mono = FALSE) {
  groups <- by_size(crosstab(rows, paste(rows$num, rows$player_short, sep = "|"),
                             if (kind == "serve") serve_line else attack_line),
                    if (kind == "serve") "n" else "ta")
  # Everyone who has charted anything, down to a single attempt. The old
  # floor of three was there to keep the strip from filling with one-swing
  # cards, and what it actually did was empty the strip early in a match,
  # when nobody has three yet: a tab with the chart switched on and nothing
  # on it. One arrow is a small card and still an answer; a coach can see
  # how many attempts it rests on, since the count is written on it.
  if (!length(groups) || !coords_ok) return("")
  body <- vapply(seq_along(groups), function(i) {
    o <- groups[[i]]
    parts <- strsplit(o$key, "|", fixed = TRUE)[[1]]
    s <- o$stat
    svg <- court_svg(paste0("sm", i), x_min = -120, x_max = 1020,
                     y_min = if (kind == "serve") -230 else 300, y_max = 1930,
                     marks = arrow_layer(o$rows, paste0("sm", i), 2.1, mono),
                     scale = 2.1, alt = paste(parts[2], "trajectories"),
                     mono = mono, coords_ok = coords_ok)
    line <- if (kind == "serve")
      sprintf("%s srv · %s eff · %s ace · %s err", s$n, eff(s$eff), s$ace, s$err)
    else
      sprintf("%s att · %s k · %s", s$ta, pct(s$kpct), eff(s$eff))
    sprintf('<div class="sm"><div class="nm"><span>%s</span><b>#%s</b></div><div class="ln">%s</div>%s</div>',
            esc(parts[2]), parts[1], line, svg)
  }, "")
  sprintf('<div class="card"><h3>By %s · %s shown</h3><div class="smgrid">%s</div></div>',
          if (kind == "serve") "server" else "hitter", length(groups),
          paste(body, collapse = ""))
}

#' Per-setter contact maps, own half, coloured by target attacker.
setter_multiples <- function(xy, coords_ok) {
  groups <- by_size(crosstab(xy, paste(xy$num, xy$player_short, sep = "|"), set_line), "n")
  groups <- groups[vapply(groups, function(o) o$stat$n >= 5, logical(1))]
  if (length(groups) < 2 || !coords_ok) return("")
  body <- vapply(seq_along(groups), function(i) {
    o <- groups[[i]]
    parts <- strsplit(o$key, "|", fixed = TRUE)[[1]]
    svg <- court_svg(paste0("st", i), x_min = -220, x_max = 1120,
                     y_min = -160, y_max = 985, halves = "own",
                     marks = set_target_layer(o$rows, r = 17), scale = 2.1,
                     alt = paste(parts[2], "set contacts"), coords_ok = coords_ok)
    sprintf('<div class="sm"><div class="nm"><span>%s</span><b>#%s</b></div><div class="ln">%s sets · %s m off net · atk %s</div>%s</div>',
            esc(parts[2]), parts[1], o$stat$n,
            m1(med(COURT$NET - o$rows$y0)), eff(o$stat$eff), svg)
  }, "")
  sprintf('<div class="card"><h3>By setter · contact points (min 5 sets)</h3><div class="smgrid">%s</div></div>',
          paste(body, collapse = ""))
}

#' The per-tab view switches. Rendered as plain buttons that post their value
#' back through the one shared input handler in www/app.js, so a tab can add a
#' control without the server growing an observer for it.
vbtn <- function(key, val, label, cur) {
  sprintf('<button class="chip %s" data-o="%s" data-v="%s">%s</button>',
          if (identical(cur, val)) "on" else "", key, val, label)
}
vtoggle <- function(key, label, cur) {
  sprintf('<button class="chip %s" data-o="%s" data-v="%s">%s</button>',
          if (isTRUE(cur)) "on" else "", key, if (isTRUE(cur)) "false" else "true", label)
}
viewbar <- function(...) {
  body <- paste0(unlist(list(...)), collapse = "")
  if (!nzchar(body)) "" else sprintf('<div class="viewbar">%s</div>', body)
}
