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
tbl_generic <- function(head, rows) {
  if (!length(rows))
    return('<div class="empty">Nothing to show here yet.</div>')
  th <- paste0(sprintf("<th>%s</th>", head), collapse = "")
  body <- vapply(rows, function(r) {
    cells <- if (is.list(r) && !is.null(r$cells)) r$cells else r
    tot <- is.list(r) && isTRUE(r$tot)
    cells <- vapply(cells, function(c0)
      if (length(c0) != 1 || is.na(c0)) "" else as.character(c0), "")
    tds <- paste0(sprintf('<td class="%s">%s</td>',
                          c("", rep("num", max(0, length(cells) - 1))), cells),
                  collapse = "")
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

tiny <- function(...) sprintf('<div class="tiny" style="margin-top:6px">%s</div>', paste0(..., collapse = ""))

note <- function(...) sprintf('<div class="note">%s</div>', paste0(..., collapse = ""))

thin <- function(x) sprintf('<span class="thin">%s</span>', x)

#' The KPI strip at the top of a tab. `items` is a list of
#' list(k = label, v = value, s = optional sub-label).
kpis <- function(items) {
  body <- vapply(items, function(i)
    sprintf('<div class="kpi"><div class="k">%s</div><div class="v">%s%s</div></div>',
            i$k, i$v, if (!is.null(i$s)) sprintf(" <small>%s</small>", i$s) else ""), "")
  paste0('<div class="kpis">', paste(body, collapse = ""), "</div>")
}

hdr <- function(title, cap = NULL)
  sprintf('<div class="hdr"><h2>%s</h2>%s</div>', title,
          if (!is.null(cap)) sprintf('<span class="cap">%s</span>', cap) else "")

#' Two-column panel layout. `left` is the chart column, `right` the tables.
cols2 <- function(left, right, cw = 460)
  sprintf('<div class="cols" style="--cw:%spx"><div>%s</div><div>%s</div></div>',
          cw, paste0(left, collapse = ""), paste0(right, collapse = ""))

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
  groups <- groups[vapply(groups, function(o) (o$stat$ta %||% o$stat$n) >= 3, logical(1))]
  if (length(groups) < 2 || !coords_ok) return("")
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
  sprintf('<div class="card"><h3>By %s · %s shown (min 3 attempts)</h3><div class="smgrid">%s</div></div>',
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
