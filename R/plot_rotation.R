# ---------------------------------------------------------------------------
# THE ROTATION LADDER
#
# A mid-match picture, not a report. Between rallies a coach has about twenty
# seconds and one question: is this rotation the problem, or is it six rallies
# of noise. The six tables below this answer it in numbers a row at a time,
# which is the wrong shape for that question, because comparing six rotations
# means reading six rows and holding them in your head.
#
# So: one bar per rotation, side-out percentage tonight, with two things most
# charts leave out.
#
# The dashed line is the team's own side-out across every rotation. A rotation
# is not bad at 55%; it is bad at 55% when the other five are at 70%. League
# averages are somebody else's team, and the comparison that matters is the
# one on the floor right now.
#
# The whisker is the 95% Wilson interval, which is what stops this being a
# chart that invents problems. Six rallies in R3 and a 33% side-out draws a
# bar that looks alarming and a whisker that runs from 10% to 70%, saying
# plainly that nothing is known yet. A rotation worth a timeout is one whose
# whole interval sits under the team line. That rule is drawn, not written in
# a footnote: those rotations get the marker.
#
# Nothing here needs a coordinate, which is the point. A live file carries
# rally winners and rotations and nothing else, and this is built from exactly
# that.
# ---------------------------------------------------------------------------

# w leaves a lane to the right of the bars for the status word, then the
# percentage, then +/-. Drawn tighter, "on court" sat on top of the number it
# was meant to qualify.
LADDER <- list(w = 486, row = 25, top = 26, bot = 30, x0 = 58, x1 = 322,
               vx = 430, px = 480, sx = 328)

#' One rotation's row, as plain numbers
ladder_rows <- function(rally) {
  lapply(ROTS, function(k) {
    s <- sideout_line(rally[!is.na(rally$rot) & rally$rot == k, ])
    list(k = k, so = s$so, lo = s$so_ci[1], hi = s$so_ci[2],
         rec = s$rec, pm = s$plus_minus)
  })
}

#' The ladder itself
#'
#' `now` is the rotation on the floor, marked only when a file is being coded:
#' on a finished match the last rally's rotation is just where the match
#' happened to end, which is not worth a marker.
rot_ladder <- function(rally, now = NA_character_, them = FALSE, alt = "") {
  if (!nrow(rally) || !any(!is.na(rally$won))) return("")
  rows <- ladder_rows(rally)
  if (!any(vapply(rows, function(r) isTRUE(r$rec > 0), TRUE))) return("")
  avg <- sideout_line(rally)$so

  L <- LADDER
  span <- L$x1 - L$x0
  at <- function(p) L$x0 + span * p
  h <- L$top + L$row * length(ROTS) + L$bot
  ink <- if (them) "var(--them-ink)" else "var(--accent)"

  grid <- paste0(vapply(c(0, .25, .5, .75, 1), function(p)
    sprintf('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="var(--rule-2)" stroke-width="1"/>%s',
            at(p), L$top - 6, at(p), h - L$bot + 2,
            if (p %in% c(0, .5, 1))
              sprintf('<text x="%.1f" y="%d" class="lt" text-anchor="middle">%d%%</text>',
                      at(p), L$top - 11, round(100 * p)) else ""), ""),
    collapse = "")

  avg_line <- if (is.na(avg)) "" else sprintf(
    '<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="%s" stroke-width="1.5" stroke-dasharray="4 3"/><text x="%.1f" y="%d" class="lt" text-anchor="middle">team %d%%</text>',
    at(avg), L$top - 4, at(avg), h - L$bot + 4, ink,
    at(avg), h - L$bot + 16, round(100 * avg))

  bars <- paste0(vapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    y <- L$top + L$row * (i - 1)
    mid <- y + L$row / 2
    on_court <- !is.na(now) && identical(now, r$k)
    # The whole interval under the team's own line: not a bad run, a rotation
    # that is actually behind the other five.
    behind <- !is.na(r$hi) && !is.na(avg) && r$hi < avg
    lab <- sprintf('<text x="4" y="%.1f" class="lb %s">%s</text><text x="34" y="%.1f" class="lt">%s</text>',
                   mid + 4, if (on_court) "nowlab" else "", r$k,
                   mid + 4, if (r$rec) sprintf("n%d", r$rec) else "")
    if (!isTRUE(r$rec > 0))
      return(paste0(lab, sprintf('<text x="%.1f" y="%.1f" class="lt">not received yet</text>',
                                 L$x0 + 4, mid + 4)))
    paste0(lab,
      if (on_court) sprintf('<rect x="0" y="%d" width="%d" height="%d" fill="%s" opacity=".07"/>',
                            y, L$w, L$row, ink) else "",
      sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="11" fill="%s" opacity="%s"/>',
              L$x0, mid - 5.5, max(1, at(r$so) - L$x0), ink, if (behind) ".95" else ".55"),
      # the interval, drawn on top of the bar it qualifies
      sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="var(--ink-3)" stroke-width="1.2"/><line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="var(--ink-3)" stroke-width="1.2"/><line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="var(--ink-3)" stroke-width="1.2"/>',
              at(r$lo), mid, at(r$hi), mid,
              at(r$lo), mid - 4, at(r$lo), mid + 4,
              at(r$hi), mid - 4, at(r$hi), mid + 4),
      sprintf('<text x="%d" y="%.1f" class="lv" text-anchor="end">%s</text>',
              L$vx, mid + 4, pct(r$so)),
      sprintf('<text x="%d" y="%.1f" class="lv %s" text-anchor="end">%+d</text>',
              L$px, mid + 4, if (r$pm > 0) "pos" else if (r$pm < 0) "neg" else "thin", r$pm),
      # One word, and never two: on court wins, because it is the row a coach
      # is looking at right now.
      if (on_court) sprintf('<text x="%d" y="%.1f" class="lt nowlab">on court</text>',
                            L$sx, mid + 4)
      else if (behind) sprintf('<text x="%d" y="%.1f" class="lt behind">behind</text>',
                               L$sx, mid + 4) else "")
  }, ""), collapse = "")

  sprintf('<svg class="ladder" viewBox="0 0 %d %d" role="img" aria-label="%s" preserveAspectRatio="xMidYMid meet">%s%s%s</svg>',
          L$w, h, esc(alt), grid, avg_line, bars)
}

#' The ladder as a card, with the one line of reading it needs
rot_ladder_card <- function(rally, label, cls, now = NA_character_) {
  svg <- rot_ladder(rally, now, them = identical(cls, "them"),
                    alt = sprintf("side-out by rotation, %s", label))
  if (!nzchar(svg)) return("")
  sprintf('<div class="card %s"><h3>Side-out by rotation · %s</h3>%s%s</div>',
          cls, label, svg,
          tiny("Bar is side-out in that rotation, whisker the 95% interval, dashed line this team's own side-out across all six. A rotation is marked behind only when its whole interval sits under that line, which is the difference between a bad run and a rotation to change something in. +/- is rallies won minus lost there, both phases."))
}
