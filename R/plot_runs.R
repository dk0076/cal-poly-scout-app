# ---------------------------------------------------------------------------
# THE RUN STRIP
#
# A set is won in runs. The tables on this tab report the set as totals, which
# is the right shape afterwards and the wrong one during: a coach calling a
# timeout is not asking "what is our side-out", they are asking "we have lost
# four in a row, in what rotation, and is this the second time tonight".
#
# So: one tick per rally, in order, ours above the line and theirs below. A
# run of three or more gets a bracket and a label saying how long it was and
# which rotation we were in when it started. Our rotation for both sides,
# because the question behind a run against us is which six of ours it
# happened to, not which six of theirs did it.
#
# Three or more, because two in a row is a rally and its answer. On the two
# test files that threshold marks about a quarter of rallies, which is few
# enough to read at a glance and often enough to be worth drawing.
#
# Runs do not pool across matches: a run is a thing that happened inside one
# set of one match. When several files are loaded this draws the most recent
# and says so.
#
# Built from rally winners and rotations, so it works on a file being coded
# live with nothing charted.
# ---------------------------------------------------------------------------

RUN_MIN <- 3L
STRIP <- list(w = 486, x0 = 52, x1 = 478, row = 48, top = 10, gap = 1.4)

#' Runs of consecutive rallies won by the same side
#'
#' Returns one row per run of RUN_MIN or more: where it starts, how long, who
#' won it, and the rotation it was won from.
#'
#' That last one is not the rotation at the first rally, which is what this
#' used to say and what made a real run read wrong. A run of ours begins with
#' a side-out: we win it in the rotation we were already in, then rotate and
#' serve the rest from the next one. Eleven straight in this file started
#' with a side-out in R6 and were served from R1, and R1 is what a coach
#' means by "the run in R1". So the label comes from the first rally of the
#' run in which the winning side served.
#'
#' A run against us needs no such care, because our rotation cannot change
#' while we are losing: we only rotate on a side-out we win. The same rule is
#' applied to both anyway, since it gives the same answer there.
run_spans <- function(won, rot = NULL, served = NULL, min_len = RUN_MIN) {
  if (!length(won)) return(list())
  r <- rle(won)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$lengths >= min_len)
  lapply(keep, function(i) {
    idx <- starts[i]:ends[i]
    ours <- isTRUE(r$values[i])
    at <- idx[1]
    if (!is.null(served)) {
      # the first rally of the run that this side served
      w <- which(if (ours) isTRUE_v(served[idx]) else !isTRUE_v(served[idx]))
      if (length(w)) at <- idx[w[1]]
    }
    label <- if (is.null(rot)) NA_character_ else rot[at]
    # a rally nobody on our side touched can carry no rotation; the rest of
    # the run is the same rotation, so take the first one that is there
    if (!is.null(rot) && is.na(label)) {
      ok <- idx[!is.na(rot[idx])]
      if (length(ok)) label <- rot[ok[1]]
    }
    list(from = starts[i], to = ends[i], n = r$lengths[i], ours = ours,
         rot = label)
  })
}

#' TRUE where TRUE, FALSE where NA, elementwise
isTRUE_v <- function(v) !is.na(v) & v

#' The strip for one set
#'
#' `rl` is that set's rallies, already in order and with a winner each.
run_set_row <- function(rl, y, label) {
  S <- STRIP
  n <- nrow(rl)
  span <- S$x1 - S$x0
  tw <- max(2.2, span / max(n, 1) - S$gap)
  at <- function(i) S$x0 + span * (i - 1) / max(n, 1)
  mid <- y + S$row / 2
  us <- cumsum(rl$won); them <- cumsum(!rl$won)

  ticks <- paste0(vapply(seq_len(n), function(i) {
    ours <- isTRUE(rl$won[i])
    h <- 11
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%d" fill="%s" opacity=".85"><title>%s</title></rect>',
            at(i), if (ours) mid - h - 1 else mid + 1, tw, h,
            if (ours) "var(--accent)" else "var(--them-ink)",
            esc(sprintf("%d-%d · %s · %s", us[i], them[i],
                        if (is.na(rl$rot[i])) "rotation unknown" else rl$rot[i],
                        if (isTRUE(rl$served[i])) "our serve" else "their serve")))
  }, ""), collapse = "")

  brackets <- paste0(vapply(run_spans(rl$won, rl$rot, rl$served), function(sp) {
    x_from <- at(sp$from)
    x_to <- at(sp$to) + tw
    ytop <- if (sp$ours) mid - 15 else mid + 15
    ylab <- if (sp$ours) mid - 18 else mid + 25
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1.2"/><text x="%.1f" y="%.1f" class="rt %s" text-anchor="middle">%s&#215;%d</text>',
            x_from, ytop, x_to, ytop,
            if (sp$ours) "var(--accent)" else "var(--them-ink)",
            (x_from + x_to) / 2, ylab, if (sp$ours) "ours" else "theirs",
            if (is.na(sp$rot)) "" else paste0(sp$rot, " "), sp$n)
  }, ""), collapse = "")

  sprintf('<text x="2" y="%.1f" class="rl">%s</text><text x="2" y="%.1f" class="rt">%d-%d</text><line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="var(--rule)" stroke-width="1"/>%s%s',
          mid, esc(label), mid + 12, us[n], them[n],
          S$x0, mid, S$x1, mid, ticks, brackets)
}

#' The whole strip: the most recent match, one row per set
run_strip <- function(rally, max_sets = 6L) {
  rl <- rally[!is.na(rally$won), ]
  if (!nrow(rl)) return("")
  # One match. A run is something that happened inside one set of one match,
  # and three files' rallies laid end to end would draw runs across the gaps
  # between them.
  last_id <- rl$match_id[nrow(rl)]
  rl <- rl[rl$match_id == last_id, ]
  data.table::setorder(rl, set_no, rally_no)
  sets <- unique(rl$set_no)
  if (length(sets) > max_sets) sets <- utils::tail(sets, max_sets)

  S <- STRIP
  rows <- paste0(vapply(seq_along(sets), function(i)
    run_set_row(rl[rl$set_no == sets[i], ], S$top + S$row * (i - 1),
                sprintf("Set %s", sets[i])), ""), collapse = "")
  h <- S$top + S$row * length(sets) + 6
  sprintf('<svg class="runs" viewBox="0 0 %d %d" role="img" aria-label="%s" preserveAspectRatio="xMidYMid meet">%s</svg>',
          S$w, h, esc("point by point, with runs of three or more marked"), rows)
}

#' The longest run against us, said in words, because it is the one a coach
#' wants without counting ticks.
run_worst <- function(rally) {
  rl <- rally[!is.na(rally$won), ]
  if (!nrow(rl)) return("")
  last_id <- rl$match_id[nrow(rl)]
  rl <- rl[rl$match_id == last_id, ]
  data.table::setorder(rl, set_no, rally_no)
  spans <- unlist(lapply(unique(rl$set_no), function(sn) {
    q <- rl[rl$set_no == sn, ]
    lapply(run_spans(q$won, q$rot, q$served), function(sp) c(sp, list(set_no = sn)))
  }), recursive = FALSE)
  against <- Filter(function(sp) !sp$ours, spans)
  if (!length(against)) return("No run of three against us in this match.")
  worst <- against[[which.max(vapply(against, function(sp) sp$n, 0L))]]
  sprintf("Longest run against us: %d straight in set %s%s.",
          worst$n, worst$set_no,
          if (is.na(worst$rot)) "" else sprintf(", in %s", worst$rot))
}

run_strip_card <- function(rally, file_label = "", n_files = 1L) {
  svg <- run_strip(rally)
  if (!nzchar(svg)) return("")
  sprintf('<div class="card"><h3>Point by point%s</h3>%s%s%s</div>',
          if (nzchar(file_label)) sprintf(' <span class="thin">%s</span>', esc(file_label)) else "",
          svg,
          sprintf('<div class="tiny" style="margin-top:2px">%s</div>', esc(run_worst(rally))),
          tiny(sprintf("One tick per rally, in order: above the line we won it, below they did. A run of %d or more carries its length and the rotation it was won from, ours in both cases, because the question behind a run against us is which six of ours it happened to. A run of ours opens with a side-out in the rotation before, then is served from the one named here. Hover a tick for the score.%s",
                       RUN_MIN,
                       if (n_files > 1L) " Only the most recent file is drawn: runs do not pool across matches." else "")))
}
