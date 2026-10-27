# ---------------------------------------------------------------------------
# PASS QUALITY
#
# Sorted worst positive% first, because the use of this page before a match is
# picking who to serve. "Positive" = perfect (#) or good (+), which is
# VolleyStation's own grouping, not a category invented here.
#
# Off-net m is the median result of the pass itself, not where the passer stood
# to receive it: a low number there means they are delivering catchable balls
# even when the serve was tough.
# ---------------------------------------------------------------------------

tab_passing <- function(F, o, coords_ok, sys_rows = NULL) {
  R0 <- F[F$skill == "R", ]
  if (!nrow(R0)) return(empty("No receptions in the current filter."))

  t0 <- receive_line(R0)


  H <- hdr("Pass quality",
           sprintf("%s receptions · who to target before serving, sorted worst positive%% first",
                   nrow(R0)))
  H <- c(H, kpis(list(
    list(k = "Receptions", v = t0$n),
    list(k = "Pass average", v = if (is.na(t0$avg)) "" else sprintf("%.2f", t0$avg),
         s = "0–4 scale"),
    list(k = "Positive %", v = pct(t0$pospct), s = "# or +"),
    list(k = "Error %", v = pct(t0$errpct), s = "ace against"),
    list(k = "Overpass", v = t0$over, s = "no attack possible"),
    list(k = "Median off-net", v = if (!is.na(t0$med_off_net)) m1(t0$med_off_net) else "",
         s = "m, pass result"),
    list(k = "Earned FBSO", v = pct(fbso_line(F)$pct), s = "first-attack kills / receptions")
  )))

  # Everyone who has passed a ball. The floor of three was there to keep a
  # one-reception line out of a sorted list, and what it did was hide the
  # player a coach is asking about: the one who has just been served twice.
  # The count is the first column, so a coach can see what a rate rests on.
  by_player <- crosstab(R0, paste(R0$num, R0$player_short, sep = "|"), receive_line)
  by_player <- by_player[order(vapply(by_player, function(g) g$stat$pospct %||% 1, 1))]

  # No court here. Reception is read as grades, not as geometry: a pass is
  # good or it is not, and where the ball crossed the floor on the way does
  # not change what the setter got. Drawn for a whole team it was a hundred
  # arrows converging on one spot, which is what every reception looks like
  # when it goes well, so the picture was the same whether the passing was
  # good or bad. The tables below say which.

  Rc <- list()

  # The last 3, 6 and 15 receptions, beside the whole figure, and the chips
  # for the last 15 themselves. A passer who has been got at in this rotation
  # shows up here before she shows up in a match average.
  rec_window <- function(rows) {
    data.table::setorder(rows, date, match_id, set_no, rally_no, seq)
    all_eff <- vs_efficiency(rows$effect, "R")
    vapply(PASS_WINDOWS, function(n) {
      q <- utils::tail(rows, n)
      if (!nrow(q)) return("")
      e <- vs_efficiency(q$effect, "R")
      arrow <- if (is.na(e) || is.na(all_eff) || nrow(rows) <= n) ""
               else if (e > all_eff + .001) '<span class="pos">&#9650;</span>'
               else if (e < all_eff - .001) '<span class="neg">&#9660;</span>'
               else '<span class="thin">&ndash;</span>'
      sprintf("%s %s", eff_td(e), arrow)
    }, "")
  }
  rec_chips <- function(rows) {
    q <- utils::tail(rows, 15)
    paste0(vapply(seq_len(nrow(q)), function(i) {
      p <- q[i, ]
      st <- land_status(p$effect)
      col <- unname(OUTCOME[st])
      sprintf('<span class="att %s" style="border-color:%s;color:%s" title="set %s · %s · %s">%s</span>',
              st, col, col, p$set_no, esc(p$score %||% ""), st,
              if (is.na(p$effect)) "?" else esc(p$effect))
    }, ""), collapse = "")
  }

  Rc <- c(Rc, card("By passer, weakest first", c(tbl_generic(
    c("Passer", "Rec", "Avg", "Perfect", "Good", "Poor", "Neg", "Err", "Pos%", "Err%",
      paste(sprintf("L%d", PASS_WINDOWS), collapse = " / "), "Last 15", "Off-net m"),
    lapply(by_player, function(g) {
      parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
      s <- g$stat
      c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), s$n,
        if (is.na(s$avg)) "" else sprintf("%.2f", s$avg),
        s$perfect, s$good, s$poor, s$neg, s$err,
        sprintf('<span class="%s">%s</span>',
                if (!is.na(s$pospct) && s$pospct < 0.5) "neg" else "", pct(s$pospct)),
        pct(s$errpct),
        paste(rec_window(g$rows), collapse = ""),
        sprintf('<div class="atts">%s</div>', rec_chips(g$rows)),
        if (!is.na(s$med_off_net)) m1(s$med_off_net) else "")
    })),
    tiny("L3, L6 and L15 are reception efficiency over that player's last 3, 6 and 15 receptions, with the arrow comparing the window with her whole figure; the chips are those receptions themselves, oldest on the left. Avg is this staff's own 0–4 scale, taken from the WEIGHTS block of their VolleyStation worksheet: # 4, + 3, ! 2, - 1, overpass 0.5, error 0. Positive = perfect (#) or good (+), VolleyStation's own \"positive reception\" grouping. Sorted so the passer most worth targeting sits at the top. Off-net m is the median result of the pass itself (distance from net), not where they stood to receive."))))

  if (!is.null(sys_rows)) Rc <- c(Rc, sys_rows)

  Rc <- c(Rc, card("By rotation", c(tbl_generic(
    c("Rotation", "Rec", "Pos%", "Err%", "FB kills", "FBSO%"),
    lapply(by_key(crosstab(R0, R0$rot, receive_line)), function(g) {
      f <- fbso_line(F[!is.na(F$rot) & F$rot == g$key, ])
      c(sprintf("<b>%s</b> %s", rot_said(g$key), thin(rot_pos(g$key))),
        g$stat$n, pct(g$stat$pospct), pct(g$stat$errpct), f$kills, pct(f$pct))
    })),
    tiny("Earned FBSO = first-attack kills ÷ receptions. Aces count as receptions; opponent serve errors are excluded, since no pass happened.",
         if ("6-2" %in% (o$sys %||% character()))
           " In a 6-2, R4–R6 are the rotations with the setter slot front row: the opposite is in the slot and the other setter sets from zone 1, 6 or 5." else ""))))

  paste0(c(H, Rc), collapse = "")
}

#' 5-1 vs 6-2 side by side.
#'
#' Built with every rail filter applied EXCEPT the system chips, so both
#' systems always show. Split by setter-slot half, because R4-R6 is the only
#' place the two systems put a different shape on court (two front-row hitters
#' vs three).
system_card <- function(G, CV) {
  if (!nrow(G)) return("")
  back  <- function(p) !is.na(p$rot) & p$rot %in% c("R1", "R2", "R3")
  front <- function(p) !is.na(p$rot) & p$rot %in% c("R4", "R5", "R6")
  ci_txt <- function(v, ci) if (is.na(v)) "" else
    sprintf("%s <small style=\"color:var(--ink-3)\">%d-%d</small>",
            pct(v), round(ci[1] * 100), round(ci[2] * 100))
  mk_row <- function(label, rs, cv) {
    c0 <- convert_line(cv$won)
    Rr <- rs[rs$skill == "R", ]; r <- receive_line(Rr); f <- fbso_line(rs)
    A <- rs[rs$skill == "A", ]; a <- attack_line(A)
    ci <- wilson(f$kills, f$rec)
    c(label, r$n, pct(r$pospct), pct(r$errpct), f$kills,
      if (f$rec) sprintf("%s <small style=\"color:var(--ink-3)\">%d-%d</small>",
                         pct(f$pct), round(ci[1] * 100), round(ci[2] * 100)) else "",
      nrow(A), eff_td(a$eff), c0$n, ci_txt(c0$pct, c0$ci))
  }
  rows <- list()
  for (k in c("5-1", "6-2")) {
    rs <- G[!is.na(G$sys) & G$sys == k, ]
    if (!nrow(rs)) next
    cv <- CV[!is.na(CV$sys) & CV$sys == k, , drop = FALSE]
    cvb <- cv[!is.na(cv$rot) & cv$rot %in% c("R1", "R2", "R3"), , drop = FALSE]
    cvf <- cv[!is.na(cv$rot) & cv$rot %in% c("R4", "R5", "R6"), , drop = FALSE]
    rows <- c(rows,
      list(mk_row(sprintf("<b>%s</b>", k), rs, cv)),
      list(mk_row(sprintf("&nbsp;&nbsp;%s R1-R3", k), rs[back(rs), ], cvb)),
      list(mk_row(sprintf("&nbsp;&nbsp;%s R4-R6", k), rs[front(rs), ], cvf)))
  }
  if (!length(rows)) return("")
  unclear <- length(unique(paste(G$match_id, G$set_no, G$rally_no)[G$sys == "?"]))
  card("By system: 5-1 vs 6-2", c(tbl_generic(
    c("System", "Rec", "Pos%", "Err%", "FB kills", "FBSO% (95% CI)", "Atk",
      "Atk eff", "Tr chances", "Convert% (95% CI)"), rows),
    tiny("Ignores the System chips so both columns always show; every other filter applies. R1-R3 look the same on court in both systems, so the real difference sits in R4-R6. Convert% = transition rallies won ÷ rallies where we dug their attack or played a free ball (one chance per rally). Small samples: overlapping intervals mean the gap is not established. ",
         if (unclear) sprintf("%s rall%s left unclear (switch mid-stretch or a lineup that fits neither) and excluded here.",
                              unclear, if (unclear == 1) "y" else "ies") else "")))
}
