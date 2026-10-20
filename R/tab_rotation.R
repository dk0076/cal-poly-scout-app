# ---------------------------------------------------------------------------
# ROTATIONS
#
# Two tables, to the staff's own layout: rotational analysis, then the same
# questions by set. Both are shown for the focus team and for the opponent,
# because a rotation is only as good as what the other six do to you in it.
#
# Column definitions live in one place, in rot_cols() below, so the two
# tables and the two teams cannot drift apart.
#
# The rail's rotation chips are ignored on this tab, the way the system card
# ignores the system chips: the whole point is comparing the six, so filtering
# to one would empty it.
# ---------------------------------------------------------------------------

ROTS <- paste0("R", 1:6)

# Rotation label -> the setter's court zone. Sequence-based, which is this
# program's convention: R1 is the setter in zone 1, then 6, 5, 4, 3, 2.
ROT_ZONE <- setNames(ROT_ORDER, ROTS)

#' The seven rotational columns for one slice of rallies and plays.
rot_cols <- function(rl, pl) {
  s <- sideout_line(rl)
  srv <- pl[pl$skill == "S", ]
  list(so = s$so, eso = s$eso, fbso = s$fbso, ps = s$ps,
       pm = s$plus_minus,
       ace = sum(!is.na(srv$effect) & srv$effect == "#"),
       err = sum(!is.na(srv$effect) & srv$effect == "="),
       rec = s$rec, srv_n = s$srv)
}

#' The seven by-set columns.
#'
#' sQ% is serve-receive quality, taken from the staff's own worksheet formula:
#'   VSPER(VSOR(VSCELL("*R#;S(n)"), VSCELL("*R+;S(n)")))
#' which is receptions graded # or + as a share of all receptions, per set.
#' That is VolleyStation's "positive reception" grouping, the same number the
#' Pass quality tab reports as Positive%.
set_cols <- function(rl, pl) {
  a <- attack_line(pl[pl$skill == "A", ])
  s <- sideout_line(rl)
  r <- receive_line(pl[pl$skill == "R", ])
  list(eff = a$eff, k = a$kpct, eb = a$epct, sq = r$pospct,
       so = s$so, fbso = s$fbso, ps = s$ps, att = a$ta)
}

pm_cell <- function(v) {
  if (is.na(v) || v == 0) return(sprintf('<span class="thin">%s</span>', v %||% "—"))
  sprintf('<span class="%s">%+d</span>', if (v > 0) "pos" else "neg", v)
}

tab_rotation <- function(rally, F, opp_rally, Opp, o) {
  if (!nrow(rally)) return(empty("No rallies in the current filter."))
  t0 <- sideout_line(rally)
  known <- sum(!is.na(rally$won))
  cover <- if (nrow(rally)) known / nrow(rally) else NA_real_

  H <- hdr("Rotations", sprintf(
    "%d rallies · %s with a recorded winner · side-out and point score by rotation",
    nrow(rally), pct(cover)))

  if (!known)
    return(paste0(c(H, empty("No rally in this filter records who won it, so none of these columns can be computed. A set coded as one long exchange carries a winner only on its last rally.")), collapse = ""))

  if (!is.na(cover) && cover < 0.9)
    H <- c(H, sprintf('<div class="warn">Only %s of rallies record a winner, so every rate here rests on %d of %d. Counts (+/-, aces, errors) are understated on those files even where the percentages hold.</div>',
                      pct(cover), known, nrow(rally)))

  H <- c(H, kpis(list(
    list(k = "Side-out", v = pct(t0$so), s = sprintf("%d received", t0$rec)),
    list(k = "Earned SO", v = pct(t0$eso), s = "won on our own kill"),
    list(k = "First-ball SO", v = pct(t0$fbso), s = "won on the first swing"),
    list(k = "Point score", v = pct(t0$ps), s = sprintf("%d served", t0$srv)),
    list(k = "+/-", v = sprintf("%+d", t0$plus_minus), s = "rallies won − lost")
  )))

  side_block <- function(rl, pl, label, cls) {
    rows <- lapply(ROTS, function(k) {
      c0 <- rot_cols(rl[!is.na(rl$rot) & rl$rot == k, ],
                     pl[!is.na(pl$rot) & pl$rot == k, ])
      c(sprintf("<b>%s</b> %s", k, thin(sprintf("z%d", ROT_ZONE[[k]]))),
        pct(c0$so), pct(c0$eso), pct(c0$fbso), pct(c0$ps),
        pm_cell(c0$pm), c0$ace, c0$err)
    })
    tot <- rot_cols(rl, pl)
    rows <- c(rows, list(tot_row(c("Total", pct(tot$so), pct(tot$eso), pct(tot$fbso),
                                   pct(tot$ps), pm_cell(tot$pm), tot$ace, tot$err))))
    setrows <- lapply(sort(unique(rl$set_no)), function(sn) {
      c0 <- set_cols(rl[rl$set_no == sn, ], pl[pl$set_no == sn, ])
      c(sprintf("<b>%s</b>", sn), eff_td(c0$eff), pct(c0$k), pct(c0$eb),
        pct(c0$sq), pct(c0$so), pct(c0$fbso), pct(c0$ps))
    })
    st <- set_cols(rl, pl)
    setrows <- c(setrows, list(tot_row(c("Total", eff_td(st$eff), pct(st$k), pct(st$eb),
                                         pct(st$sq), pct(st$so), pct(st$fbso), pct(st$ps)))))
    paste0(
      sprintf('<div class="card %s"><h3>Rotational analysis · %s</h3>%s</div>', cls, label,
              tbl_generic(c("Rot", "SO%", "ESO%", "FBSO%", "PS%", "+/-", "Ace", "Err"), rows)),
      sprintf('<div class="card %s"><h3>Analysis by set · %s</h3>%s</div>', cls, label,
              tbl_generic(c("Set", "Eff", "K%", "E+B%", "sQ%", "SO%", "FBSO%", "PS%"), setrows)))
  }

  body <- cols2(
    side_block(rally, F, "us", "us"),
    side_block(opp_rally, Opp, "them", "them"), 620)

  paste0(c(H, body, note(
    "SO% is rallies won of rallies received. ESO% counts only the ones we finished ourselves with a kill or a block, so the gap between SO% and ESO% is the share the opponent handed over. FBSO% counts the ones won on the first swing off our own pass; all three share the same denominator, so FBSO% ≤ ESO% ≤ SO%. PS% is rallies won of rallies served — a break point. +/- is rallies won minus lost in that rotation, both phases. Ace and Err are our serves in that rotation. Rotation labels are sequence-based, with the setter's court zone shown beside each. sQ% is serve-receive quality — receptions graded # or +, as a share of all receptions — matching the worksheet formula VSPER(VSOR(R#, R+)) and the Positive% on the Pass quality tab."
  )), collapse = "")
}
