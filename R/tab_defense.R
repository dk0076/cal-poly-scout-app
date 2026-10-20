# ---------------------------------------------------------------------------
# DEFENSE
#
# How we are defending, which is three questions the tables keep apart because
# they have different answers:
#
#   what are they able to do to us   -- their attack efficiency against us
#   do we touch it                   -- block touches and digs kept up
#   do we win the rally after        -- transition conversion off a dig
#
# A defence that digs a lot of balls and converts none of them is not a good
# defence, and a block that never stuffs anything can still be the reason the
# dig was possible. So block, dig and conversion are reported side by side
# rather than rolled into one number.
#
# The court is drawn from our side: the opponent's attacks are mirrored into
# our frame, so their contact is across the net and the ball lands in our
# court, the way the defence sees it.
# ---------------------------------------------------------------------------

tab_defense <- function(F, Opp, cv, o, coords_ok) {
  OA <- Opp[Opp$skill == "A", ]
  D  <- F[F$skill == "D", ]
  B  <- F[F$skill == "B", ]
  if (!nrow(OA) && !nrow(D) && !nrow(B))
    return(empty("No opponent attacks, digs or blocks in the current filter."))

  oatk <- attack_line(OA)
  dg <- dig_line(D)
  bl <- block_line(B)
  conv <- convert_line(cv$won)
  mono <- isTRUE(o$mono)

  H <- hdr("Defense", sprintf(
    "%d opponent attacks faced · %d digs · %d block touches",
    nrow(OA), dg$n, bl$n))

  H <- c(H, kpis(list(
    list(k = "They hit", v = eff(oatk$eff), s = sprintf("%s kill", pct(oatk$kpct))),
    list(k = "Digs", v = dg$n, s = sprintf("%s in system", pct(dg$in_sys_pct))),
    list(k = "Dig errors", v = dg$err, s = pct(dg$err_pct)),
    list(k = "Stuff blocks", v = bl$stuff, s = sprintf("of %d touches", bl$n)),
    list(k = "Convert", v = pct(conv$pct),
         s = sprintf("%d transition chances", conv$n))
  )))

  # ---- the court, from our side -------------------------------------
  MA <- mirror_plays(OA[OA$has_xy, ])
  rect <- list(x0 = 0, y0 = 0, x1 = COURT$W, y1 = COURT$NET)
  court <- court_svg("DF", x_min = -170, x_max = 1070, y_min = -190, y_max = 1700,
                     marks = arrow_layer(MA, "DF", 1, mono,
                       extra = function(r) paste("lands", vapply(COURT$NET - r$y1, m1, ""),
                                                 "m our side of the net")),
                     labels = zone_number_layer("own", rect),
                     alt = "opponent attacks into our court",
                     mono = mono, coords_ok = coords_ok)

  left <- plain_card(c(
    court,
    legend(c("var(--kill)", "their kill"), c("var(--inplay)", "we kept it up"),
           c("var(--blocked)", "we blocked it"), c("var(--err)", "they erred"),
           trailing = "their contact at the top · the ball lands in our half"),
    tiny("Their attacks mirrored into our frame, so this is the view from our baseline. Where the green concentrates is where we are being beaten.")
  ))

  R <- list()

  # ---- the blocking read --------------------------------------------
  # Only worth a table if the file actually varies the count. Every Cal Poly
  # file to hand records 2 blockers on all 369 or 370 attacks -- the field is
  # not being coded -- and a one-row table there reads like a finding when it
  # is a constant. Scouted opponent files do vary, so the table appears for
  # those.
  nb <- unique(OA$blockers[!is.na(OA$blockers)])
  R <- c(R, if (length(nb) < 2)
    card("Against our block", c(
      empty(if (!length(nb))
              "No attack in this filter records how many blockers were up."
            else sprintf("Every one of these %d attacks records %s blocker%s, so there is nothing to compare. The field is not being coded in this file.",
                         nrow(OA), nb, if (nb == 1) "" else "s")),
      tiny("Code the blocker count on the opponent's attack and this becomes the most useful table on the page: whether their efficiency actually falls as the block grows.")))
  else card("Against our block", c(tbl_generic(
    c("Blockers up", "Their att", "Their K%", "Their eff", "We dug", "Stuff"),
    lapply(by_key(crosstab(OA, OA$blockers)), function(g) {
      dug <- sum(g$rows$uid %in% dug_after(g$rows, F))
      c(paste(g$key, if (g$key == "1") "blocker" else "blockers"),
        g$stat$ta, pct(g$stat$kpct), eff_td(g$stat$eff),
        pct(if (g$stat$ta) dug / g$stat$ta else NA),
        sum(!is.na(g$rows$effect) & g$rows$effect == "/"))
    })),
    tiny("Blockers up is the number the coder recorded on their attack. \"Stuff\" counts their attacks graded as blocked. The row that matters is whether their efficiency actually falls as the block grows. If it does not, the block is not the problem."))))

  # ---- what they run at us ------------------------------------------
  R <- c(R, card("What they run at us", tbl_generic(
    c("Call", "Att", "Sh%", "Their K%", "Their eff"),
    lapply(by_size(crosstab(OA, OA$season_call)), function(g)
      c(sprintf("<b>%s</b>", esc(call_label(g$key, g$rows$call_name[1]))),
        g$stat$ta, pct(g$stat$ta / nrow(OA)), pct(g$stat$kpct), eff_td(g$stat$eff))))))

  # ---- where it lands ------------------------------------------------
  if (nrow(OA[OA$has_xy, ])) {
    MX <- mirror_plays(OA[OA$has_xy, ])
    R <- c(R, card("Where their attacks land in our court", c(tbl_generic(
      c("Zone", "Att", "Sh%", "Their K%", "Their eff"),
      lapply(by_size(crosstab(MX, zone_or_coded(NA, MX$x1, MX$y1, "own"))), function(g)
        c(paste("Zone", g$key), g$stat$ta, pct(g$stat$ta / nrow(MX)),
          pct(g$stat$kpct), eff_td(g$stat$eff)))),
      tiny("Zones of our own court, from the landing coordinate of their attack. Zone 6 heavy with a high kill rate is a different problem from zone 5 heavy."))))
  }

  # ---- our diggers ----------------------------------------------------
  R <- c(R, card("By digger", c(tbl_generic(
    c("Player", "Digs", "In system", "OOS", "Over", "Err", "In-sys%", "Err%"),
    lapply(by_size(crosstab(D, paste(D$num, D$player_short, sep = "|"), dig_line), "n"),
      function(g) {
        parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
        s <- g$stat
        c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), s$n, s$in_sys,
          s$oos, s$over, s$err, pct(s$in_sys_pct), pct(s$err_pct))
      })),
    tiny("In system is a dig graded # or +, the ones the setter can run an offence off; OOS is ! or -; Over is a dig sent straight back over. This grouping is the program's own, from the wording of its scout report."),
    # A file that never uses '#' is not grading digs on the full scale, so
    # "in system" quietly becomes "kept up" and reads better than it is.
    if (nrow(D) && !any(!is.na(D$effect) & D$effect == "#"))
      sprintf('<div class="warn">No dig in this filter is graded <code>#</code>, so this file is not using the full scale; in-system here means little more than "kept up". Grades present: %s.</div>',
              paste(sort(unique(na.omit(D$effect))), collapse = " ")) else "")))

  # ---- our blockers ---------------------------------------------------
  if (nrow(B))
    R <- c(R, card("By blocker", tbl_generic(
      c("Player", "Touches", "Stuff", "Touch", "Err", "Stuff%"),
      lapply(by_size(crosstab(B, paste(B$num, B$player_short, sep = "|"), block_line), "n"),
        function(g) {
          parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
          s <- g$stat
          c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), s$n, s$stuff,
            s$touch, s$err, pct(s$stuff_pct))
        }))))

  # ---- by rotation ----------------------------------------------------
  R <- c(R, card("By our rotation", c(tbl_generic(
    c("Rot", "Their att", "Their eff", "Digs", "In-sys%", "Tr chances", "Convert%"),
    lapply(ROTS, function(k) {
      oa <- attack_line(OA[!is.na(OA$opp_rot) & OA$opp_rot == k, ])
      d <- dig_line(D[!is.na(D$rot) & D$rot == k, ])
      c0 <- convert_line(cv$won[!is.na(cv$rot) & cv$rot == k])
      c(sprintf("<b>%s</b>", k), oa$ta, eff_td(oa$eff), d$n,
        pct(d$in_sys_pct), c0$n, pct(c0$pct))
    })),
    tiny("Their attack efficiency is cut by OUR rotation (what they managed while we stood in it), which is the cut that tells you where our defence is leaking. Convert% is transition rallies won of rallies where we dug their attack or played a free ball, one chance per rally."))))

  paste0(c(H, cols2(left, R, 460), note(
    "A dig that stays up is not a dig the setter can use, so In-sys% is the column to read rather than the raw count. Convert% is the one that says whether any of it mattered: digging well and converting nothing means the problem is after the dig, not in the defence."
  )), collapse = "")
}

#' uids of opponent attacks that we dug, matched within the rally.
#'
#' An attack we dug is one followed by a dig of ours in the same rally before
#' the other team touches it again. Matching on the rally rather than on the
#' play order keeps it correct when a rally carries several attacks.
dug_after <- function(oa, F) {
  if (!nrow(oa) || !nrow(F)) return(integer())
  dig <- F[F$skill == "D" & (is.na(F$effect) | F$effect != "="), ]
  if (!nrow(dig)) return(integer())
  k <- paste(dig$match_id, dig$set_no, dig$rally_no)
  oa$uid[paste(oa$match_id, oa$set_no, oa$rally_no) %in% k]
}
