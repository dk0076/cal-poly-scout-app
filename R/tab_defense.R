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

  # Two ways to cut it, because a staff asks it both ways. The tempo is the
  # shape of the ball in the air; the call is the play that produced it, and
  # "how do we block their Go" is the question that gets asked out loud. A
  # pick is one or the other, written t:H or c:PG, so the two rows of chips
  # are one choice between them.
  blk_type <- as.character(o$blkType %||% "all")
  ht_keys <- names(sort(table(OA$hit_type[!is.na(OA$hit_type)]), decreasing = TRUE))
  cl_keys <- names(sort(table(OA$season_call[!is.na(OA$season_call)]), decreasing = TRUE))
  # A bare code is a tempo, which is what this picker stored before it could
  # take a call, and a browser remembers the last thing it was given.
  if (blk_type %in% ht_keys) blk_type <- paste0("t:", blk_type)
  valid <- c("all", paste0("t:", ht_keys), paste0("c:", cl_keys))
  if (!(blk_type %in% valid)) blk_type <- "all"
  blk_kind <- substr(blk_type, 1, 1)
  blk_key <- if (blk_type == "all") "" else substring(blk_type, 3)
  blk_name <- if (blk_type == "all") ""
              else if (blk_kind == "t")
                tolower(if (is.na(HITTYPE[blk_key])) blk_key else unname(HITTYPE[blk_key]))
              else call_label(blk_key, OA$call_name[!is.na(OA$season_call) &
                                                      OA$season_call == blk_key][1])

  picked <- if (blk_type == "all") OA
            else if (blk_kind == "t") OA[!is.na(OA$hit_type) & OA$hit_type == blk_key, ]
            else OA[!is.na(OA$season_call) & OA$season_call == blk_key, ]
  FR <- data.table::copy(picked)
  data.table::set(FR, j = "blocker", value = blocker_of(picked))
  FR <- FR[!is.na(FR$blocker), ]

  # ---- the court, from our side -------------------------------------
  # Of the ball that is picked below, not of everything: picking their Go and
  # reading a court of all 136 attacks was the chart answering a question
  # nobody had asked.
  touched <- block_touched(picked, F)
  drawn <- if (isTRUE(o$defTouch)) picked[picked$has_xy & !touched, ]
           else picked[picked$has_xy, ]
  MA <- mirror_plays(drawn)
  rect <- list(x0 = 0, y0 = 0, x1 = COURT$W, y1 = COURT$NET)
  court <- court_svg("DF", x_min = -170, x_max = 1070, y_min = -190, y_max = 1700,
                     marks = arrow_layer(MA, "DF", 1, mono,
                       extra = function(r) paste("lands", vapply(COURT$NET - r$y1, m1, ""),
                                                 "m our side of the net")),
                     labels = zone_number_layer("own", rect),
                     alt = "opponent attacks into our court",
                     mono = mono, coords_ok = coords_ok)

  left <- plain_card(c(
    viewbar(vtoggle("defTouch", "Drop balls we touched", o$defTouch)),
    if (blk_type != "all" && !nrow(MA))
      caveat(sprintf("No %s of theirs carries a drawn line, so there is nothing to chart. The tables below still count them.",
                     if (blk_kind == "t") paste(blk_name, "ball") else blk_name))
    else "",
    court,
    legend(c("var(--kill)", "their kill"), c("var(--inplay)", "we kept it up"),
           c("var(--blocked)", "we blocked it"), c("var(--err)", "they erred"),
           trailing = "their contact at the top · the ball lands in our half"),
    tiny(sprintf("Their attacks mirrored into our frame, so this is the view from our baseline. Where the green concentrates is where we are being beaten.%s%s",
                 if (!isTRUE(o$defTouch)) ""
                 else sprintf(" %d of them came off our block and are left out, so this is where they hit when we did not touch it; the tables below still count all of them.",
                              sum(touched & picked$has_xy)),
                 if (blk_type == "all") ""
                 else sprintf(" Their %s only, picked below; the numbers above it are every attack they made.",
                              if (blk_kind == "t") paste(blk_name, "ball") else blk_name)))
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
      dug <- sum(dug_after(g$rows, F))
      c(paste(g$key, if (g$key == "1") "blocker" else "blockers"),
        g$stat$ta, pct(g$stat$kpct), eff_td(g$stat$eff),
        pct(if (g$stat$ta) dug / g$stat$ta else NA),
        sum(!is.na(g$rows$effect) & g$rows$effect == "/"))
    })),
    tiny("Blockers up is the number the coder recorded on their attack. \"Stuff\" counts their attacks graded as blocked. The row that matters is whether their efficiency actually falls as the block grows. If it does not, the block is not the problem."))))

  # ---- the block, against each kind of swing --------------------------
  #
  # A block is not one thing. The same three bodies that stuff a high ball
  # outside are the ones a quick goes through, and a tip goes over all of it,
  # so a block read as one number says nothing about what to change. Grouped
  # by the tempo the coder put on their attack, it does: if their quick is
  # the row with no stuffs and their best efficiency, the answer is the
  # middle, not the pins.
  #
  # Stuff% is of their attacks on that tempo, so the rows do not pool into
  # anything but themselves. A count of four stuffs is four, whatever the
  # percentage beside it reads, which is why the count is the column before
  # it and the rate goes quiet underneath a handful.
  ht <- by_size(crosstab(OA, OA$hit_type))
  R <- c(R, if (!length(ht) || (length(ht) == 1 && is.na(ht[[1]]$key)))
    card("Against our block, by hit type",
         c(empty("No attack in this filter records how it was hit, so there is nothing to split."),
           tiny("Hit type is the tempo on the coder's own attack entry: high, quick, standard, tense, off-speed, slow.")))
  else card("Against our block, by hit type", c(tbl_generic(
    c("Hit type", "Their att", "Sh%", "Their K%", "Their eff", "Stuff", "Stuff%", "We dug"),
    lapply(ht, function(g) {
      stuff <- sum(!is.na(g$rows$effect) & g$rows$effect == "/")
      dug <- sum(dug_after(g$rows, F))
      rate <- function(k, n) if (!n) "" else if (n < PASS_FLOOR)
        sprintf('<span class="thin" title="off %d attack%s">%s</span>',
                n, if (n == 1L) "" else "s", pct(k / n))
        else pct(k / n)
      c(sprintf("<b>%s</b>", esc(if (is.na(g$key)) "Not coded"
                                 else if (is.na(HITTYPE[g$key])) g$key
                                 else unname(HITTYPE[g$key]))),
        g$stat$ta, pct(g$stat$ta / nrow(OA)), pct(g$stat$kpct), eff_td(g$stat$eff),
        stuff, rate(stuff, g$stat$ta), rate(dug, g$stat$ta))
    })),
    tiny("Hit type is the tempo on the coder's own attack entry. Stuff counts their attacks graded blocked, We dug the ones a dig of ours came up on, both out of their attacks on that tempo. Rates go quiet under five attacks. A tempo they hit well and we neither stuff nor dig is the one to move the block to."))))

  # ---- one blocker against one ball -----------------------------------
  #
  # Whoever ends up in front of that attack, and only her. A block is three
  # people, but the question is about one of them: their outside hitting at
  # our right side is the matchup a staff talks about, and crediting all
  # three front-row players with every attack sorted this table by who had
  # been on court the longest, which is no use at all.
  #
  # `blocker_of` is the whole of it: our setter's zone says who is doing
  # which job, the zone their attack was struck from says whose job it was
  # to be there. See R/model.R.

  chip <- function(v, label, n)
    sprintf('<button class="chip %s" data-o="blkType" data-v="%s">%s<span class="n">%d</span></button>',
            if (identical(blk_type, v)) "on" else "", esc(v), esc(label), n)

  R <- c(R, if (!length(ht_keys) && !length(cl_keys)) "" else
    card("Blocking by player, against one ball", c(
      sprintf('<div class="viewbar">%s%s</div>',
              chip("all", "Everything they hit", nrow(OA)),
              paste0(vapply(ht_keys, function(k)
                chip(paste0("t:", k),
                     if (is.na(HITTYPE[k])) k else unname(HITTYPE[k]),
                     sum(!is.na(OA$hit_type) & OA$hit_type == k)), ""), collapse = "")),
      if (length(cl_keys))
        sprintf('<div class="viewbar">%s</div>',
                paste0(vapply(cl_keys, function(k)
                  chip(paste0("c:", k),
                       call_label(k, OA$call_name[!is.na(OA$season_call) &
                                                    OA$season_call == k][1]),
                       sum(!is.na(OA$season_call) & OA$season_call == k)), ""),
                  collapse = "")) else "",
      if (!nrow(FR))
        empty(if (!nrow(picked)) "Nothing of theirs in this filter was hit that way."
              else "These attacks carry no lineup or no attack zone, so there is nobody to put in front of them.")
      else tbl_generic(
        c("Blocker", "At her", "Stuff", "Stuff%", "Their K%", "Their eff", "We dug"),
        lapply(by_size(crosstab(FR, paste(FR$blocker,
                                          F$player_short[match(FR$blocker, F$num)],
                                          sep = "|")), "ta"),
          function(g) {
            parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
            stuff <- sum(!is.na(g$rows$effect) & g$rows$effect == "/")
            dug <- sum(dug_after(g$rows, F))
            rate <- function(k, n) if (!n) "" else if (n < PASS_FLOOR)
              sprintf('<span class="thin" title="off %d attack%s">%s</span>',
                      n, if (n == 1L) "" else "s", pct(k / n))
              else pct(k / n)
            c(sprintf("<b>%s</b> %s", esc(parts[1]), esc(parts[2] %||% "")),
              g$stat$ta, stuff, rate(stuff, g$stat$ta),
              pct(g$stat$kpct), eff_td(g$stat$eff), rate(dug, g$stat$ta))
          })),
      tiny(sprintf("Their attacks hit at her%s: one blocker an attack, the one whose job put her in front of it. Their left pin is our opposite, their right is our outside, and their middle, their dump and anything out of the back row is our middle, read off where the ball was struck rather than off the call. Who is doing which job comes from our setter's zone, because everybody switches after the contact. She is credited whether or not she touched it, since the matchup is the point and a ball hit past her counts against her. Their K%% and eff are theirs on those attacks, so low is good down those two columns and high is good on the other two. The first row of chips is the tempo, the second the play. Rates go quiet under five attacks.",
                   if (blk_type == "all") ""
                   else if (blk_kind == "t") sprintf(", on their %s ball", blk_name)
                   else sprintf(", on their %s", blk_name))))))

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
#' Which of their attacks our block got a hand on.
#'
#' A ball off the block is not where the hitter put it: it drops short, or
#' sideways, or back over, and on a chart of where they hit it is the block's
#' mark rather than theirs. So the chart can drop them and show the swings
#' that went past untouched.
#'
#' A touch is our own block row sitting directly after their attack, which is
#' how both formats record it: 82 of 136 attacks in a scouted match, every
#' one of the nine stuffs among them. The grade on that row does not matter,
#' a block error is still a touch. An attack graded blocked counts as well,
#' for a file that records the stuff on the attack and never writes the block
#' row.
#' Answers per row rather than by uid: a uid is unique inside its own file
#' and starts again at 1 in the next one, so a library of four films has four
#' plays numbered 7 and `%in%` cannot tell them apart.
block_touched <- function(oa, F) {
  if (!nrow(oa)) return(logical(0))
  stuffed <- !is.na(oa$effect) & oa$effect == "/"
  B <- F[F$skill == "B", ]
  if (!nrow(B)) return(stuffed)
  after <- paste(B$match_id, B$set_no, B$rally_no, B$seq - 1L)
  stuffed | paste(oa$match_id, oa$set_no, oa$rally_no, oa$seq) %in% after
}

#' Per row, for the same reason as `block_touched`: by uid, a dig in one film
#' was answering an attack in another.
dug_after <- function(oa, F) {
  if (!nrow(oa) || !nrow(F)) return(logical(max(0L, nrow(oa))))
  dig <- F[F$skill == "D" & (is.na(F$effect) | F$effect != "="), ]
  if (!nrow(dig)) return(logical(nrow(oa)))
  k <- paste(dig$match_id, dig$set_no, dig$rally_no)
  paste(oa$match_id, oa$set_no, oa$rally_no) %in% k
}
