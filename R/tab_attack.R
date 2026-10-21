# ---------------------------------------------------------------------------
# ATTACK DIRECTION
#
# The question this tab answers is "where does this hitter actually put the
# ball, and does it work from there". Everything else on it exists to keep that
# honest: tendency lines rather than mean lines, line/seam/cross left blank for
# attacks taken from the middle of the court where the terms have no meaning,
# and errors includable so a lane's volume is never read without its cost.
# ---------------------------------------------------------------------------

tab_attack <- function(F, o, coords_ok) {
  A <- attacks_of(F, o$phases)
  # Cards built on where the ball went are drawn only when some of it has a
  # position on the court. A match coded live has none, and six frames of
  # zeros push the numbers a coach needs off the screen.
  landed <- has_lines(A)
  # The blocker count is a coded field, not something this works out. Cal
  # Poly's own files record 2 on every attack because nobody changes the
  # default, so the card would be one row saying nothing. Shown only when the
  # file actually distinguishes.
  blk_varies <- length(unique(A$blockers[!is.na(A$blockers)])) > 1L
  if (!nrow(A)) return(empty("No attacks in the current filter."))
  shown <- if (isTRUE(o$aIncErr)) A else A[!(!is.na(A$effect) & A$effect %in% c("=", "/")), ]
  xy <- A[A$has_xy, ]
  st <- attack_line(A)
  rect <- list(x0 = 0, y0 = COURT$NET, x1 = COURT$W, y1 = COURT$L)
  mono <- isTRUE(o$mono)

  H <- hdr("Attacking",
           sprintf("%s attempts · %s with trajectory · contact→landing",
                   nrow(A), nrow(xy)))
  H <- c(H, kpis(list(
    list(k = "Attempts", v = st$ta),
    list(k = "Kills", v = st$k, s = pct(st$kpct)),
    list(k = "Errors", v = st$e + st$b, s = pct(st$epct)),
    list(k = "Blocked", v = st$b, s = pct(st$bpct)),
    list(k = "Hitting", v = eff(st$eff)),
    list(k = "Median depth", v = if (nrow(xy)) m1(med(xy$y1 - COURT$NET)) else "",
         s = "m past net")
  )))

  # Which view to draw. The count that matters is the one actually drawn,
  # so errors and blocks are in or out of it exactly as the toggle says.
  aView <- view_for(o$aView %||% "auto", sum(shown$has_xy))
  auto_grid <- identical(o$aView %||% "auto", "auto") && identical(aView, "heat")
  heat <- identical(aView, "heat")
  cells <- if (heat)
    cell_layer(grid_bins(shown[shown$has_xy, ], "x1", "y1", rect, o$aBins, o$aBins),
               o$aBins, o$aBins, rect, o$aColor) else ""
  marks <- if (identical(aView, "arrows"))
    paste0(arrow_layer(shown, "A", 1, mono,
                       extra = function(r) paste(vapply(r$y1 - COURT$NET, m1, ""), "m past net")),
           if (isTRUE(o$tend)) tendency_layer(shown, "A", 1, 2, "attack", mono = mono) else "")
  else if (identical(aView, "contact")) dot_layer(shown, "x0", "y0", 1)
  else ""

  court <- court_svg("A", x_min = -170, x_max = 1070,
                     y_min = if (heat) 855 else 300, y_max = 1990,
                     cells = cells, halves = if (heat) "opp" else "both",
                     labels = if (heat && o$aBins == 3) "" else zone_number_layer("opp", rect),
                     marks = marks, alt = "attack trajectories",
                     mono = mono, coords_ok = coords_ok)

  left <- plain_card(c(
    viewbar(vbtn("aView", "arrows", "Trajectories", aView),
            vbtn("aView", "heat", "Landing grid", aView),
            vbtn("aView", "contact", "Contact points", aView)),
    if (auto_grid)
      tiny(sprintf("%d attacks is too many arrows to read at once, so this is where they landed. Pick a hitter in the rail, or press Trajectories to draw them anyway.",
                   sum(shown$has_xy))) else "",
    viewbar(if (heat) c(vbtn("aBins", "3", "3×3 zones", as.character(o$aBins)),
                        vbtn("aBins", "6", "6×6", as.character(o$aBins)),
                        vbtn("aColor", "volume", "By volume", o$aColor),
                        vbtn("aColor", "eff", "By hitting %", o$aColor)) else character(),
            if (identical(aView, "arrows")) vtoggle("tend", "Tendency lines", o$tend) else character(),
            vtoggle("aIncErr", "Include errors &amp; blocks", o$aIncErr)),
    court,
    legend(c("var(--kill)", "kill"), c("var(--inplay)", "in play"),
           c("var(--blocked)", "blocked"), c("var(--err)", "error"),
           trailing = "net at bottom · dot = contact")
  ))
  left <- c(left, small_multiples(shown, "attack", coords_ok, mono))

  R <- list()

  R <- c(R, card("By hitter", tbl_attack(
    crosstab(A, paste(A$num, A$player_short, sep = "|")), A)))

  R <- c(R, card_extra(landed, "Landing third of the opponent court", tbl_generic(
    c("Third", "Zones", "Att", "Sh%", "K%", "Hit"),
    lapply(LAT, function(l) {
      rs <- xy[lat_third(xy$x1) == l$key, ]; s <- attack_line(rs)
      c(l$label, thin(l$zones), s$ta,
        pct(if (nrow(xy)) s$ta / nrow(xy) else NA), pct(s$kpct), eff_td(s$eff))
    }))))

  pins <- if (nrow(xy)) !is.na(rel_direction(xy$x0, xy$x1)) else logical(0)
  R <- c(R, card_extra(landed, "Line vs cross (pin attacks only)", c(tbl_generic(
    c("Direction", "Att", "Sh%", "K%", "Hit"),
    lapply(c("line", "seam", "cross"), function(d) {
      rs <- xy[!is.na(rel_direction(xy$x0, xy$x1)) & rel_direction(xy$x0, xy$x1) == d, ]
      s <- attack_line(rs); den <- sum(pins)
      c(paste0(toupper(substr(d, 1, 1)), substring(d, 2)), s$ta,
        pct(if (den) s$ta / den else NA), pct(s$kpct), eff_td(s$eff))
    })),
    tiny(sprintf("Origin band from contact x: left &lt;3 m, right &gt;6 m. Middle-of-court attacks are excluded, since line and cross have no meaning from there (%s attempts).",
                 sum(!pins))))))

  R <- c(R, card_extra(landed, "Depth", tbl_generic(
    c("Band", "Att", "Sh%", "K%", "Hit"),
    lapply(DEPTH, function(d) {
      rs <- xy[depth_of(xy$y1) == d$key, ]; s <- attack_line(rs)
      c(d$label, s$ta, pct(if (nrow(xy)) s$ta / nrow(xy) else NA),
        pct(s$kpct), eff_td(s$eff))
    }))))

  R <- c(R, middle_routes(A, coords_ok, mono))

  R <- c(R, card("By hit type", tbl_generic(
    c("Hit type", "Code", "Att", "K%", "Hit", "Most-used lane"),
    lapply(by_size(crosstab(A, A$season_call)), function(g) {
      rs <- g$rows[g$rows$has_xy, ]
      lanes <- by_size(crosstab(rs, lat_third(rs$x1)))
      lane <- if (length(lanes)) LAT[[match(lanes[[1]]$key, LAT_KEYS)]] else NULL
      c(sprintf("<b>%s</b>", esc(call_label(g$key, g$rows$call_name[1]))),
        thin(esc(raw_codes_of(g$rows$call))), g$stat$ta,
        pct(g$stat$kpct), eff_td(g$stat$eff),
        if (!is.null(lane)) sprintf("%s %s", lane$label,
                                    thin(pct(lanes[[1]]$stat$ta / nrow(rs)))) else "")
    }))))

  # SR vs transition, crossed against rotation and against hit type. FT (the
  # serving team's first attack) and TR (everything later) are both transition
  # in coaching terms, so they are combined into one Trans column, with the
  # split still available via the phase chips in the rail.
  phase_split <- function(rows, label) {
    sr <- attack_line(rows[!is.na(rows$phase) & rows$phase == "SR", ])
    tr <- attack_line(rows[!is.na(rows$phase) & rows$phase %in% c("FT", "TR"), ])
    all <- attack_line(rows)
    c(label, sr$ta, pct(sr$kpct), eff_td(sr$eff),
      tr$ta, pct(tr$kpct), eff_td(tr$eff), all$ta, eff_td(all$eff))
  }
  phase_head <- c("", "SR Att", "SR K%", "SR Hit", "Trans Att", "Trans K%",
                  "Trans Hit", "Total", "Total Hit")

  R <- c(R, card("Serve receive vs transition, by rotation", c(tbl_generic(
    phase_head,
    c(lapply(by_key(crosstab(A, A$rot)), function(g) phase_split(g$rows, g$key)),
      list(tot_row(phase_split(A, "All rotations"))))),
    tiny("SR = the team's first attack off their own serve receive. Trans = first transition and all later swings combined. Rotation is the setter's court position."))))

  R <- c(R, card("Serve receive vs transition, by hit type", tbl_generic(
    phase_head,
    c(lapply(by_size(crosstab(A, A$hit_type)), function(g)
        phase_split(g$rows, if (is.na(HITTYPE[g$key])) g$key else unname(HITTYPE[g$key]))),
      list(tot_row(phase_split(A, "All hit types")))))))

  R <- c(R, card("Hard vs tip", c(tbl_generic(
    c("Shot", "Att", "Sh%", "K%", "Hit", "Depth m"),
    lapply(by_size(crosstab(A, A$skill_type)), function(g) {
      gg <- g$rows[g$rows$has_xy, ]
      c(shot_label(g$key), g$stat$ta, pct(g$stat$ta / nrow(A)),
        pct(g$stat$kpct), eff_td(g$stat$eff),
        if (nrow(gg)) m1(med(gg$y1 - COURT$NET)) else "")
    })),
    tiny("Shot type comes from the coder's own skill-type entry. Tips land a median 3.4 m past the net against 7.0 m for hard swings, which is what identifies them."))))

  R <- c(R, card_extra(blk_varies, "Against the block", tbl_generic(
    c("Blockers", "Att", "K%", "Hit"),
    lapply(by_key(crosstab(A, A$blockers)), function(g)
      c(paste0(g$key, if (g$key == "1") " blocker" else " blockers"),
        g$stat$ta, pct(g$stat$kpct), eff_td(g$stat$eff))))))

  paste0(c(H, cols2(left, R, 460)), collapse = "")
}

#' One row per hitter: where they hit from, where it lands, and what it returns.
tbl_attack <- function(groups, all) {
  groups <- by_size(groups)
  rows <- lapply(groups, function(g) {
    parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
    xy <- g$rows[g$rows$has_xy, ]
    lat <- function(k) if (nrow(xy)) pct(sum(lat_third(xy$x1) == k) / nrow(xy)) else ""
    rd <- if (nrow(xy)) rel_direction(xy$x0, xy$x1) else character(0)
    pins <- sum(!is.na(rd))
    rel <- function(k) if (pins) pct(sum(!is.na(rd) & rd == k) / pins) else ""
    bands <- by_size(crosstab(xy, origin_band(xy$x0)))
    from <- if (length(bands)) {
      nm <- c(left = "z4 side", right = "z2 side", middle = "middle")[bands[[1]]$key]
      paste0(unname(nm), if (length(bands) > 1)
        paste0(" ", thin(pct(bands[[1]]$stat$ta / nrow(xy)))) else "")
    } else ""
    tl <- tip_line(g$rows)
    c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), from, g$stat$ta,
      pct(g$stat$kpct), pct(g$stat$epct), eff_td(g$stat$eff),
      pct(tl$tip_pct), rel("line"), rel("seam"), rel("cross"),
      lat("R"), lat("M"), lat("L"),
      if (nrow(xy)) m1(med(xy$y1 - COURT$NET)) else "")
  })
  t0 <- attack_line(all)
  rows <- c(rows, list(tot_row(c("Total", "", t0$ta, pct(t0$kpct), pct(t0$epct),
                                 eff_td(t0$eff), pct(tip_line(all)$tip_pct),
                                 "", "", "", "", "", "", ""))))
  paste0(tbl_generic(
    c("Hitter", "From", "Att", "K%", "E%", "Hit", "Tip%", "Line", "Seam", "Cross",
      "→Opp R", "→Mid", "→Opp L", "Depth m"), rows),
    tiny("Line / seam / cross are relative to the hitter's own contact point and are left blank for attacks taken from the middle of the court. The three arrow columns are absolute: the receiving team's right, middle and left thirds."))
}

#' Middle-attack routes: every call whose attack-combination definition targets
#' the middle (C) in this file's own coding scheme -- not a fixed list, so this
#' reflects whatever call vocabulary was used to code the match. Empty when the
#' file's metadata never targets 'C'; some schemes route middles elsewhere.
middle_routes <- function(A, coords_ok, mono = FALSE) {
  routes <- A[!is.na(A$target) & A$target == "C", ]
  if (!nrow(routes)) return("")
  key <- routes$season_call; key[is.na(key)] <- "(no call)"
  groups <- by_size(crosstab(routes, key))

  rows <- lapply(groups, function(g) {
    nm <- call_label(g$key, g$rows$call_name[1])
    zones <- sort(unique(paste0(g$rows$start_zone,
                                ifelse(is.na(g$rows$start_sub_zone), "", g$rows$start_sub_zone))))
    xs <- g$rows$x0[g$rows$has_xy]
    c(sprintf("<b>%s</b>", esc(g$key)), esc(nm), g$stat$ta,
      pct(g$stat$kpct), eff_td(g$stat$eff), paste(zones, collapse = ", "),
      if (length(xs)) (if (length(xs) > 1) sprintf("%s–%s", min(xs), max(xs)) else as.character(xs[1])) else "")
  })
  t0 <- attack_line(routes)
  rows <- c(rows, list(tot_row(c("Total", "", t0$ta, pct(t0$kpct), eff_td(t0$eff), "", ""))))

  runners <- by_size(crosstab(routes, paste(routes$num, routes$player_short, sep = "|")))
  run_by <- paste(vapply(runners, function(g)
    sprintf("%s (%s)", strsplit(g$key, "|", fixed = TRUE)[[1]][2], g$stat$ta), ""),
    collapse = ", ")

  smg <- if (coords_ok) paste0(vapply(seq_along(groups), function(i) {
    g <- groups[[i]]
    if (g$stat$ta < 2) return("")
    rs <- g$rows[g$rows$has_xy, ]
    svg <- court_svg(paste0("mr", i), x_min = -170, x_max = 1070,
                     y_min = 855, y_max = 1990, halves = "opp", scale = 2.1,
                     marks = arrow_layer(rs, paste0("mr", i), 2.1, mono),
                     alt = paste(g$key, "route"), mono = mono, coords_ok = coords_ok)
    sprintf('<div class="sm"><div class="nm"><span>%s</span><b>n=%s</b></div><div class="ln">%s · %s k · %s</div>%s</div>',
            esc(g$key), g$stat$ta,
            esc(g$rows$call_name[1] %||% ""), pct(g$stat$kpct), eff(g$stat$eff), svg)
  }, ""), collapse = "") else ""

  card(sprintf("Middle routes · %s attempts · run by %s", t0$ta, esc(run_by)),
       c(tbl_generic(c("Route", "Name", "Att", "K%", "Hit", "Approach zones",
                       "Contact x range"), rows),
         tiny("Routes are every call whose attack-combination definition targets the middle (C) in this file's own coding scheme. Approach zones are where the attacker's own contact was coded; contact x range shows how far left/right of centre that call is actually run from."),
         if (nzchar(smg)) sprintf('<div class="smgrid" style="margin-top:12px">%s</div>', smg) else ""))
}
