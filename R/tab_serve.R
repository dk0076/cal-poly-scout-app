# ---------------------------------------------------------------------------
# SERVING
#
# Contact behind the endline -> landing point. Eff and Op reproduce
# VolleyStation's own report columns exactly, so these two can be read against
# their numbers; everything else on the tab is geometry off the coordinates.
#
# The passer-start marks are the game-prep read: against this server, where
# should our passer stand. They come from her in-court serves only, because a
# serve into the stands is the server's problem and not the passer's.
# ---------------------------------------------------------------------------

tab_serve <- function(F, o, coords_ok) {
  V0 <- F[F$skill == "S", ]
  if (!nrow(V0)) return(empty("No serves in the current filter."))
  tkeys <- unique(srv_type_of(V0$hit_type, V0$skill_type))
  sel_type <- if (identical(o$srvType, "all") || !(o$srvType %in% tkeys)) "all" else o$srvType
  V <- if (sel_type == "all") V0 else V0[srv_type_of(V0$hit_type, V0$skill_type) == sel_type, ]
  landed <- has_lines(V)
  xy <- V[V$has_xy, ]
  st <- serve_line(V)
  rect <- list(x0 = 0, y0 = COURT$NET, x1 = COURT$W, y1 = COURT$L)
  in_court <- xy[!out_of_court(xy$x1, xy$y1), ]
  mono <- isTRUE(o$mono)

  H <- hdr("Serving",
           sprintf("%s serves%s · contact behind the endline → landing point",
                   nrow(V),
                   if (sel_type == "all") "" else paste0(" · ", esc(srv_type_name(sel_type)))))

  ordered <- c(c("TS", "JF", "SF"), sort(setdiff(tkeys, c("TS", "JF", "SF"))))
  chip <- function(k, l, n)
    sprintf('<button class="chip wide %s" data-o="srvType" data-v="%s">%s<span class="n">%s</span></button>',
            if (identical(sel_type, k)) "on" else "", k, esc(l), n)
  H <- c(H, sprintf('<div class="chips" style="margin:0 0 10px">%s%s</div>',
                    chip("all", "All types", nrow(V0)),
                    paste0(vapply(ordered, function(k)
                      chip(k, srv_type_name(k),
                           sum(srv_type_of(V0$hit_type, V0$skill_type) == k)), ""),
                      collapse = "")))

  H <- c(H, kpis(list(
    list(k = "Serves", v = st$n),
    list(k = "Eff", v = eff(st$eff), s = "VolleyStation"),
    list(k = "Serve average", v = if (is.na(st$avg)) "" else sprintf("%.2f", st$avg),
         s = "staff scale"),
    list(k = "Aces", v = st$ace, s = pct(st$acepct)),
    list(k = "Errors", v = st$err, s = pct(st$errpct)),
    list(k = "Op", v = st$op, s = "no attack"),
    # What the serve did to their pass, which is the question a serving plan
    # is actually about. Aces and no-attack say the serve won something; this
    # says how often it took their first option away.
    list(k = "Perfect against", v = pct(st$opp_perfpct),
         s = sprintf("%d of %d passes", st$opp_perf, st$rec_n)),
    list(k = "KO%", v = pct(st$kopct), s = "not # or +"),
    list(k = "Median depth",
         v = if (!is.na(st$med_depth)) m1(st$med_depth - COURT$NET) else "",
         s = "m past net")
  )))

  # Same rule as the attack chart: arrows while they are readable, the
  # landing grid when a whole team's serves would be a ball of wool.
  sView <- view_for(o$sView %||% "auto", sum(V$has_xy))
  auto_grid <- identical(o$sView %||% "auto", "auto") && identical(sView, "heat")
  heat <- identical(sView, "heat")
  cells <- if (heat)
    cell_layer(grid_bins(xy, "x1", "y1", rect, o$sBins, o$sBins),
               o$sBins, o$sBins, rect, "volume") else ""
  ps <- passer_start(V)
  marks <- paste0(
    if (identical(sView, "arrows"))
      arrow_layer(xy, "S", 1, mono,
                  extra = function(r) paste(vapply(r$y1 - COURT$NET, m1, ""), "m past net"))
    else "",
    if (identical(sView, "arrows") && isTRUE(o$tend))
      tendency_layer(V, "S", 1, 2, "serve", mono = mono) else "",
    if (isTRUE(o$sPass)) passer_start_layer(ps, 1, mono) else "")

  court <- court_svg("S", x_min = -170, x_max = 1070, y_min = -260, y_max = 1990,
                     cells = cells, labels = zone_number_layer("opp", rect),
                     marks = marks, alt = "serve trajectories",
                     mono = mono, coords_ok = coords_ok)

  left <- plain_card(c(
    viewbar(vbtn("sView", "arrows", "Trajectories", sView),
            vbtn("sView", "heat", "Landing grid", sView),
            if (heat) c(vbtn("sBins", "3", "3×3 zones", as.character(o$sBins)),
                        vbtn("sBins", "6", "6×6", as.character(o$sBins))) else character(),
            if (identical(sView, "arrows")) vtoggle("tend", "Tendency lines", o$tend) else character(),
            vtoggle("sPass", "Passer start", o$sPass)),
    if (auto_grid)
      tiny(sprintf("%d serves is too many arrows to read at once, so this is where they landed. Pick a server in the rail, or press Trajectories to draw them anyway.",
                   sum(V$has_xy))) else "",
    court,
    legend(c("var(--kill)", "ace"), c("var(--inplay)", "returned"),
           c("var(--blocked)", "opponent scramble"), c("var(--err)", "error"),
           trailing = if (isTRUE(o$sPass)) "P = recommended passer start, box = middle half of landings" else NULL),
    if (isTRUE(o$sPass)) tiny(esc(passer_start_text(ps))) else ""
  ))
  left <- c(left, small_multiples(xy, "serve", coords_ok, mono))

  R <- list()

  R <- c(R, card("By serve type", c(tbl_generic(
    c("Type", "Srv", "Sh%", "Ace", "Ace%", "Err", "Err%", "Op", "Eff", "Depth m"),
    lapply(ordered, function(k) {
      rs <- V0[srv_type_of(V0$hit_type, V0$skill_type) == k, ]
      s <- serve_line(rs)
      ic <- rs$y1[rs$has_xy & !out_of_court(rs$x1, rs$y1)]
      c(esc(srv_type_name(k)), s$n, pct(s$n / nrow(V0)), s$ace, pct(s$acepct),
        s$err, pct(s$errpct), s$op, if (s$n) eff_td(s$eff) else "",
        if (length(ic)) m1(med(ic) - COURT$NET) else "")
    })),
    tiny("Ignores the type chips above. Codes: top spin = Q or T, jump float = M, standing float = H. Not coded = no type on the serve."))))

  # A rate off three passes is not a rate. Rather than hide the row or set a
  # threshold that quietly drops a server, the two pass columns go grey below
  # a handful of passes and say how few they came from. The number is still
  # there for anyone who wants it, and nobody reads 100% off two serves by
  # accident.
  PASS_FLOOR <- 5L
  pass_pct <- function(v, n)
    if (is.na(v)) "" else if (n < PASS_FLOOR)
      sprintf('<span class="thin" title="off %d pass%s">%s</span>',
              n, if (n == 1L) "" else "es", pct(v))
    else pct(v)

  R <- c(R, card("By server", c(tbl_generic(
    c("Server", "Type", "Eff", "Srv", "Ace", "%Ace", "Err", "%Err", "Op",
      "Pass #%", "KO%",
      "→Opp R", "→Mid", "→Opp L", "Depth m"),
    lapply(by_size(crosstab(V, paste(V$num, V$player_short, sep = "|"), serve_line), "n"),
      function(g) {
        parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
        gg <- g$rows[g$rows$has_xy & !out_of_court(g$rows$x1, g$rows$y1), ]
        lane <- function(k) if (nrow(gg)) pct(sum(lat_third(gg$x1) == k) / nrow(gg)) else ""
        mix <- paste(vapply(by_size(crosstab(g$rows, srv_type_of(g$rows$hit_type, g$rows$skill_type), serve_line), "n"),
          function(t) sprintf("%s %d%%",
                              if (startsWith(t$key, "X:")) substring(t$key, 3) else t$key,
                              round(100 * nrow(t$rows) / nrow(g$rows))), ""),
          collapse = " · ")
        c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), thin(mix),
          eff_td(g$stat$eff), g$stat$n, g$stat$ace, pct(g$stat$acepct),
          g$stat$err, pct(g$stat$errpct), g$stat$op,
          pass_pct(g$stat$opp_perfpct, g$stat$rec_n),
          pass_pct(g$stat$kopct, g$stat$rec_n),
          lane("R"), lane("M"), lane("L"),
          if (!is.na(g$stat$med_depth)) m1(g$stat$med_depth - COURT$NET) else "")
      })),
    tiny("Eff and Op reproduce VolleyStation's report exactly: Eff = (aces + no-attack + positive − errors) ÷ serves, Op = serves after which the receiving team could not set up an attack. Pass #% is how often the receiving team passed her serve perfect, and KO% how often they did not pass it # or +, which is VolleyStation's positive reception inverted. Both count the serves that drew a pass, so a serve into the net is in neither, and both go grey under five passes, where a percentage is not yet a rate. Lane columns are the receiving team's right / middle / left thirds, zones 1·9·2, 6·8·3 and 5·7·4."))))

  R <- c(R, card("By target zone", c(tbl_generic(
    c("Zone", "", "Srv", "Sh%", "Ace", "Err", "Ace%"),
    lapply(by_key(crosstab(in_court,
                           srv_zone6_or_coded(in_court$end_zone, in_court$x1, in_court$y1),
                           serve_line)),
      function(g) c(paste("Zone", g$key), thin(esc(SRV_ZONE_NAME[[g$key]] %||% "")),
                    g$stat$n, pct(g$stat$n / nrow(in_court)), g$stat$ace,
                    g$stat$err, pct(g$stat$acepct)))),
    tiny("Zones are the six standard receiving positions seen from the receiving team: 1 right back, 2 right front, 3 middle front, 4 left front, 5 left back, 6 middle back. Taken from the zone the coder entered where there is one, otherwise from the landing coordinate."))))

  R <- c(R, card_extra(landed, "By depth band", tbl_generic(
    c("Band", "Srv", "Sh%", "Ace%", "Err%"),
    lapply(DEPTH, function(d) {
      rs <- in_court[depth_of(in_court$y1) == d$key, ]; s <- serve_line(rs)
      c(d$label, s$n, pct(if (nrow(in_court)) s$n / nrow(in_court) else NA),
        pct(s$acepct), pct(s$errpct))
    }))))

  R <- c(R, card_extra(landed, "Serving position behind the endline", tbl_generic(
    c("Start", "Srv", "Sh%", "Ace%", "Err%", "Median target"),
    lapply(list(c("left", "Left (z5 side)"), c("middle", "Middle (z6)"),
                c("right", "Right (z1 side)")), function(kl) {
      rs <- xy[origin_band(xy$x0) == kl[1], ]; s <- serve_line(rs)
      t0 <- rs[!out_of_court(rs$x1, rs$y1), ]
      c(kl[2], s$n, pct(if (nrow(xy)) s$n / nrow(xy) else NA),
        pct(s$acepct), pct(s$errpct),
        if (nrow(t0)) sprintf("x=%.0f %s", med(t0$x1),
                              thin(sprintf("(%s m from left sideline)", m1(med(t0$x1))))) else "")
    }))))

  paste0(c(H, cols2(left, R, 420)), collapse = "")
}
