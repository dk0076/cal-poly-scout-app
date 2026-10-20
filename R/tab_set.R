# ---------------------------------------------------------------------------
# SET ORIGIN
#
# Where the setter touched the ball, and what she could run from there. The
# coordinates are the point of the tab: distance off the net and lateral
# position are the two things that actually change a setter's options, so the
# explorer at the bottom cuts by exactly those.
#
# Every "Atk" column here describes the swing the set produced, linked
# rally-by-rally in finalize_match(), not the set itself.
# ---------------------------------------------------------------------------

tab_set <- function(F, o, coords_ok) {
  E <- F[F$skill == "E", ]
  if (!nrow(E)) return(empty("No sets in the current filter."))
  xy <- E[E$has_xy, ]
  linked <- E[!is.na(E$atk_effect), ]
  s0 <- set_line(E)
  rect <- list(x0 = 0, y0 = 0, x1 = COURT$W, y1 = COURT$NET)
  offnet <- COURT$NET - xy$y0

  BANDS <- list(
    list(k = "tight",  l = "Tight ≤1.5 m",   t = function(p) COURT$NET - p$y0 <= 150),
    list(k = "normal", l = "Normal 1.5–3 m", t = function(p) { d <- COURT$NET - p$y0; d > 150 & d <= 300 }),
    list(k = "off",    l = "Off the net >3 m",    t = function(p) COURT$NET - p$y0 > 300)
  )

  H <- hdr("Set origin",
           sprintf("%s sets · %s with a contact point · %s linked to the attack they produced",
                   nrow(E), nrow(xy), nrow(linked)))
  H <- c(H, kpis(list(
    list(k = "Sets", v = nrow(E)),
    list(k = "Median off-net", v = if (length(offnet)) m1(med(offnet)) else "—", s = "m"),
    list(k = "Front-row band", v = pct(if (nrow(xy)) sum(xy$y0 >= 600) / nrow(xy) else NA)),
    list(k = "Attack kill% after", v = pct(s0$kpct)),
    list(k = "Attack hitting after", v = eff(s0$eff))
  )))

  marks <- if (identical(o$eView, "points")) set_target_layer(xy)
           else if (identical(o$eView, "toatk")) set_to_attack_layer(xy, "E")
           else ""
  cells <- if (identical(o$eView, "heat"))
    cell_layer(grid_bins(xy, "x0", "y0", rect, o$eBins, o$eBins),
               o$eBins, o$eBins, rect, "volume") else ""

  court <- court_svg("E", x_min = -260, x_max = 1160, y_min = -260, y_max = 985,
                     halves = "own", cells = cells,
                     labels = zone_number_layer("own", rect), marks = marks,
                     alt = "set contact points", coords_ok = coords_ok)

  left <- plain_card(c(
    viewbar(vbtn("eView", "points", "Contact points", o$eView),
            vbtn("eView", "toatk", "Set → hitter", o$eView),
            vbtn("eView", "heat", "Contact grid", o$eView),
            if (identical(o$eView, "heat"))
              c(vbtn("eBins", "3", "3×3 zones", as.character(o$eBins)),
                vbtn("eBins", "6", "6×6", as.character(o$eBins))) else character()),
    court,
    if (identical(o$eView, "points"))
      swatch_legend(c("var(--accent)", "outside"), c("var(--kill)", "middle"),
                    c("var(--blocked)", "opposite"), c("var(--err)", "pipe"),
                    trailing = "net at top · own half only")
    else
      legend(c("var(--kill)", "kill"), c("var(--inplay)", "in play"),
             c("var(--blocked)", "blocked"), c("var(--err)", "error"))
  ))
  left <- c(left, setter_multiples(xy, coords_ok))

  R <- list()

  R <- c(R, card("By setter", tbl_generic(
    c("Setter", "Sets", "Off-net m", "x spread", "Out", "Mid", "Opp", "Pipe",
      "Atk K%", "Atk hit"),
    lapply(by_size(crosstab(E, paste(E$num, E$player_short, sep = "|"), set_line), "n"),
      function(g) {
        parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
        gg <- g$rows[g$rows$has_xy, ]
        t0 <- function(k) pct(sum(!is.na(g$rows$target) & g$rows$target == k) / nrow(g$rows))
        xs <- gg$x0
        c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), g$stat$n,
          if (nrow(gg)) m1(med(COURT$NET - gg$y0)) else "—",
          if (length(xs)) sprintf("%s–%s", m1(min(xs)), m1(max(xs))) else "—",
          t0("F"), t0("C"), t0("B"), t0("P"), pct(g$stat$kpct), eff_td(g$stat$eff))
      }))))

  R <- c(R, setter_calls_card(E))

  row_for <- function(label, rs) {
    s <- set_line(rs)
    t0 <- function(k) if (nrow(rs)) pct(sum(!is.na(rs$target) & rs$target == k) / nrow(rs)) else "—"
    c(label, nrow(rs), pct(if (nrow(xy)) nrow(rs) / nrow(xy) else NA),
      t0("F"), t0("C"), t0("B"), t0("P"), pct(s$kpct), eff_td(s$eff))
  }
  R <- c(R, card("Where the set came from → who got it", c(tbl_generic(
    c("Origin", "Sets", "Sh%", "Out", "Mid", "Opp", "Pipe", "Atk K%", "Atk hit"),
    c(lapply(list(c("right", "Right of centre (z2 side)"), c("middle", "Centre (z3)"),
                  c("left", "Left of centre (z4 side)")),
             function(kl) row_for(kl[2], xy[origin_band(xy$x0) == kl[1], ])),
      lapply(BANDS, function(b) row_for(b$l, xy[b$t(xy), ])))),
    tiny("Top three rows split the contact by lateral position, the bottom three by distance off the net. Attack columns describe the swing that set produced."))))

  R <- c(R, card("Origin zone", c(tbl_generic(
    c("Zone", "Sets", "Sh%", "Atk K%", "Atk hit"),
    lapply(by_size(crosstab(xy, zone_or_coded(xy$start_zone, xy$x0, xy$y0, "own"),
                            set_line), "n"), function(g)
      c(paste("Zone", g$key), g$stat$n, pct(g$stat$n / nrow(xy)),
        pct(g$stat$kpct), eff_td(g$stat$eff)))),
    tiny("The zone the coder entered on the set, where there is one; otherwise the zone its contact point falls in."))))

  R <- c(R, card("Target attacker", tbl_generic(
    c("Target", "Sets", "Sh%", "Median off-net m", "Atk K%", "Atk hit"),
    lapply(by_size(crosstab(E, E$target, set_line), "n"), function(g) {
      gg <- g$rows[g$rows$has_xy, ]
      c(if (is.na(TARGET[g$key])) g$key else unname(TARGET[g$key]),
        g$stat$n, pct(g$stat$n / nrow(E)),
        if (nrow(gg)) m1(med(COURT$NET - gg$y0)) else "—",
        pct(g$stat$kpct), eff_td(g$stat$eff))
    }))))

  # ---- interactive explorer -------------------------------------------
  setters <- by_size(crosstab(E, paste(E$num, E$player_short, sep = "|"), set_line), "n")
  setters <- setters[vapply(setters, function(g) g$stat$n >= 3, logical(1))]
  pick <- o$setSetter %||% ""
  pool <- xy[nzchar(pick) == FALSE | paste(xy$num, xy$player_short, sep = "|") == pick, ]
  zsel <- o$setZones %||% character()
  sz <- set_zone_of(pool)
  sel <- if (length(zsel)) pool[!is.na(sz) & sz %in% zsel, ] else pool
  st_sel <- set_line(sel)

  grid <- court_svg("Ez", x_min = -120, x_max = 1020, y_min = 300, y_max = 1010,
                    halves = "own",
                    marks = paste0(set_target_layer(sel, r = 7), set_zone_grid(zsel)),
                    alt = "set origin zones", coords_ok = coords_ok)

  R <- c(R, sprintf('<div class="card"><h3>Explore by set origin</h3>%s%s%s%s</div>',
    viewbar(sprintf('<button class="chip %s" data-o="setSetter" data-v="">All setters</button>',
                    if (!nzchar(pick)) "on" else ""),
            paste0(vapply(setters, function(g) {
              parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
              sprintf('<button class="chip %s" data-o="setSetter" data-v="%s">%s %s <span class="n">%s</span></button>',
                      if (identical(pick, g$key)) "on" else "", esc(g$key),
                      esc(parts[2]), thin(paste0("#", parts[1])), g$stat$n)
            }, ""), collapse = "")),
    viewbar(sprintf('<span class="tiny" style="align-self:center">Click zones · %s</span>',
                    if (length(zsel)) paste(length(zsel), "selected") else "all shown"),
            if (length(zsel)) '<button class="chip" data-zoneclear="1">Clear zones</button>' else ""),
    grid,
    tiny("Zones are the setter's own half: lane across, distance from the net down. Selecting none shows everything.")))

  by_type <- by_size(crosstab(sel[!is.na(sel$atk_call), ],
                              sel$atk_season_call[!is.na(sel$atk_call)], set_line), "n")
  no_call <- sum(is.na(sel$atk_call))
  R <- c(R, card(sprintf("What got set from here — %s set%s", st_sel$n,
                         if (st_sel$n == 1) "" else "s"),
    c(tbl_generic(c("Hit type", "Sets", "Sh%", "Atk K%", "Atk hit"),
        c(lapply(by_type, function(g)
            c(sprintf("<b>%s</b>", esc(call_label(g$key, g$rows$atk_call_name[1]))),
              g$stat$n, pct(g$stat$n / max(1, nrow(sel))),
              pct(g$stat$kpct), eff_td(g$stat$eff))),
          if (no_call) list(c(thin("no attack / not coded"), no_call,
                              pct(no_call / max(1, nrow(sel))), "—", "—")) else list())),
      if (!nrow(sel)) '<div class="tiny">No sets in the selected zones.</div>' else "")))

  # Season codes that actually appear under a target letter, commonest first.
  codes_for <- function(k, max_n = 3) {
    rs <- E[!is.na(E$target) & E$target == k & !is.na(E$atk_call), ]
    if (!nrow(rs)) return(character())
    tb <- sort(table(rs$atk_season_call), decreasing = TRUE)
    names(tb)[seq_len(min(max_n, length(tb)))]
  }
  col_head <- function(k, label) {
    cs <- codes_for(k)
    if (!length(cs)) label else
      sprintf('%s<br><span class="thin" style="font-weight:400">%s</span>',
              label, esc(paste(cs, collapse = ", ")))
  }

  R <- c(R, card("Distribution by rotation", c(tbl_generic(
    c("Rotation", "Sets", col_head("F", "Out"), col_head("C", "Mid"),
      col_head("B", "Opp"), col_head("P", "Back row"), col_head("S", "Setter"),
      "No code", "Atk hit"),
    lapply(by_key(crosstab(E, E$rot, set_line)), function(g) {
      t0 <- function(k) pct(sum(!is.na(g$rows$target) & g$rows$target == k) / nrow(g$rows))
      none <- sum(is.na(g$rows$target))
      c(g$key, g$stat$n, t0("F"), t0("C"), t0("B"), t0("P"), t0("S"),
        if (none) thin(pct(none / nrow(g$rows))) else "—", eff_td(g$stat$eff))
    })),
    tiny("Each column is the share of that rotation's sets whose target code matches. Columns cover every set, so rows total 100%."))))

  # The column labels are only meaningful if you can see who is actually in
  # each bucket -- "Opp" reading 0% means nothing until you know whether the
  # opposite was unset or simply off the court.
  code_rows <- list(list("F", "Out"), list("C", "Mid"), list("B", "Opp"),
                    list("P", "Back row"), list("S", "Setter"),
                    list(NA_character_, "No code"))
  rots <- paste0("R", 1:6)
  wrows <- Filter(Negate(is.null), lapply(code_rows, function(kl) {
    k <- kl[[1]]; label <- kl[[2]]
    rs <- if (is.na(k)) E[is.na(E$target), ] else E[!is.na(E$target) & E$target == k, ]
    if (!nrow(rs)) return(NULL)
    who_g <- by_size(crosstab(rs[!is.na(rs$atk_num), ],
                              paste(rs$atk_num[!is.na(rs$atk_num)],
                                    rs$atk_player[!is.na(rs$atk_num)])), "ta")
    who <- paste(vapply(who_g[seq_len(min(4, length(who_g)))], function(g)
      sprintf("%s %s", esc(g$key), thin(nrow(g$rows))), ""), collapse = ", ")
    spread <- paste(Filter(nzchar, vapply(rots, function(r) {
      n <- sum(!is.na(rs$rot) & rs$rot == r)
      if (n) sprintf("%s·%s", r, n) else ""
    }, "")), collapse = "  ")
    cs <- if (is.na(k)) character() else codes_for(k, 6)
    c(sprintf("<b>%s</b> %s", label, thin(if (is.na(k)) "—" else k)), nrow(rs),
      if (length(cs)) paste(vapply(cs, function(c0)
        sprintf("%s %s", esc(c0), thin(esc(call_label(c0)))), ""), collapse = ", ")
      else thin("—"),
      if (nzchar(who)) who else thin("no attack linked"),
      thin(spread))
  }))
  R <- c(R, card("What each column counts", c(tbl_generic(
    c("Column", "Sets", "Calls it contains", "Who received them", "By rotation"), wrows),
    tiny("The target code is entered by the coder on the set itself — it is a label for the intended attacker, not derived from where the ball went. A column reading 0% in some rotation usually means that attacker was in the back row or substituted out, so check the rotation spread before reading it as a tendency."))))

  paste0(c(H, cols2(left, R, 470)), collapse = "")
}

#' What a setter's call actually produces.
#'
#' The call lives on the SET play; the resulting attack gets its OWN,
#' separately-coded classification. The two are not the same field and do not
#' always agree -- a called quick can turn into a bailout swing if the pass or
#' set is off. This shows what each call actually became, not what it was
#' designed to be.
setter_calls_card <- function(E) {
  called <- E[!is.na(E$setter_call), ]
  if (!nrow(called)) return("")
  key <- called$setter_call
  mapped <- VM_SETTER_TO_SEASON[key]
  key[!is.na(mapped)] <- mapped[!is.na(mapped)]
  groups <- by_size(crosstab(called, key))

  rows <- lapply(groups, function(g) {
    # The file's own setter-call table wins, exactly as it does for attack
    # combinations: the built-in vocabulary in core.R does not cover every call
    # a staff uses, and a name read off the file is never the wrong one.
    own <- g$rows$call_name[!is.na(g$rows$call_name)]
    known <- if (length(own)) own[1] else setter_call_label(g$key)
    linked <- g$rows[!is.na(g$rows$atk_call), ]
    outs <- by_size(crosstab(linked, linked$atk_call))
    top <- paste(vapply(outs[seq_len(min(3, length(outs)))], function(x)
      sprintf("%s %s", esc(x$key), pct(x$stat$ta / nrow(linked))), ""), collapse = ", ")
    lk <- linked[!is.na(linked$atk_effect), ]
    k <- sum(lk$atk_effect == "#"); e <- sum(lk$atk_effect %in% c("=", "/"))
    eff_val <- if (nrow(lk)) (k - e) / nrow(lk) else NA_real_
    c(sprintf("<b>%s</b>%s", esc(g$key), if (nzchar(known)) paste0(" ", thin(esc(known))) else ""),
      g$stat$ta,
      if (nrow(linked)) pct(nrow(linked) / g$stat$ta) else "—",
      if (nzchar(top)) top else "—",
      pct(if (nrow(lk)) k / nrow(lk) else NA), eff_td(eff_val))
  })

  card("Setter calls → what actually resulted", c(tbl_generic(
    c("Call", "Called", "Became an attack", "Resulting combo(s)", "Atk K%", "Atk hit"), rows),
    tiny("The call is recorded on the setter's own play; \"resulting combo(s)\" is whatever code the attack that followed was independently classified as — often different from the call, since a called route can break down into something else. Calls not defined in this file's own combination table are labelled from the program's known call vocabulary where recognised.")))
}
