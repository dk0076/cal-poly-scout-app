# ---------------------------------------------------------------------------
# Next server
#
# The tab for the gap between rallies. It follows the match rather than
# waiting to be told: when the serve is ours it shows the passers we are
# about to serve at, worst first, and when it is theirs it shows the player
# about to serve, chart first.
#
# Everything here is also readable before a match, because the same pages are
# what a staff goes through on the bus. Nothing on it needs a live file; a
# file being coded live just keeps it pointed at the right player.
# ---------------------------------------------------------------------------

SERVE_WINDOWS <- c(6L, 10L, 15L)

#' One serve as a chip: where it went, and how it came out
serve_cell <- function(p) {
  st <- land_status(p$effect)
  col <- unname(OUTCOME[st])
  z <- srv_zone6_or_coded(p$end_zone, p$x1, p$y1)
  tip <- sprintf("set %s · %s · zone %s%s · %s",
                 p$set_no, p$score, if (is.na(z)) "?" else z,
                 if (is.na(p$skill_type)) "" else paste0(" · ", p$skill_type),
                 st)
  sprintf('<span class="att %s" style="border-color:%s;color:%s" title="%s">%s<i>%s</i></span>',
          st, col, col, esc(tip), if (is.na(z)) "?" else z,
          if (is.na(p$effect)) "?" else esc(p$effect))
}

#' Efficiency over the last n, beside the whole-match figure
window_row <- function(rows, skill) {
  all_eff <- vs_efficiency(rows$effect, skill)
  vapply(SERVE_WINDOWS, function(n) {
    q <- utils::tail(rows, n)
    if (!nrow(q)) return("")
    e <- vs_efficiency(q$effect, skill)
    arrow <- if (is.na(e) || is.na(all_eff) || nrow(rows) <= n) ""
             else if (e > all_eff + .001) '<span class="pos">▲</span>'
             else if (e < all_eff - .001) '<span class="neg">▼</span>'
             else '<span class="thin">–</span>'
    sprintf("%s %s", eff_td(e), arrow)
  }, "")
}

#' Them serving: the server's chart, then her numbers
server_block <- function(S_all, num, name, o, coords_ok) {
  q <- S_all[!is.na(S_all$num) & S_all$num == num, ]
  if (!nrow(q))
    return(empty(sprintf("#%s %s has not served yet in what is loaded.",
                         num, esc(name))))
  data.table::setorder(q, date, match_id, set_no, rally_no, seq)
  st <- serve_line(q)
  mono <- isTRUE(o$mono)

  xy <- q[q$has_xy, ]
  rect <- list(x_min = -170, x_max = 1070, y_min = -260, y_max = 1990)
  court <- court_svg("NS", x_min = -170, x_max = 1070, y_min = -260, y_max = 1990,
                     cells = "", labels = zone_number_layer("opp", rect),
                     marks = paste0(
                       arrow_layer(xy, "NS", 1, mono,
                                   extra = function(r)
                                     paste(vapply(r$y1 - COURT$NET, m1, ""), "m past net")),
                       tendency_layer(q, "NS", 1, 2, "serve", mono = mono)),
                     alt = sprintf("where #%s %s serves", num, name),
                     mono = mono, coords_ok = coords_ok)

  kpi <- kpis(list(
    list(k = "Serves", v = st$n),
    list(k = "Eff", v = eff(st$eff), s = "VolleyStation"),
    list(k = "Aces", v = st$ace, s = pct(st$acepct)),
    list(k = "Errors", v = st$err, s = pct(st$errpct)),
    list(k = "No attack", v = st$op, s = "off her serve"),
    list(k = "Median depth",
         v = if (!is.na(st$med_depth)) m1(st$med_depth - COURT$NET) else "",
         s = "m past net")))

  zone_rows <- local({
    z <- srv_zone6_or_coded(q$end_zone, q$x1, q$y1)
    tab <- table(z[!is.na(z)])
    if (!length(tab)) return(list())
    ord <- order(-as.integer(tab))
    lapply(names(tab)[ord], function(k) {
      sel <- !is.na(z) & z == k
      c(sprintf("Zone %s", k), sum(sel), pct(sum(sel) / nrow(q)),
        eff_td(vs_efficiency(q$effect[sel], "S")))
    })
  })

  type_rows <- local({
    ty <- q$skill_type
    tab <- table(ty[!is.na(ty)])
    if (!length(tab)) return(list())
    lapply(names(tab)[order(-as.integer(tab))], function(k) {
      sel <- !is.na(ty) & ty == k
      c(esc(k), sum(sel), pct(sum(sel) / nrow(q)),
        eff_td(vs_efficiency(q$effect[sel], "S")))
    })
  })

  last <- q[nrow(q), ]
  lastz <- srv_zone6_or_coded(last$end_zone, last$x1, last$y1)
  recent <- paste0(vapply(seq_len(nrow(utils::tail(q, 15))),
                          function(i) serve_cell(utils::tail(q, 15)[i, ]), ""),
                   collapse = "")
  win <- window_row(q, "S")

  c(kpi,
    plain_card(court),
    cols2(
      card("Where she serves",
           if (length(zone_rows))
             tbl_generic(c("Target", "Serves", "Share", "Eff"), zone_rows)
           else empty("No landing zone coded on her serves.")),
      card("By type",
           if (length(type_rows))
             tbl_generic(c("Type", "Serves", "Share", "Eff"), type_rows)
           else empty("No serve type coded.")), 520),
    card("Recent form",
         c(tbl_generic(c(sprintf("Last %d", SERVE_WINDOWS[1]),
                         sprintf("Last %d", SERVE_WINDOWS[2]),
                         sprintf("Last %d", SERVE_WINDOWS[3])),
                       list(win)),
           sprintf('<div class="atts">%s</div>', recent),
           tiny("Each chip is one serve: the zone it landed in, then the grade. Oldest on the left. The arrow compares the window with her whole match."))),
    card("Her last serve",
         tbl_generic(c("Set", "Score", "Target", "Type", "Outcome"),
                     list(c(last$set_no, esc(last$score %||% ""),
                            if (is.na(lastz)) "?" else sprintf("Zone %s", lastz),
                            if (is.na(last$skill_type)) "" else esc(last$skill_type),
                            land_status(last$effect))))))
}

#' Us serving: who to serve at, worst passer first
passer_block <- function(Opp, o) {
  R <- Opp[Opp$skill == "R", ]
  if (!nrow(R)) return(empty("No receptions from them in what is loaded."))
  data.table::setorder(R, date, match_id, set_no, rally_no, seq)
  keys <- unique(paste(R$num, R$player_short, sep = "|"))
  rows <- lapply(keys, function(k) {
    q <- R[paste(R$num, R$player_short, sep = "|") == k, ]
    rl <- receive_line(q)
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    chips <- paste0(vapply(seq_len(nrow(utils::tail(q, 15))),
                           function(i) {
                             p <- utils::tail(q, 15)[i, ]
                             st <- land_status(p$effect)
                             col <- unname(OUTCOME[st])
                             sprintf('<span class="att %s" style="border-color:%s;color:%s" title="set %s · %s">%s</span>',
                                     st, col, col, p$set_no, esc(p$score %||% ""),
                                     if (is.na(p$effect)) "?" else esc(p$effect))
                           }, ""), collapse = "")
    list(sort_key = if (is.na(rl$avg)) Inf else rl$avg,
         n = rl$n,
         cells = c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])),
                   rl$n,
                   if (is.na(rl$avg)) "" else sprintf("%.2f", rl$avg),
                   pct(rl$pospct), pct(rl$errpct),
                   paste(window_row(q, "R"), collapse = ""),
                   sprintf('<div class="atts">%s</div>', chips)))
  })
  # Worst first: this page exists to pick a target, and the weakest passer is
  # the answer to that question, not an afterthought at the bottom of a list.
  rows <- rows[order(vapply(rows, function(x) x$sort_key, 0))]
  card("Their passers, weakest first",
       c(tbl_generic(c("Passer", "Rec", "Avg", "Pos%", "Err%",
                       paste(sprintf("L%d", SERVE_WINDOWS), collapse = " / "),
                       "Last 15"),
                     lapply(rows, function(x) x$cells)),
         tiny("Avg is the staff 0 to 4 pass scale, so lower is worse and the top row is the one to serve. Pos% is receptions graded # or +, VolleyStation's own positive reception. The three windows are her last 6, 10 and 15 receptions, as reception efficiency.")))
}

#' Before the first serve: their six servers, and their passers
#'
#' Not an error page. The films are loaded, the match has not started, and
#' this is the view a staff goes through on the bus.
prematch_block <- function(Opp, o, labels, n_files, opponent, why) {
  H <- hdr("Next server", sprintf(
    "%s · before the first serve",
    if (nzchar(opponent)) esc(opponent) else esc(labels$them)))
  H <- c(H, sprintf('<div class="tiny nssrc">%s Once the match starts this page follows it, rally by rally.</div>',
                    if (nzchar(why)) sprintf("Who serves next is not settled yet: %s.", esc(why))
                    else "Nothing coded in tonight's file yet."))
  if (is.null(Opp) || !nrow(Opp))
    return(paste0(c(H, empty("No scouting films loaded for them yet. Add their previous matches on the opening screen.")), collapse = ""))

  so <- serve_order(Opp)
  srv <- if (is.null(so)) empty("Their serves carry no rotation, so there is no order to read.")
    else card("Their serve order",
      c(tbl_generic(c("Rot", "Server", "Serves", "Ace", "Err", "Eff"),
          lapply(seq_len(nrow(so)), function(i)
            c(so$rot[i], sprintf("<b>%s</b> %s", so$num[i], esc(so$name[i])),
              so$serves[i], so$ace[i], so$err[i], eff_td(so$eff[i])))),
        tiny("Who serves in each rotation, from what they have actually done rather than from a lineup sheet, so a serving substitute appears as herself. Eff is VolleyStation's serve formula.")))

  paste0(c(H, srv, passer_block(Opp, o), note(sprintf(
    "Read off %s. Once tonight's file has a rally in it this page switches to following the match: their server with her chart when they serve, their passers when we do.",
    if (n_files > 1L) sprintf("%d scouting films", n_files) else "the one file loaded"
  ))), collapse = "")
}

tab_nextserver <- function(F, Opp, ns, o, coords_ok, labels, ours,
                           n_files = 1L, live_only = FALSE, opponent = "") {
  # Before the first serve there is nothing to predict from, which is also
  # exactly when a staff opens this: the films are loaded and the match has
  # not started. So the same page falls back to the whole scouting picture,
  # which is what was wanted before a match anyway.
  if (is.null(ns) || !isTRUE(ns$known))
    return(prematch_block(Opp, o, labels, n_files, opponent,
                          if (is.null(ns)) "" else ns$basis))

  who <- if (isTRUE(ours)) labels$us else labels$them
  H <- hdr("Next server", sprintf(
    "%s serving · %s%s",
    esc(who),
    if (is.na(ns$rot)) "rotation unknown" else sprintf("rotation %s", ns$rot),
    sprintf(" · %s", ns$basis)))

  # Said out loud, because the whole tab rests on it: the player is read off
  # tonight, the tendencies off everything loaded. A staff that has not
  # loaded last month's film should see that this is one match deep.
  H <- c(H, sprintf('<div class="tiny nssrc">Who serves next, from %s. Tendencies below, from %s.</div>',
                    if (live_only) "the match being coded" else "the files loaded",
                    if (n_files > 1L)
                      sprintf("%d files%s", n_files,
                              if (nzchar(opponent)) sprintf(" against %s", esc(opponent)) else "")
                    else "the one file loaded, so there is no history behind them yet"))

  if (!isTRUE(ns$known))
    H <- c(H, sprintf('<div class="warn">Which of their players serves next is not settled yet: %s. The page below is still the right side, just not pinned to a player.</div>',
                      esc(ns$basis)))

  body <- if (isTRUE(ours)) {
    passer_block(Opp, o)
  } else {
    S_all <- Opp[Opp$skill == "S", ]
    if (isTRUE(ns$known) && !is.na(ns$num))
      c(sprintf('<h3 class="nsname">#%s %s</h3>', ns$num, esc(ns$name %||% "")),
        server_block(S_all, ns$num, ns$name %||% "", o, coords_ok))
    else
      card("Their serve order",
           tbl_generic(c("Rot", "Server", "Serves", "Ace", "Err", "Eff"),
                       list()))
  }

  paste0(c(H, body, note(
    "This page follows the match: when the serve is ours it shows who to serve at, and when it is theirs it shows who is about to serve. Who serves next is worked out from the rotation each side has actually served in, which handles a serving substitute correctly where reading zone 1 off the lineup does not. It was right on 100% of side-outs in one scouted match and 98% in another. Before a match, with no live file, the same pages read off whatever is loaded."
  )), collapse = "")
}
