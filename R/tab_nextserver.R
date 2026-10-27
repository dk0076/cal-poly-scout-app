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
#'
#' Between rallies there is time to read one picture, not a page. So once a
#' file is being coded, this is the chart and where she serves, and nothing
#' else: her season totals, her serve types and her last serve are true but
#' they are not what the next twenty seconds are about. They fold away behind
#' Detail rather than going anywhere, and before a match, when there is time,
#' they are all on screen as before.
#'
#' Recent form narrows the same way. Her last six serves from a film three
#' weeks ago are not form, they are history, and the arrow beside them was
#' comparing tonight with a different month. With a live file it counts only
#' what has been coded tonight.
server_block <- function(S_all, jersey, name, o, coords_ok, live_id = "") {
  # jersey, not num. A data.table expression resolves a bare name to its own
  # column first, so a parameter called num compares the column with itself,
  # every row passes, and the card shows the whole team's serves under one
  # player's name. The same shadowing bit serve_order() earlier.
  keep <- !is.na(S_all$num) & S_all$num == jersey
  q <- S_all[keep, ]
  if (!nrow(q))
    return(empty(sprintf("#%s %s has not served in what is loaded.",
                         jersey, esc(if (is.na(name)) "" else name))))
  data.table::setorder(q, date, match_id, set_no, rally_no, seq)
  st <- serve_line(q)
  mono <- isTRUE(o$mono)
  # Live for this card only once she has actually served in tonight's file.
  # A match that is being coded but in which she has not served yet is, for
  # her, still a pre-match page.
  live <- nzchar(live_id) && any(q$match_id == live_id)

  xy <- q[q$has_xy, ]
  rect <- list(x_min = -170, x_max = 1070, y_min = -260, y_max = 1990)
  court <- court_svg("NS", x_min = -170, x_max = 1070, y_min = -260, y_max = 1990,
                     cells = "", labels = zone_number_layer("opp", rect),
                     marks = paste0(
                       arrow_layer(xy, "NS", 1, mono,
                                   extra = function(r)
                                     paste(vapply(r$y1 - COURT$NET, m1, ""), "m past net")),
                       # The same toggle as the other charts, so turning
                       # tendency lines off means off everywhere rather than
                       # everywhere except the one page that draws them.
                       if (isTRUE(o$tend))
                         tendency_layer(q, "NS", 1, 2, "serve", mono = mono) else ""),
                     alt = sprintf("where #%s %s serves", jersey, name),
                     mono = mono, coords_ok = coords_ok)

  drawn <- sum(q$has_xy)
  kpi <- kpis(list(
    # Both numbers, because the staff scout sheet counts the charted ones and
    # this counts every serve. Checked against their own UCSB sheet: all nine
    # servers agree to three decimals on serves, aces and efficiency once the
    # uncharted serves are set aside, so the two differ only in population.
    # Every serve is the fuller answer, and the chart can only draw the ones
    # with a line, so both belong on the screen.
    list(k = "Serves", v = st$n,
         s = if (drawn < st$n) sprintf("%d drawn", drawn) else "all drawn"),
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
  qr <- if (live) q[q$match_id == live_id, ] else q
  chips15 <- utils::tail(qr, 15)
  recent <- paste0(vapply(seq_len(nrow(chips15)),
                          function(i) serve_cell(chips15[i, ]), ""),
                   collapse = "")
  win <- window_row(qr, "S")

  # The serve court is a tall portrait shape, so at full width it is the
  # whole screen and the numbers are below the fold. Beside the tables it
  # stays first to read and leaves room for the rest.
  c(fold(live, kpi),
    cols2(
      plain_card(c(viewbar(vtoggle("tend", "Tendency lines", o$tend)), court)),
      c(card("Where she serves",
             if (length(zone_rows))
               tbl_generic(c("Target", "Serves", "Share", "Eff"), zone_rows)
             else empty("No landing zone coded on her serves.")),
        fold(live, card("By type",
             if (length(type_rows))
               tbl_generic(c("Type", "Serves", "Share", "Eff"), type_rows)
             else empty("No serve type coded.")))), 300),
    if (live && !nrow(qr))
      card("Recent form", empty("She has not served yet tonight."))
    else card(if (live) "Recent form <span class=\"thin\">tonight</span>" else "Recent form",
         c(tbl_generic(c(sprintf("Last %d", SERVE_WINDOWS[1]),
                         sprintf("Last %d", SERVE_WINDOWS[2]),
                         sprintf("Last %d", SERVE_WINDOWS[3])),
                       list(win)),
           sprintf('<div class="atts">%s</div>', recent),
           tiny(sprintf("Each chip is one serve: the zone it landed in, then the grade. Oldest on the left. The arrow compares the window with %s.",
                        if (live) "the rest of tonight" else "her whole match")))),
    if (drawn < nrow(q))
      caveat(sprintf("The chart shows %d of her %d serves. The other %d were coded without a line, so they count in the numbers and cannot be drawn. A staff sheet built from the chart will show %d.",
                   drawn, nrow(q), nrow(q) - drawn, drawn)) else "",
    fold(live, card("Her last serve",
         tbl_generic(c("Set", "Score", "Target", "Type", "Outcome"),
                     list(c(last$set_no, esc(last$score %||% ""),
                            if (is.na(lastz)) "?" else sprintf("Zone %s", lastz),
                            if (is.na(last$skill_type)) "" else esc(last$skill_type),
                            land_status(last$effect)))))))
}

#' Us serving: who to serve at, worst passer first
#'
#' Her record is the whole library, because that is what the films are for
#' and one match is too thin to pick a target from. Her form is tonight, for
#' the reason the serve card narrowed: six receptions from three weeks ago
#' are not how she is passing now, and the arrow was comparing tonight with
#' a different month. A passer who has not received yet tonight keeps her
#' row, with the windows blank.
passer_block <- function(Opp, o, live_id = "") {
  R <- Opp[Opp$skill == "R", ]
  if (!nrow(R)) return(empty("No receptions from them in what is loaded."))
  data.table::setorder(R, date, match_id, set_no, rally_no, seq)
  live <- nzchar(live_id) && any(R$match_id == live_id)
  keys <- unique(paste(R$num, R$player_short, sep = "|"))
  rows <- lapply(keys, function(k) {
    q <- R[paste(R$num, R$player_short, sep = "|") == k, ]
    rl <- receive_line(q)
    qr <- if (live) q[q$match_id == live_id, ] else q
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    chips <- paste0(vapply(seq_len(nrow(utils::tail(qr, 15))),
                           function(i) {
                             p <- utils::tail(qr, 15)[i, ]
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
                   if (nrow(qr)) paste(window_row(qr, "R"), collapse = "")
                   else '<span class="thin">not yet tonight</span>',
                   sprintf('<div class="atts">%s</div>', chips)))
  })
  # Worst first: this page exists to pick a target, and the weakest passer is
  # the answer to that question, not an afterthought at the bottom of a list.
  rows <- rows[order(vapply(rows, function(x) x$sort_key, 0))]
  card("Their passers, weakest first",
       c(tbl_generic(c("Passer", "Rec", "Avg", "Pos%", "Err%",
                       paste0(paste(sprintf("L%d", SERVE_WINDOWS), collapse = " / "),
                              if (live) ' <span class="thin">tonight</span>' else ""),
                       paste0("Last 15",
                              if (live) ' <span class="thin">tonight</span>' else "")),
                     lapply(rows, function(x) x$cells)),
         tiny(sprintf("Avg is the staff 0 to 4 pass scale, so lower is worse and the top row is the one to serve. Pos%% is receptions graded # or +, VolleyStation's own positive reception. The three windows are her last 6, 10 and 15 receptions, as reception efficiency%s.",
                      if (live) ", counted in tonight's match only, while Rec, Avg, Pos% and Err% are every file loaded" else ""))))
}

#' The row of servers, with whichever one is pinned marked
#'
#' The prediction is right about 97% of the time on a side-out, which leaves
#' a handful a match where it is not, and a coach cannot use a page that is
#' confidently about the wrong player. So every server they have is one tap
#' away, and "Follow the match" gives the prediction back.
#'
#' Pinning is per viewer, like the filters and unlike the roster. Two coaches
#' watching the same match can be reading different servers, and one of them
#' looking ahead does not drag the bench along.
server_chips <- function(Opp, pinned) {
  S <- Opp[Opp$skill == "S" & !is.na(Opp$num), ]
  if (!nrow(S)) return("")
  tab <- table(paste(S$num, S$player_short, sep = "|"))
  keys <- names(sort(tab, decreasing = TRUE))
  chips <- paste0(vapply(keys, function(k) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    sprintf('<button class="chip %s" data-o="pinServer" data-v="%s">%s <span class="thin">%s</span><span class="n">%d</span></button>',
            if (identical(pinned, parts[1])) "on" else "",
            esc(parts[1]), esc(parts[1]), esc(parts[2]), as.integer(tab[[k]]))
  }, ""), collapse = "")
  sprintf('<div class="viewbar srvpick"><button class="chip %s" data-o="pinServer" data-v="">Follow the match</button>%s</div>',
          if (!nzchar(pinned)) "on" else "", chips)
}

#' Their serve order, one table per film, most recent first
#'
#' The share of each rotation served by somebody other than the named player
#' is printed rather than hidden. A low share means the film tracked their
#' rotation well and the order can be trusted; a high one means it did not,
#' and the row is a label rather than a fact. On four UCSB films that ranged
#' from 1% to 57%, which is the difference between a usable order and noise.
serve_order_cards <- function(Opp) {
  S <- Opp[Opp$skill == "S", ]
  if (!nrow(S)) return(empty("No serves from them in what is loaded."))
  ids <- unique(S$match_id)
  # Most recent first: the latest film is the one whose lineup is likeliest
  # to be the one taking the court.
  dates <- vapply(ids, function(id) as.character(S$date[S$match_id == id][1]), "")
  ids <- ids[order(dates, decreasing = TRUE)]

  out <- lapply(ids, function(id) {
    q <- S[S$match_id == id, ]
    so <- serve_order(q)
    nm <- q$file_name[1]
    when <- q$date[1]
    if (is.null(so))
      return(card(sprintf("%s", esc(nm)),
                  empty("No rotation recorded on their serves in this film.")))
    shared <- sum(so$others)
    total <- sum(so$serves) + shared
    hdr_note <- if (total && shared / total > 0.25)
      sprintf('<div class="warn">%.0f%% of the serves in these rotations were by someone other than the player named, so this film did not track their rotation closely. Read the order as a guess.</div>',
              100 * shared / total) else ""
    card(sprintf("%s <span class=\"thin\">%s</span>", esc(nm), esc(when %||% "")),
         c(hdr_note,
           tbl_generic(c("Rot", "Server", "Serves", "Shared", "Ace", "Err", "Eff"),
             lapply(seq_len(nrow(so)), function(i)
               c(so$rot[i], sprintf("<b>%s</b> %s", so$num[i], esc(so$name[i])),
                 so$serves[i],
                 if (so$others[i]) sprintf('<span class="thin">%d by others</span>', so$others[i]) else "",
                 so$ace[i], so$err[i], eff_td(so$eff[i]))))))
  })
  c(unlist(out),
    tiny("One table per film. A rotation label means their setter is in that zone, which only compares across matches while the lineup does, so these are not pooled. Serves are the named player's own; anyone else who served that rotation is counted beside, not folded in. Eff is VolleyStation's serve formula."))
}

#' Before the first serve: their six servers, and their passers
#'
#' Not an error page. The films are loaded, the match has not started, and
#' this is the view a staff goes through on the bus.
prematch_block <- function(Opp, o, labels, n_files, opponent, why, coords_ok = TRUE,
                           live_id = "") {
  H <- hdr("Next server", sprintf(
    "%s · before the first serve",
    if (nzchar(opponent)) esc(opponent) else esc(labels$them)))
  H <- c(H, sprintf('<div class="tiny nssrc">%s Once the match starts this page follows it, rally by rally.</div>',
                    if (nzchar(why)) sprintf("Who serves next is not settled yet: %s.", esc(why))
                    else "Nothing coded in tonight's file yet."))
  if (is.null(Opp) || !nrow(Opp))
    return(paste0(c(H, empty("No scouting films loaded for them yet. Add their previous matches on the opening screen.")), collapse = ""))

  pinned <- as.character(o$pinServer %||% "")
  chips <- server_chips(Opp, pinned)
  if (nzchar(pinned)) {
    S_all <- Opp[Opp$skill == "S", ]
    nm <- S_all$player_short[!is.na(S_all$num) & S_all$num == as.integer(pinned)][1]
    return(paste0(c(H, chips,
                    sprintf('<h3 class="nsname">#%s %s <span class="thin">picked by hand</span></h3>',
                            esc(pinned), esc(nm %||% "")),
                    server_block(S_all, as.integer(pinned), nm %||% "", o,
                                 coords_ok, live_id)),
                  collapse = ""))
  }

  # One table per film, not one pooled table. A rotation label means "their
  # setter is in zone N", which is comparable across matches only while the
  # lineup is. Across these four films it is not: the same label picks a
  # different server in different films, and pooling them produced a table
  # where nearly half the serves in each rotation belonged to someone other
  # than the player the row named. Per film the picture is clean where the
  # film is clean, and visibly not where it is not.
  srv <- serve_order_cards(Opp)

  paste0(c(H, chips, srv, passer_block(Opp, o), note(sprintf(
    "Read off %s. Once tonight's file has a rally in it this page switches to following the match: their server with her chart when they serve, their passers when we do.",
    if (n_files > 1L) sprintf("%d scouting films", n_files) else "the one file loaded"
  ))), collapse = "")
}

tab_nextserver <- function(F, Opp, ns, o, coords_ok, labels, ours,
                           n_files = 1L, live_only = FALSE, opponent = "",
                           live_id = "") {
  # Before the first serve there is nothing to predict from, which is also
  # exactly when a staff opens this: the films are loaded and the match has
  # not started. So the same page falls back to the whole scouting picture,
  # which is what was wanted before a match anyway.
  pinned <- as.character(o$pinServer %||% "")

  if (is.null(ns) || (!isTRUE(ns$known) && !nzchar(pinned)))
    return(prematch_block(Opp, o, labels, n_files, opponent,
                          if (is.null(ns)) "" else ns$basis, coords_ok, live_id))

  who <- if (isTRUE(ours)) labels$us else labels$them
  H <- hdr("Next server", sprintf(
    "%s serving · %s%s",
    esc(who),
    if (is.na(ns$rot)) "rotation unknown" else rot_said(ns$rot),
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

  S_all <- Opp[Opp$skill == "S", ]
  chips <- server_chips(Opp, pinned)

  body <- if (nzchar(pinned)) {
    # A pin overrides the prediction and the side: the coach has said who
    # they want, and the page should be about her whoever is serving.
    nm <- S_all$player_short[!is.na(S_all$num) & S_all$num == as.integer(pinned)][1]
    c(chips,
      sprintf('<h3 class="nsname">#%s %s <span class="thin">picked by hand</span></h3>',
              esc(pinned), esc(nm %||% "")),
      server_block(S_all, as.integer(pinned), nm %||% "", o, coords_ok, live_id))
  } else if (isTRUE(ours)) {
    c(chips, passer_block(Opp, o, live_id))
  } else {
    c(chips,
      sprintf('<h3 class="nsname">#%s %s</h3>', ns$num, esc(ns$name %||% "")),
      server_block(S_all, ns$num, ns$name %||% "", o, coords_ok, live_id))
  }

  paste0(c(H, body, note(
    "Pick a server by hand when the rotation gets it wrong, or to read ahead; Follow the match hands it back. That choice is yours alone, so another coach on another iPad is unaffected. This page follows the match: when the serve is ours it shows who to serve at, and when it is theirs it shows who is about to serve. Who serves next is worked out from the rotation each side has actually served in, which handles a serving substitute correctly where reading zone 1 off the lineup does not. It was right on 100% of side-outs in one scouted match and 98% in another. Before a match, with no live file, the same pages read off whatever is loaded."
  )), collapse = "")
}
