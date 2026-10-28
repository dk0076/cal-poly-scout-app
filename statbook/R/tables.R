# ---------------------------------------------------------------------------
# Tables for the screen.
#
# Everything here takes the counters from compute() and returns a character
# data frame ready to print. Rates are formatted last, after the sort, and a
# divider row marks where the sample gets too small for the headline number to
# mean anything, the way the printed report does it.
# ---------------------------------------------------------------------------

DASH <- "–"

#' One side of the file, or both folded into one squad. An intra-squad file
#' carries the same roster twice, so folding is how you read it as one team.
pick <- function(S, view) {
  L <- S$lines
  # Counters add; a shirt number and a count of sets do not. The sets have to
  # be unioned, or a player who appeared in set 1 on both sides of a
  # scrimmage would be credited with two.
  fold <- function(rows) {
    out <- rows[1, ]
    for (c in names(rows)[sapply(rows, is.numeric)])
      if (!c %in% c("number", "sets")) out[[c]] <- sum(rows[[c]])
    out$sets <- length(unique(unlist(S$set_list[rows$key])))
    out
  }
  if (view == "both") {
    P <- L[!is.na(L$number), ]
    agg <- do.call(rbind, lapply(sort(unique(P$number)),
                                 function(n) fold(P[P$number == n, ])))
    list(players = agg, team = fold(L[is.na(L$number), ]), sides = c("*", "a"),
         label = "both sides as one squad")
  } else {
    list(players = L[!is.na(L$number) & L$side == view, ],
         team = L[is.na(L$number) & L$side == view, ],
         sides = view, label = view)
  }
}

#' Insert the "below this line the sample is thin" divider.
split_rule <- function(df, ok, note) {
  if (!nrow(df) || all(ok) || !any(ok)) return(df)
  div <- df[1, ]; div[] <- ""; div[[1]] <- note
  rbind(df[seq_len(sum(ok)), ], div, df[(sum(ok) + 1):nrow(df), ])
}

#' Order a box score best first, with everyone short of `min_n` attempts
#' dropped below the rule and sorted by volume instead.
order_box <- function(P, key, n, min_n) {
  o <- order(n >= min_n, ifelse(n >= min_n, key, -Inf), n, decreasing = TRUE)
  list(P = P[o, ], ok = (n >= min_n)[o])
}

num <- function(x) format(x)

box_attacking <- function(S, view, min_n = 10) {
  P <- pick(S, view)$players; P <- P[P$atk_att > 0, ]
  if (!nrow(P)) return(NULL)
  s <- order_box(P, eff_num(P$atk_kill, P$atk_err, P$atk_blocked, P$atk_att), P$atk_att, min_n)
  P <- s$P
  split_rule(data.frame(
    Player = P$label, Att = num(P$atk_att), K = num(P$atk_kill),
    `K%` = pct(P$atk_kill, P$atk_att), E = num(P$atk_err),
    `E%` = pct(P$atk_err, P$atk_att), Blk = num(P$atk_blocked),
    Eff = eff(P$atk_kill, P$atk_err, P$atk_blocked, P$atk_att),
    Pts = num(P$points), check.names = FALSE, stringsAsFactors = FALSE),
    s$ok, paste("under", min_n, "attempts"))
}

box_serving <- function(S, view, min_n = 8) {
  P <- pick(S, view)$players; P <- P[P$srv_att > 0, ]
  if (!nrow(P)) return(NULL)
  # Lower oPGP is a harder serve, so rank on its negative.
  key <- ifelse(P$opp_graded > 0, -P$opp_grade / P$opp_graded, -9)
  s <- order_box(P, key, P$srv_att, min_n); P <- s$P
  split_rule(data.frame(
    Player = P$label, Att = num(P$srv_att), Aces = num(P$srv_ace),
    `Ace%` = pct(P$srv_ace, P$srv_att), SE = num(P$srv_err),
    `SE%` = pct(P$srv_err, P$srv_att), `PS%` = pct(P$pressure, P$opp_graded),
    oPGP = avg(P$opp_grade, P$opp_graded), check.names = FALSE,
    stringsAsFactors = FALSE), s$ok, paste("under", min_n, "serves"))
}

box_passing <- function(S, view, min_n = 8) {
  P <- pick(S, view)$players; P <- P[P$rec_att > 0, ]
  if (!nrow(P)) return(NULL)
  key <- ifelse(P$graded > 0, P$grade / P$graded, -9)
  s <- order_box(P, key, P$rec_att, min_n); P <- s$P
  split_rule(data.frame(
    Player = P$label, Att = num(P$rec_att), Perfect = num(P$g4), Good = num(P$g3),
    Medium = num(P$g2), Poor = num(P$g1), OverPass = num(P$rec_over),
    Error = num(P$rec_err), `PGP%` = pct(P$g4 + P$g3, P$graded),
    AVG = avg(P$grade, P$graded), check.names = FALSE, stringsAsFactors = FALSE),
    s$ok, paste("under", min_n, "receptions"))
}

box_blocking <- function(S, view, min_n = 1) {
  P <- pick(S, view)$players; P <- P[P$blk_att > 0 | P$set_att > 0, ]
  if (!nrow(P)) return(NULL)
  s <- order_box(P, P$assists + P$blk_stuff, P$blk_att + P$set_att, min_n); P <- s$P
  data.frame(Player = P$label, `Blk att` = num(P$blk_att), Stuff = num(P$blk_stuff),
             Touch = num(P$blk_touch), Cover = num(P$blk_cover),
             `Blk err` = num(P$blk_err), Assists = num(P$assists),
             `Set att` = num(P$set_att), `Set err` = num(P$set_err),
             check.names = FALSE, stringsAsFactors = FALSE)
}

box_digging <- function(S, view, min_n = 10) {
  P <- pick(S, view)$players; P <- P[P$dig_att > 0, ]
  if (!nrow(P)) return(NULL)
  key <- ifelse(P$dig_att > 0, P$dig_conv / P$dig_att, -9)
  s <- order_box(P, key, P$dig_att, min_n); P <- s$P
  split_rule(data.frame(
    Player = P$label, Att = num(P$dig_att), Err = num(P$dig_err),
    Over = num(P$dig_over), Total = num(P$dig_att - P$dig_err),
    `Succ%` = pct(P$dig_att - P$dig_err, P$dig_att),
    Eff = eff(P$dig_conv, P$dig_err, P$dig_over, P$dig_att),
    `To atk` = num(P$dig_conv), `Conv%` = pct(P$dig_conv, P$dig_att),
    `Kill%` = pct(P$dig_kill, P$dig_conv), check.names = FALSE,
    stringsAsFactors = FALSE), s$ok, paste("under", min_n, "digs"))
}

#' Serving and receiving side by side, every player who did either, with the
#' team's own line underneath.
tbl_serve_rec <- function(S, view) {
  u <- pick(S, view); P <- u$players; T <- u$team
  P <- P[P$srv_att > 0 | P$rec_att > 0, ]
  P <- P[order(P$number), ]
  row <- function(L, who) data.frame(
    Player = who, Serves = num(L$srv_att), Aces = num(L$srv_ace),
    `Ace%` = pct(L$srv_ace, L$srv_att), `Serve err` = num(L$srv_err),
    `Err%` = pct(L$srv_err, L$srv_att), Receptions = num(L$rec_att),
    `Pass avg` = avg(L$grade, L$graded), `Perfect%` = pct(L$rec_perfect, L$rec_att),
    `Positive%` = pct(L$rec_positive, L$rec_att), Overpass = num(L$rec_over),
    `Rec err%` = pct(L$rec_err, L$rec_att), check.names = FALSE,
    stringsAsFactors = FALSE)
  rbind(row(P, P$label), row(T, "TOTAL"))
}

tbl_serve_types <- function(S, view) {
  u <- pick(S, view)
  d <- S$plays
  d <- d[d$side %in% u$sides & !is.na(d$skill_name) & d$skill_name == "Serve", ]
  if (!nrow(d)) return(NULL)
  code <- ifelse(is.na(d$skill_type), "?", d$skill_type)
  tab <- lapply(split(seq_len(nrow(d)), code), function(i)
    data.frame(att = length(i), ace = sum(d$out[i] == "ace"), err = sum(d$out[i] == "err")))
  v <- do.call(rbind, tab); v$code <- names(tab)
  v <- v[order(-v$att), ]
  data.frame(Code = v$code,
             Serve = ifelse(is.na(SERVE_TYPES[v$code]), "not in the guide", SERVE_TYPES[v$code]),
             Att = num(v$att), Share = pct(v$att, sum(v$att)), Aces = num(v$ace),
             `Ace%` = pct(v$ace, v$att), Errors = num(v$err), `Err%` = pct(v$err, v$att),
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' Attack counts for one subset of plays, grouped by whatever `by` says.
by_combo <- function(d, by) {
  g <- lapply(split(seq_len(nrow(d)), by), function(i) data.frame(
    att = length(i), kill = sum(d$out[i] == "kill"),
    err = sum(d$out[i] == "err"), blocked = sum(d$out[i] == "blocked")))
  v <- do.call(rbind, g); v$code <- names(g)
  v[order(-v$att), ]
}

attacks_of <- function(S, sides, coded_only = TRUE) {
  d <- S$plays
  d <- d[d$side %in% sides & !is.na(d$skill_name) & d$skill_name == "Attack", ]
  if (coded_only) d[!is.na(d$combo), ] else d
}

tbl_combo <- function(S, view, file_combos) {
  u <- pick(S, view)
  d <- attacks_of(S, u$sides)
  if (!nrow(d)) return(NULL)
  v <- by_combo(d, d$combo)
  info <- combo_info(v$code, file_combos)
  mark <- ifelse(info$confirmed, " ✓",
          ifelse(info$source == "guide", " (unconfirmed)", ""))
  data.frame(Code = v$code, Call = paste0(info$call, mark), Group = info$group,
             Att = num(v$att), Share = pct(v$att, u$team$atk_att), Kills = num(v$kill),
             `Kill%` = pct(v$kill, v$att), Err = num(v$err), Blocked = num(v$blocked),
             `Hit%` = eff(v$kill, v$err, v$blocked, v$att), check.names = FALSE,
             stringsAsFactors = FALSE)
}

tbl_combo_group <- function(S, view, file_combos) {
  u <- pick(S, view)
  d <- attacks_of(S, u$sides)
  if (!nrow(d)) return(NULL)
  g <- combo_info(d$combo, file_combos)$group
  v <- by_combo(d, g)
  data.frame(Group = v$code, Att = num(v$att), Share = pct(v$att, u$team$atk_att),
             Kills = num(v$kill), `Kill%` = pct(v$kill, v$att),
             `Hit%` = eff(v$kill, v$err, v$blocked, v$att), check.names = FALSE,
             stringsAsFactors = FALSE)
}

#' How rallies ended: off the block, on the floor, or unplayable.
tbl_ends <- function(S, view) {
  u <- pick(S, view); d <- S$plays
  d <- d[d$side %in% u$sides & !is.na(d$number) & !is.na(d$special) &
         d$special %in% names(RALLY_END) &
         d$skill_name %in% c("Attack", "Block", "Serve"), ]
  if (!nrow(d)) return(NULL)
  t <- sort(table(d$special), decreasing = TRUE)
  data.frame(Code = names(t), Meaning = unname(RALLY_END[names(t)]),
             n = num(as.integer(t)), check.names = FALSE, stringsAsFactors = FALSE)
}

#' Attacking split into the first swing off a reception and everything after.
tbl_phase <- function(S, view) {
  u <- pick(S, view); P <- rbind(u$players, u$team)
  P <- P[P$fb_att + P$tr_att > 0, ]
  if (!nrow(P)) return(NULL)
  P <- P[order(-(P$fb_att + P$tr_att)), ]
  data.frame(Player = ifelse(is.na(P$number), "TOTAL", P$label),
             `FB att` = num(P$fb_att), `FB kills` = num(P$fb_kill),
             `FB kill%` = pct(P$fb_kill, P$fb_att), `FB err` = num(P$fb_err),
             `Tr att` = num(P$tr_att), `Tr kills` = num(P$tr_kill),
             `Tr kill%` = pct(P$tr_kill, P$tr_att), `Tr err` = num(P$tr_err),
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' Sideout and point scoring by the setter's position at the start of the
#' rally, so S1 means the setter was in position 1.
tbl_rotation <- function(S, view) {
  if (view == "both" || is.null(S$rot)) return(NULL)
  r <- S$rot[S$rot$side == view, ]
  p <- S$phase[S$phase$side == view, ]
  rbind(data.frame(`Setter at` = paste0("S", r$pos), Receptions = num(r$so),
                   `Sideout%` = pct(r$sow, r$so), `First ball%` = pct(r$fbso, r$so),
                   Serves = num(r$srv), `Point%` = pct(r$srvw, r$srv),
                   check.names = FALSE, stringsAsFactors = FALSE),
        data.frame(`Setter at` = "all", Receptions = num(p$so),
                   `Sideout%` = pct(p$sow, p$so), `First ball%` = pct(p$fbso, p$so),
                   Serves = num(p$srv), `Point%` = pct(p$srvw, p$srv),
                   check.names = FALSE, stringsAsFactors = FALSE))
}

#' The setters, and what each of them ran.
#'
#' Anyone can end up making a set in a scramble, so this follows the position
#' code in the file's own players table. A fixed cut would hide every setter
#' once you filter to one set, so it takes the four with the most volume that
#' fed at least five attacks and marks the thin ones instead of dropping them.
setter_list <- function(S) {
  d <- S$plays
  a <- d[!is.na(d$skill_name) & d$skill_name == "Attack" & !is.na(d$fed), ]
  if (!nrow(a)) return(integer(0))
  setters <- S$roster$number[S$roster$position == "setter"]
  n <- table(a$fed[a$fed %in% setters])
  n <- n[n >= 5]
  as.integer(names(sort(n, decreasing = TRUE)))[seq_len(min(4, length(n)))]
}

tbl_setter_calls <- function(S, nums, file_combos) {
  d <- S$plays
  a <- d[!is.na(d$skill_name) & d$skill_name == "Attack" & d$fed %in% nums, ]
  if (!nrow(a)) return(NULL)
  a$combo[is.na(a$combo)] <- "(no code)"
  codes <- names(sort(table(a$combo), decreasing = TRUE))
  info <- combo_info(codes, file_combos)
  info$call[codes == "(no code)"] <- "no call recorded"
  out <- data.frame(Code = codes, Call = info$call, stringsAsFactors = FALSE,
                    check.names = FALSE)
  for (n in nums) {
    b <- a[a$fed == n, ]
    tot <- nrow(b)
    att <- as.integer(table(factor(b$combo, levels = codes)))
    kill <- as.integer(table(factor(b$combo[b$out == "kill"], levels = codes)))
    err <- as.integer(table(factor(b$combo[b$out == "err"], levels = codes)))
    blk <- as.integer(table(factor(b$combo[b$out == "blocked"], levels = codes)))
    who <- paste0("#", n)
    out[[paste(who, "att")]] <- ifelse(att > 0, num(att), DASH)
    out[[paste(who, "share")]] <- ifelse(att > 0, pct(att, tot), DASH)
    out[[paste(who, "hit%")]] <- ifelse(att > 0, eff(kill, err, blk, att), DASH)
  }
  tot <- data.frame(Code = "", Call = "all attacks fed", check.names = FALSE,
                    stringsAsFactors = FALSE)
  for (n in nums) {
    b <- a[a$fed == n, ]
    tot[[paste(paste0("#", n), "att")]] <- num(nrow(b))
    tot[[paste(paste0("#", n), "share")]] <- "100%"
    tot[[paste(paste0("#", n), "hit%")]] <- eff(sum(b$out == "kill"), sum(b$out == "err"),
                                                sum(b$out == "blocked"), nrow(b))
  }
  rbind(out, tot)
}

#' Where each setter went, counted off the set rows themselves, so it includes
#' sets the attack code never got recorded for.
tbl_setter_targets <- function(S, nums) {
  d <- S$plays
  s <- d[!is.na(d$skill_name) & d$skill_name == "Set" & d$number %in% nums, ]
  if (!nrow(s)) return(NULL)
  out <- data.frame(Target = names(SET_TARGETS), Means = unname(SET_TARGETS),
                    check.names = FALSE, stringsAsFactors = FALSE)
  for (n in nums) {
    b <- s[s$number == n, ]
    k <- as.integer(table(factor(b$target, levels = names(SET_TARGETS))))
    out[[paste0("#", n, " sets")]] <- num(k)
    out[[paste0("#", n, " share")]] <- pct(k, sum(k))
  }
  out
}

# ---------------------------------------------------------------------------
# One player
# ---------------------------------------------------------------------------

player_line <- function(S, view, number) {
  u <- pick(S, view)
  u$players[u$players$number == number, ]
}

#' Every skill she performed, one row each.
tbl_player_skills <- function(L) {
  sets <- max(L$sets, 1)
  r <- list()
  add <- function(...) r[[length(r) + 1]] <<- c(...)
  if (L$atk_att) add("Attacking", L$atk_att,
    sprintf("%d kills (%s), %d errors, %d blocked", L$atk_kill,
            pct(L$atk_kill, L$atk_att), L$atk_err, L$atk_blocked),
    eff(L$atk_kill, L$atk_err, L$atk_blocked, L$atk_att))
  if (L$srv_att) add("Serving", L$srv_att,
    sprintf("%d aces, %d errors (%s), held receivers to %s", L$srv_ace, L$srv_err,
            pct(L$srv_err, L$srv_att), avg(L$opp_grade, L$opp_graded)),
    pct(L$pressure, L$opp_graded))
  if (L$rec_att) add("Passing", L$rec_att,
    sprintf("%d perfect, %d good, %d medium, %d poor, %d overpass, %d error",
            L$g4, L$g3, L$g2, L$g1, L$rec_over, L$rec_err),
    avg(L$grade, L$graded))
  if (L$set_att) add("Setting", L$set_att,
    sprintf("%d assists, %d errors", L$assists, L$set_err), avg(L$assists, sets))
  if (L$blk_att) add("Blocking", L$blk_att,
    sprintf("%d stuff, %d touch, %d cover, %d errors", L$blk_stuff, L$blk_touch,
            L$blk_cover, L$blk_err), avg(L$blk_stuff, sets))
  if (L$dig_att) add("Digging", L$dig_att,
    sprintf("%d digs, %d errors, %d over, %d led to an attack (%s), eff %s",
            L$dig_att - L$dig_err, L$dig_err, L$dig_over, L$dig_conv,
            pct(L$dig_conv, L$dig_att),
            trimws(eff(L$dig_conv, L$dig_err, L$dig_over, L$dig_att))),
    pct(L$dig_conv, L$dig_att))
  if (!length(r)) return(NULL)
  out <- as.data.frame(do.call(rbind, r), stringsAsFactors = FALSE)
  names(out) <- c("Skill", "Attempts", "Detail", "Headline")
  out
}

tbl_player_combo <- function(S, view, number, file_combos) {
  u <- pick(S, view)
  d <- attacks_of(S, u$sides)
  d <- d[!is.na(d$number) & d$number == number, ]
  if (!nrow(d)) return(NULL)
  v <- by_combo(d, d$combo)
  info <- combo_info(v$code, file_combos)
  data.frame(Code = v$code, Call = info$call, Att = num(v$att),
             Share = pct(v$att, sum(v$att)), Kills = num(v$kill),
             `Kill%` = pct(v$kill, v$att),
             `Hit%` = eff(v$kill, v$err, v$blocked, v$att),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Notes
# ---------------------------------------------------------------------------

#' Tagged plays. A tag on a play coded against 00 is a note to come back and
#' fix it; a tag on a fully coded play is something for the coach to watch.
tbl_flagged <- function(S, roster, kind) {
  d <- S$plays
  tag <- ifelse(!is.na(d$custom), d$custom, ifelse(is.na(d$number), d$special, NA))
  d <- d[!is.na(tag), ]; tag <- tag[!is.na(tag)]
  want <- if (kind == "fix") is.na(d$number) else !is.na(d$number)
  d <- d[want, ]; tag <- tag[want]
  if (!nrow(d)) return(NULL)
  who <- roster$label[match(paste(d$side, d$number), paste(roster$side, roster$number))]
  out <- data.frame(Tag = tag, Set = ifelse(is.na(d$set), DASH, num(d$set)),
                    Time = ifelse(nzchar(d$clock), d$clock, DASH),
                    Player = ifelse(is.na(who), DASH, who),
                    Action = ifelse(is.na(d$skill_name), DASH, d$skill_name),
                    Code = d$code, check.names = FALSE, stringsAsFactors = FALSE)
  if (kind == "fix") out$Player <- NULL
  out
}

#' The mapping every number above is built from.
tbl_defs <- function(grade) {
  ev <- c("#", "+", "!", "-", "/", "=")
  do.call(rbind, lapply(names(DEFS), function(s) data.frame(
    Skill = s, Symbol = ev, `Counts as` = unname(DEFS[[s]][ev]),
    Meaning = unname(DEF_TEXT[[s]][ev]),
    `Pass weight` = if (s == "Reception") num(unname(grade[ev])) else rep("", length(ev)),
    check.names = FALSE, stringsAsFactors = FALSE)))
}

#' Codes the mapping has nothing to say about. They count as neutral, which
#' makes them invisible in every rate above, so they are worth seeing.
tbl_unknown <- function(S) {
  d <- S$plays
  bad <- !is.na(d$eval) & !is.na(d$skill_name) &
    mapply(function(s, e) is.na(DEFS[[s]][e]), d$skill_name, d$eval)
  out <- list()
  if (any(bad)) {
    t <- sort(table(paste(d$skill_name[bad], d$eval[bad])), decreasing = TRUE)
    out$evals <- data.frame(Code = names(t), n = as.integer(t), check.names = FALSE)
  }
  a <- d[!is.na(d$skill_name) & d$skill_name == "Attack" & !is.na(d$combo), ]
  if (nrow(a)) {
    info <- combo_info(sort(unique(a$combo)), S$file_combos)
    miss <- info$code[info$source == "none"]
    if (length(miss)) {
      t <- table(a$combo[a$combo %in% miss])
      out$combos <- data.frame(Code = names(t), n = as.integer(t), check.names = FALSE)
    }
  }
  out
}

#' The full per-player export, the same columns the HTML report writes out.
csv_box <- function(S, teams) {
  P <- S$lines[!is.na(S$lines$number), ]
  P <- P[order(P$side, P$number), ]
  n2 <- function(n, d) ifelse(d > 0, sprintf("%.1f", 100 * n / d), "")
  data.frame(
    side = unname(teams[P$side]), number = P$number, name = P$label, sets = P$sets,
    atk_att = P$atk_att,
    atk_eff = trimws(eff(P$atk_kill, P$atk_err, P$atk_blocked, P$atk_att)),
    K = P$atk_kill, K_pct = n2(P$atk_kill, P$atk_att), E = P$atk_err,
    E_pct = n2(P$atk_err, P$atk_att), blocked = P$atk_blocked,
    srv_att = P$srv_att, aces = P$srv_ace, PS_pct = n2(P$pressure, P$opp_graded),
    oPGP = ifelse(P$opp_graded > 0, avg(P$opp_grade, P$opp_graded), ""),
    SE = P$srv_err, SE_pct = n2(P$srv_err, P$srv_att),
    rec_att = P$rec_att, pass_avg = avg(P$grade, P$graded),
    PGP_pct = n2(P$g4 + P$g3, P$graded),
    two_minus_pct = n2(P$graded - P$g4 - P$g3, P$graded),
    medium = P$g2, rec_err = P$rec_err, overpass = P$rec_over,
    blk_att = P$blk_att, stuff = P$blk_stuff, touch = P$blk_touch,
    cover = P$blk_cover, blk_err = P$blk_err,
    dig_in_system = P$dig_insys, dig_oos = P$dig_oos, over_dig = P$dig_over,
    dig_total = P$dig_att - P$dig_err, dig_err = P$dig_err, dig_att = P$dig_att,
    assists = P$assists, set_att = P$set_att, set_err = P$set_err, points = P$points,
    check.names = FALSE, stringsAsFactors = FALSE)
}

#' Folding the two sides into one squad only makes sense when they are the
#' same squad, which is what an intra-squad scrimmage looks like: the same
#' list of shirt numbers written down twice.
rosters_match <- function(roster) {
  a <- sort(unique(roster$number[roster$side == "*"]))
  b <- sort(unique(roster$number[roster$side == "a"]))
  length(a) > 0 && identical(a, b)
}
