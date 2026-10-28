# ---------------------------------------------------------------------------
# From coded touches to counters.
#
# Most statistics are a count of rows matching a condition, so the work is to
# get the rows right once and then count them many times. Two of them are not:
# a serve is judged by the pass it forced, and a dig by the swing it set up,
# and both of those have to be read out of the rally around the touch. Those,
# and the handful of others that need a neighbour (assists, covers, who fed
# each attack, first ball against transition, rotation), come out of one pass
# over the rallies in rally_context().
# ---------------------------------------------------------------------------

#' The outcome a skill's evaluation character stands for.
outcome <- function(skill_name, ev) {
  out <- rep("neutral", length(ev))
  for (s in names(DEFS)) {
    i <- which(!is.na(skill_name) & skill_name == s & !is.na(ev))
    if (length(i)) {
      o <- DEFS[[s]][ev[i]]
      out[i] <- ifelse(is.na(o), "neutral", o)
    }
  }
  out
}

#' Number the rallies.
#'
#' A rally normally runs from a serve to the point code that ends it. Drill
#' work often carries no score at all, and splitting only on point codes would
#' then collapse a whole set into one rally, so when fewer than half the serves
#' are answered by a point code the serves do the numbering instead. Either
#' way a second serve inside a rally starts a new one.
rally_ids <- function(d) {
  is_s <- d$kind == "skill" & !is.na(d$skill) & d$skill == "S"
  is_p <- d$kind == "point"
  anchor <- if (sum(is_p) >= 0.5 * max(sum(is_s), 1)) "score" else "serve"
  block <- cumsum(c(0L, head(as.integer(is_p), -1)))      # point codes close a block
  # Running count of TRUEs since the start of the current block.
  first <- !duplicated(block)
  within <- function(x) {
    cs <- cumsum(x)
    cs - (cs - x)[first][match(block, block[first])]
  }
  brk <- if (anchor == "serve") is_s & within(d$kind == "skill") > 1
         else is_s & within(is_s) > 1
  list(rally = cumsum(brk | c(TRUE, diff(block) != 0)), anchor = anchor)
}

#' One pass over the rallies for everything that needs a neighbouring touch.
#' Returns columns to bolt onto the play table, plus the per-rally summary the
#' rotation and sideout numbers are built from.
rally_context <- function(d, grade) {
  n <- nrow(d)
  sgrade <- rep(NA_real_, n); conv <- kill <- cover <- assist <- rep(FALSE, n)
  phase <- rep(NA_character_, n); fed <- rep(NA_integer_, n)
  groups <- split(seq_len(n), d$rally)
  z <- rep(NA, length(groups))
  rs <- list(set = as.integer(z), serving = as.character(z), receiving = as.character(z),
             winner = as.character(z), pos_srv = as.integer(z), pos_rec = as.integer(z),
             so_won = z == 1, fbso = z == 1, srv_won = z == 1)
  nr <- 0L

  for (ix in groups) {
    sk <- ix[d$kind[ix] == "skill"]
    if (!length(sk)) next
    team <- d$side[sk]; what <- d$skill_name[sk]; out <- d$out[sk]
    served <- team[what == "Serve"][1]
    first_attack <- c("*" = FALSE, "a" = FALSE)

    for (k in seq_along(sk)) {
      i <- sk[k]
      # Rows after this one, up to the moment the ball crosses the net.
      after <- if (k < length(sk)) seq(k + 1, length(sk)) else integer(0)
      ours <- after[seq_len(match(FALSE, team[after] == team[k], length(after) + 1) - 1)]

      if (what[k] == "Serve" && out[k] != "err") {
        # The pass the receiver managed. An ace forced no pass at all.
        if (out[k] == "ace") sgrade[i] <- 0
        else {
          r <- after[what[after] == "Reception" & team[after] != team[k]][1]
          if (!is.na(r)) sgrade[i] <- unname(grade[d$eval[sk[r]]])
        }
      }
      if (what[k] == "Dig") {
        # A dig coded against 00 converted something, but there is nobody to
        # credit it to, so it is left out of the team's conversion too.
        if (out[k] != "err" && !is.na(d$number[i])) {
          a <- ours[what[ours] == "Attack"][1]
          if (!is.na(a)) { conv[i] <- TRUE; kill[i] <- out[a] == "kill" }
        }
        # Digging your own attack off the opponent's block is a cover.
        if (k > 1 && what[k - 1] == "Block" && team[k - 1] != team[k]) cover[i] <- TRUE
      }
      if (what[k] == "Attack") {
        # The attack belongs to whoever set it: the last set by this team
        # before the ball crossed the net.
        back <- if (k > 1) rev(seq_len(k - 1)) else integer(0)
        back <- back[seq_len(match(FALSE, team[back] == team[k], length(back) + 1) - 1)]
        f <- back[what[back] == "Set"][1]
        if (!is.na(f)) fed[i] <- d$number[sk[f]]
        # A rally the team received is theirs to side out. The first swing off
        # that reception is the first-ball chance, everything later transition.
        recv <- !is.na(served) && team[k] != served
        phase[i] <- if (recv && !first_attack[[team[k]]]) "fb" else "tr"
        first_attack[[team[k]]] <- TRUE
        if (out[k] == "kill" && k > 1 && what[k - 1] == "Set" &&
            team[k - 1] == team[k] && !is.na(d$number[sk[k - 1]]))
          assist[sk[k - 1]] <- TRUE
      }
    }

    w <- d$winner[ix]; w <- w[!is.na(w)][1]
    if (!is.na(served)) {
      recv <- if (served == "*") "a" else "*"
      won <- !is.na(w) && w == recv
      nr <- nr + 1L
      rs$set[nr] <- d$set[sk][1]
      rs$serving[nr] <- served; rs$receiving[nr] <- recv
      rs$winner[nr] <- if (is.na(w)) NA_character_ else w
      rs$pos_srv[nr] <- if (served == "*") d$pos_home[sk[1]] else d$pos_away[sk[1]]
      rs$pos_rec[nr] <- if (recv == "*") d$pos_home[sk[1]] else d$pos_away[sk[1]]
      rs$so_won[nr] <- won
      # Won on the first swing off the reception, so the rally never turned.
      rs$fbso[nr] <- won && sum(what == "Attack" & team == recv) <= 1
      rs$srv_won[nr] <- !is.na(w) && w == served
    }
  }
  list(sgrade = sgrade, conv = conv, kill = kill, cover = cover, assist = assist,
       phase = phase, fed = fed,
       rallies = if (nr) as.data.frame(lapply(rs, `[`, seq_len(nr))) else NULL)
}

# Counting helpers. `key` names the line a row belongs to, which is both a
# player and her team, so every row is counted twice over.
tally <- function(key, keep, keys) {
  keep <- keep & !is.na(keep)
  if (!any(keep)) return(rep(0L, length(keys)))
  t <- table(key[keep])
  n <- as.integer(t[match(keys, names(t))])
  ifelse(is.na(n), 0L, n)
}
tsum <- function(x, key, keep, keys) {
  keep <- keep & !is.na(keep)
  if (!any(keep)) return(rep(0, length(keys)))
  s <- tapply(x[keep], key[keep], sum)
  v <- as.numeric(s[match(keys, names(s))])
  ifelse(is.na(v), 0, v)
}

#' Every counter, for every player and for each side as a whole.
#'
#' `keys` are "* 12" for a player and "*" for the team she plays on. A touch
#' coded against 00 belongs to its team but to no player, so it lands in the
#' team row only.
compute <- function(plays, roster, grade = GRADE_DEFAULT) {
  # Some files list a shirt number twice, two players sharing it across a
  # season. The play-by-play has no way to tell them apart, so neither does
  # this: one line per number, under the first name given.
  roster <- roster[!duplicated(paste(roster$side, roster$number)), ]
  d <- plays[plays$kind %in% c("skill", "point"), ]
  r <- rally_ids(d)
  d$rally <- r$rally
  d$out <- outcome(d$skill_name, d$eval)
  ctx <- rally_context(d, grade)
  for (nm in c("sgrade", "conv", "kill", "cover", "assist", "phase", "fed"))
    d[[nm]] <- ctx[[nm]]
  all_d <- d
  d <- d[d$kind == "skill", ]

  # Each touch counted once for the player and once for her side.
  e <- rbind(cbind(d[!is.na(d$number), ], key = paste(d$side[!is.na(d$number)],
                                                      d$number[!is.na(d$number)])),
             cbind(d, key = d$side))
  # A number can turn up in the play-by-play without being in the file's own
  # players table. She still played, so she gets a line, under her number.
  seen <- unique(d[!is.na(d$number), c("side", "number")])
  seen <- seen[!paste(seen$side, seen$number) %in% paste(roster$side, roster$number), ]
  who <- rbind(roster[, c("side", "number", "label", "position")],
               if (nrow(seen)) data.frame(seen, label = paste0("#", seen$number),
                                          position = "", stringsAsFactors = FALSE))
  who <- who[order(who$side, who$number), ]
  keys <- c("*", "a", paste(who$side, who$number))
  L <- data.frame(key = keys,
                  side = c("*", "a", who$side),
                  number = c(NA, NA, who$number),
                  label = c("TEAM", "TEAM", who$label),
                  position = c("", "", who$position),
                  stringsAsFactors = FALSE)
  is_ <- function(s) !is.na(e$skill_name) & e$skill_name == s
  cnt <- function(keep) tally(e$key, keep, keys)

  # Which sets each line appeared in. Kept as the list, not just the count,
  # because folding both sides of a scrimmage together has to take the union
  # rather than add two counts of the same sets.
  seen_sets <- tapply(e$set, e$key, function(x) unique(x[!is.na(x)]))
  set_list <- setNames(seen_sets[match(keys, names(seen_sets))], keys)
  L$sets <- lengths(set_list)

  srv <- is_("Serve")
  L$srv_att <- cnt(srv); L$srv_ace <- cnt(srv & e$out == "ace")
  L$srv_err <- cnt(srv & e$out == "err")
  graded <- srv & !is.na(e$sgrade)
  L$opp_graded <- cnt(graded)
  L$opp_grade <- tsum(ifelse(is.na(e$sgrade), 0, e$sgrade), e$key, graded, keys)
  L$pressure <- cnt(graded & e$sgrade <= 2)
  for (g in 0:4) L[[paste0("f", g)]] <- cnt(graded & round(e$sgrade) == g)

  rec <- is_("Reception")
  L$rec_att <- cnt(rec)
  L$rec_perfect <- cnt(rec & e$out == "perfect")
  L$rec_positive <- cnt(rec & e$out %in% c("perfect", "positive"))
  L$rec_over <- cnt(rec & e$out == "overpass"); L$rec_err <- cnt(rec & e$out == "err")
  w <- unname(grade[e$eval]); known <- rec & !is.na(w)
  L$graded <- cnt(known); L$grade <- tsum(ifelse(is.na(w), 0, w), e$key, known, keys)
  # Perfect, good, medium, poor: the band a pass falls in, so a programme that
  # edits the weights still gets its passes counted somewhere.
  L$g4 <- cnt(known & w >= 4)
  for (g in 3:1) L[[paste0("g", g)]] <- cnt(known & w >= g & w < g + 1)

  atk <- is_("Attack")
  L$atk_att <- cnt(atk); L$atk_kill <- cnt(atk & e$out == "kill")
  L$atk_blocked <- cnt(atk & e$out == "blocked"); L$atk_err <- cnt(atk & e$out == "err")
  for (p in c("fb", "tr")) {
    i <- atk & !is.na(e$phase) & e$phase == p
    L[[paste0(p, "_att")]] <- cnt(i)
    L[[paste0(p, "_kill")]] <- cnt(i & e$out == "kill")
    L[[paste0(p, "_err")]] <- cnt(i & e$out %in% c("err", "blocked"))
  }

  blk <- is_("Block")
  L$blk_att <- cnt(blk); L$blk_stuff <- cnt(blk & e$out == "stuff")
  L$blk_err <- cnt(blk & e$out == "err")
  L$blk_touch <- L$blk_att - L$blk_stuff - L$blk_err
  L$blk_cover <- cnt(e$cover)

  dig <- is_("Dig")
  L$dig_att <- cnt(dig); L$dig_err <- cnt(dig & e$out == "err")
  L$dig_good <- cnt(dig & e$out == "good")
  L$dig_over <- cnt(dig & !is.na(e$eval) & e$eval == "/")
  L$dig_insys <- cnt(dig & !is.na(e$eval) & e$eval %in% c("#", "+"))
  L$dig_oos <- cnt(dig & !is.na(e$eval) & e$eval %in% c("!", "-"))
  L$dig_conv <- cnt(e$conv); L$dig_kill <- cnt(e$kill)

  st <- is_("Set")
  L$set_att <- cnt(st); L$set_err <- cnt(st & e$out == "err")
  L$assists <- cnt(e$assist)
  L$points <- L$atk_kill + L$srv_ace + L$blk_stuff

  # Side totals for sideout and point scoring, per rotation and overall.
  # A file of pure drill work can have no rally with a serve in it at all.
  # The tables still want their shape, so the counts come out as zeros.
  rl <- ctx$rallies
  if (is.null(rl)) rl <- data.frame(
    set = integer(0), serving = character(0), receiving = character(0),
    winner = character(0), pos_srv = integer(0), pos_rec = integer(0),
    so_won = logical(0), fbso = logical(0), srv_won = logical(0))
  rot <- local({
    do.call(rbind, lapply(c("*", "a"), function(s) {
      data.frame(side = s, pos = 1:6,
        so  = sapply(1:6, function(p) sum(rl$receiving == s & !is.na(rl$pos_rec) & rl$pos_rec == p & !is.na(rl$winner))),
        sow = sapply(1:6, function(p) sum(rl$receiving == s & !is.na(rl$pos_rec) & rl$pos_rec == p & rl$so_won)),
        fbso = sapply(1:6, function(p) sum(rl$receiving == s & !is.na(rl$pos_rec) & rl$pos_rec == p & rl$fbso)),
        srv = sapply(1:6, function(p) sum(rl$serving == s & !is.na(rl$pos_srv) & rl$pos_srv == p & !is.na(rl$winner))),
        srvw = sapply(1:6, function(p) sum(rl$serving == s & !is.na(rl$pos_srv) & rl$pos_srv == p & rl$srv_won)))
    }))
  })
  phase <- do.call(rbind, lapply(c("*", "a"), function(s) data.frame(side = s,
    so = sum(rl$receiving == s & !is.na(rl$winner)), sow = sum(rl$receiving == s & rl$so_won),
    fbso = sum(rl$receiving == s & rl$fbso),
    srv = sum(rl$serving == s & !is.na(rl$winner)), srvw = sum(rl$serving == s & rl$srv_won))))
  # Share of rallies that end in a point code. Below a half, the file is drill
  # work and the sideout rates would rest on a fraction of the play.
  played <- tapply(all_d$kind == "skill", all_d$rally, any)
  won <- tapply(!is.na(all_d$winner), all_d$rally, any)
  scored <- if (any(played)) mean(won[played]) else 0

  list(lines = L, plays = d, events = e, rallies = rl, rot = rot, phase = phase,
       set_list = set_list, n_rallies = sum(played), scored = scored,
       anchor = r$anchor, roster = roster)
}

# ---------------------------------------------------------------------------
# Formatting. Rates are held as numbers for sorting and turned into strings at
# the last moment, so a table never sorts "9%" above "10%".
# ---------------------------------------------------------------------------

#' A rate, or a dash where there is nothing to divide by.
#'
#' The denominator is often a single team total against a column of players,
#' so these recycle it themselves rather than leaving it to ifelse(), which
#' takes its length from the test and would quietly return one row.
rated <- function(n, d, f) {
  len <- max(length(n), length(d))
  n <- rep_len(as.numeric(n), len); d <- rep_len(as.numeric(d), len)
  ok <- !is.na(n) & !is.na(d) & d > 0
  out <- rep("–", len)
  out[ok] <- f(n[ok], d[ok])
  out
}
pct <- function(n, d, dp = 0) rated(n, d, function(a, b) paste0(round(100 * a / b, dp), "%"))
avg <- function(n, d, dp = 2) rated(n, d, function(a, b) formatC(a / b, format = "f", digits = dp))
#' Hitting percentage, printed the way a box score prints it: three decimals,
#' no leading zero, a space where the minus sign would be.
eff <- function(k, err, blocked, att) {
  len <- max(length(k), length(att))
  v <- rep_len(k - err - blocked, len)
  rated(v, att, function(a, b) sub("^([ -]?)0\\.", "\\1.", sprintf("% .3f", a / b)))
}
eff_num <- function(k, err, blocked, att) ifelse(att > 0, (k - err - blocked) / att, -9)

#' Best first on the headline number, but a .500 on two swings should not lead
#' the table, so anyone short of the threshold drops below those who clear it.
rank_lines <- function(L, key, n, min_n) {
  L[order(ifelse(n >= min_n, 1, 0), ifelse(n >= min_n, key, -Inf), n, decreasing = TRUE), ]
}

#' Read a formatted rate back as a number, with the dash meaning "no data".
as_num <- function(x) suppressWarnings(as.numeric(ifelse(x == "\u2013", NA, x)))
