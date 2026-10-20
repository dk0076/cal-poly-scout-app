# ---------------------------------------------------------------------------
# Everything between a parsed file and a table on screen: rally linking, role
# inference, the 5-1 / 6-2 classifier, the stat lines, and the filter layer.
#
# The stat lines are the part worth being careful about. Where a formula
# reproduces VolleyStation's own report column it says so; where it is a
# convention defined by this tool it says that instead, so a number is never
# read against published figures it was not built to match.
# ---------------------------------------------------------------------------

PLAY_COLS <- c("uid", "match_id", "file_name", "date", "set_no", "rally_no", "seq",
               "side", "tag", "team_name", "num", "player", "player_short", "libero",
               "skill", "hit_type", "effect", "skill_type", "call", "call_name",
               "setter_call", "target", "blockers", "start_zone", "start_sub_zone",
               "end_zone", "end_sub_zone", "zone6", "custom", "time",
               "x0", "y0", "x1", "y1", "has_xy", "setter_at", "rot",
               "opp_setter_at", "opp_rot",
               paste0("lu", 1:6), paste0("olu", 1:6),
               "court_pos", "front_row", "serving", "phase", "passer",
               "rec_effect", "point_won", "score", "setter_num", "reportable")

empty_plays <- function() {
  z <- as.data.frame(setNames(rep(list(logical(0)), length(PLAY_COLS)), PLAY_COLS))
  data.table::as.data.table(z)
}

# ------------------------------------------------------------ rally structure

#' Phase, passer and set->attack linking, applied after parsing.
#'
#' `do_phase` is TRUE for .dvw, whose parser cannot do the within-rally walk as
#' it reads; the .vsm parser already assigns phase inline from the exchange.
finalize_match <- function(m) {
  p <- m$plays
  if (!nrow(p)) return(m)
  data.table::setDT(p)

  if (all(is.na(p$seq))) p[, seq := seq_len(.N), by = .(set_no, rally_no)]

  blanks <- list(atk_uid = NA_integer_, atk_num = NA_integer_,
                 atk_player = NA_character_, atk_effect = NA_character_,
                 atk_call = NA_character_, atk_call_name = NA_character_,
                 atk_zone = NA_integer_, atk_x = NA_real_, atk_y = NA_real_,
                 atk_land_x = NA_real_, atk_land_y = NA_real_)
  for (col in names(blanks))
    if (!col %in% names(p)) data.table::set(p, j = col, value = blanks[[col]])

  data.table::setorder(p, set_no, rally_no, seq)
  idx <- split(seq_len(nrow(p)), paste(p$set_no, p$rally_no, sep = "|"))

  # Pulled out as plain vectors for the walk: assigning into a data.table
  # column inside a per-rally loop shallow-copies the whole table each time,
  # which cost more than the walk itself on a league file.
  v <- list(skill = p$skill, side = p$side, num = p$num, uid = p$uid,
            effect = p$effect, call = p$call, call_name = p$call_name,
            player_short = p$player_short, start_zone = p$start_zone,
            x0 = p$x0, y0 = p$y0, x1 = p$x1, y1 = p$y1,
            phase = p$phase, passer = p$passer, rec_effect = p$rec_effect,
            atk_uid = p$atk_uid, atk_num = p$atk_num, atk_player = p$atk_player,
            atk_effect = p$atk_effect, atk_call = p$atk_call,
            atk_call_name = p$atk_call_name, atk_zone = p$atk_zone,
            atk_x = p$atk_x, atk_y = p$atk_y,
            atk_land_x = p$atk_land_x, atk_land_y = p$atk_land_y)

  for (ii in idx) {
    sk <- v$skill[ii]; sd <- v$side[ii]

    # Run for both formats, always. This walk used to be .dvw only, with the
    # .vsm parser deriving phase inline as it read the exchange -- two
    # implementations of one rule, which drifted: on a match exported both
    # ways they agreed on only 86% of attacks. One rule, one place.
    {
      sv <- which(sk == "S")
      serving_side <- if (length(sv)) sd[sv[1]] else NA_character_
      did <- character()
      for (k in which(sk == "A")) {
        if (sd[k] %in% did) {
          v$phase[ii[k]] <- "TR"
        } else if (!is.na(serving_side) && sd[k] != serving_side) {
          # True serve-receive test: walk backward through this team's OWN
          # unbroken touches only. If the other team touched the ball first (a
          # dig or block recovery) the chain is broken and this is transition,
          # regardless of an R earlier in the rally.
          rec <- NA_integer_
          if (k > 1) for (j in (k - 1):1) {
            if (sd[j] != sd[k]) break
            if (sk[j] == "R") { rec <- j; break }
          }
          v$phase[ii[k]] <- if (!is.na(rec)) "SR" else "TR"
          if (!is.na(rec)) {
            v$passer[ii[k]] <- v$num[ii[rec]]
            v$rec_effect[ii[k]] <- v$effect[ii[rec]]
          }
        } else {
          v$phase[ii[k]] <- "FT"
        }
        did <- c(did, sd[k])
      }
    }

    # Link each set to the attack it produced: the next same-team attack in the
    # SAME possession. Stop at an opponent touch (the possession ended) and at
    # a later set (that attack belongs to the later set). Scanning for "the
    # next same-team attack anywhere in the rally" let two sets claim one
    # attack -- measured at 268 links over 263 distinct attacks on one file.
    for (k in which(sk == "E")) {
      hit <- NA_integer_
      if (k < length(ii)) for (j in (k + 1):length(ii)) {
        if (sd[j] != sd[k]) break
        if (sk[j] == "E") break
        if (sk[j] == "A") { hit <- j; break }
      }
      if (is.na(hit)) next
      a <- ii[hit]; s <- ii[k]
      v$atk_uid[s]    <- v$uid[a]
      v$atk_num[s]    <- v$num[a]
      v$atk_player[s] <- v$player_short[a]
      v$atk_effect[s] <- v$effect[a]
      v$atk_call[s]   <- v$call[a]
      v$atk_call_name[s] <- v$call_name[a]
      v$atk_zone[s]   <- v$start_zone[a]
      v$atk_x[s] <- v$x0[a]; v$atk_y[s] <- v$y0[a]
      v$atk_land_x[s] <- v$x1[a]; v$atk_land_y[s] <- v$y1[a]
      # A set has no phase or passer of its own in either format; both belong
      # to the attack it produced. Copied here so the Setter tab can cut sets
      # by side-out vs transition and by the grade of the pass that fed them.
      v$phase[s] <- v$phase[a]
      v$passer[s] <- v$passer[a]
      v$rec_effect[s] <- v$rec_effect[a]
    }
  }
  for (nm in c("phase", "passer", "rec_effect", "atk_uid", "atk_num", "atk_player",
               "atk_effect", "atk_call", "atk_call_name", "atk_zone",
               "atk_x", "atk_y", "atk_land_x", "atk_land_y"))
    data.table::set(p, j = nm, value = v[[nm]])
  # What may appear in a report. Kept as a column rather than dropped, so the
  # Plays tab can still show the touch and the count can be reported.
  cust <- if ("custom" %in% names(p)) p$custom else rep(NA_character_, nrow(p))
  prac <- !is.na(p$num) & p$num %in% PRACTICE_PLAYERS
  prov <- is_provisional(cust)
  data.table::set(p, j = "reportable", value = !prac & !prov)

  w <- character()
  if (any(prac))
    w <- c(w, sprintf("%d touches by practice players (#%s) excluded from every rate; they remain on the Plays tab.",
                      sum(prac), paste(sort(unique(p$num[prac])), collapse = ", #")))
  if (any(prov))
    w <- c(w, sprintf("%d touches tagged '00' (coder marked them to revisit) excluded from every rate, because they are not final.",
                      sum(prov)))

  m$plays <- p
  m$warnings <- c(m$warnings, w, contradictory_grades(p, idx))
  m
}

#' Grades that contradict the rally they sit in.
#'
#' A serve graded '#' is an ace, so the rally ends on it. Where the rally
#' carries on -- a set, a swing, a block -- the grade and the touches disagree,
#' and one of them is a coding slip. Found in a real file: a serve graded '#'
#' followed by a set, a kill and a block, which inflated that set's ace count
#' by one against the staff's own report.
#'
#' scoutR counts the grade as coded, the way VolleyStation's own serve formulas
#' do, rather than quietly dropping the serve the way that report did. Silently
#' correcting it would hide a coding error that will keep producing wrong
#' numbers in every other tool reading the same file. So it is surfaced.
contradictory_grades <- function(p, idx) {
  bad <- 0L; where <- character()
  for (ii in idx) {
    sk <- p$skill[ii]; ef <- p$effect[ii]
    for (k in which(sk == "S" & !is.na(ef) & ef == "#")) {
      after <- seq_len(length(ii))[-seq_len(k)]
      # a reception graded '=' is the ace being recorded from the other side
      after <- after[!(sk[after] == "R" & !is.na(ef[after]) & ef[after] == "=")]
      if (!length(after)) next
      bad <- bad + 1L
      where <- c(where, sprintf("set %d rally %d", p$set_no[ii[k]], p$rally_no[ii[k]]))
    }
  }
  if (!bad) return(character())
  sprintf("%d serve%s graded '#' (ace) but the rally carried on afterwards, the grade and the touches disagree, so the ace count is one per slip too high until the coding is fixed: %s.",
          bad, if (bad == 1) "" else "s",
          paste(utils::head(where, 6), collapse = "; "))
}

#' Distinct team names across loaded files, with file coverage.
team_index <- function(matches) {
  if (!length(matches)) return(data.frame(name = character(), files = integer()))
  nm <- unlist(lapply(matches, function(m)
    unique(c(m$teams$home$name, m$teams$away$name))))
  ids <- unlist(lapply(matches, function(m)
    rep(m$match_id, length(unique(c(m$teams$home$name, m$teams$away$name))))))
  tb <- tapply(ids, nm, function(v) length(unique(v)))
  out <- data.frame(name = names(tb), files = as.integer(tb), stringsAsFactors = FALSE)
  out[order(-out$files, out$name), ]
}

#' Point every file at a named team. A file that does not feature that team by
#' name has its side assignment explicitly cleared, not left stale -- otherwise
#' switching focus from one opponent to another lets a leftover file keep
#' contributing the PREVIOUS focus team's plays under the new team's report.
focus_sides <- function(matches, name) {
  out <- list()
  for (m in matches)
    out[[m$match_id]] <- if (identical(m$teams$away$name, name)) "away"
      else if (identical(m$teams$home$name, name)) "home" else NA_character_
  out
}

# ------------------------------------------------------- roles and the system

#' Role per jersey, derived from what each player actually does. Roster position
#' tags are unreliable for scouted opponents, so they are not used.
infer_roles <- function(matches, team_name, keep_ids = NULL) {
  st <- at <- mid <- rec <- integer()
  bump <- function(v, k) { k <- as.character(k); v[k] <- (v[k] %||% 0L); if (is.na(v[k])) v[k] <- 0L; v[k] <- v[k] + 1L; v }
  for (m in matches) {
    if (!is.null(keep_ids) && length(keep_ids) && !(m$match_id %in% keep_ids)) next
    for (sd in c("home", "away")) {
      if (!identical(m$teams[[sd]]$name, team_name)) next
      p <- m$plays[m$plays$side == sd & !is.na(m$plays$num) & m$plays$reportable, ]
      if (!nrow(p)) next
      t_e <- table(p$num[p$skill == "E"])
      t_r <- table(p$num[p$skill == "R"])
      t_a <- table(p$num[p$skill == "A"])
      t_m <- table(p$num[p$skill == "A" & !is.na(p$target) & p$target == "C"])
      add <- function(v, tb) {
        for (k in names(tb)) v[k] <- (if (is.na(v[k] %||% NA)) 0L else v[k]) + as.integer(tb[[k]])
        v
      }
      st <- add(st, t_e); rec <- add(rec, t_r); at <- add(at, t_a); mid <- add(mid, t_m)
    }
  }
  nums <- unique(c(names(st), names(at), names(rec)))
  role <- setNames(rep("", length(nums)), nums)
  g <- function(v, k) { x <- v[k]; if (is.na(x %||% NA)) 0L else as.integer(x) }
  for (k in nums) {
    S_ <- g(st, k); A <- g(at, k); M <- g(mid, k); R <- g(rec, k)
    role[k] <- if (S_ >= 15 && S_ > A) "S"
      else if (A == 0 && R > 5) "L"
      else if (A > 0 && M / A >= 0.5) "MB"
      else if (A > 0) "OH" else ""
  }
  role
}

#' Setter's court zone for one side of a play.
setter_zone_of <- function(p, own) {
  z <- if (own) ifelse(!is.na(p$setter_at), p$setter_at, NA_integer_) else p$opp_setter_at
  r <- if (own) p$rot else p$opp_rot
  # dvw files with no z rows: recover the zone from the sequence label
  fallback <- ifelse(is.na(r), NA_integer_, ROT_ORDER[suppressWarnings(as.integer(substring(r, 2)))])
  ifelse(is.na(z), fallback, z)
}

#' OFFENSIVE SYSTEM PER RALLY: 5-1 or 6-2.
#'
#' The 6-2 here is the classic one: the setter is always back row, and a
#' front-row setter is subbed out for an opposite. The coder does not reselect
#' the setter zone after that sub, so setter_at keeps pointing at the same
#' lineup SLOT, which now holds the opposite. Rotation labels therefore stay
#' sequence-based (R1..R6) across both systems.
#'
#' Only R4..R6 can tell the systems apart. In R1..R3 a 5-1 and a 6-2 put the
#' same shape on court, so a single rally there carries no evidence.
#'   R4..R6  player in the setter slot is a setter            -> 5-1
#'           not a setter, and a setter sits opposite them     -> 6-2
#'           anything else (bad lineup, zone mis-entry)        -> unclear
#'   R1..R3  label of the nearest classified R4..R6 rally before and after it in
#'           the same set; if those disagree it is left unclear.
#' "Setter" = at least 3 sets and more sets than attacks, pooled per team over
#' every loaded file regardless of the filters, so filtering can never change a
#' rally's system.
classify_systems <- function(matches) {
  cnt <- list()
  for (m in matches) {
    p <- m$plays
    if (!nrow(p)) next
    k <- paste(p$team_name, p$num, sep = "|")
    for (kind in c("E", "A")) {
      tb <- table(k[p$skill == kind])
      for (nm in names(tb)) {
        cur <- cnt[[nm]] %||% c(e = 0L, a = 0L)
        cur[[if (kind == "E") "e" else "a"]] <- cur[[if (kind == "E") "e" else "a"]] + as.integer(tb[[nm]])
        cnt[[nm]] <- cur
      }
    }
  }
  is_setter <- function(team, num) {
    c0 <- cnt[[paste(team, num, sep = "|")]]
    !is.null(c0) && c0[["e"]] >= 3 && c0[["e"]] > c0[["a"]]
  }

  for (mi in seq_along(matches)) {
    p <- matches[[mi]]$plays
    if (!nrow(p)) { matches[[mi]]$plays$sys <- character(); next }
    rkey <- paste(p$set_no, p$rally_no, sep = "|")
    lab <- list()
    for (sd in c("home", "away")) {
      team <- matches[[mi]]$teams[[sd]]$name
      rs <- data.frame(key = unique(rkey), stringsAsFactors = FALSE)
      rs$set_no <- NA_integer_; rs$rally_no <- NA_integer_
      rs$z <- NA_integer_; rs$lab <- NA_character_
      lus <- vector("list", nrow(rs))
      for (i in seq_len(nrow(rs))) {
        ii <- which(rkey == rs$key[i])
        rs$set_no[i] <- p$set_no[ii[1]]; rs$rally_no[i] <- p$rally_no[ii[1]]
        own <- p$side[ii] == sd
        # this side's state, read off any touch in the rally
        pick <- ii[1]; own1 <- own[1]
        cols <- if (own1) paste0("lu", 1:6) else paste0("olu", 1:6)
        lus[[i]] <- as.integer(unlist(p[pick, cols, with = FALSE]))
        rs$z[i] <- setter_zone_of(p[pick, ], own1)
      }
      for (sn in unique(rs$set_no)) {
        w <- which(rs$set_no == sn)
        w <- w[order(rs$rally_no[w])]
        front <- rs$z[w] %in% c(2, 3, 4)
        for (j in seq_along(w)) {
          i <- w[j]
          if (any(is.na(lus[[i]])) || is.na(rs$z[i])) { rs$lab[i] <- "?"; next }
          if (!front[j]) { rs$lab[i] <- NA_character_; next }
          in_slot <- lus[[i]][rs$z[i]]
          opp <- lus[[i]][unname(WHEEL_OPP[as.character(rs$z[i])])]
          rs$lab[i] <- if (is.na(in_slot)) "?"
            else if (is_setter(team, in_slot)) "5-1"
            else if (!is.na(opp) && is_setter(team, opp)) "6-2"
            else "?"
        }
        # R1-R3 stretches take the label of the surrounding R4-R6 rallies
        j <- 1L
        while (j <= length(w)) {
          if (!is.na(rs$lab[w[j]])) { j <- j + 1L; next }
          k <- j
          while (k <= length(w) && is.na(rs$lab[w[k]])) k <- k + 1L
          prev <- NA_character_; nxt <- NA_character_
          if (j > 1) for (a in (j - 1):1)
            if (front[a] && !identical(rs$lab[w[a]], "?")) { prev <- rs$lab[w[a]]; break }
          if (k <= length(w)) for (a in k:length(w))
            if (front[a] && !identical(rs$lab[w[a]], "?")) { nxt <- rs$lab[w[a]]; break }
          L <- if (!is.na(prev) && !is.na(nxt)) (if (identical(prev, nxt)) prev else "?")
               else if (!is.na(prev)) prev else if (!is.na(nxt)) nxt else "?"
          rs$lab[w[j:(k - 1)]] <- L
          j <- k
        }
      }
      lab[[sd]] <- setNames(rs$lab, rs$key)
    }
    own_lab <- ifelse(p$side == "home", lab$home[rkey], lab$away[rkey])
    opp_lab <- ifelse(p$side == "home", lab$away[rkey], lab$home[rkey])
    own_lab[is.na(own_lab)] <- "?"; opp_lab[is.na(opp_lab)] <- "?"
    matches[[mi]]$plays$sys <- unname(own_lab)
    matches[[mi]]$plays$opp_sys <- unname(opp_lab)
  }
  matches
}

#' Season call code per play, resolved once roles are known. Kept as a column
#' rather than recomputed per tab: the zone 2 quick needs the hitter's role,
#' and threading roles through every grouping call is how that gets dropped.
add_season_codes <- function(matches) {
  for (mi in seq_along(matches)) {
    p <- matches[[mi]]$plays
    if (!nrow(p)) next
    roles <- list()
    for (sd in c("home", "away")) {
      tn <- matches[[mi]]$teams[[sd]]$name
      roles[[tn]] <- infer_roles(matches, tn)
    }
    rl <- vapply(seq_len(nrow(p)), function(i) {
      r <- roles[[p$team_name[i]]]
      v <- if (is.null(r)) NA_character_ else r[as.character(p$num[i])]
      if (is.na(v %||% NA)) NA_character_ else unname(v)
    }, "")
    p$role <- rl
    p$season_call <- season_code(p$call, rl)
    p$atk_season_call <- season_code(p$atk_call, rl)
    matches[[mi]]$plays <- p
  }
  matches
}

# ------------------------------------------------------------------ stat lines

is_kill <- function(e) !is.na(e) & e == "#"
is_err  <- function(e) !is.na(e) & e == "="
is_blk  <- function(e) !is.na(e) & e == "/"

attack_line <- function(rows) {
  ta <- nrow(rows)
  k <- sum(is_kill(rows$effect)); e <- sum(is_err(rows$effect)); b <- sum(is_blk(rows$effect))
  list(ta = ta, n = ta, k = k, e = e, b = b,
       kpct = if (ta) k / ta else NA_real_,
       epct = if (ta) (e + b) / ta else NA_real_,
       bpct = if (ta) b / ta else NA_real_,
       eff  = if (ta) (k - e - b) / ta else NA_real_)
}

#' Serve line. `eff` and `op` reproduce VolleyStation's own report columns:
#' Eff = (aces + no-attack + positive - errors) / total, Op = count of
#' no-attack serves.
serve_line <- function(rows) {
  n <- nrow(rows)
  ace <- sum(is_kill(rows$effect)); err <- sum(is_err(rows$effect))
  op <- sum(is_blk(rows$effect))        # receiver could not set up an attack
  plus <- sum(!is.na(rows$effect) & rows$effect == "+")
  d <- rows$y1[rows$has_xy]
  list(ta = n, n = n, ace = ace, err = err, op = op, plus = plus,
       k = ace, e = err, b = 0,
       acepct = if (n) ace / n else NA_real_,
       errpct = if (n) err / n else NA_real_,
       pospct = if (n) (ace + op + plus) / n else NA_real_,
       eff    = if (n) (ace + op + plus - err) / n else NA_real_,
       kpct   = if (n) ace / n else NA_real_,
       avg    = grade_avg(rows$effect, SERVE_SCALE),
       med_depth = med(d))
}

set_line <- function(rows) {
  n <- nrow(rows)
  lk <- !is.na(rows$atk_effect)
  k <- sum(lk & rows$atk_effect == "#")
  e <- sum(lk & rows$atk_effect %in% c("=", "/"))
  list(ta = n, n = n, k = k, e = e, b = 0, linked = sum(lk),
       kpct = if (sum(lk)) k / sum(lk) else NA_real_,
       eff  = if (sum(lk)) (k - e) / sum(lk) else NA_real_)
}

#' Reception quality. #/+ = "positive reception" -- this exact grouping is
#' VolleyStation's own terminology, confirmed against their tutorial values,
#' not an invented category.
receive_line <- function(rows) {
  n <- nrow(rows); e <- rows$effect
  perfect <- sum(!is.na(e) & e == "#"); good <- sum(!is.na(e) & e == "+")
  poor <- sum(!is.na(e) & e == "!");    neg  <- sum(!is.na(e) & e == "-")
  err <- sum(!is.na(e) & e == "=");     over <- sum(!is.na(e) & e == "/")
  off <- COURT$NET - rows$y1[rows$has_xy]
  list(n = n, perfect = perfect, good = good, poor = poor, neg = neg,
       err = err, over = over,
       pospct = if (n) (perfect + good) / n else NA_real_,
       errpct = if (n) err / n else NA_real_,
       avg = grade_avg(e, PASS_SCALE),
       med_off_net = med(off))
}

#' Setter line. Assist% and Set eff are conventions defined by this tool, not
#' VolleyStation report columns: Set eff charges the setter for her own
#' ball-handling errors and for sets that produced no attack at all.
setter_eff <- function(rows) {
  n <- nrow(rows)
  bhe <- sum(is_err(rows$effect))
  lk <- !is.na(rows$atk_effect)
  k <- sum(lk & rows$atk_effect == "#")
  e <- sum(lk & rows$atk_effect %in% c("=", "/"))
  list(n = n, ta = n, bhe = bhe, linked = sum(lk), k = k, e = e,
       assist = if (n) k / n else NA_real_,
       kpct = if (sum(lk)) k / sum(lk) else NA_real_,
       hit  = if (sum(lk)) (k - e) / sum(lk) else NA_real_,
       seff = if (n) (k - e - bhe) / n else NA_real_)
}

#' Dig line, on this program's own grading, taken from the wording of their
#' scout report: "In System digs are graded # or +, OOS ! or -, Over Dig is a
#' dig sent straight back over."
#'
#' `in_sys` is the number that matters: a dig the setter can run an offence
#' off. A dig that stays up but goes nowhere useful is not the same thing, and
#' lumping the two together is how a defence looks better than it plays.
dig_line <- function(rows) {
  n <- nrow(rows); e <- rows$effect
  in_sys <- sum(!is.na(e) & e %in% c("#", "+"))
  oos    <- sum(!is.na(e) & e %in% c("!", "-"))
  over   <- sum(!is.na(e) & e == "/")
  err    <- sum(!is.na(e) & e == "=")
  list(n = n, in_sys = in_sys, oos = oos, over = over, err = err,
       up = n - err,
       in_sys_pct = if (n) in_sys / n else NA_real_,
       up_pct = if (n) (n - err) / n else NA_real_,
       err_pct = if (n) err / n else NA_real_,
       # VolleyStation counts an over-dig as winning on a dig; this program's
       # scout report does not. Both are reported so neither is silent.
       vs_eff = vs_efficiency(e, "D"))
}

#' Block line. A stuff ends the rally; a touch keeps it alive and is the thing
#' a defence is usually actually trying to do.
block_line <- function(rows) {
  n <- nrow(rows); e <- rows$effect
  stuff <- sum(!is.na(e) & e == "#")
  err   <- sum(!is.na(e) & e == "=")
  touch <- sum(!is.na(e) & e %in% c("+", "!", "-"))
  list(n = n, stuff = stuff, touch = touch, err = err,
       stuff_pct = if (n) stuff / n else NA_real_,
       err_pct = if (n) err / n else NA_real_,
       vs_eff = vs_efficiency(e, "B"))
}

#' Put the opponent's plays into our frame.
#'
#' Every play is stored in its own acting team's frame, so an opponent attack
#' reads as contact in their own half and landing beyond the net -- the mirror
#' image of what our defence sees. Flipping both ends puts their contact at
#' the far side and the ball landing in our court, which is the view a
#' defensive chart has to show.
mirror_plays <- function(rows) {
  if (!nrow(rows)) return(rows)
  r <- data.table::copy(rows)
  for (p in list(c("x0", "y0"), c("x1", "y1"))) {
    r[[p[1]]] <- COURT$W - r[[p[1]]]
    r[[p[2]]] <- COURT$L - r[[p[2]]]
  }
  r
}

#' Tip rate for a set of attacks.
tip_line <- function(rows) {
  n <- nrow(rows)
  tips <- rows[!is.na(rows$skill_type) & rows$skill_type == "T", ]
  hard <- rows[!is.na(rows$skill_type) & rows$skill_type == "H", ]
  kr <- function(r) if (nrow(r)) sum(is_kill(r$effect)) / nrow(r) else NA_real_
  list(n = n, tips = nrow(tips), tip_pct = if (n) nrow(tips) / n else NA_real_,
       tip_kill = kr(tips), hard_kill = kr(hard), rows = tips)
}

#' Earned FBSO = first-attack kills / receptions.
#' Numerator: an SR-phase attack graded '#'. Denominator: receptions, aces
#' included since they are coded as reception errors. An opponent serve error
#' produces no reception, so it never enters either side of the ratio.
fbso_line <- function(rows) {
  rec <- sum(rows$skill == "R")
  kills <- sum(rows$skill == "A" & !is.na(rows$phase) & rows$phase == "SR" & is_kill(rows$effect))
  list(rec = rec, kills = kills, pct = if (rec) kills / rec else NA_real_)
}

convert_line <- function(won) {
  n <- length(won); w <- sum(won)
  list(n = n, won = w, pct = if (n) w / n else NA_real_,
       ci = wilson(w, n))
}

#' Group rows by a key and attach a stat line to each group.
crosstab <- function(rows, key, line = attack_line) {
  if (!nrow(rows)) return(list())
  keep <- !is.na(key)
  rows <- rows[keep, ]; key <- key[keep]
  if (!nrow(rows)) return(list())
  sp <- split(seq_len(nrow(rows)), key)
  lapply(names(sp), function(k) {
    rs <- rows[sp[[k]], ]
    list(key = k, rows = rs, stat = line(rs))
  })
}

#' Sort a crosstab, biggest group first.
by_size <- function(groups, field = "ta") {
  if (!length(groups)) return(groups)
  groups[order(-vapply(groups, function(o) o$stat[[field]] %||% 0, numeric(1)))]
}
by_key <- function(groups) {
  if (!length(groups)) return(groups)
  groups[order(vapply(groups, function(o) as.character(o$key), ""))]
}

#' Bin points into a grid. Row 1 is the highest-y band (top of screen).
grid_bins <- function(rows, xcol, ycol, rect, cols, rows_n) {
  cells <- vector("list", cols * rows_n)
  for (r in seq_len(rows_n)) for (c in seq_len(cols))
    cells[[(r - 1) * cols + c]] <- list(r = r - 1L, c = c - 1L, rows = rows[0, ])
  if (!nrow(rows)) return(cells)
  cw <- (rect$x1 - rect$x0) / cols; ch <- (rect$y1 - rect$y0) / rows_n
  x <- rows[[xcol]]; y <- rows[[ycol]]
  ok <- !is.na(x) & !is.na(y)
  cc <- clamp(floor((x - rect$x0) / cw), 0, cols - 1) + 1L
  rr <- clamp(floor((rect$y1 - y) / ch), 0, rows_n - 1) + 1L
  for (i in which(ok)) {
    j <- (rr[i] - 1L) * cols + cc[i]
    cells[[j]]$rows <- rbind(cells[[j]]$rows, rows[i, ])
  }
  cells
}

# -------------------------------------------------------------------- filters

#' Plays by the focus team, with every rail filter applied.
focus_plays <- function(matches, sel, focus, ignore_sys = FALSE) {
  out <- list()
  for (m in matches) {
    if (length(sel$matches) && !(m$match_id %in% sel$matches)) next
    side <- focus[[m$match_id]]
    if (is.null(side) || is.na(side)) next
    p <- m$plays
    if (!nrow(p)) next
    keep <- p$side == side & (is.null(p$reportable) | p$reportable)
    if (length(sel$sets))    keep <- keep & p$set_no %in% sel$sets
    if (length(sel$players)) keep <- keep & p$num %in% sel$players
    if (length(sel$rots))    keep <- keep & !is.na(p$rot) & p$rot %in% sel$rots
    if (!ignore_sys && length(sel$sys)) keep <- keep & p$sys %in% sel$sys
    # Selecting a passer asks "what happened off this player's pass", which
    # only has meaning for attacks in serve receive; everything else in the
    # rally is dropped rather than silently left unfiltered.
    if (length(sel$passers))
      keep <- keep & p$skill == "A" & !is.na(p$passer) & p$passer %in% sel$passers
    if (any(keep)) out[[length(out) + 1L]] <- p[keep, ]
  }
  if (!length(out)) return(empty_plays())
  data.table::rbindlist(out, fill = TRUE)
}

#' Plays by the OTHER team -- what the opponent did against the focus team.
#' Player and rotation chips select focus-team players and the focus team's own
#' rotation, so applying them to opponent plays would filter by the wrong
#' roster. The rotation chips are instead matched against opp_rot, i.e. "what
#' they ran while we were in rotation N", which is the cut that answers the
#' question.
opponent_plays <- function(matches, sel, focus) {
  out <- list()
  for (m in matches) {
    if (length(sel$matches) && !(m$match_id %in% sel$matches)) next
    fs <- focus[[m$match_id]]
    if (is.null(fs) || is.na(fs)) next
    opp <- if (fs == "home") "away" else "home"
    p <- m$plays
    if (!nrow(p)) next
    keep <- p$side == opp & (is.null(p$reportable) | p$reportable)
    if (length(sel$sets)) keep <- keep & p$set_no %in% sel$sets
    if (length(sel$rots)) keep <- keep & !is.na(p$opp_rot) & p$opp_rot %in% sel$rots
    if (length(sel$sys))  keep <- keep & p$opp_sys %in% sel$sys   # the FOCUS team's system
    if (any(keep)) out[[length(out) + 1L]] <- p[keep, ]
  }
  if (!length(out)) return(empty_plays())
  data.table::rbindlist(out, fill = TRUE)
}

attacks_of <- function(rows, sel_phases = character()) {
  if (!nrow(rows)) return(rows)
  keep <- rows$skill == "A"
  if (length(sel_phases)) keep <- keep & !is.na(rows$phase) & rows$phase %in% sel_phases
  rows[keep, ]
}

#' Roster of the focus team pooled over loaded matches.
focus_roster <- function(matches, focus) {
  acc <- list()
  for (m in matches) {
    side <- focus[[m$match_id]]
    if (is.null(side) || is.na(side)) next
    # the roster chips follow the same exclusions as the rates behind them,
    # or a practice player gets a chip that selects nothing
    p <- m$plays[m$plays$side == side & m$plays$reportable, ]
    if (!nrow(p)) next
    for (n in unique(p$num[!is.na(p$num)])) {
      k <- as.character(n)
      e <- acc[[k]] %||% list(num = n, name = p$player_short[p$num == n][1],
                              n = 0L, atk = 0L, srv = 0L, set = 0L)
      q <- p[!is.na(p$num) & p$num == n, ]
      e$n <- e$n + nrow(q)
      e$atk <- e$atk + sum(q$skill == "A")
      e$srv <- e$srv + sum(q$skill == "S")
      e$set <- e$set + sum(q$skill == "E")
      acc[[k]] <- e
    }
  }
  if (!length(acc)) return(data.frame(num = integer(), name = character(),
                                      n = integer(), atk = integer(),
                                      srv = integer(), set = integer()))
  out <- do.call(rbind, lapply(acc, as.data.frame))
  out[order(out$num), ]
}

#' True when at least one file in the current filter carries coordinates.
#' .dvw files have none, so every court would render as an empty frame -- worse
#' than useless on a printed sheet. When this is false the charts are
#' suppressed and the tables carry the report on their own.
coords_available <- function(matches, sel) {
  for (m in matches) {
    if (length(sel$matches) && !(m$match_id %in% sel$matches)) next
    if (!m$no_coords) return(TRUE)
  }
  FALSE
}

#' Per-file coding depth for the focus team. Pooling files coded to different
#' depths biases every rate, so this is surfaced rather than buried.
#'
#' Only rallies where the focus team COULD have attacked belong in the
#' denominator: serve errors, aces, reception errors and overpasses end the
#' possession before a swing is possible. Counting them as "missing attacks"
#' flagged fully coded files at 78% where the true figure was 96%.
coverage <- function(m, side) {
  p <- m$plays
  if (!nrow(p) || is.null(side) || is.na(side))
    return(list(recv = 0, recv_live = 0, recv_with_atk = 0, atk_total = 0, pct = NA_real_))
  idx <- split(seq_len(nrow(p)), paste(p$set_no, p$rally_no, sep = "|"))
  recv <- 0L; recv_live <- 0L; recv_with_atk <- 0L; atk_total <- 0L
  for (ii in idx) {
    mine <- p$side[ii] == side
    sv <- which(p$skill[ii] == "S")
    atk_total <- atk_total + sum(p$skill[ii] == "A")
    if (!length(sv) || p$side[ii[sv[1]]] == side) next
    recv <- recv + 1L
    srv_eff <- p$effect[ii[sv[1]]]
    rec_i <- which(mine & p$skill[ii] == "R")
    dead <- (!is.na(srv_eff) && srv_eff %in% c("#", "=")) ||
      (length(rec_i) && !is.na(p$effect[ii[rec_i[1]]]) &&
         p$effect[ii[rec_i[1]]] %in% c("=", "/"))
    if (dead) next
    recv_live <- recv_live + 1L
    if (any(mine & p$skill[ii] == "A")) recv_with_atk <- recv_with_atk + 1L
  }
  list(recv = recv, recv_live = recv_live, recv_with_atk = recv_with_atk,
       atk_total = atk_total,
       pct = if (recv_live) recv_with_atk / recv_live else NA_real_)
}

# ---------------------------------------------------- derived scouting reads

#' Recommended passer start against one server.
#'
#' Uses that server's serves that landed in the court (errors and net serves
#' are the server's problem, not the passer's). Split into the three passing
#' lanes. For each lane with at least 3 serves and 15% of them, the start spot
#' is the median landing point of serves into that lane, and the box is the
#' middle half of them (25th to 75th percentile). Median, not mean, so one
#' shank-deep serve cannot drag the spot.
passer_start <- function(rows) {
  ic <- rows[rows$has_xy & !out_of_court(rows$x1, rows$y1), ]
  if (nrow(ic) < 5) return(list(n = nrow(ic), spots = list()))
  spots <- list()
  for (k in c("R", "M", "L")) {
    g <- ic[lat_third(ic$x1) == k, ]
    if (nrow(g) < 3 || nrow(g) / nrow(ic) < 0.15) next
    spots[[length(spots) + 1L]] <- list(
      k = k, n = nrow(g), share = nrow(g) / nrow(ic),
      x = med(g$x1), y = med(g$y1),
      x25 = quant(g$x1, .25), x75 = quant(g$x1, .75),
      y25 = quant(g$y1, .25), y75 = quant(g$y1, .75))
  }
  spots <- spots[order(-vapply(spots, function(s) s$n, numeric(1)))]
  list(n = nrow(ic), spots = spots)
}

passer_start_text <- function(ps) {
  if (!length(ps$spots))
    return(if (ps$n < 5) "too few in-court serves to place a passer" else "no lane gets 3+ serves")
  paste0("Pass from: ", paste(vapply(ps$spots, function(s)
    sprintf("%s %s m (%s)", LANE_NAME[[s$k]], m1(s$y - COURT$NET), pct(s$share)), ""),
    collapse = " · "))
}

#' One row per rally, from the focus team's point of view.
#'
#' Built from every play in the rally rather than only the focus team's,
#' because the rallies the focus team never touches are exactly the ones that
#' matter most to a side-out rate: an opponent serve error is a side-out won
#' without a single touch, and reading only our own plays drops it. An
#' opponent's play carries our rotation in `opp_rot`, so the rotation is
#' recoverable either way.
#'
#' `won` is NA where the file records no winner for that rally. That is not
#' rare: a set coded as one long exchange has a winner only on its final
#' rally, so rates here must be taken over rallies with a known outcome and
#' the coverage reported alongside. See `rotation_table()`.
#' @param opp read the rally from the OPPONENT's point of view instead -- their
#'   rotation, their serve or receive, their win. Used to put the two teams
#'   side by side, since a rotation is only as good as what the other six do
#'   to you while you are in it.
rally_outcomes <- function(matches, sel, focus, opp = FALSE) {
  out <- list()
  for (m in matches) {
    if (length(sel$matches) && !(m$match_id %in% sel$matches)) next
    fs <- focus[[m$match_id]]
    if (is.null(fs) || is.na(fs)) next
    if (opp) fs <- if (fs == "home") "away" else "home"
    p <- m$plays
    if (!nrow(p)) next
    if (length(sel$sets)) p <- p[p$set_no %in% sel$sets, ]
    if (!nrow(p)) next
    mine <- p$side == fs

    d <- data.table::data.table(
      match_id = p$match_id, set_no = p$set_no, rally_no = p$rally_no,
      mine = mine,
      # our rotation, from our own touch or from the opponent's view of us
      rot = ifelse(mine, p$rot, p$opp_rot),
      sys = ifelse(mine, p$sys, p$opp_sys),
      served = ifelse(p$skill == "S", mine, NA),
      won = ifelse(is.na(p$point_won), NA, ifelse(mine, p$point_won, !p$point_won)),
      # did we finish the rally ourselves, rather than be handed it: an attack
      # or a block of ours graded '#'. The gap between this and simply winning
      # is the share of side-outs the opponent gave away.
      kill = mine & p$skill %in% c("A", "B") & !is.na(p$effect) & p$effect == "#",
      # the first swing off our own reception, which is what first-ball
      # side-out counts
      fb = mine & p$skill == "A" & !is.na(p$phase) & p$phase == "SR" &
           !is.na(p$effect) & p$effect == "#"
    )
    # typed: a rally with nothing but NAs must still yield an NA of the
    # column's own type, or data.table rejects the group
    first_ok <- function(v) { w <- v[!is.na(v)]; if (length(w)) w[1] else v[NA_integer_][1] }
    r <- d[, .(rot = first_ok(rot), sys = first_ok(sys),
               served = first_ok(served), won = first_ok(won),
               kill = any(kill), fb = any(fb)),
           by = .(match_id, set_no, rally_no)]
    out[[length(out) + 1L]] <- r
  }
  if (!length(out))
    return(data.table::data.table(match_id = character(), set_no = integer(),
                                  rally_no = integer(), rot = character(),
                                  sys = character(), served = logical(),
                                  won = logical(), kill = logical(), fb = logical()))
  data.table::rbindlist(out, fill = TRUE)
}

#' The side-out family, over rallies whose winner the file records.
#'
#'   SO%   side-outs won, of rallies received
#'   ESO%  of those, the ones we finished ourselves with a kill or a block --
#'         "earned", as against handed over by an opponent error
#'   FBSO% the ones won on the first swing off our own pass
#'
#' All three share the same denominator so they stack: FBSO% <= ESO% <= SO%.
#' Point score% is the mirror on the serving side, a break point.
sideout_line <- function(r) {
  rec <- r[!is.na(r$served) & !r$served & !is.na(r$won), ]
  srv <- r[!is.na(r$served) &  r$served & !is.na(r$won), ]
  n <- nrow(rec)
  list(
    rec = n, srv = nrow(srv),
    so   = if (n) sum(rec$won) / n else NA_real_,
    eso  = if (n) sum(rec$won & rec$kill) / n else NA_real_,
    # a side-out you did not win is not a side-out. Requiring the rally to be
    # won here as well is what makes the three stack; without it a file that
    # records a first-ball kill alongside a lost point -- which happens, the
    # same way a serve gets graded as an ace while the rally carries on --
    # pushed FBSO above ESO.
    fbso = if (n) sum(rec$fb & rec$won) / n else NA_real_,
    ps   = if (nrow(srv)) sum(srv$won) / nrow(srv) else NA_real_,
    so_ci = wilson(sum(rec$won), n),
    ps_ci = wilson(sum(srv$won), nrow(srv)),
    # points won minus points lost, both phases, which is the column a coach
    # reads first
    plus_minus = sum(r$won, na.rm = TRUE) - sum(!r$won, na.rm = TRUE)
  )
}

#' Side-out and point-scoring rates for one set of rallies.
#'
#' Side-out% is rallies won of rallies received; point-scoring% (a break point)
#' is rallies won of rallies served. Both are over rallies whose winner the
#' file actually records.
rally_line <- function(r) {
  rec <- r[!is.na(r$served) & !r$served & !is.na(r$won), ]
  srv <- r[!is.na(r$served) &  r$served & !is.na(r$won), ]
  so <- if (nrow(rec)) sum(rec$won) / nrow(rec) else NA_real_
  ps <- if (nrow(srv)) sum(srv$won) / nrow(srv) else NA_real_
  known <- sum(!is.na(r$won))
  list(n = nrow(r), known = known,
       cover = if (nrow(r)) known / nrow(r) else NA_real_,
       rec = nrow(rec), rec_won = sum(rec$won),
       srv = nrow(srv), srv_won = sum(srv$won),
       so = so, so_ci = wilson(sum(rec$won), nrow(rec)),
       ps = ps, ps_ci = wilson(sum(srv$won), nrow(srv)),
       won = sum(r$won, na.rm = TRUE),
       net = sum(r$won, na.rm = TRUE) - sum(!r$won, na.rm = TRUE))
}

#' Convert% = transition rallies won / transition rallies.
#'
#' A rally is a transition chance for the focus team when it controls the
#' opponent's attack: a dig (D, not graded '=') whose last opponent touch
#' before it was an attack, or a free ball (F) played by the focus team. Cover
#' digs after our own blocked attack do not count. One chance per rally however
#' many digs it had; the rally counts as converted if the focus team won it.
#' Rally-level, so the player, passer and phase chips do not apply.
convert_rallies <- function(matches, sel, focus) {
  out <- list()
  for (m in matches) {
    if (length(sel$matches) && !(m$match_id %in% sel$matches)) next
    fs <- focus[[m$match_id]]
    if (is.null(fs) || is.na(fs)) next
    p <- m$plays
    if (!nrow(p)) next
    idx <- split(seq_len(nrow(p)), paste(p$set_no, p$rally_no, sep = "|"))
    for (ii in idx) {
      if (length(sel$sets) && !(p$set_no[ii[1]] %in% sel$sets)) next
      chance <- NA_integer_
      for (j in seq_along(ii)) {
        i <- ii[j]
        if (p$side[i] != fs) next
        if (p$skill[i] == "F") { chance <- i; break }
        if (p$skill[i] != "D" || (!is.na(p$effect[i]) && p$effect[i] == "=")) next
        if (j > 1) for (b in (j - 1):1) {
          if (p$side[ii[b]] == fs) next
          if (p$skill[ii[b]] == "A") chance <- i
          break
        }
        if (!is.na(chance)) break
      }
      if (is.na(chance)) next
      if (length(sel$rots) && !(p$rot[chance] %in% sel$rots)) next
      pw <- which(!is.na(p$point_won[ii]))
      if (!length(pw)) next
      w <- ii[pw[1]]
      won <- if (p$point_won[w]) p$side[w] == fs else p$side[w] != fs
      out[[length(out) + 1L]] <- list(sys = p$sys[chance], rot = p$rot[chance], won = won)
    }
  }
  if (!length(out)) return(data.frame(sys = character(), rot = character(), won = logical()))
  data.frame(sys = vapply(out, function(r) r$sys %||% "?", ""),
             rot = vapply(out, function(r) r$rot %||% NA_character_, ""),
             won = vapply(out, function(r) isTRUE(r$won), logical(1)),
             stringsAsFactors = FALSE)
}
