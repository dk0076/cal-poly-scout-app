# ---------------------------------------------------------------------------
# DataVolley (.dvw) reader.
#
# A plain-text format, unrelated to the JSON .vsm. It carries the same events
# but no ball coordinates, so every chart is unavailable while every count,
# rate and rotation split works exactly as it does for .vsm.
#
# Play codes are fixed-width, decoded empirically from the files:
#
#     a 12 A T # X5 ~ 4 5 B H 4 ~ 00 F
#     | |  | | | |  | | | | | |
#     | |  | | | |  | | | | | +- blockers
#     | |  | | | |  | | | | +--- attack subtype (H hard / P soft / T tip)
#     | |  | | | |  | | | +----- end subzone
#     | |  | | | |  | | +------- end zone
#     | |  | | | |  | +--------- start zone
#     | |  | | | |  +----------- target attacker (sets only: F/B/C/P/S)
#     | |  | | | +-------------- combination / setter call ("~~" = none)
#     | |  | | +---------------- evaluation  # + ! - / =
#     | |  | +------------------ skill type
#     | |  +-------------------- skill  S R A B D E F
#     | +----------------------- jersey number
#     +------------------------- team, * home / a visiting
#
# The [3SCOUT] section is processed as columns rather than row by row. A row
# loop over a league file's ~3000 rows took 4.6 s, which is most of a minute
# once a coach drops six files on the app before a match; the same work done
# vectorised is about a tenth of a second. The three pieces of state that a row
# loop carried -- rally number, each side's declared setter zone, and the
# running score -- are all last-value-carried-forward over the rows, so they
# come out of cumsum() and nafill() instead.
# ---------------------------------------------------------------------------

# DataVolley numbers nine zones; the tool reports the six standard receiving
# positions. The middle band collapses onto the back row, which is where the
# coordinates put it when the same rally is available in .vsm form.
DV_ZONE6 <- c(`1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L,
              `7` = 5L, `8` = 6L, `9` = 1L)

# DataVolley stores a contact as an index into a 100 x 100 grid laid over the
# court, 1..10000, reading left to right then up. Verified against
# datavolley::dv_index2xy() over all 10000 indices: exact, no error anywhere.
# The grid's own frame is 0.14375..3.85625 across and -0.2037..7.1296 along,
# with the court 0.5..3.5 and 0.5..6.5 inside it, so it rescales onto this
# program's 0..900 by 0..1800 frame with the net at 900.
DV_X0 <- -106.875;      DV_XSTEP <- 11.25
DV_Y0 <- -211.1111111;  DV_YSTEP <- 22.2222222

#' Coordinate pair for a DataVolley 1..10000 grid index, in this app's frame.
dv_xy <- function(idx) {
  i <- suppressWarnings(as.integer(idx))
  bad <- is.na(i) | i < 1L | i > 10000L
  col <- (i - 1L) %% 100L
  row <- (i - 1L) %/% 100L
  x <- DV_X0 + col * DV_XSTEP
  y <- DV_Y0 + row * DV_YSTEP
  x[bad] <- NA_real_; y[bad] <- NA_real_
  list(x = x, y = y)
}

#' Character i of each string, with the format's three null markers as NA.
dv_chr <- function(s, i) {
  ch <- substr(s, i, i)
  ch[ch %in% c("", "~", " ")] <- NA_character_
  ch
}
dv_int <- function(s, i) suppressWarnings(as.integer(dv_chr(s, i)))

#' Split a character vector on ";" into a padded character matrix.
dv_matrix <- function(lines, min_cols = 26L) {
  sp <- strsplit(lines, ";", fixed = TRUE)
  len <- lengths(sp)
  nc <- max(min_cols, max(len, 0L))
  m <- matrix("", nrow = length(sp), ncol = nc)
  if (length(sp)) {
    m[cbind(rep.int(seq_along(sp), len), sequence(len))] <- unlist(sp, use.names = FALSE)
  }
  m
}

#' Last non-NA value carried forward.
dv_locf <- function(x) {
  if (!length(x) || all(is.na(x))) return(x)
  idx <- cumsum(!is.na(x))
  idx[idx == 0L] <- NA_integer_
  x[!is.na(x)][idx]
}

parse_dvw <- function(text, file_name) {
  lines <- strsplit(text, "\r?\n")[[1]]
  hdr <- regmatches(lines, regexec("^\\[3([A-Z-]+)\\]", lines))
  is_hdr <- lengths(hdr) == 2L
  if (!any(is_hdr)) stop(sprintf("%s: not a DataVolley scout file.", file_name), call. = FALSE)
  names_at <- vapply(hdr[is_hdr], `[`, "", 2)
  grp <- cumsum(is_hdr)
  sec <- split(lines[!is_hdr], grp[!is_hdr])
  names(sec) <- names_at[as.integer(names(sec))]
  if (is.null(sec$SCOUT))
    stop(sprintf("%s: not a DataVolley scout file (no [3SCOUT] section).", file_name),
         call. = FALSE)

  cut1 <- function(s) strsplit(s %||% "", ";", fixed = TRUE)[[1]]
  fld <- function(f, i) if (length(f) >= i) trimws(f[i]) else ""

  # ---- teams ------------------------------------------------------------
  tl <- sec$TEAMS[nzchar(trimws(sec$TEAMS %||% character()))]
  t1 <- cut1(tl[1]); t2 <- cut1(tl[2])
  teams <- list(
    home = list(key = "home", name = if (nzchar(fld(t1, 2))) fld(t1, 2) else "Home",
                code = fld(t1, 1)),
    away = list(key = "away", name = if (nzchar(fld(t2, 2))) fld(t2, 2) else "Away",
                code = fld(t2, 1))
  )

  # ---- rosters ----------------------------------------------------------
  # Surnames come from the explicit field (column 10), never from splitting a
  # full name: last-token extraction silently mangles compound surnames.
  rosters <- list(); warn <- character()
  for (kv in list(c("PLAYERS-H", "home"), c("PLAYERS-V", "away"))) {
    raw <- sec[[kv[1]]] %||% character()
    raw <- raw[nzchar(trimws(raw))]
    out <- data.frame(num = integer(), last = character(), full = character(),
                      libero = logical(), stringsAsFactors = FALSE)
    if (length(raw)) {
      m <- dv_matrix(raw, 12L)
      num <- suppressWarnings(as.integer(trimws(m[, 2])))
      ok <- !is.na(num)
      last <- trimws(m[ok, 10]); first <- trimws(m[ok, 11]); num <- num[ok]
      bogus <- !nzchar(last) | grepl("^#?\\d+$", last)
      nm <- ifelse(bogus, paste0("#", num), last)
      out <- data.frame(num = num, last = nm,
                        full = ifelse(!bogus & nzchar(first), paste(first, nm), nm),
                        libero = FALSE, stringsAsFactors = FALSE)
    }
    rosters[[kv[2]]] <- out
    dup <- unique(out$num[duplicated(out$num)])
    if (length(dup))
      warn <- c(warn, sprintf("%s: duplicate jersey number(s) %s on the %s roster, a new-season roster stacked on the old one, so player-level stats will merge two people.",
                              file_name, paste(dup, collapse = ", "), kv[2]))
  }

  # ---- code metadata ----------------------------------------------------
  calls <- list()
  ac <- (sec$ATTACKCOMBINATION %||% character())
  ac <- ac[nzchar(trimws(ac))]
  if (length(ac)) {
    m <- dv_matrix(ac, 9L)
    for (i in seq_len(nrow(m))) {
      k <- trimws(m[i, 1])
      if (nzchar(k)) calls[[k]] <- list(
        name = trimws(m[i, 5]),
        target = if (nzchar(trimws(m[i, 9]))) trimws(m[i, 9]) else NA_character_)
    }
  }
  setter_calls <- list()
  sc <- (sec$SETTERCALL %||% character())
  sc <- sc[nzchar(trimws(sc))]
  if (length(sc)) {
    m <- dv_matrix(sc, 3L)
    for (i in seq_len(nrow(m))) {
      k <- trimws(m[i, 1])
      if (nzchar(k)) setter_calls[[k]] <- trimws(m[i, 3])
    }
  }

  mline <- (sec$MATCH %||% character())
  mline <- mline[nzchar(trimws(mline))]
  date <- if (length(mline)) fld(cut1(mline[1]), 1) else NA_character_
  if (!length(date) || !nzchar(date %||% "")) date <- NA_character_

  # ---- scout section, as columns ---------------------------------------
  scout <- sec$SCOUT
  scout <- scout[nzchar(trimws(scout))]
  M <- dv_matrix(scout)
  code <- M[, 1]

  set_no <- suppressWarnings(as.integer(trimws(M[, 9])))
  set_no[is.na(set_no)] <- 1L
  # Group by runs of the same set number rather than by its value, so the
  # per-set resets match the row loop this replaced even in a file whose set
  # numbers are not contiguous.
  run <- cumsum(c(TRUE, diff(set_no) != 0L))

  lu_home <- matrix(suppressWarnings(as.integer(trimws(M[, 15:20]))), ncol = 6L)
  lu_away <- matrix(suppressWarnings(as.integer(trimws(M[, 21:26]))), ncol = 6L)
  lu_home[!is.na(lu_home) & lu_home == 0L] <- NA_integer_
  lu_away[!is.na(lu_away) & lu_away == 0L] <- NA_integer_

  pm <- regmatches(code, regexec("^([*a])p(\\d+):(\\d+)", code))
  is_pt <- lengths(pm) == 4L
  zm <- regmatches(code, regexec("^([*a])z(\\d)", code))
  is_z <- lengths(zm) == 3L
  sm <- regmatches(code, regexec("^([*a])P(\\d+)", code))
  is_sp <- lengths(sm) == 3L
  is_play <- grepl("^[*a]\\d{2}[SRABDEF]", code)

  # Rally number: how many point rows closed before this row, inside this set.
  pre <- cumsum(is_pt) - as.integer(is_pt)
  rally_no <- pre - ave_min(pre, run)

  # Point rows alone are not enough. A practice file is routinely coded with
  # no score at all -- one here has six sets and four point rows -- and then
  # every set collapses into a single rally, which silently wrecks the
  # serve-receive walk downstream: on this file it called 101 side-out
  # attacks transition. A rally begins with a serve, so the boundaries are
  # recoverable the same way the .vsm path recovers them. On a file with
  # proper point rows each rally holds one serve and nothing changes.
  # The rule is the .vsm's rule: a serve opens a rally unless it is already
  # the first touch of one. Keying on "the second serve in the group" instead
  # misses a set that opens mid-rally -- one here begins with a set, an attack
  # and a block before anyone serves, and that serve has to start a rally even
  # though it is the first in its group.
  is_serve <- is_play & substr(code, 4, 4) == "S"
  grp <- paste(run, rally_no)
  nth_play <- stats::ave(as.integer(is_play), grp, FUN = cumsum)
  starts_new <- is_serve & nth_play > 1L
  rally_no <- rally_no + stats::ave(as.integer(starts_new), run, FUN = cumsum)

  # The file declares each setter's zone directly with rows like "*z6" / "az5",
  # updated as the team rotates. That is authoritative: deriving the rotation
  # from the first coded PLAY of a set is wrong whenever the receiving team
  # wins the opening rally, because by then they have already rotated. One
  # observed file opened a set in zone 6 (R2) while its first play row read
  # zone 5 (R3).
  z_tag <- ifelse(is_z, vapply(zm, function(v) if (length(v) == 3L) v[2] else NA_character_, ""), NA_character_)
  z_val <- ifelse(is_z, suppressWarnings(as.integer(vapply(zm, function(v) if (length(v) == 3L) v[3] else NA_character_, ""))), NA_integer_)
  zone_home <- dv_locf(ifelse(!is.na(z_tag) & z_tag == "*", z_val, NA_integer_))
  zone_away <- dv_locf(ifelse(!is.na(z_tag) & z_tag == "a", z_val, NA_integer_))

  s_tag <- ifelse(is_sp, vapply(sm, function(v) if (length(v) == 3L) v[2] else NA_character_, ""), NA_character_)
  s_val <- ifelse(is_sp, suppressWarnings(as.integer(vapply(sm, function(v) if (length(v) == 3L) v[3] else NA_character_, ""))), NA_integer_)
  setter_home <- dv_locf(ifelse(!is.na(s_tag) & s_tag == "*", s_val, NA_integer_))
  setter_away <- dv_locf(ifelse(!is.na(s_tag) & s_tag == "a", s_val, NA_integer_))

  # Running score: the last point row inside this set run, 0-0 before any.
  pt_h <- ifelse(is_pt, suppressWarnings(as.integer(vapply(pm, function(v) if (length(v) == 4L) v[3] else NA_character_, ""))), NA_integer_)
  pt_a <- ifelse(is_pt, suppressWarnings(as.integer(vapply(pm, function(v) if (length(v) == 4L) v[4] else NA_character_, ""))), NA_integer_)
  sh <- dv_locf(ifelse(is_pt, pt_h, NA_integer_)); sa <- dv_locf(ifelse(is_pt, pt_a, NA_integer_))
  # a carried-forward score must not leak across a set boundary
  new_run <- c(TRUE, diff(run) != 0L)
  reset <- dv_locf(ifelse(new_run, seq_along(run), NA_integer_))
  carried <- dv_locf(ifelse(is_pt, seq_along(run), NA_integer_))
  stale <- is.na(carried) | carried < reset
  sh[stale] <- 0L; sa[stale] <- 0L
  sh[is.na(sh)] <- 0L; sa[is.na(sa)] <- 0L

  # ---- opening lineup per set, from the z rows -------------------------
  start_lineups <- list(home = list(), away = list())
  for (i in which(is_z)) {
    side <- if (z_tag[i] == "*") "home" else "away"
    seen <- vapply(start_lineups[[side]], function(x) x$set_no, 0)
    if (set_no[i] %in% seen) next
    pos <- if (side == "home") lu_home[i, ] else lu_away[i, ]
    if (any(is.na(pos))) next
    start_lineups[[side]][[length(start_lineups[[side]]) + 1L]] <-
      list(set_no = set_no[i], positions = pos, setter_at = z_val[i], served = NA)
  }

  # ---- play rows -------------------------------------------------------
  k <- which(is_play)
  if (!length(k)) {
    plays <- empty_plays()
  } else {
    cd <- code[k]
    tag <- substr(cd, 1, 1)
    side <- ifelse(tag == "*", "home", "away")
    num <- suppressWarnings(as.integer(substr(cd, 2, 3)))
    skill <- substr(cd, 4, 4)
    combo <- gsub("~", "", substr(cd, 7, 8), fixed = TRUE)
    combo[!nzchar(combo)] <- NA_character_
    targ <- dv_chr(cd, 9)
    atk_type <- ifelse(skill == "A", dv_chr(cd, 13), NA_character_)
    blockers <- ifelse(skill == "A", dv_int(cd, 14), NA_integer_)
    end_zone <- dv_int(cd, 11)

    # ---- coordinates -------------------------------------------------
    # Fields 5 and 7 are the start and end grid indices (6 is the midpoint,
    # which nothing here uses). Empty on files coded without ball positions.
    #
    # The frame needs care, and getting it wrong is invisible rather than
    # loud. Read off whole rallies: Serve, Set, Attack and Block are recorded
    # in the ACTING team's own frame, each team attacking "up" in its own
    # rows. Reception and Dig are not -- those rows carry a verbatim copy of
    # the serve or attack they answer, so they sit in the OPPONENT's frame. A
    # reception left alone therefore puts the receiver behind the far endline
    # and disagrees with the same rally read from a .vsm, where a reception is
    # already in the receiver's own frame. Mirroring R and D is what makes the
    # two formats describe the same court.
    st <- dv_xy(trimws(M[k, 5]))
    en <- dv_xy(trimws(M[k, 7]))
    x0 <- st$x; y0 <- st$y; x1 <- en$x; y1 <- en$y

    # A reception or dig row is the incoming ball, not the player's own shot,
    # so its two ends mean different things from every other skill: start is
    # where the serve or attack was struck, end is where the ball reached the
    # defender. Mirrored into that defender's own frame, the END is their
    # contact point -- which is exactly what a .vsm stores as the reception's
    # START. Measured on the same match in both formats, those two agree
    # (median y 300 against 306), while the raw start sits at y 1922, out
    # beyond the far endline.
    #
    # There is no counterpart to the .vsm pass destination, so it stays NA
    # rather than being filled with the serve origin, which would draw an
    # arrow pointing the wrong way down the court.
    def <- skill %in% c("R", "D")
    if (any(def)) {
      x0[def] <- COURT$W - x1[def]; y0[def] <- COURT$L - y1[def]
      x1[def] <- NA_real_;          y1[def] <- NA_real_
    }
    has_xy <- !is.na(x0) & !is.na(y0) & !is.na(x1) & !is.na(y1)

    cmeta_name <- vapply(combo, function(c0)
      if (is.na(c0) || is.null(calls[[c0]])) NA_character_ else (calls[[c0]]$name %||% NA_character_), "")
    cmeta_targ <- vapply(combo, function(c0)
      if (is.na(c0) || is.null(calls[[c0]])) NA_character_ else (calls[[c0]]$target %||% NA_character_), "")
    scall_name <- vapply(combo, function(c0)
      if (is.na(c0) || is.null(setter_calls[[c0]])) NA_character_ else setter_calls[[c0]], "")

    # lineups, re-based so lu* is always the acting side and olu* the other one
    own_mat  <- lu_home[k, , drop = FALSE]
    oth_mat  <- lu_away[k, , drop = FALSE]
    swap <- side == "away"
    if (any(swap)) {
      tmp <- own_mat[swap, , drop = FALSE]
      own_mat[swap, ] <- oth_mat[swap, , drop = FALSE]
      oth_mat[swap, ] <- tmp
    }

    rl <- rosters
    look <- function(sd, field) {
      v <- setNames(rl[[sd]][[field]], as.character(rl[[sd]]$num))
      v
    }
    name_full <- character(length(k)); name_last <- character(length(k))
    for (sd in c("home", "away")) {
      w <- which(side == sd)
      if (!length(w)) next
      fl <- look(sd, "full"); ll <- look(sd, "last")
      key <- as.character(num[w])
      name_full[w] <- ifelse(is.na(fl[key]), paste0("#", num[w]), fl[key])
      name_last[w] <- ifelse(is.na(ll[key]), paste0("#", num[w]), ll[key])
    }

    own_zone <- ifelse(side == "home", zone_home[k], zone_away[k])
    oth_zone <- ifelse(side == "home", zone_away[k], zone_home[k])

    plays <- data.table::data.table(
      uid = seq_along(k), match_id = file_name, file_name = file_name,
      date = date, set_no = set_no[k], rally_no = rally_no[k],
      seq = NA_integer_, side = side, tag = tag,
      team_name = ifelse(side == "home", teams$home$name, teams$away$name),
      num = num, player = name_full, player_short = name_last, libero = FALSE,
      skill = skill, hit_type = dv_chr(cd, 5), effect = dv_chr(cd, 6),
      skill_type = atk_type,
      call = ifelse(skill == "A", combo, NA_character_),
      call_name = ifelse(skill == "E", scall_name, cmeta_name),
      setter_call = ifelse(skill == "E", combo, NA_character_),
      target = ifelse(skill == "E", targ, cmeta_targ),
      blockers = blockers,
      start_zone = dv_int(cd, 10), start_sub_zone = NA_character_,
      end_zone = end_zone, end_sub_zone = dv_chr(cd, 12),
      zone6 = unname(DV_ZONE6[as.character(end_zone)]),
      custom = NA_character_, time = NA_real_,
      x0 = x0, y0 = y0, x1 = x1, y1 = y1, has_xy = has_xy,
      setter_at = own_zone, rot = rot_name(own_zone),
      opp_setter_at = oth_zone, opp_rot = rot_name(oth_zone),
      lu1 = own_mat[, 1], lu2 = own_mat[, 2], lu3 = own_mat[, 3],
      lu4 = own_mat[, 4], lu5 = own_mat[, 5], lu6 = own_mat[, 6],
      olu1 = oth_mat[, 1], olu2 = oth_mat[, 2], olu3 = oth_mat[, 3],
      olu4 = oth_mat[, 4], olu5 = oth_mat[, 5], olu6 = oth_mat[, 6],
      court_pos = NA_integer_, front_row = NA,
      serving = NA, phase = NA_character_, passer = NA_integer_,
      rec_effect = NA_character_, point_won = NA,
      score = paste0(sh[k], "-", sa[k]),
      setter_num = ifelse(side == "home", setter_home[k], setter_away[k])
    )

    # The point row closes the rally; its winner is stamped on every touch of
    # that rally.
    if (any(is_pt)) {
      win <- ifelse(vapply(pm[is_pt], `[`, "", 2) == "*", "home", "away")
      wk <- data.table::data.table(set_no = set_no[is_pt], rally_no = rally_no[is_pt],
                                   winner = win)
      wk <- unique(wk, by = c("set_no", "rally_no"))
      plays <- merge(plays, wk, by = c("set_no", "rally_no"), all.x = TRUE, sort = FALSE)
      plays$point_won <- ifelse(is.na(plays$winner), NA, plays$side == plays$winner)
      plays$winner <- NULL
      data.table::setorder(plays, uid)
    }
  }

  if (nrow(plays)) {
    # ---- rotation + front row: the setter is whoever sets most often -----
    for (sd in c("home", "away")) {
      idx <- which(plays$side == sd)
      if (!length(idx)) next
      tb <- table(plays$num[idx][plays$skill[idx] == "E"])
      if (!length(tb)) next
      setter <- as.integer(names(tb)[which.max(tb)])
      luc <- as.matrix(plays[idx, paste0("lu", 1:6), with = FALSE])
      # no z row for this side: derive the rotation from where the setter stands
      need <- is.na(plays$rot[idx])
      if (any(need)) {
        zz <- apply(luc[need, , drop = FALSE], 1, function(r) {
          h <- which(!is.na(r) & r == setter); if (length(h)) h[1] else NA_integer_
        })
        plays$rot[idx[need]] <- rot_name(zz)
        plays$setter_at[idx[need]] <- zz
      }
      own <- apply(cbind(luc, plays$num[idx]), 1, function(r) {
        h <- which(!is.na(r[1:6]) & r[1:6] == r[7]); if (length(h)) h[1] else NA_integer_
      })
      plays$court_pos[idx] <- own
      plays$front_row[idx] <- ifelse(is.na(own), NA, own >= 2L & own <= 4L)  # zones 2, 3, 4
    }
    plays <- dvw_fill_opp_rot(plays)
    # who served this rally, for the phase walk in finalize_match()
    plays[, serving := {
      sv <- side[skill == "S"]
      if (length(sv)) side == sv[1] else NA
    }, by = .(set_no, rally_no)]
  }

  set_nos <- sort(unique(plays$set_no))
  sets_meta <- lapply(set_nos, function(n)
    list(set_no = n, rallies = length(unique(plays$rally_no[plays$set_no == n]))))

  # opening lineup per set for any side the z rows did not cover, plus whether
  # that team opened the set serving
  setter_of <- list()
  for (sd in c("home", "away")) {
    tb <- table(plays$num[plays$side == sd & plays$skill == "E"])
    setter_of[[sd]] <- if (length(tb)) as.integer(names(tb)[which.max(tb)]) else NA_integer_
  }
  for (n in set_nos) {
    in_set <- plays[plays$set_no == n, ]
    fs <- in_set$side[in_set$skill == "S"]
    fside <- if (length(fs)) fs[1] else NA_character_
    for (sd in c("home", "away")) {
      hit <- which(vapply(start_lineups[[sd]], function(x) x$set_no == n, logical(1)))
      if (length(hit)) {
        if (is.na(start_lineups[[sd]][[hit[1]]]$served) && !is.na(fside))
          start_lineups[[sd]][[hit[1]]]$served <- fside == sd
        next
      }
      fr <- in_set[in_set$side == sd, ]
      if (!nrow(fr)) next
      pos <- as.integer(unlist(fr[1, paste0("lu", 1:6), with = FALSE]))
      if (any(is.na(pos))) next
      sa2 <- which(pos == setter_of[[sd]])
      start_lineups[[sd]][[length(start_lineups[[sd]]) + 1L]] <- list(
        set_no = n, positions = pos,
        setter_at = if (length(sa2)) sa2[1] else NA_integer_,
        served = if (is.na(fside)) NA else fside == sd)
    }
  }

  list(match_id = file_name, file_name = file_name, date = date,
       comment = substr(paste(sec$COMMENTS %||% "", collapse = " "), 1, 300),
       teams = teams, rosters = rosters, calls = calls,
       sets = sets_meta, plays = plays, start_lineups = start_lineups,
       # as for .vsm, this is a property of the contents: a DataVolley file
       # coded without ball positions leaves the coordinate fields empty
       format = "dvw", no_coords = !nrow(plays) || !any(plays$has_xy),
       warnings = warn)
}

#' Minimum of x within each group, returned aligned to x.
ave_min <- function(x, g) {
  m <- tapply(x, g, min)
  unname(m[as.character(g)])
}

#' Opponent rotation per play, filled from the other side's rotation at the
#' same moment in the same rally. The z rows give it directly for most files;
#' this covers the ones where one side's zone was never declared.
dvw_fill_opp_rot <- function(plays) {
  need <- which(is.na(plays$opp_rot))
  if (!length(need)) return(plays)
  key <- paste(plays$set_no, plays$rally_no, plays$side, sep = "|")
  first_rot <- tapply(plays$rot, key, function(v) {
    v <- v[!is.na(v)]; if (length(v)) v[1] else NA_character_
  })
  other <- ifelse(plays$side == "home", "away", "home")
  want <- paste(plays$set_no, plays$rally_no, other, sep = "|")[need]
  plays$opp_rot[need] <- unname(first_rot[want])
  plays
}
