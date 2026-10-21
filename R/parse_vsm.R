# ---------------------------------------------------------------------------
# VolleyStation (.vsm) reader.
#
# The file is JSON. Parse it as JSON; never regex it.
#   team$home$name, team$home$players[[i]]$shirtNumber / firstName / lastName
#   scout$sets[[i]]$events[[j]]$exchange$plays[[k]]
# Team on a play is "*" for home and "a" for away, matching DataVolley.
#
# One exchange is one rally. Coordinates come from each play's travelPath and
# are already in the acting team's own frame (see R/core.R), so nothing here
# mirrors or re-bases them.
# ---------------------------------------------------------------------------

#' Roster for one side, as a data.frame keyed by jersey number.
#'
#' Three roster failure modes are checked rather than assumed away: duplicate
#' jersey numbers (a new-season roster appended on top of the old one, which
#' would silently merge two people), compound surnames (must be read from the
#' explicit lastName field -- last-token splitting mangles "Cordero Barr"), and
#' placeholder names (must render as an explicit unknown, never as a plausible
#' fabricated identifier).
vsm_roster <- function(side) {
  players <- side$players %||% list()
  libero <- unlist(side$libero %||% list())
  if (!length(players))
    return(data.frame(num = integer(), last = character(), full = character(),
                      libero = logical(), pos = character(),
                      stringsAsFactors = FALSE))
  rows <- lapply(players, function(p) {
    n <- suppressWarnings(as.integer(p$shirtNumber %||% p$number %||% NA))
    if (is.na(n)) return(NULL)
    last  <- trimws(as.character(p$lastName  %||% ""))
    first <- trimws(as.character(p$firstName %||% ""))
    bogus <- !nzchar(last) || grepl("^#?\\d+$", last) ||
             grepl("^(filler|player|unknown)$", last, ignore.case = TRUE)
    nm <- if (bogus) paste0("#", n) else last
    full <- if (!bogus && nzchar(first) && !grepl("^player$", first, ignore.case = TRUE))
      paste(first, nm) else nm
    code <- suppressWarnings(as.integer(p$position %||% NA))
    lib <- n %in% libero | (!is.na(code) & code == 0L)
    # An export without a position field is still telling us about its
    # liberos, since it lists them separately. Older exports do exactly this.
    pos <- pos_label(code)
    if (lib && pos == "?") pos <- "L"
    data.frame(num = n, last = nm, full = full, libero = lib,
               pos = pos, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows))
    return(data.frame(num = integer(), last = character(), full = character(),
                      libero = logical(), pos = character(),
                      stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

#' A .vsm lineup's positions block as a length-6 integer vector (zones 1..6),
#' or NA x 6 when it is absent or partial.
vsm_six <- function(lu) {
  pos <- lu$positions
  out <- rep(NA_integer_, 6)
  if (is.null(pos)) return(out)
  for (z in 1:6) {
    v <- suppressWarnings(as.integer(pos[[as.character(z)]] %||% NA))
    out[z] <- if (is.na(v) || v == 0L) NA_integer_ else v
  }
  out
}

parse_vsm <- function(text, file_name) {
  raw <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                  error = function(e)
                    stop(sprintf("%s: not readable as VolleyStation JSON. %s",
                                 file_name, conditionMessage(e)), call. = FALSE))
  if (is.null(raw$scout) || !length(raw$scout$sets))
    stop(sprintf("%s: no scout.sets block, is this a .vsm match file?", file_name),
         call. = FALSE)

  home <- raw$team$home %||% list(name = "Home", players = list())
  away <- raw$team$away %||% list(name = "Away", players = list())
  rosters <- list(home = vsm_roster(home), away = vsm_roster(away))

  warn <- character()
  for (sd in c("home", "away")) {
    nums <- rosters[[sd]]$num
    dup <- unique(nums[duplicated(nums)])
    if (length(dup))
      warn <- c(warn, sprintf("%s: duplicate jersey number(s) %s on the %s roster, two seasons stacked in one file, player-level stats will merge two people.",
                              file_name, paste(dup, collapse = ", "), sd))
  }

  calls <- list()
  for (c0 in raw$attackCombinations %||% list()) {
    if (!is.null(c0$code))
      calls[[as.character(c0$code)]] <-
        list(name = c0$name %||% NA_character_, target = c0$targetAttacker %||% NA_character_)
  }
  # The file's own setter-call table. Worth reading rather than leaning on the
  # built-in vocabulary in core.R: a real file here defines KD "drift", which
  # that list does not have, and a call the program cannot name reads as a bare
  # code on the Set origin tab.
  setter_calls <- list()
  for (c0 in raw$setterCalls %||% list())
    if (!is.null(c0$code)) setter_calls[[as.character(c0$code)]] <- c0$name %||% NA_character_

  teams <- list(
    home = list(key = "home", name = as.character(home$name %||% "Home"),
                code = as.character(home$shortCode %||% home$code %||% "HOM")),
    away = list(key = "away", name = as.character(away$name %||% "Away"),
                code = as.character(away$shortCode %||% away$code %||% "AWY"))
  )
  match_id <- file_name
  date <- if (!is.null(raw$startDate)) substr(as.character(raw$startDate), 1, 10) else ""

  rows <- vector("list", 4096L); nrow_i <- 0L
  sets_meta <- list()
  start_lineups <- list(home = list(), away = list())
  uid <- 0L
  split_exchanges <- 0L; split_sets <- list()

  push <- function(r) {
    nrow_i <<- nrow_i + 1L
    if (nrow_i > length(rows)) length(rows) <<- length(rows) * 2L
    rows[[nrow_i]] <<- r
  }

  for (si in seq_along(raw$scout$sets)) {
    st <- raw$scout$sets[[si]]
    set_no <- si
    rally_no <- 0L
    events <- st$events %||% list()
    coded <- vapply(events, function(e) length(e$exchange$plays %||% list()) > 0, logical(1))

    # Opening lineup of the set: the first rally that actually has plays. A
    # team's opening rotation differs depending on whether it opens the set
    # serving or receiving, so record which it was.
    if (any(coded)) {
      first <- events[[which(coded)[1]]]
      srv_tags <- vapply(first$exchange$plays, function(q) as.character(q$skill %||% ""), "")
      serving_tag0 <- if (any(srv_tags == "S"))
        as.character(first$exchange$plays[[which(srv_tags == "S")[1]]]$team) else NA_character_
      for (sd in c("home", "away")) {
        l <- first$lineup[[sd]]
        six <- vsm_six(l)
        if (!any(is.na(six))) {
          my_tag <- if (sd == "home") "*" else "a"
          start_lineups[[sd]][[length(start_lineups[[sd]]) + 1L]] <- list(
            set_no = set_no, positions = six,
            setter_at = suppressWarnings(as.integer(l$setterAt %||% NA)),
            served = if (is.na(serving_tag0)) NA else serving_tag0 == my_tag)
        }
      }
    }

    for (ev in events) {
      ex <- ev$exchange
      ev_plays <- ex$plays %||% list()
      if (!length(ev_plays)) next
      lineup <- ev$lineup %||% list()

      # One exchange is supposed to be one rally. In practice a lot of this
      # program's own files are coded with a whole set dropped into a single
      # exchange -- one observed file holds 245 plays and 44 serves in one of
      # them. Taken at face value that makes the entire set one rally, and
      # since the serve-receive test resets per rally, every attack after the
      # first in each set reads as transition. The splits on those files were
      # quietly meaningless.
      #
      # A rally begins with a serve, so the boundaries are recoverable: split
      # at every serve after the first. On a properly segmented exchange there
      # is exactly one serve and this changes nothing. On the set above it
      # recovers 44 rallies, which is what that team's own report says.
      ev_skills <- vapply(ev_plays, function(p) as.character(p$skill %||% ""), "")
      cuts <- which(ev_skills == "S")
      cuts <- cuts[cuts > 1L]
      bounds <- c(1L, cuts, length(ev_plays) + 1L)
      n_seg <- length(bounds) - 1L
      if (n_seg > 1L) {
        split_exchanges <- split_exchanges + 1L
        split_sets[[as.character(set_no)]] <- TRUE
      }

      for (sg in seq_len(n_seg)) {
      plays <- ev_plays[bounds[sg]:(bounds[sg + 1L] - 1L)]
      last_seg <- sg == n_seg
      rally_no <- rally_no + 1L
      skills <- ev_skills[bounds[sg]:(bounds[sg + 1L] - 1L)]
      srv_i <- if (any(skills == "S")) which(skills == "S")[1] else 1L
      serving_tag <- as.character(plays[[srv_i]]$team %||% NA)

      score <- if (!is.null(ev$score))
        sprintf("%s-%s", ev$score$home %||% "", ev$score$away %||% "") else ""

      for (pi in seq_along(plays)) {
        p <- plays[[pi]]
        tag <- as.character(p$team %||% "")
        side <- if (tag == "*") "home" else "away"
        other <- if (side == "home") "away" else "home"
        lu <- lineup[[side]] %||% list()
        six <- vsm_six(lu)
        osix <- vsm_six(lineup[[other]] %||% list())
        num <- suppressWarnings(as.integer(p$player %||% NA))
        rp <- rosters[[side]][match(num, rosters[[side]]$num), ]
        has_rp <- nrow(rp) == 1L && !is.na(rp$num)
        court_pos <- if (!is.na(num) && any(!is.na(six) & six == num))
          which(!is.na(six) & six == num)[1] else NA_integer_

        tp <- p$travelPath %||% list()
        a <- if (length(tp)) tp[[1]] else NULL
        b <- if (length(tp)) tp[[length(tp)]] else NULL
        x0 <- if (!is.null(a)) suppressWarnings(as.numeric(a$x %||% NA)) else NA_real_
        y0 <- if (!is.null(a)) suppressWarnings(as.numeric(a$y %||% NA)) else NA_real_
        x1 <- if (!is.null(b)) suppressWarnings(as.numeric(b$x %||% NA)) else NA_real_
        y1 <- if (!is.null(b)) suppressWarnings(as.numeric(b$y %||% NA)) else NA_real_
        has_xy <- !is.na(x0) && !is.na(y0) && !is.na(x1) && !is.na(y1)

        # Phase, passer and reception grade are left for finalize_match(),
        # which derives them the same way for both formats. Computing them
        # here as well was how the two paths drifted apart.
        phase <- NA_character_; passer <- NA_integer_; rec_effect <- NA_character_

        combo <- p$combination %||% NA
        combo <- if (is.null(combo) || is.na(combo)) NA_character_ else as.character(combo)
        cmeta <- if (!is.na(combo)) calls[[combo]] else NULL
        targ <- p$targetAttacker %||% NA
        targ <- if (is.null(targ) || is.na(targ)) NA_character_ else as.character(targ)
        if (is.na(targ) && identical(as.character(p$skill), "A") && !is.null(cmeta))
          targ <- as.character(cmeta$target %||% NA)

        uid <- uid + 1L
        push(list(
          uid = uid, match_id = match_id, file_name = file_name, date = date,
          set_no = set_no, rally_no = rally_no, seq = pi,
          side = side, tag = tag, team_name = teams[[side]]$name,
          num = num,
          player       = if (has_rp) rp$full else paste0("#", num),
          player_short = if (has_rp) rp$last else paste0("#", num),
          libero = isTRUE(has_rp && rp$libero),
          skill = as.character(p$skill %||% NA),
          hit_type   = as.character(p$hitType   %||% NA),
          effect     = as.character(p$effect    %||% NA),
          skill_type = as.character(p$skillType %||% NA),
          call = combo,
          # a set's combination is a setter call, which lives in its own table
          call_name = if (identical(as.character(p$skill), "E"))
              (if (!is.na(combo) && !is.null(setter_calls[[combo]]))
                 as.character(setter_calls[[combo]]) else NA_character_)
            else if (!is.null(cmeta)) as.character(cmeta$name %||% NA) else NA_character_,
          setter_call = if (identical(as.character(p$skill), "E")) combo else NA_character_,
          target = targ,
          blockers = suppressWarnings(as.integer(p$players %||% NA)),
          start_zone = suppressWarnings(as.integer(p$startZone %||% NA)),
          start_sub_zone = as.character(p$startSubZone %||% NA),
          end_zone = suppressWarnings(as.integer(p$endZone %||% NA)),
          end_sub_zone = as.character(p$endSubZone %||% NA),
          custom = as.character(p$custom %||% NA),
          time = suppressWarnings(as.numeric(p$time %||% NA)),
          x0 = x0, y0 = y0, x1 = x1, y1 = y1, has_xy = has_xy,
          setter_at = suppressWarnings(as.integer(lu$setterAt %||% NA)),
          rot = rot_name(lu$setterAt %||% NA),
          opp_setter_at = suppressWarnings(as.integer((lineup[[other]] %||% list())$setterAt %||% NA)),
          opp_rot = rot_name((lineup[[other]] %||% list())$setterAt %||% NA),
          lu1 = six[1], lu2 = six[2], lu3 = six[3], lu4 = six[4], lu5 = six[5], lu6 = six[6],
          olu1 = osix[1], olu2 = osix[2], olu3 = osix[3],
          olu4 = osix[4], olu5 = osix[5], olu6 = osix[6],
          court_pos = court_pos,
          front_row = if (is.na(court_pos)) NA else court_pos >= 2L && court_pos <= 4L,
          serving = if (is.na(serving_tag)) NA else tag == serving_tag,
          phase = phase, passer = passer, rec_effect = rec_effect,
          # The exchange records one winner. Where it held several rallies,
          # that winner belongs to the last of them; the earlier ones have no
          # recorded outcome and must not borrow this one.
          point_won = if (last_seg && !is.null(ex$point))
            tag == as.character(ex$point) else NA,
          score = score
        ))
      }
      }
    }
    if (rally_no)
      sets_meta[[length(sets_meta) + 1L]] <- list(set_no = set_no, rallies = rally_no)
  }

  length(rows) <- nrow_i
  plays <- if (nrow_i) data.table::rbindlist(rows, fill = TRUE) else empty_plays()

  # Whether the charts can be drawn is a property of the CONTENT, not of the
  # format. Plenty of real .vsm files are coded without ball coordinates --
  # every practice file to hand here is -- and taking "it is a .vsm" to mean
  # "it has coordinates" draws an empty court frame on four tabs, which is
  # worse than drawing nothing.
  # A re-split exchange carries one lineup for everything inside it, so for
  # those sets the rotation is only the one standing at the start. Counts and
  # rates are fine; anything cut by rotation is not, and that has to be said
  # rather than left to look like a finding.
  if (split_exchanges)
    warn <- c(warn, sprintf(
      "%s: %d exchange%s held more than one rally (set%s %s). They have been split at each serve so the serve-receive and transition splits are right, but the file records only one lineup per exchange, treat rotation cuts on those sets as the starting rotation only.",
      file_name, split_exchanges, if (split_exchanges == 1) "" else "s",
      if (length(split_sets) == 1) "" else "s",
      paste(sort(as.integer(names(split_sets))), collapse = ", ")))

  no_coords <- !nrow(plays) || !any(plays$has_xy)
  if (no_coords && nrow(plays))
    warn <- c(warn, sprintf("%s: no ball coordinates in this file, so the charts are suppressed. Every count, rate and rotation split still works.",
                            file_name))

  list(match_id = match_id, file_name = file_name, date = date,
       comment = as.character(raw$comment %||% ""),
       teams = teams, rosters = rosters, calls = calls,
       setter_calls = setter_calls,
       sets = sets_meta, plays = plays, start_lineups = start_lineups,
       format = "vsm", no_coords = no_coords, warnings = warn)
}
