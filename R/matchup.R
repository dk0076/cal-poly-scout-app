# ---------------------------------------------------------------------------
# THE MATCHUP COMING UP
#
# The Next server tab answers "who serves next". The question behind it is
# bigger: which six of theirs are about to be on the court against which six
# of ours, and what has that pairing done so far.
#
# The staff's other in-match app keeps this as a 71 row lookup, keyed on both
# setters' positions and who serves, carrying the rotation each side is in,
# which middle is front row, and the order the states come in. It is a good
# idea and it does not need a file: the whole table is a consequence of three
# things, so this generates it.
#
#   a side rotates when it wins a rally it received, and then serves
#   the serving side that wins a rally keeps serving, and nothing rotates
#   so from (our rotation, their rotation, who serves) the next two states
#   are the two ways the rally can go
#
# Generated rather than copied means it is checked rather than trusted: the
# tests walk their 71 rows and ours have to agree about which rotation each
# side is in at every step of a set.
#
# What it shows is the pairing, not a prediction of the result: who is front
# row for each side once the rotation lands, and what our side-out and point
# score have been in that pairing across the files loaded.
# ---------------------------------------------------------------------------

#' The two ways the next rally can go, from one state
#'
#' `ours` and `theirs` are rotation keys, `we_serve` says who is serving now.
#' Returns the state after we win the rally and the state after they do.
matchup_next <- function(ours, theirs, we_serve) {
  if (is.na(ours) || is.na(theirs) || is.na(we_serve)) return(NULL)
  if (isTRUE(we_serve))
    list(we_win  = list(ours = ours, theirs = theirs, we_serve = TRUE),
         they_win = list(ours = ours, theirs = rot_advance(theirs),
                         we_serve = FALSE))
  else
    list(we_win  = list(ours = rot_advance(ours), theirs = theirs,
                        we_serve = TRUE),
         they_win = list(ours = ours, theirs = theirs, we_serve = FALSE))
}

#' Every state a set passes through, from a starting one
#'
#' The order the two teams meet in is fixed once the first serve is known:
#' each side-out moves the receiving team on one and hands it the serve. This
#' is the sequence their lookup stores, and it is what lets the tab say what
#' is coming rather than only what is here.
matchup_order <- function(ours, theirs, we_serve, n = 12L) {
  out <- list()
  st <- list(ours = ours, theirs = theirs, we_serve = isTRUE(we_serve))
  for (i in seq_len(n)) {
    out[[i]] <- st
    nx <- matchup_next(st$ours, st$theirs, st$we_serve)
    if (is.null(nx)) break
    # the sequence of pairings is the side-out walk: whoever receives wins
    st <- if (st$we_serve) nx$they_win else nx$we_win
  }
  out
}

#' Front row for a rotation, from a lineup read off the file
#'
#' `lu` is the six players by zone as the file records them for the rotation
#' `at`; rotating forward moves zone 2 into 1, 3 into 2 and so on, so a future
#' rotation's front row is the lineup shifted that many steps.
rotate_lineup <- function(lu, steps = 1L) {
  if (length(lu) != 6L) return(rep(NA_integer_, 6L))
  for (i in seq_len(steps %% 6L)) lu <- c(lu[2:6], lu[1])
  lu
}

#' Zones 4, 3 and 2, which is the front row left to right
front_row <- function(lu) if (length(lu) == 6L) lu[c(4L, 3L, 2L)] else rep(NA_integer_, 3L)

#' How many steps from one rotation key to another, forward
rot_steps <- function(from, to) {
  a <- suppressWarnings(as.integer(substring(from, 2)))
  b <- suppressWarnings(as.integer(substring(to, 2)))
  if (is.na(a) || is.na(b)) return(NA_integer_)
  (b - a) %% 6L
}

#' Side-out and point score for every pairing of rotations
#'
#' One row per (our rotation, their rotation): how many rallies we received
#' in it and how they went, how many we served and how those went. Counts
#' before rates, because most pairings are a handful of rallies and a
#' percentage off three of them is not a number to plan from.
matchup_history <- function(rally_us, rally_them) {
  if (is.null(rally_us) || !nrow(rally_us)) return(NULL)
  key <- function(d) paste(d$match_id, d$set_no, d$rally_no, sep = "|")
  them <- data.table::data.table(k = key(rally_them), theirs = rally_them$rot)
  us <- data.table::data.table(k = key(rally_us), ours = rally_us$rot,
                               served = rally_us$served, won = rally_us$won)
  d <- merge(us, them, by = "k", all.x = TRUE)
  d <- d[!is.na(d$ours) & !is.na(d$theirs) & !is.na(d$won), ]
  if (!nrow(d)) return(NULL)
  d[, .(rec = sum(!served, na.rm = TRUE),
        so  = sum(!served & won, na.rm = TRUE),
        srv = sum(served, na.rm = TRUE),
        ps  = sum(served & won, na.rm = TRUE)),
    by = .(ours, theirs)]
}

#' One line about a pairing: who is front row, and what it has done
matchup_row <- function(st, hist, lu_us, lu_them, at_us, at_them, label) {
  h <- if (is.null(hist)) NULL
       else hist[hist$ours == st$ours & hist$theirs == st$theirs, ]
  fr <- function(lu, at, to) {
    if (is.null(lu) || !length(lu)) return("")
    f <- front_row(rotate_lineup(lu, rot_steps(at, to)))
    paste(vapply(f[!is.na(f)], function(n) sprintf("%s", n), ""), collapse = " ")
  }
  so <- if (is.null(h) || !nrow(h) || !h$rec[1]) NA_real_ else h$so[1] / h$rec[1]
  ps <- if (is.null(h) || !nrow(h) || !h$srv[1]) NA_real_ else h$ps[1] / h$srv[1]
  c(label,
    sprintf("<b>%s</b> %s", rot_said(st$ours), thin(fr(lu_us, at_us, st$ours))),
    sprintf("<b>%s</b> %s", rot_said(st$theirs), thin(fr(lu_them, at_them, st$theirs))),
    if (isTRUE(st$we_serve)) "we serve" else "they serve",
    if (is.na(so)) "" else sprintf("%s %s", pct(so),
                                   thin(sprintf("of %d", h$rec[1]))),
    if (is.na(ps)) "" else sprintf("%s %s", pct(ps),
                                   thin(sprintf("of %d", h$srv[1]))))
}

#' The card: where we are, and the next two pairings
matchup_card <- function(ours, theirs, we_serve, hist,
                         lu_us = NULL, lu_them = NULL) {
  if (is.na(ours) || is.na(theirs)) return("")
  states <- matchup_order(ours, theirs, we_serve, n = 3L)
  labs <- c("On court", "Next", "Then")
  rows <- lapply(seq_along(states), function(i)
    matchup_row(states[[i]], hist, lu_us, lu_them, ours, theirs, labs[i]))
  sprintf('<div class="card"><h3>The matchup</h3>%s%s</div>',
          tbl_generic(c("", "Us", "Them", "Serve", "SO%", "PS%"), rows),
          tiny("Each side-out moves the receiving team on one rotation and hands it the serve, so the next two pairings are fixed whatever else happens. Numbers under the jerseys are the front row, left to right, for that rotation. SO% and PS% are what this pairing has done across the files loaded, with the rally count beside them: most pairings are a handful of rallies, and a percentage off three of them is not a number to plan from."))
}
