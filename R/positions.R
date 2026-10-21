# ---------------------------------------------------------------------------
# Position overrides
#
# Both file formats carry a position per player, so the roster arrives filled
# in and nothing below is needed to read a match. What it is for is the case
# where the file is wrong or blank: a scouted opponent typed in from a lineup
# sheet, a freshman listed at the position she was recruited for rather than
# the one she plays, a practice file where everyone was entered in a hurry.
#
# Overrides live in a file beside the app rather than in the session, because
# the point is to set them once before coding and have them hold through a
# whole match of live re-reads, a restart, and the next file off the same
# roster. They are keyed by team and jersey number, never by the match, so
# fixing #12 once fixes her everywhere.
#
# The file is small, plain JSON, and safe to delete: lose it and the app goes
# back to what the match files say.
# ---------------------------------------------------------------------------

POS_FILE <- file.path("data", "positions.json")

#' Every override, as a named list of team -> (jersey -> position)
read_positions <- function(path = POS_FILE) {
  if (!file.exists(path)) return(list())
  out <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                  error = function(e) list())
  if (!is.list(out)) list() else out
}

#' Write them back, creating data/ if this is the first time
write_positions <- function(ov, path = POS_FILE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  # Through a temporary file: a half-written positions file read back by the
  # next startup would silently drop a roster's worth of corrections.
  writeLines(jsonlite::toJSON(ov, auto_unbox = TRUE, pretty = TRUE), tmp)
  file.rename(tmp, path)
  invisible(TRUE)
}

#' Set or clear one player's position
#'
#' `pos` of "" removes the override, which is how a player goes back to what
#' the file says rather than being pinned to whatever was clicked last.
set_position <- function(ov, team, num, pos) {
  team <- as.character(team); num <- as.character(num)
  if (!nzchar(pos)) {
    if (!is.null(ov[[team]])) ov[[team]][[num]] <- NULL
    if (!is.null(ov[[team]]) && !length(ov[[team]])) ov[[team]] <- NULL
    return(ov)
  }
  if (is.null(ov[[team]])) ov[[team]] <- list()
  ov[[team]][[num]] <- pos
  ov
}

#' Apply the overrides to every roster and every play in a set of matches
#'
#' Applied after parsing rather than during it, so the parsers stay a
#' description of what the file says and this stays a description of what the
#' staff says. Both are then visible side by side in the editor.
apply_positions <- function(matches, ov) {
  if (!length(matches)) return(matches)
  for (k in seq_along(matches)) {
    m <- matches[[k]]
    for (sd in c("home", "away")) {
      r <- m$rosters[[sd]]
      if (is.null(r) || !nrow(r) || !"pos_file" %in% names(r)) next
      team <- m$teams[[sd]]$name %||% ""
      t_ov <- ov[[team]]

      # Rebuilt from the file's answer every time rather than edited in
      # place. Editing in place is a one-way door: once a position has been
      # overwritten, removing the override has nothing left to go back to,
      # and Undo silently does nothing. Starting from pos_file also makes
      # this safe to run on every live refresh, which it is.
      pos <- r$pos_file
      if (!is.null(t_ov) && length(t_ov)) {
        hit <- match(as.character(r$num), names(t_ov))
        got <- !is.na(hit)
        if (any(got)) pos[got] <- unlist(t_ov)[hit[got]]
      }
      r$pos <- pos
      r$libero <- pos == "L"
      m$rosters[[sd]] <- r

      # The plays carry a copy for grouping, so re-derive it from the roster
      # that was just settled rather than patching it separately.
      if (nrow(m$plays)) {
        sel <- m$plays$side == sd
        if (any(sel)) {
          i <- match(m$plays$num[sel], r$num)
          data.table::set(m$plays, which(sel), "pos",
                          ifelse(is.na(i), "?", r$pos[i]))
        }
      }
    }
    matches[[k]] <- m
  }
  matches
}

#' The roster of both teams, with where each position came from
#'
#' One row per player per team, carrying the file's answer and the override
#' separately so the editor can show which is in force and offer the way
#' back. Players who never touched the ball are kept: a position is set
#' before the match, when nobody has touched it yet.
position_table <- function(matches, ov) {
  rows <- list()
  for (m in matches) {
    for (sd in c("home", "away")) {
      team <- m$teams[[sd]]$name %||% ""
      r <- m$rosters[[sd]]
      if (is.null(r) || !nrow(r)) next
      touched <- if (nrow(m$plays)) m$plays$num[m$plays$side == sd] else integer()
      for (i in seq_len(nrow(r))) {
        key <- paste(team, r$num[i], sep = "|")
        if (!is.null(rows[[key]])) {
          rows[[key]]$n <- rows[[key]]$n + sum(touched == r$num[i])
          next
        }
        o <- ov[[team]][[as.character(r$num[i])]]
        src <- if ("pos_file" %in% names(r)) r$pos_file[i] else r$pos[i]
        rows[[key]] <- list(team = team, num = r$num[i], name = r$last[i],
                            from_file = src %||% "?",
                            override = if (is.null(o)) "" else as.character(o),
                            n = sum(touched == r$num[i]))
      }
    }
  }
  if (!length(rows))
    return(data.frame(team = character(), num = integer(), name = character(),
                      from_file = character(), override = character(),
                      n = integer(), stringsAsFactors = FALSE))
  out <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  out$in_force <- ifelse(nzchar(out$override), out$override, out$from_file)
  out[order(out$team, out$num), ]
}

# ---------------------------------------------------------------------------
# Which team is ours
# ---------------------------------------------------------------------------
# Kept beside the position overrides, and for the same reason: it is a fact
# about the staff, not about any one file.
#
# The default picks the team appearing in the most loaded files, which is
# right until a scouting library is loaded. Three films of the opponent and
# one match of ours, and the opponent is in all four: the app would decide we
# are Grand Canyon and put their numbers under our colour. So once a staff
# says which team is theirs, that answer is written down and used for every
# file afterwards.

TEAM_FILE <- file.path("data", "our_team.txt")

read_our_team <- function(path = TEAM_FILE) {
  if (!file.exists(path)) return("")
  out <- tryCatch(trimws(paste(readLines(path, warn = FALSE), collapse = "")),
                  error = function(e) "")
  out
}

write_our_team <- function(name, path = TEAM_FILE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(as.character(name), tmp)
  file.rename(tmp, path)
  invisible(TRUE)
}
