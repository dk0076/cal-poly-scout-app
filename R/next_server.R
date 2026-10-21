# ---------------------------------------------------------------------------
# Who serves next
#
# The whole point of the Next server tab: between rallies a coach wants the
# card for the player about to serve, or for the passers about to receive,
# without going to find it.
#
# Three rules were measured against real matches before picking one.
#
#   lu1      whoever stands in zone 1 of the serving side's lineup
#   hold     the same server if they held serve, else zone 1 of the other side
#   history  the same server if they held, else whoever served the last time
#            that side was in the rotation they are about to be in
#
# On side-outs, which is the only hard case, lu1 and hold both managed 72%
# and history 96.7%. The reason is serving substitutes: a team that brings a
# server on for a front-row player breaks the zone-1 rule every single time
# she serves, and does it consistently enough that history gets it right.
# Per match, history was 100% on the Cal-Gra .dvw and 98% on the UC Davis
# .vsm.
#
# One file scored 0%: an intrasquad scrimmage, where both sides are the same
# team and the drill resets rather than rotating. Pooled with the real
# matches it drags the overall figure to 53%, which is the same pooling trap
# the coverage banner warns about elsewhere. A wash drill does not need to
# know who serves next, so this is left alone rather than engineered around,
# and the tab says when it is guessing.
# ---------------------------------------------------------------------------

#' The rotation a side moves into when it wins serve back
rot_advance <- function(r) {
  if (length(r) != 1L || is.na(r)) return(NA_character_)
  i <- suppressWarnings(as.integer(substring(r, 2)))
  if (is.na(i) || i < 1L || i > 6L) return(NA_character_)
  sprintf("R%d", if (i == 6L) 1L else i + 1L)
}

#' One row per rally: who served it, for which side, and whether they held
serve_log <- function(P) {
  if (!nrow(P)) return(NULL)
  S <- P[P$skill == "S"]
  if (!nrow(S)) return(NULL)
  data.table::setorder(S, date, match_id, set_no, rally_no, seq)
  S[!duplicated(paste(S$match_id, S$set_no, S$rally_no))]
}

#' Who is about to serve, and on what evidence
#'
#' Returns NULL when there is nothing to go on. `basis` says which rule
#' answered, so the tab can show "holding serve" differently from a rotation
#' lookup, and say plainly when it does not know.
next_server <- function(P) {
  S <- serve_log(P)
  if (is.null(S) || nrow(S) < 2L) return(NULL)
  last <- S[nrow(S)]

  # Still mid-rally: the serve on screen is the one being played, so the
  # answer is simply who just served.
  if (is.na(last$point_won))
    return(list(side = last$side, num = last$num, name = last$player_short,
                rot = last$rot, basis = "serving now", known = TRUE))

  server_won <- isTRUE(last$point_won)
  if (server_won)
    return(list(side = last$side, num = last$num, name = last$player_short,
                rot = last$rot, basis = "holding serve", known = TRUE))

  # Side-out: the other side serves, in the rotation after the one they were
  # last in. Who that is comes from the last time they stood there.
  other <- S[S$side != last$side & S$set_no == last$set_no]
  if (!nrow(other))
    return(list(side = NA_character_, num = NA_integer_, name = NA_character_,
                rot = NA_character_, basis = "no serve from them yet",
                known = FALSE))
  prev <- other[nrow(other)]
  want <- rot_advance(prev$rot)
  hit <- other[!is.na(other$rot) & other$rot == want]
  if (!nrow(hit) || is.na(want))
    return(list(side = prev$side, num = NA_integer_, name = NA_character_,
                rot = want, basis = "not seen in this rotation yet",
                known = FALSE))
  pick <- hit[nrow(hit)]
  list(side = pick$side, num = pick$num, name = pick$player_short,
       rot = want, basis = "last time they were in this rotation", known = TRUE)
}

#' The order a side has actually served in, by rotation
#'
#' What a staff reads before the match: six rotations, who serves each, and
#' how that serve has gone. Built from what happened rather than the lineup
#' sheet, so a serving substitute shows up as herself.
serve_order <- function(P, which_side) {
  S <- serve_log(P)
  if (is.null(S)) return(NULL)
  # Not `side`. A data.table expression resolves a bare name to its own
  # column first, so a parameter called side compares the column with
  # itself, every row passes, and both teams come back looking identical.
  keep <- S$side == which_side & !is.na(S$rot)
  S <- S[keep]
  if (!nrow(S)) return(NULL)
  out <- lapply(ROTS, function(r) {
    q <- S[S$rot == r]
    if (!nrow(q)) return(NULL)
    tab <- sort(table(paste(q$num, q$player_short, sep = "|")), decreasing = TRUE)
    who <- strsplit(names(tab)[1], "|", fixed = TRUE)[[1]]
    data.frame(rot = r, num = as.integer(who[1]), name = who[2],
               serves = nrow(q),
               ace = sum(q$effect == "#"), err = sum(q$effect == "="),
               eff = vs_efficiency(q$effect, "S"),
               stringsAsFactors = FALSE)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}
