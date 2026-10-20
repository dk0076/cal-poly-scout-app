# ---------------------------------------------------------------------------
# PLAYS & EXPORT
#
# The raw touches behind every other tab, so a number on a card can be traced
# back to the rallies that produced it. Only the first 400 rows are rendered --
# the CSV button in the header writes every filtered play, which is the right
# tool for anything longer than a spot check.
# ---------------------------------------------------------------------------

CSV_COLS <- c("match_id", "date", "team_name", "set_no", "rally_no", "seq", "score",
              "num", "player", "skill", "hit_type", "effect", "skill_type",
              "call", "call_name", "season_call", "setter_call", "target",
              "blockers", "phase", "passer", "rec_effect", "rot", "sys",
              "court_pos", "front_row", "serving", "start_zone", "start_sub_zone",
              "end_zone", "end_sub_zone", "x0", "y0", "x1", "y1",
              "atk_num", "atk_player", "atk_effect", "atk_call",
              "custom", "time", "point_won")

SKILL_NAME <- c(S = "Serve", R = "Reception", A = "Attack", B = "Block",
                D = "Dig", E = "Set", F = "Free ball")

tab_plays <- function(F, o) {
  if (!nrow(F)) return(empty("No plays in the current filter."))
  show <- F[F$skill %in% c("S", "R", "E", "A"), ]
  data.table::setorder(show, match_id, set_no, rally_no, seq)
  n_total <- nrow(show)
  show <- show[seq_len(min(400L, n_total)), ]

  H <- hdr("Plays &amp; export", sprintf(
    "%s filtered plays · showing the first %s serves, receptions, sets and attacks · full table via the CSV button in the header",
    nrow(F), nrow(show)))

  rows <- lapply(seq_len(nrow(show)), function(i) {
    p <- show[i, ]
    c(esc(p$file_name), p$set_no, p$rally_no, p$score,
      sprintf("<b>%s</b> %s", p$num, esc(p$player_short)),
      unname(SKILL_NAME[p$skill] %||% p$skill),
      if (is.na(p$effect)) "" else sprintf("<code>%s</code>", esc(p$effect)),
      if (is.na(p$call)) "" else sprintf("%s %s", esc(p$call),
                                               thin(esc(call_label(p$season_call, p$call_name)))),
      if (is.na(p$target)) "" else unname(TARGET[p$target] %||% p$target),
      p$phase %||% "", p$rot %||% "", p$sys %||% "",
      if (p$has_xy) sprintf("%s,%s → %s,%s", round(p$x0), round(p$y0),
                            round(p$x1), round(p$y1)) else thin("none"))
  })

  paste0(c(H, card("Touches", c(tbl_generic(
    c("File", "Set", "Rally", "Score", "Player", "Skill", "Eval", "Call",
      "Target", "Phase", "Rot", "Sys", "Coordinates"), rows),
    tiny(sprintf("Eval codes are the coder's own: # perfect or kill, + positive, ! poor, - negative, / overpass or blocked, = error. %s",
                 if (n_total > nrow(show))
                   sprintf("%s further plays are in the CSV but not shown here.",
                           n_total - nrow(show)) else ""))))),
    collapse = "")
}
