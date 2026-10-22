# ---------------------------------------------------------------------------
# LAST 6
#
# An in-match view: who is swinging well right now, as against who has a good
# line in the box score. Laid out after this program's own "MATCH Last 6
# Attempts Viewer" worksheet -- a row per attacker, total attempts and
# efficiency, then the recent attempts oldest-to-newest with the most recent
# on the right.
#
# Split into pins and middles because the two are not comparable: a middle
# swinging .500 on six quicks and a pin swinging .500 on six high balls are
# different pieces of information, and reading them in one list invites
# comparing them.
#
# Efficiency here is VolleyStation's, via vs_efficiency(), both overall and
# across the recent window, so "last 6" and the box score are the same
# formula on different rows.
# ---------------------------------------------------------------------------

LAST_N <- 6

#' Pin or middle, from the role inferred from what the player actually does.
#' Roster position tags are unreliable for scouted opponents, so role comes
#' from behaviour: a hitter whose attacks are at least half middle-targeted is
#' a middle. See infer_roles().
#' Pins, middles, or neither
#'
#' `pos` is the position the file lists the player at, which is the honest
#' answer when it is there. `role` is inferred from the attack codes a player
#' actually runs, which is the only answer available for a scouted opponent
#' whose roster came in blank. Listed position wins where it exists, and the
#' inference is the fallback rather than the other way round.
attack_group <- function(role, pos = NULL) {
  by_role <- ifelse(is.na(role), "Other",
                    ifelse(role == "MB", "Middles",
                           ifelse(role == "OH", "Pins", "Other")))
  if (is.null(pos)) return(by_role)
  by_pos <- ifelse(is.na(pos) | pos == "?", NA_character_,
                   ifelse(pos == "MB", "Middles",
                          ifelse(pos %in% PINS, "Pins", "Other")))
  ifelse(is.na(by_pos), by_role, by_pos)
}

#' One attempt, as a coloured chip.
#'
#' The chip says what the call is called, not its code. A coach reading a
#' row of recent swings between rallies should not be translating PG into Go
#' in their head, and the code is in the tooltip for anyone who wants it.
attempt_cell <- function(p) {
  st <- land_status(p$effect)
  col <- unname(OUTCOME[st])
  code <- if (!is.na(p$season_call)) call_chip(p$season_call, p$call_name) else
          if (!is.na(p$call)) call_chip(p$call, p$call_name) else "-"
  raw <- if (!is.na(p$season_call)) p$season_call else
         if (!is.na(p$call)) p$call else ""
  tip <- sprintf("set %s · %s · %s%s%s · %s",
                 p$set_no, p$score,
                 if (is.na(p$season_call)) "no call" else call_label(p$season_call, p$call_name),
                 if (nzchar(raw)) sprintf(" (%s)", raw) else "",
                 if (is.na(p$phase)) "" else paste0(" · ", PHASE[[p$phase]]),
                 st)
  sprintf('<span class="att %s" style="border-color:%s;color:%s" title="%s">%s<i>%s</i></span>',
          st, col, col, esc(tip), esc(code),
          if (is.na(p$effect)) "?" else esc(p$effect))
}

tab_last6 <- function(F, o) {
  A <- attacks_of(F, o$phases)
  if (!nrow(A)) return(empty("No attacks in the current filter."))
  # chronological: the whole point is which swing came last
  data.table::setorder(A, date, match_id, set_no, rally_no, seq)
  A$grp <- attack_group(A$role, A$pos)

  n <- as.integer(o$lastN %||% LAST_N)
  if (is.na(n) || n < 3) n <- LAST_N

  H <- hdr("Last 6", sprintf(
    "%d attacks · most recent on the right · who is swinging well now, not who has the better box score",
    nrow(A)))

  H <- c(H, viewbar(
    vbtn("lastN", "6", "Last 6", as.character(n)),
    vbtn("lastN", "10", "Last 10", as.character(n)),
    vbtn("lastN", "15", "Last 15", as.character(n))))

  block <- function(g, label, note_txt) {
    rows_g <- A[A$grp == g, ]
    if (!nrow(rows_g))
      return(card(label, empty(sprintf("No %s attacks in this filter.", tolower(label)))))
    keys <- unique(paste(rows_g$num, rows_g$player_short, sep = "|"))
    ord <- vapply(keys, function(k) sum(paste(rows_g$num, rows_g$player_short, sep = "|") == k), 0)
    keys <- keys[order(-ord)]

    rows <- lapply(keys, function(k) {
      q <- rows_g[paste(rows_g$num, rows_g$player_short, sep = "|") == k, ]
      parts <- strsplit(k, "|", fixed = TRUE)[[1]]
      tail_q <- utils::tail(q, n)
      cells <- paste0(vapply(seq_len(nrow(tail_q)),
                             function(i) attempt_cell(tail_q[i, ]), ""), collapse = "")
      all_eff <- vs_efficiency(q$effect, "A")
      win_eff <- vs_efficiency(tail_q$effect, "A")
      trend <- if (is.na(all_eff) || is.na(win_eff) || nrow(q) <= n) ""
               else sprintf('<span class="%s">%s</span>',
                            if (win_eff > all_eff + .001) "pos"
                            else if (win_eff < all_eff - .001) "neg" else "thin",
                            if (win_eff > all_eff + .001) "▲"
                            else if (win_eff < all_eff - .001) "▼" else "–")
      c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])),
        nrow(q), eff_td(all_eff),
        sprintf("%s %s", eff_td(win_eff), trend),
        sprintf('<div class="atts">%s</div>', cells))
    })
    card(label, c(tbl_generic(
      c("Hitter", "Att", "Eff", sprintf("Last %d", n), "Attempts, oldest to newest"), rows),
      tiny(note_txt)))
  }

  body <- c(
    block("Pins", "Pins",
          "Outsides and opposites, as the file lists them. Eff is VolleyStation's own formula, (# − / − =) ÷ attempts, applied to all their attempts and then to the recent window. The arrow compares the two."),
    block("Middles", "Middles",
          "Middles, as the file lists them. Where a roster carries no position, which happens on a scouted opponent, the split falls back to what each hitter actually runs."),
    if (any(A$grp == "Other"))
      block("Other", "Setters, liberos and unclassified",
            "Anyone the role inference could not place as a pin or a middle: setter dumps, a libero swing, or a hitter with too few attempts to classify.") else NULL)

  paste0(c(H, body, note(
    "Each chip is one attempt: the season call, then the coder's grade. Green is a kill, amber blocked, red an error, grey in play. Hover for the set, score, call and phase. Players are ordered by attempts, so the ones carrying the match sit at the top."
  )), collapse = "")
}
