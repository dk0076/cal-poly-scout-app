# ---------------------------------------------------------------------------
# SERVE RECEIVE SETS, BY ROTATION
#
# Out of serve receive the setter has a choice, and the choice is different in
# each rotation: who is front row, who passed the ball, who the other team is
# blocking. The Setter tab answers "where does the ball go" for a whole match
# and the Rotations tab answers "how did each rotation do"; neither answers
# the question a coach asks in the gap between rallies, which is what has been
# happening in THIS rotation, lately.
#
# So: one row per rotation, the last 3, 6 and 15 serve-receive sets in it, and
# the swings they produced. Three windows rather than one because they say
# different things. 15 is most of a set and is roughly "this match". 6 is the
# last time through the rotation or so. 3 is now, and is noise on its own,
# which is exactly why it sits beside the other two rather than alone.
#
# The chips are the same idiom as Last 6: one per set, oldest on the left,
# the attacker's number and how the swing came out. A row of green on the
# right is a hitter who is up; a row that goes red in one rotation and not in
# the others is the thing this card exists to show.
#
# A set carries no phase of its own: it inherits the phase of the swing it
# produced, which is how a serve-receive set is identified here.
# ---------------------------------------------------------------------------

SR_WINDOWS <- c(3L, 6L, 15L)

#' Hitting off a set of sets, by the attack each one produced
set_hit <- function(rows) {
  lk <- !is.na(rows$atk_effect)
  n <- sum(lk)
  if (!n) return(NA_real_)
  (sum(lk & rows$atk_effect == "#") - sum(lk & rows$atk_effect %in% c("=", "/"))) / n
}

#' One set as a chip: who it went to, and what they did with it
sr_set_chip <- function(p) {
  st <- if (is.na(p$atk_effect)) "in" else land_status(p$atk_effect)
  col <- unname(OUTCOME[st])
  who <- if (!is.na(p$atk_num)) p$atk_num else "?"
  tip <- sprintf("set %s · %s · %s%s · %s",
                 p$set_no, p$score %||% "",
                 if (is.na(p$atk_player)) "no attack" else p$atk_player,
                 if (is.na(p$rec_effect)) "" else paste0(" · pass ", p$rec_effect),
                 st)
  sprintf('<span class="att %s" style="border-color:%s;color:%s" title="%s">%s<i>%s</i></span>',
          st, col, col, esc(tip), esc(as.character(who)),
          if (is.na(p$atk_effect)) "-" else esc(p$atk_effect))
}

#' The windows for one rotation, each beside the rotation's own full figure
sr_window_cells <- function(rows) {
  all_hit <- set_hit(rows)
  vapply(SR_WINDOWS, function(n) {
    q <- utils::tail(rows, n)
    if (!nrow(q)) return("")
    h <- set_hit(q)
    arrow <- if (is.na(h) || is.na(all_hit) || nrow(rows) <= n) ""
             else if (h > all_hit + .001) '<span class="pos">&#9650;</span>'
             else if (h < all_hit - .001) '<span class="neg">&#9660;</span>'
             else '<span class="thin">&ndash;</span>'
    sprintf("%s %s", eff_td(h), arrow)
  }, "")
}

#' The card. `E` is the sets for one side; rotation is that side's own.
sr_sets_card <- function(E, label = "", cls = "") {
  if (is.null(E) || !nrow(E)) return("")
  SR <- E[E$skill == "E" & !is.na(E$phase) & E$phase == "SR", ]
  if (!nrow(SR)) return("")
  # chronological, because every window here is "the last n"
  data.table::setorder(SR, date, match_id, set_no, rally_no, seq)

  rows <- lapply(ROTS, function(k) {
    q <- SR[!is.na(SR$rot) & SR$rot == k, ]
    if (!nrow(q))
      return(c(sprintf("<b>%s</b>", k), 0, "", "", "", "",
               sprintf('<span class="thin">not in serve receive yet</span>')))
    chips <- paste0(vapply(seq_len(nrow(utils::tail(q, 15))),
                           function(i) sr_set_chip(utils::tail(q, 15)[i, ]), ""),
                    collapse = "")
    c(sprintf("<b>%s</b> %s", k, thin(sprintf("z%d", ROT_ZONE[[k]]))),
      nrow(q), eff_td(set_hit(q)), sr_window_cells(q),
      sprintf('<div class="atts">%s</div>', chips))
  })

  unknown <- sum(is.na(SR$rot))
  sprintf('<div class="card %s"><h3>Serve receive sets by rotation%s</h3>%s%s%s</div>',
          cls, if (nzchar(label)) sprintf(" · %s", label) else "",
          tbl_generic(c("Rot", "SR sets", "Hit",
                        sprintf("L%d", SR_WINDOWS[1]), sprintf("L%d", SR_WINDOWS[2]),
                        sprintf("L%d", SR_WINDOWS[3]), "Last 15"),
                      rows),
          if (unknown) sprintf('<div class="tiny">%d serve-receive set%s carries no rotation and is not in any row.</div>',
                               unknown, if (unknown == 1) "" else "s") else "",
          tiny("Sets out of serve receive only, so this is the offence the setter runs off a pass rather than off a dig. Hit is the hitting efficiency of the swings those sets produced; L3, L6 and L15 are the same figure over the last 3, 6 and 15 of them in that rotation, and the arrow compares the window with the rotation's own match. Chips are one set each, oldest on the left: the attacker's number and how the swing came out."))
}
