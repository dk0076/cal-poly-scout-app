# ---------------------------------------------------------------------------
# Roster: who plays where
#
# Set before coding, which is the point: every other tab that says "pins" or
# "middles" is reading this. The file's own answer is shown beside the choice
# so it is obvious when the staff and the file disagree, and a player can be
# put back to the file's answer rather than left pinned to a click.
# ---------------------------------------------------------------------------

tab_roster <- function(PT, focus_name) {
  if (!nrow(PT))
    return(empty("No roster yet. Load a match file and the players appear here."))

  H <- hdr("Roster", sprintf(
    "%d players · position decides pins and middles on every other tab",
    nrow(PT)))

  teams <- unique(PT$team)
  # Ours first: it is the one a staff actually corrects.
  teams <- c(teams[teams == focus_name], teams[teams != focus_name])

  blocks <- lapply(teams, function(tm) {
    q <- PT[PT$team == tm, ]
    rows <- lapply(seq_len(nrow(q)), function(i) {
      chips <- paste0(vapply(POS_ORDER[POS_ORDER != "?"], function(code)
        sprintf('<button class="chip %s" data-pos="%s|%s|%s">%s</button>',
                if (identical(q$in_force[i], code)) "on" else "",
                esc(tm), q$num[i], code, code), ""), collapse = "")
      src <- if (nzchar(q$override[i]))
        sprintf('<span class="thin">set by hand, file says %s</span> <button class="chip ghost" data-pos="%s|%s|">undo</button>',
                esc(q$from_file[i]), esc(tm), q$num[i])
      else '<span class="thin">from the file</span>'
      c(sprintf("<b>%s</b> %s", q$num[i], esc(q$name[i])),
        if (q$n[i]) format(q$n[i], big.mark = ",") else "<span class=\"thin\">0</span>",
        sprintf('<div class="poschips">%s</div>', chips),
        src)
    })
    cls <- if (identical(tm, focus_name)) "us" else "them"
    sprintf('<div class="card %s"><h3>%s</h3>%s</div>', cls, esc(tm),
            tbl_generic(c("Player", "Touches", "Position", "Where it came from"), rows))
  })

  paste0(c(H, unlist(blocks), note(
    "Both file formats store a position per player, so these arrive filled in and most rosters need nothing done here. Correct one when the file is wrong or blank, which is usual for an opponent typed from a lineup sheet. A correction is kept by team and jersey number, not by match, so it holds for the next file off the same roster and through a whole match of live refreshes. Undo puts a player back to what the file says. Pins means outsides and opposites together, as against middles."
  )), collapse = "")
}
