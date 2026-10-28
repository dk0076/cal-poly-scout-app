# ---------------------------------------------------------------------------
# The tabs, and the two ways into them.
#
# Kept apart from app.R because the tab list is the one piece of the UI that
# other things are checked against: a mode names tabs, and a mode that names
# a tab nobody has any more is a device that opens on nothing.
# ---------------------------------------------------------------------------

# Tabs that describe one team's play, and so can be shown for both at once.
# Rotations and Defense already compare the two by construction, and Plays is
# a wide table where a second copy would only cost width.
PAIRED_TABS <- c("attack", "serve", "setter", "passing", "last6")

# Set origin is not here. It was a second tab about setting whose own chart
# needs a coordinate on the setter's contact, and no file in this program
# carries one: not a match, not a practice, not an opponent film scouted off
# video. Its one table that did not need coordinates, the setter calls, now
# sits on the Setter tab. The code is kept, and the tab comes back the day
# anyone codes set contacts.
TABS <- list(c("attack", "Attacking"), c("serve", "Serving"),
             c("setter", "Setter"),
             c("passing", "Serve receive"), c("defense", "Defense"),
             c("rotation", "Rotations"), c("last6", "Last 6"),
             c("nextserver", "Next server"),
             c("sheet", "Scout sheet"),
             c("plays", "Plays &amp; export"), c("roster", "Roster"))

# Two ways into the same app, offered on the opening screen because the
# answer is known before a file is loaded: a staff watching film wants the
# tendencies and the printed sheet, and a staff on the bench wants tonight's
# file. It is the tab picker underneath, so it is a starting point rather
# than a mode anything else in the app checks, and any tab can be turned
# back on afterwards.
MODES <- list(
  list(id = "scouting", label = "Scouting",
       blurb = "Film, tendencies and the printed sheet.",
       tabs = c("sheet", "attack", "serve", "setter", "passing", "rotation",
                "plays", "roster")),
  list(id = "game", label = "In game",
       blurb = "Tonight's file: who serves next, who to serve at.",
       tabs = c("nextserver", "passing", "attack", "serve", "rotation", "last6"))
)

#' The tabs a mode leaves off, which is what the browser actually stores.
mode_hidden <- function(m) {
  all_ids <- vapply(TABS, function(t) t[1], "")
  paste(setdiff(all_ids, m$tabs), collapse = ",")
}
