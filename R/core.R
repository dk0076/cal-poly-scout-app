# ---------------------------------------------------------------------------
# Court geometry, code vocabularies and the small helpers every tab leans on.
#
# Ported from scout_vsm v94 (single-file HTML). The numbers here are not
# conventions invented for this app -- they are the frames the files themselves
# use, so changing one changes what every chart means:
#
#   .vsm travelPath coordinates are already in the ACTING TEAM's own frame.
#   x 0-900 left to right, y 0-1800 with the net at 900. Mirroring them "to
#   normalise" is a bug: it produces attack charts that look almost right and
#   are left-right reversed for one of the two teams.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

COURT <- list(W = 900, L = 1800, NET = 900, THREE = 300, U_PER_M = 100)

# Zone lattice. Rows run net-outward; cols are x-thirds (low -> high) in the
# acting team's own frame. Opponent zones mirror, because the opponent's right
# side appears on the acting team's left.
OPP_ZONE <- rbind(c(2, 3, 4), c(9, 8, 7), c(1, 6, 5))  # y 900-1200 / 1200-1500 / 1500-1800
OWN_ZONE <- rbind(c(4, 3, 2), c(7, 8, 9), c(5, 6, 1))  # y 600-900  / 300-600   / 0-300

TARGET  <- c(F = "Outside", C = "Middle", B = "Opposite", P = "Pipe", S = "Setter")
HITTYPE <- c(H = "High", Q = "Quick", M = "Standard", T = "Tense", O = "Off-speed", N = "Slow")

# How the ball was struck, from the play's skillType. Read off the data rather
# than assumed: T lands a median 3.4 m past the net against 7.0 m for H, which
# is the signature of a tip.
SHOTTYPE <- c(H = "Hard", T = "Tip", P = "Roll / off-speed")

PHASE <- c(SR = "Serve receive", FT = "First transition", TR = "Transition")

# VolleyStation's own winning/losing symbols, transcribed from its source:
#   app/server/services/user-settings/user-settings.service.js  (defaults)
#   app/submodules/functions/get-efficiency.function.js         (the formula)
#
# Its efficiency for any skill is, exactly:
#     (count of winning symbols - count of losing symbols) / total
#
# Keeping the table here rather than hardcoding each formula means a new
# metric can be defined the way VolleyStation defines it, and checked against
# it, instead of being invented and reconciled afterwards. These are the
# DEFAULTS: a user can change them in VolleyStation's settings, so a file from
# a staff that has done so needs this table adjusted to match.
VS_SYMBOLS <- list(
  S = list(win = c("#", "+", "/"), lose = c("=")),             # serve
  R = list(win = c("#", "+"),      lose = character()),        # reception
  A = list(win = c("#"),           lose = c("/", "=")),        # attack
  B = list(win = c("#", "+"),      lose = c("/", "=")),        # block
  D = list(win = c("#", "+", "/"), lose = character()),        # dig
  E = list(win = c("#", "+"),      lose = c("/", "=")),        # set
  F = list(win = c("#", "+"),      lose = c("/", "="))         # free ball
)

#' Efficiency for a skill, by VolleyStation's own definition.
#'
#' Use this for anything that should agree with VolleyStation's reports. Where
#' this tool deliberately departs -- the dig "in system" grouping, for
#' instance, which follows the staff's own scout report rather than
#' VolleyStation's default -- report both and say which is which.
vs_efficiency <- function(effect, skill) {
  sym <- VS_SYMBOLS[[skill]]
  if (is.null(sym)) return(NA_real_)
  n <- length(effect)
  if (!n) return(NA_real_)
  (sum(effect %in% sym$win, na.rm = TRUE) -
   sum(effect %in% sym$lose, na.rm = TRUE)) / n
}

# Jerseys that are not players. This program codes practice bodies as #98 and
# #99, and they turn up a lot -- 1447 touches across 15 files here, 663 of
# them attacks -- so leaving them in moves every team rate. A play by one of
# them is excluded from reports rather than deleted, and the count is
# surfaced, so the exclusion is visible instead of silent.
PRACTICE_PLAYERS <- c(98L, 99L)

# A custom tag containing "00" means the coder marked the play to revisit: it
# is not final and must not reach a coach's number. "Z" is the opposite -- a
# play flagged FOR the coach -- so the two must never be treated alike.
is_provisional <- function(custom) !is.na(custom) & grepl("00", custom, fixed = TRUE)

# The 0-4 scales this staff grades on, read off their own VolleyStation
# worksheet (coachs spreadsheets/1. Cal Poly Practice Totals.vss, WEIGHTS
# block). These are the numbers coaches actually quote -- "we passed 2.2" --
# so they belong here rather than being left as positive%.
#
# The overpass weight is the staff's own call, settled directly: an overpass
# scores the same as a negative pass. That is 1, not the 0.5 in the worksheet
# nor the 0 in the HTML scout report, which disagreed with each other.
PASS_SCALE  <- c(`#` = 4, `+` = 3, `!` = 2, `-` = 1, `/` = 1, `=` = 0)
SERVE_SCALE <- c(`#` = 4, `+` = 2, `!` = 1.5, `-` = 1, `/` = 3, `=` = -1)

#' Mean grade on a 0-4 scale. NA when nothing was graded.
grade_avg <- function(effect, scale) {
  w <- scale[effect]
  if (!length(w) || all(is.na(w))) return(NA_real_)
  sum(w, na.rm = TRUE) / sum(!is.na(w))
}

LAT <- list(
  list(key = "R", label = "Opp right", zones = "2/9/1"),
  list(key = "M", label = "Middle",    zones = "3/8/6"),
  list(key = "L", label = "Opp left",  zones = "4/7/5")
)
LAT_KEYS <- vapply(LAT, `[[`, "", "key")
LANE_NAME <- c(R = "Z1 lane", M = "Z6 lane", L = "Z5 lane")

DEPTH <- list(
  list(key = "short", label = "Short 0-3 m"),
  list(key = "mid",   label = "Mid 3-6 m"),
  list(key = "deep",  label = "Deep 6-9 m")
)

# Serve type, from the serve's type character (DataVolley column 5,
# VolleyStation hitType). Q and T are both topspin (jump and standing topspin),
# M jump float, H standing float. Any other code is shown raw so an unexpected
# convention is visible instead of silently folded.
SRV_TYPES <- list(
  list(key = "TS", label = "Top spin",      codes = c("Q", "T")),
  list(key = "JF", label = "Jump float",    codes = "M"),
  list(key = "SF", label = "Standing float", codes = "H")
)
SRV_ZONE_NAME <- c(`1` = "right back", `2` = "right front", `3` = "middle front",
                   `4` = "left front", `5` = "left back",  `6` = "middle back")

# ---------------------------------------------------------------------- codes

# VolleyMetrics match codes -> this program's season codes ("vm to cp").
# Some files arrive already in season codes and others raw from VolleyMetrics,
# so translation is conditional: a code that is already a season code is left
# alone, and only an unrecognised code is looked up here. Two VM codes can
# collapse onto one season code (V5 and X9 both -> V4).
VM_TO_SEASON <- c(
  CF = "LS", PP = "PD", V5 = "V4", V6 = "V2", VB = "VP",
  X1 = "LT", X2 = "L2", X3 = "P2", X5 = "PG",
  X7 = "LG", X9 = "V4", XM = "LP",
  # X4 and X6 are the zone 2 quick. Measured from this team's own files, PR
  # (Red) contacts at x=840 and X6 at x=824 -- the same ball. They are handled
  # by ROLE in season_code() rather than listed here: a zone 2 quick is a Red
  # when the opposite hits it and a slide when a middle does, and the code
  # itself carries no position.
  CB = "LS",   # slide, setter in zone 2 (CF is the zone 3 version)
  X8 = "ID",   # quick from zone 1 -- merged with 40, see SEASON_MERGE
  V8 = "ID",   # high from zone 1, same attack one tempo up
  XB = "ID",   # 40
  XP = "IP",   # bic and pipe are the same call for this team
  VR = "V4"    # known coding typo for V4, confirmed by the staff
)
VM_SETTER_TO_SEASON <- c(K1 = "KT", K7 = "KG", KM = "KP")

SEASON_CODES <- c("PG", "V4", "P4", "PR", "P2", "PH", "V2", "PS",
                  "LT", "LG", "LS", "LP", "L2", "LD", "IP", "ID", "VP", "PD", "PO")

# Codes that mean "quick out of zone 2". The zone 2 quick is the opposite's
# Red, but a middle running the same ball is hitting a slide, and the code does
# not record which. Resolved by the hitter's role.
Z2_QUICK <- c("X4", "X6")

# Season codes that fold into another season code. PH (D ball), ID (40) and the
# high version V8 are all the back-row right attack -- the staff treats them as
# one call, so they are reported as one.
SEASON_MERGE <- c(PH = "ID", V8 = "ID")

CALL_LABEL <- c(
  PG = "Go",           V4 = "Highball outside", P4 = "2 by outside",
  PR = "Red",          V2 = "Highball oppo",    P2 = "2 ball",
  PH = "D ball",       PS = "Slide (Op)",
  LT = "1",            LG = "Gap",              LS = "Slide",
  LP = "Push",         L2 = "MB back 1",        LD = "Poly",
  IP = "Pipe / bic",   ID = "40 / D ball",      VP = "OOS pipe",
  PD = "Setter dump",  PO = "Overpass"
)

# What a call is called out loud, short enough to sit on a chip beside its
# grade. The full names in CALL_LABEL are what a table column wants; a row of
# fifteen recent attempts wants the word a coach would say. "Highball
# outside" becomes "HB out", and the code itself is kept in the tooltip, so
# nothing is lost by shortening.
#
# A code with no short form falls back to its full name and then to the code,
# which is what makes an unmapped code visible rather than silently blank.
CALL_SHORT <- c(
  PG = "Go",      V4 = "HB out",  P4 = "2 out",
  PR = "Red",     V2 = "HB oppo", P2 = "2",
  PH = "D ball",  PS = "Slide op",
  LT = "1",       LG = "Gap",     LS = "Slide",
  LP = "Push",    L2 = "Back 1",  LD = "Poly",
  IP = "Pipe",    ID = "40",      VP = "OOS pipe",
  PD = "Dump",    PO = "Over"
)

#' The word for a chip: short name, else full name, else the code itself.
call_chip <- function(code, file_name = NULL) {
  if (is.na(code) || !nzchar(code)) return("-")
  if (!is.na(CALL_SHORT[code])) return(unname(CALL_SHORT[code]))
  sc <- season_code(code)
  if (!is.na(CALL_SHORT[sc])) return(unname(CALL_SHORT[sc]))
  call_label(code, file_name)
}

# Known setter-call codes for middle routes (recorded on the SET play itself,
# not the attack). A call and the attack it produces are DIFFERENT fields on
# DIFFERENT plays -- KT becomes an LT-classified attack only ~53% of the time.
# Used only to label a call code the file's own attack-combination metadata
# does not define, never to rewrite what an attack's own recorded code says.
KNOWN_SETTER_CALLS <- c(
  KT = "Front Quick", KG = "Quick in 3 (gap)", KS = "Slide", KP = "Push",
  K2 = "A", KB = "MB back red"
)

#' Season code for a raw call code, translating VolleyMetrics codes only when
#' the code isn't already one of ours. `role` is the hitter's inferred role and
#' is only consulted for the zone 2 quick.
season_code <- function(code, role = NULL) {
  out <- rep(NA_character_, length(code))
  ok <- !is.na(code) & nzchar(code)
  if (!any(ok)) return(out)
  cc <- code[ok]
  rr <- if (is.null(role)) rep(NA_character_, length(code))[ok] else role[ok]

  res <- cc
  merged <- !is.na(SEASON_MERGE[cc])
  res[merged] <- SEASON_MERGE[cc[merged]]

  q2 <- cc %in% Z2_QUICK
  res[q2] <- ifelse(!is.na(rr[q2]) & rr[q2] == "MB", "LS", "PR")

  rest <- !merged & !q2 & !(cc %in% SEASON_CODES)
  if (any(rest)) {
    mapped <- VM_TO_SEASON[cc[rest]]
    mapped[is.na(mapped)] <- cc[rest][is.na(mapped)]
    m2 <- SEASON_MERGE[mapped]
    mapped[!is.na(m2)] <- m2[!is.na(m2)]
    res[rest] <- mapped
  }
  out[ok] <- res
  out
}

#' Display name for a play's call.
#'
#' The file's own combination table wins when it has one. VM_TO_SEASON was built
#' from one team's personal conversion sheet; applying it to any file that
#' happens to use X-codes mislabels them. So: known season label first (the
#' grouping keys ARE season codes), then the file's own name, then the bare code.
call_label <- function(code, file_name = NULL) {
  if (is.na(code) || !nzchar(code)) return("(no call)")
  if (!is.na(CALL_LABEL[code])) return(unname(CALL_LABEL[code]))
  if (!is.null(file_name) && length(file_name) && !is.na(file_name[1]) && nzchar(file_name[1]))
    return(file_name[1])
  sc <- season_code(code)
  if (!is.na(CALL_LABEL[sc])) unname(CALL_LABEL[sc]) else code
}

#' Name for a setter call, translating VolleyMetrics K-codes first.
setter_call_label <- function(code) {
  if (is.na(code)) return("")
  sc <- if (!is.na(VM_SETTER_TO_SEASON[code])) VM_SETTER_TO_SEASON[code] else code
  v <- KNOWN_SETTER_CALLS[sc]
  if (is.na(v)) "" else unname(v)
}

#' Raw match codes that folded into a season code, e.g. "PR + X4 + X6".
raw_codes_of <- function(calls) {
  u <- sort(unique(calls[!is.na(calls) & nzchar(calls)]))
  paste(u, collapse = " + ")
}

shot_label <- function(t) {
  if (is.na(t)) return("unknown")
  v <- SHOTTYPE[t]
  if (is.na(v)) as.character(t) else unname(v)
}

srv_type_of <- function(hit_type, skill_type = NULL) {
  c0 <- hit_type
  if (!is.null(skill_type)) c0[is.na(c0)] <- skill_type[is.na(c0)]
  out <- rep("NC", length(c0))
  live <- !is.na(c0) & c0 != "~" & nzchar(c0)
  for (t in SRV_TYPES) out[live & c0 %in% t$codes] <- t$key
  other <- live & out == "NC"
  out[other] <- paste0("X:", c0[other])
  out
}

srv_type_name <- function(k) {
  if (identical(k, "NC")) return("Not coded")
  if (startsWith(k, "X:")) return(sprintf("Other (%s)", substring(k, 3)))
  for (t in SRV_TYPES) if (t$key == k) return(t$label)
  k
}

# -------------------------------------------------------------------- geometry

clamp <- function(v, a, b) pmin(pmax(v, a), b)

x_third <- function(x) ifelse(x < 300, 1L, ifelse(x < 600, 2L, 3L))

#' The zone the file recorded, falling back to one derived from the coordinate.
#'
#' Prefer the recorded field, always. Measured across 14 scouted files, a zone
#' derived from the coordinate disagrees with the coder's own entry on 4% of
#' attack landings and 12% of reception contacts -- and every one of those
#' disagreements sits within about 7 units (7 cm) of a zone line, because
#' VolleyStation's click grid quantises coordinates onto a lattice that piles
#' points up right on the boundary.
#'
#' It cannot be fixed by moving the cut. One file puts x=303 in the middle
#' third and x=307 in the left third, which no single threshold satisfies: the
#' files carry slightly different court calibrations. So the coordinate is the
#' wrong source for a question the file already answers, and zone_of() below is
#' a geometric approximation for the files that answer it with nothing.
zone_or_coded <- function(coded, x, y, half) {
  out <- suppressWarnings(as.integer(coded))
  need <- is.na(out) & !is.na(x) & !is.na(y)
  if (any(need)) out[need] <- zone_of(x[need], y[need], half)
  out
}

#' Volleyball zone number for a point. half: "opp" | "own"
#'
#' The geometric thirds of the half. Use zone_or_coded() wherever the play
#' might carry a zone of its own -- see the note there.
zone_of <- function(x, y, half) {
  cc <- x_third(clamp(x, 0, 899))
  if (identical(half, "opp")) {
    rr <- ifelse(y < 1200, 1L, ifelse(y < 1500, 2L, 3L))
    OPP_ZONE[cbind(rr, cc)]
  } else {
    rr <- ifelse(y >= 600, 1L, ifelse(y >= 300, 2L, 3L))
    OWN_ZONE[cbind(rr, cc)]
  }
}

#' Landing status. Only a coded error is treated as out; a few in-play balls sit
#' a couple of centimetres past a line from click rounding.
land_status <- function(effect) {
  out <- rep("in", length(effect))
  out[!is.na(effect) & effect == "="] <- "error"
  out[!is.na(effect) & effect == "/"] <- "blocked"
  out[!is.na(effect) & effect == "#"] <- "kill"
  out
}

out_of_court <- function(x, y) x < 0 | x > COURT$W | y > COURT$L | y < COURT$NET

origin_band <- function(x) ifelse(x < 300, "left", ifelse(x > 600, "right", "middle"))

lat_third <- function(x) LAT_KEYS[x_third(clamp(x, 0, 899))]

#' Relative direction; NA for middle-of-court origins where line/cross is
#' undefined.
rel_direction <- function(x0, x1) {
  band <- origin_band(x0)
  t <- x_third(clamp(x1, 0, 899))
  ifelse(band == "left",  ifelse(t == 1L, "line", ifelse(t == 2L, "seam", "cross")),
  ifelse(band == "right", ifelse(t == 3L, "line", ifelse(t == 2L, "seam", "cross")),
         NA_character_))
}

depth_of <- function(y) ifelse(y < 1200, "short", ifelse(y < 1500, "mid", "deep"))

#' Serve landing mapped to the six standard receiving positions, seen from the
#' receiving team. Low x is their right (zones 1, 9, 2), high x their left.
srv_zone6 <- function(x1, y1) {
  out <- rep(NA_integer_, length(x1))
  ok <- !is.na(x1) & !is.na(y1) & y1 >= COURT$NET
  if (!any(ok)) return(out)
  lane <- ifelse(x1[ok] < 300, "R", ifelse(x1[ok] < 600, "M", "L"))
  back <- y1[ok] >= (COURT$NET + COURT$L) / 2
  out[ok] <- ifelse(lane == "R", ifelse(back, 1L, 2L),
             ifelse(lane == "M", ifelse(back, 6L, 3L),
                                 ifelse(back, 5L, 4L)))
  out
}

#' Serve target as one of the six receiving positions, preferring the zone the
#' coder entered. DV_ZONE6 folds the middle band onto the back row, which is
#' where the coordinates put it when the same serve is available both ways.
srv_zone6_or_coded <- function(end_zone, x1, y1) {
  out <- unname(DV_ZONE6[as.character(suppressWarnings(as.integer(end_zone)))])
  need <- is.na(out)
  if (any(need)) out[need] <- srv_zone6(x1[need], y1[need])
  out
}

# Rotation number from the setter's court zone. Players rotate 1 -> 6 -> 5 -> 4
# -> 3 -> 2, so R1..R3 have the setter in the back row and R4..R6 in the front
# row, which is the convention coaches use. Labelling by raw zone number
# instead -- the tool's original mistake -- puts the front-row-setter rotations
# at S2/S3/S4 and reads as though the opposite vanishes there.
ROT_ORDER <- c(1, 6, 5, 4, 3, 2)
rot_name <- function(zone) {
  i <- match(suppressWarnings(as.integer(zone)), ROT_ORDER)
  ifelse(is.na(i), NA_character_, paste0("R", i))
}
WHEEL_OPP <- c(`1` = 4, `4` = 1, `2` = 5, `5` = 2, `3` = 6, `6` = 3)
SYS_LABEL <- c(`5-1` = "5-1", `6-2` = "6-2", `?` = "Unclear")

# --------------------------------------------------------------- small numbers

med <- function(a) {
  a <- a[!is.na(a)]
  if (!length(a)) NA_real_ else stats::median(a)
}

#' Linear-interpolated quantile, matching the JS original exactly.
quant <- function(a, q) {
  s <- sort(a[!is.na(a)])
  if (!length(s)) return(NA_real_)
  i <- (length(s) - 1) * q
  lo <- floor(i); hi <- ceiling(i)
  s[lo + 1] + (s[hi + 1] - s[lo + 1]) * (i - lo)
}

#' Wilson 95% interval for k of n.
wilson <- function(k, n) {
  if (!length(n) || is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- 1.96; p <- k / n; d <- 1 + z^2 / n
  c0 <- (p + z^2 / (2 * n)) / d
  h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(max(0, c0 - h), min(1, c0 + h))
}

# ------------------------------------------------------------------ formatting

pct <- function(v) if (length(v) != 1 || is.na(v)) "" else sprintf("%d%%", round(v * 100))

#' Hitting efficiency, printed the way a stat sheet prints it: no leading zero.
eff <- function(v) {
  if (length(v) != 1 || is.na(v)) return("")
  s <- sprintf("%.3f", abs(v))
  paste0(if (v < 0) "-" else "", sub("^0", "", s))
}
eff_cls <- function(v) if (length(v) != 1 || is.na(v)) "" else if (v > 0.001) "pos" else if (v < -0.001) "neg" else ""
m1 <- function(u) if (length(u) != 1 || is.na(u)) "" else sprintf("%.1f", u / 100)

esc <- function(s) {
  s <- ifelse(is.na(s), "", as.character(s))
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  gsub('"', "&quot;", s, fixed = TRUE)
}

OUTCOME <- c(kill = "var(--kill)", `in` = "var(--inplay)",
             blocked = "var(--blocked)", error = "var(--err)")

# Monochrome encoding. On a greyscale printer the colour palette collapses --
# "in play" and "blocked" land 0.3 grey levels apart out of 255, i.e. the same
# ink. So mono mode drops colour as the carrier and uses three redundant
# channels instead: line weight, dash pattern, and endpoint shape.
MONO <- list(
  kill    = list(col = "#000000", w = 3.4, dash = "",         head = "arrow"),
  `in`    = list(col = "#9b9b9b", w = 1.7, dash = "",         head = "arrow"),
  blocked = list(col = "#4a4a4a", w = 2.7, dash = "13 6",     head = "bar"),
  error   = list(col = "#000000", w = 2.1, dash = "2.5 6.5",  head = "cross")
)

#' What a watched file looks like right now
#'
#' Modification time and size together, as one string, empty if the file is
#' not readable. A high-water mark on the timestamp alone is not enough for a
#' file being coded live. Correcting a grade in place, `-` to `#`, leaves the
#' size identical, and a tool that rewrites the file can stamp it with a time
#' no later than the one before. Comparing the pair for equality catches both,
#' and treats any difference as something to re-read rather than only a later
#' one. Timestamps here carry sub-millisecond resolution, so two writes in the
#' same second are still distinguishable.
file_sig <- function(path) {
  fi <- file.info(path)
  if (is.na(fi$size)) return("")
  sprintf("%.6f/%.0f", as.numeric(fi$mtime), fi$size)
}

# ---------------------------------------------------------------------------
# Positions
# ---------------------------------------------------------------------------
# Nobody has to type these in: both formats already carry a position per
# player. The codes are VolleyStation's own, read out of its enum rather than
# inferred, and a .dvw stores the same code one higher.
#
#   0 libero, 1 outside hitter, 2 opposite, 3 middle blocker, 4 setter
#
# Cal Poly says "pins" for outside hitters and opposites together, as against
# middles, which is the split the Last 6 tab is built on.

POS_CODES <- c("L", "OH", "OPP", "MB", "S")        # index = code + 1
POS_NAMES <- c(L = "Libero", OH = "Outside", OPP = "Opposite",
               MB = "Middle", S = "Setter")
POS_ORDER <- c("OH", "OPP", "MB", "S", "L", "?")
PINS <- c("OH", "OPP")

#' A stored position code to its short label
#'
#' Anything out of range becomes "?" rather than a guess. VolleyStation falls
#' back to middle blocker here, which is a sensible default for coding and a
#' bad one for a report: a player silently filed as a middle would be counted
#' against the middles' numbers with nothing on the screen to say so.
pos_label <- function(code) {
  i <- suppressWarnings(as.integer(code)) + 1L
  out <- rep("?", length(i))
  ok <- !is.na(i) & i >= 1L & i <= length(POS_CODES)
  out[ok] <- POS_CODES[i[ok]]
  out
}

is_pin <- function(pos) pos %in% PINS

# ---------------------------------------------------------------------------
# When a trajectory chart stops being readable
# ---------------------------------------------------------------------------
# A team's worth of swings drawn as arrows is a ball of wool. Eighty attacks
# over one court is not a picture of anything: the lines cross each other so
# often that neither direction nor volume survives, and the two tendency
# arrows laid on top are the only thing left to read.
#
# A single hitter's swings in a match is the scale this chart was built for,
# which is why the default view follows the count rather than being fixed.
# Above this many, the landing grid says the same thing and says it legibly;
# below it, the arrows are worth more. Either is one click away regardless.
ARROWS_READABLE <- 30L

#' The view to draw, given what the staff asked for and how much there is
#'
#' "auto" is the default and means "whichever is readable". An explicit
#' choice is always obeyed, including an explicit choice of arrows for a
#' hundred attacks: a coach who presses Trajectories wants trajectories.
view_for <- function(chosen, n) {
  if (!identical(chosen, "auto")) return(chosen)
  if (n > ARROWS_READABLE) "heat" else "arrows"
}
