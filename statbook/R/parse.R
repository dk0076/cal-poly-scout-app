# ---------------------------------------------------------------------------
# Reading a DataVolley .dvw.
#
# Plain text in [3SECTION] blocks. [3SCOUT] is the play-by-play, one coded
# touch per row, semicolon separated. The code in the first field is fixed
# width, so every piece of it is a substr():
#
#     a12AT#X5~45BH4~~00F
#     ||||||||||||||||
#     |||||||||||||||+- custom tag, 16-20
#     ||||||||||||||+-- special: how the rally ended, 15
#     |||||||||||||+--- number of blockers, 14
#     ||||||||||||+---- skill subtype (hard / soft / tip), 13
#     |||||||||||+----- end subzone, 12
#     ||||||||||+------ end zone, 11
#     |||||||||+------- start zone, 10
#     ||||||||+-------- set target: F C B P S, 9
#     ||||||+---------- combination or setter call, 7-8
#     |||||+----------- evaluation: # + ! - / =, 6
#     ||||+------------ skill type, 5
#     |||+------------- skill: S R E A B D F, 4
#     |+--------------- jersey number, 2-3
#     +---------------- team: * home, a visiting, 1
#
# Rows reading "*p12:10" are the score after a rally. Everything else in the
# section is a substitution, a timeout or a set boundary, and is kept only so
# the rally numbering can see it.
# ---------------------------------------------------------------------------

SKILLS <- c(S = "Serve", R = "Reception", E = "Set", A = "Attack",
            B = "Block", D = "Dig", F = "Freeball")

# Position codes as VolleyStation writes them into the players table.
ROLES <- c("1" = "libero", "2" = "outside", "3" = "opposite",
           "4" = "middle", "5" = "setter")

#' Pull one fixed-width field out of a play code. "~" is the format's filler.
fld <- function(code, a, b) {
  s <- trimws(gsub("~", "", substr(code, a, b), fixed = TRUE))
  ifelse(nchar(s) > 0, s, NA_character_)
}

#' Split a .dvw into its named sections.
read_sections <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  head <- grepl("^\\s*\\[3?[A-Za-z0-9 _-]+\\]\\s*$", lines)
  grp <- cumsum(head)
  body <- !head & grp > 0                   # skip anything above the first header
  sec <- split(lines[body], grp[body])
  names(sec) <- gsub("[^A-Z-]", "", toupper(lines[head]))[as.integer(names(sec))]
  sec
}

#' Fields of a semicolon-separated section, as a character matrix padded out
#' to `n` columns so every row can be indexed the same way.
fields <- function(rows, n) {
  p <- strsplit(rows, ";", fixed = TRUE)
  do.call(rbind, lapply(p, function(x) c(x, rep("", max(0, n - length(x))))[seq_len(n)]))
}

#' One row per rostered player: side, number, name, position.
read_roster <- function(sec) {
  one <- function(key, side) {
    if (is.null(sec[[key]]) || !length(sec[[key]])) return(NULL)
    m <- fields(sec[[key]], 14)
    num <- suppressWarnings(as.integer(m[, 2]))
    m <- m[!is.na(num), , drop = FALSE]; num <- num[!is.na(num)]
    if (!length(num)) return(NULL)
    name <- trimws(paste(trimws(m[, 11]), trimws(m[, 10])))
    data.frame(side = side, number = num,
               name = ifelse(nchar(name) > 1, name, paste0("#", num)),
               position = unname(ifelse(is.na(ROLES[trimws(m[, 14])]), "",
                                        ROLES[trimws(m[, 14])])),
               libero = toupper(trimws(m[, 13])) == "L")
  }
  r <- rbind(one("PLAYERS-H", "*"), one("PLAYERS-V", "a"))
  r$label <- paste0("#", r$number, " ", r$name)
  r
}

#' One row per line of [3SCOUT], coded or not.
read_scout <- function(sec) {
  m <- fields(sec$SCOUT, 26)
  code <- trimws(m[, 1])
  keep <- nchar(code) > 0
  m <- m[keep, , drop = FALSE]; code <- code[keep]
  skill <- grepl("^[*a](\\d\\d|\\$\\$)[SREABDF]", code)
  point <- grepl("^[*a]p\\d+:\\d+$", code)
  d <- data.frame(
    code       = code,
    kind       = ifelse(skill, "skill", ifelse(point, "point", "other")),
    side       = substr(code, 1, 1),
    number     = suppressWarnings(as.integer(fld(code, 2, 3))),
    skill      = ifelse(skill, substr(code, 4, 4), NA_character_),
    skill_type = fld(code, 5, 5),
    eval       = fld(code, 6, 6),
    combo      = fld(code, 7, 8),
    target     = fld(code, 9, 9),
    special    = fld(code, 15, 15),
    custom     = fld(code, 16, 20),
    clock      = trimws(m[, 8]),
    set        = suppressWarnings(as.integer(m[, 9])),
    pos_home   = suppressWarnings(as.integer(m[, 10])),
    pos_away   = suppressWarnings(as.integer(m[, 11])),
    video      = suppressWarnings(as.integer(m[, 13])),
    stringsAsFactors = FALSE)
  d$skill_name <- unname(SKILLS[d$skill])
  # A touch coded against 00 or $$ belongs to the team but to nobody on it.
  d$number[!skill | substr(code, 2, 3) %in% c("00", "$$")] <- NA
  sc <- regmatches(code, regexec("^([*a])p(\\d+):(\\d+)$", code))
  d$winner <- ifelse(point, substr(code, 1, 1), NA_character_)
  d$home_score <- d$away_score <- NA_integer_
  d$home_score[point] <- as.integer(sapply(sc[point], `[`, 3))
  d$away_score[point] <- as.integer(sapply(sc[point], `[`, 4))
  d$side[!skill & !point] <- NA
  d
}

#' Everything one file carries: teams, roster, plays, and the code tables the
#' scout wrote into it.
read_dvw <- function(path, label) {
  sec <- read_sections(path)
  if (is.null(sec$SCOUT) || !length(sec$SCOUT))
    stop(label, ": no [3SCOUT] section, so there is no play-by-play in it.")
  teams <- c("*" = "Home", a = "Away")
  if (!is.null(sec$TEAMS)) {
    m <- fields(sec$TEAMS, 2)
    teams[seq_len(min(2, nrow(m)))] <- trimws(m[seq_len(min(2, nrow(m))), 2])
  }
  combos <- NULL
  if (!is.null(sec$ATTACKCOMBINATION)) {
    m <- fields(sec$ATTACKCOMBINATION, 5)
    m <- m[nchar(trimws(m[, 1])) > 0, , drop = FALSE]
    combos <- data.frame(code = trimws(m[, 1]), group = trimws(m[, 3]),
                         tempo = trimws(m[, 4]), desc = trimws(m[, 5]))
    combos$desc[!nzchar(combos$desc)] <- combos$code[!nzchar(combos$desc)]
  }
  calls <- NULL
  if (!is.null(sec$SETTERCALL)) {
    m <- fields(sec$SETTERCALL, 3)
    m <- m[nchar(trimws(m[, 1])) > 0, , drop = FALSE]
    calls <- data.frame(code = trimws(m[, 1]), desc = trimws(m[, 3]))
  }
  d <- read_scout(sec)
  d$file <- label
  # The date is dd/mm/yyyy in [3MATCH]; the filename is the fallback.
  date <- NA
  if (!is.null(sec$MATCH) && length(sec$MATCH)) {
    # dd/mm/yyyy, except in the files where it is mm/dd/yyyy. Rather than
    # guess, take whichever way round gives a real date and nothing if neither
    # does, since nothing downstream depends on it.
    g <- regmatches(sec$MATCH[1], regexec("^(\\d{2})/(\\d{2})/(\\d{4})", sec$MATCH[1]))[[1]]
    if (length(g) == 4)
      date <- as.Date(paste(g[4], g[3], g[2], sep = "-"), optional = TRUE)
    if (length(g) == 4 && is.na(date))
      date <- as.Date(paste(g[4], g[2], g[3], sep = "-"), optional = TRUE)
  }
  list(plays = d, roster = read_roster(sec), teams = teams,
       combos = combos, calls = calls, date = date, label = label)
}

# ---------------------------------------------------------------------------
# Telling a real set from a drill block
#
# Coaches want the sets where the team played out points. Those differ from
# drill work in several independent ways, so the call is made on the weight of
# the evidence rather than any one test: whether score was kept and climbed a
# point at a time, whether it reached a real scoreline, whether rallies ran
# their full length, and whether serves came at game pace. Ported from the
# HTML report so the two agree on which sets are worth counting.
# ---------------------------------------------------------------------------

classify_set <- function(d) {
  sk <- d[d$kind == "skill", ]
  srv <- sk[!is.na(sk$skill) & sk$skill == "S", ]
  pts <- d[d$kind == "point", ]
  if (!nrow(srv)) return(list(kind = "empty", score = 0, rallies = 0, why = "no serves"))

  coverage <- nrow(pts) / nrow(srv)
  top <- if (nrow(pts)) max(pts$home_score, pts$away_score, na.rm = TRUE) else 0
  step <- if (nrow(pts) > 1) diff(pts$home_score) + diff(pts$away_score) else numeric(0)
  seq_ok <- if (length(step)) mean(step == 1) else 0
  per_rally <- nrow(sk) / nrow(srv)
  gaps <- diff(srv$video); gaps <- gaps[!is.na(gaps) & gaps > 0]
  pace <- if (length(gaps)) median(gaps) else 0

  why <- character(0); score <- 0
  add <- function(n, msg = NULL) { score <<- score + n; if (!is.null(msg)) why <<- c(why, msg) }
  if (coverage >= 0.8) add(3, "score kept on nearly every rally")
  else if (coverage >= 0.35) add(1, "score kept on some rallies")
  else add(0, "almost no point codes")
  if (top >= 20) add(3, paste("played to", top))
  else if (top >= 10) add(2, paste("reached", top))
  else if (top >= 5) add(1, paste("reached", top))
  if (seq_ok >= 0.8 && nrow(pts) > 3) add(2, "score climbs one point at a time")
  else if (seq_ok >= 0.5 && nrow(pts) > 3) add(1)
  if (per_rally >= 4.5) add(2, "full rallies")
  else if (per_rally >= 3.5) add(1)
  else add(0, "short repetitive rallies")
  if (pace >= 20 && pace <= 60) add(2, "game pace")
  else if (pace >= 12 && pace < 20) add(1)
  else if (pace > 60) add(0, "long gaps between serves")
  else if (pace) add(0, "serves every few seconds")
  # More points awarded than serves taken, or rallies far longer than a real
  # one, means the block was scored by the coach rather than played out.
  if (coverage > 1.3) add(-2, "more points than serves")
  if (per_rally > 10) add(-2, "rallies far longer than a real one")

  list(kind = if (score >= 8) "game" else if (score >= 5) "scrimmage" else "drill",
       score = score, rallies = nrow(srv), why = paste(why, collapse = ", "))
}
