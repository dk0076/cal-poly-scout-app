# ---------------------------------------------------------------------------
# Compare one match exported two ways.
#
#   Rscript tests/parity.R <fileA> <fileB> [team name]
#
# The point of this script is a question the app cannot answer on its own:
# when the same match is read from a .vsm and from a .dvw, does anything
# differ? Everything that does is either a parser bug or a real difference
# between the two exports, and the report separates those by saying how big
# each disagreement is and where it sits.
#
# Coordinates get their own treatment. A .dvw stores a contact as an index
# into a 100x100 grid, so it cannot reproduce a .vsm position exactly -- half
# a cell is 5.6 units across and 11.1 along. Anything inside that is the
# format; anything outside it is not.
# ---------------------------------------------------------------------------

suppressMessages(library(data.table))
root <- dirname(dirname(normalizePath(sub("--file=", "",
         grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
setwd(root)
for (f in c("core.R", "parse_vsm.R", "parse_dvw.R", "model.R"))
  source(file.path("R", f))

args <- commandArgs(TRUE)
if (length(args) < 2)
  stop("usage: Rscript tests/parity.R <fileA> <fileB> [team]", call. = FALSE)

read_any <- function(p) {
  txt <- paste(readLines(p, warn = FALSE, encoding = "latin-1"), collapse = "\n")
  txt <- sub("^﻿", "", txt)
  nm <- basename(p)
  is_dvw <- grepl("\\.dvw$", nm, ignore.case = TRUE) ||
    grepl("^\\s*\\[3DATAVOLLEYSCOUT\\]", txt)
  finalize_match(if (is_dvw) parse_dvw(txt, nm) else parse_vsm(txt, nm))
}

DIFFS <- 0L
hdr <- function(s) cat(sprintf("\n%s\n%s\n", s, strrep("-", nchar(s))))
same <- function(label, a, b, note = "") {
  eq <- isTRUE(all.equal(a, b))
  if (!eq) DIFFS <<- DIFFS + 1L
  cat(sprintf("  %-28s %14s %14s   %s%s\n", label,
              format(a), format(b), if (eq) "same" else "DIFFER",
              if (nzchar(note)) paste0("  ", note) else ""))
}

A <- read_any(args[1]); B <- read_any(args[2])
cat(sprintf("A: %-52s %s\n", basename(args[1]), A$format))
cat(sprintf("B: %-52s %s\n", basename(args[2]), B$format))

hdr("Match level")
same("plays", nrow(A$plays), nrow(B$plays))
same("sets", length(A$sets), length(B$sets))
same("home team", A$teams$home$name, B$teams$home$name)
same("away team", A$teams$away$name, B$teams$away$name)
same("plays with coordinates", sum(A$plays$has_xy), sum(B$plays$has_xy),
     "a file coded without ball positions will read 0 in both")

pa <- A$plays; pb <- B$plays
data.table::setorder(pa, set_no, rally_no, seq)
data.table::setorder(pb, set_no, rally_no, seq)

hdr("Touch counts by skill")
sk <- sort(union(unique(pa$skill), unique(pb$skill)))
for (s in sk) same(s, sum(pa$skill == s), sum(pb$skill == s))

hdr("Evaluation counts")
ev <- sort(union(unique(na.omit(pa$effect)), unique(na.omit(pb$effect))))
for (e in ev) same(sprintf("'%s'", e), sum(pa$effect == e, na.rm = TRUE),
                   sum(pb$effect == e, na.rm = TRUE))

teams <- if (length(args) >= 3) args[3] else
  unique(c(A$teams$home$name, A$teams$away$name))
for (tm in teams) {
  hdr(sprintf("Rates for %s", tm))
  qa <- pa[pa$team_name == tm, ]; qb <- pb[pb$team_name == tm, ]
  if (!nrow(qa) || !nrow(qb)) { cat("  not present in both files\n"); next }
  la <- attack_line(qa[qa$skill == "A", ]); lb <- attack_line(qb[qb$skill == "A", ])
  same("attack attempts", la$ta, lb$ta); same("kills", la$k, lb$k)
  same("errors", la$e, lb$e); same("blocked", la$b, lb$b)
  same("hitting", round(la$eff, 4), round(lb$eff, 4))
  sa <- serve_line(qa[qa$skill == "S", ]); sb <- serve_line(qb[qb$skill == "S", ])
  same("serves", sa$n, sb$n); same("aces", sa$ace, sb$ace)
  same("serve errors", sa$err, sb$err)
  same("serve Eff", round(sa$eff, 4), round(sb$eff, 4))
  ra <- receive_line(qa[qa$skill == "R", ]); rb <- receive_line(qb[qb$skill == "R", ])
  same("receptions", ra$n, rb$n)
  same("positive%", round(ra$pospct, 4), round(rb$pospct, 4))
  same("pass average", round(ra$avg, 3), round(rb$avg, 3))
  ea <- set_line(qa[qa$skill == "E", ]); eb <- set_line(qb[qb$skill == "E", ])
  same("sets", ea$n, eb$n)
  same("sets linked to an attack", ea$linked, eb$linked)
  same("rotations seen", length(unique(na.omit(qa$rot))),
       length(unique(na.omit(qb$rot))))
}

# ---- play by play ---------------------------------------------------------
hdr("Play-by-play alignment")
n <- min(nrow(pa), nrow(pb))
if (nrow(pa) != nrow(pb))
  cat(sprintf("  different play counts (%d vs %d); comparing the first %d in file order\n",
              nrow(pa), nrow(pb), n))
a <- pa[seq_len(n), ]; b <- pb[seq_len(n), ]
agree <- function(x, y) mean(ifelse(is.na(x), "\001", as.character(x)) ==
                             ifelse(is.na(y), "\001", as.character(y)))
for (f in c("skill", "num", "effect", "team_name", "set_no")) {
  v <- agree(a[[f]], b[[f]])
  if (v < 1) DIFFS <- DIFFS + 1L
  cat(sprintf("  %-28s %5.1f%% identical %s\n", f, 100 * v,
              if (v == 1) "" else "<- plays are not aligned on this field"))
}
aligned <- agree(a$skill, b$skill) > 0.99 && agree(a$num, b$num) > 0.99
if (!aligned)
  cat("  plays do not line up in file order, so the field and coordinate\n  comparisons below are not meaningful for this pair.\n")

if (aligned) {
  hdr("Coded fields, play by play")
  for (f in c("call", "target", "rot", "phase", "start_zone", "end_zone",
              "blockers", "hit_type", "skill_type")) {
    if (!f %in% names(a) || !f %in% names(b)) next
    both <- !is.na(a[[f]]) & !is.na(b[[f]])
    # judge only the plays where both files actually state the field: one
    # export carrying a zone the other omits is not a disagreement about its
    # value, and counting it as one buries the cases that are
    v <- if (any(both)) agree(a[[f]][both], b[[f]][both]) else NA_real_
    if (!is.na(v) && v < 1) DIFFS <- DIFFS + 1L
    cat(sprintf("  %-28s %5.1f%% identical where both state it (%d plays) %s\n",
                f, 100 * v, sum(both), if (is.na(v) || v == 1) "" else "<- DIFFER"))
  }

  ok <- which(a$has_xy & b$has_xy)
  hdr(sprintf("Coordinates, play by play (%d plays with both)", length(ok)))
  if (!length(ok)) {
    cat("  neither file carries coordinates for the same play, nothing to compare\n")
  } else {
    halfx <- DV_XSTEP / 2 + 1e-6; halfy <- DV_YSTEP / 2 + 1e-6
    for (f in c("x0", "y0", "x1", "y1")) {
      d <- abs(a[[f]][ok] - b[[f]][ok])
      lim <- if (grepl("x", f)) halfx else halfy
      within <- mean(d <= lim)
      if (within < 0.99) DIFFS <- DIFFS + 1L
      cat(sprintf("  %-4s median |diff| %6.1f  p95 %6.1f  max %7.1f  within half a grid cell: %5.1f%% %s\n",
                  f, median(d), quantile(d, .95), max(d), 100 * within,
                  if (within >= 0.99) "" else "<- more than the grid explains"))
    }
    cat(sprintf("\n  half a grid cell is %.1f across and %.1f along; anything inside\n  that is the .dvw grid, not a disagreement about where the ball was.\n",
                DV_XSTEP / 2, DV_YSTEP / 2))
    # Decode fault or different data? A decode fault is a deterministic
    # function of the stored value, so it can tilt the line or shift it but
    # cannot scatter the points: R2 stays near 1. Two files that were clicked
    # independently scatter. So read the slope and R2 together --
    #   R2 near 1, slope off 1      -> the decoding is wrong, fix it
    #   R2 near 1, slope 1, offset  -> the decoding is shifted, fix it
    #   R2 low                      -> the files hold different coordinates
    # A negative correlation means the frames are mirrored, which is the one
    # failure that looks plausible on a chart.
    # Regressed per skill, never pooled. Serves contact at y about -150 and
    # attacks at about +750, so pooling two populations that each sit on
    # their own line bends the combined slope and invents a scale error that
    # is not there -- it reported 0.976 on a file whose per-skill slopes were
    # 1.01 and 1.00.
    cat("\n  slope and R2 per skill (pooling skills would bend these):\n")
    for (sk in c("S", "A", "E", "R", "D", "F")) {
      for (f in c("x0", "y0", "x1", "y1")) {
        i <- ok[a$skill[ok] == sk]
        if (length(i) < 12) next
        x <- b[[f]][i]; y <- a[[f]][i]
        if (length(unique(x)) < 5) next
        # a skill nobody re-cleaned fits perfectly, and lm says so loudly
        m <- stats::lm(y ~ x)
        r2 <- suppressWarnings(summary(m)$r.squared)
        step <- if (grepl("x", f)) DV_XSTEP else DV_YSTEP
        verdict <- if (coef(m)[2] < 0) "MIRRORED FRAME"
          else if (r2 > 0.95 && abs(coef(m)[2] - 1) > 0.03) "decoding scale looks wrong"
          else if (r2 > 0.95 && abs(median(y - x)) > step) "decoding offset looks wrong"
          else if (r2 > 0.95) "same coordinates"
          else "different coordinates, not a decoding fault"
        cat(sprintf("  %-2s %-3s n=%4d slope %+.3f  median diff %+7.1f (%.1f cells)  R2 %.3f  %s\n",
                    sk, f, length(i), coef(m)[2], median(y - x),
                    abs(median(y - x)) / step, r2, verdict))
      }
    }
    cat("\n  how far apart, in grid cells (a cell is the format's own limit):\n")
    for (lab in c("contact", "landing")) {
      fx <- if (lab == "contact") "x0" else "x1"
      fy <- if (lab == "contact") "y0" else "y1"
      d <- pmax(abs(a[[fx]][ok] - b[[fx]][ok]) / DV_XSTEP,
                abs(a[[fy]][ok] - b[[fy]][ok]) / DV_YSTEP)
      cat(sprintf("  %-8s within 1 cell %5.1f%%   within 2 %5.1f%%   beyond 4 %5.1f%%\n",
                  lab, 100 * mean(d <= 1), 100 * mean(d <= 2), 100 * mean(d > 4)))
    }
  }
}

hdr("Verdict")
if (DIFFS == 0) {
  cat("  No differences. The two files read identically.\n")
} else {
  cat(sprintf("  %d field%s differ. Read the lines marked DIFFER above: a coordinate\n  gap inside half a grid cell is the format, and anything else is either a\n  parser fault or two genuinely different exports.\n",
              DIFFS, if (DIFFS == 1) "" else "s"))
}
