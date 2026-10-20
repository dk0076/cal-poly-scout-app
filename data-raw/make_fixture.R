# ---------------------------------------------------------------------------
# Build a .vsm test fixture that actually exercises the coordinate paths.
#
# Why this exists: the coordinate half of this app -- trajectory charts,
# landing grids, lateral thirds, line/seam/cross, depth bands, set origin,
# passer start -- is dead code against a .dvw, which carries no coordinates,
# and the only .vsm files to hand were skeletons with no travelPath, no lineup
# and no combination table. So there was nothing to run it against.
#
# The fixture is generated from a real .dvw's own coded events, and every
# coordinate is placed DETERMINISTICALLY inside the zone the coder actually
# recorded, plus a little jitter. That makes it more than a smoke test: the
# zone a coordinate falls in has to come back out as the zone the file coded,
# so round-tripping it checks zone_of(), lat_third(), srv_zone6(), depth_of()
# and the OPP_ZONE / OWN_ZONE lattices against DataVolley's own numbering.
#
# It is synthetic. It proves the geometry is self-consistent and the charts
# render; it does not substitute for validating a rate against VolleyStation's
# own report on a real file.
#
# Usage:  Rscript data-raw/make_fixture.R <in.dvw> <out.vsm>
# ---------------------------------------------------------------------------

suppressMessages(library(data.table))
setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))))
for (f in c("core.R", "parse_vsm.R", "parse_dvw.R", "model.R"))
  source(file.path("R", f))

args <- commandArgs(TRUE)
src <- args[1]; dst <- args[2]
set.seed(20261019)

txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
m <- finalize_match(parse_dvw(txt, basename(src)))
p <- m$plays

jit <- function(n, lo, hi) runif(n, lo, hi)

#' A point inside an opponent-half zone (y 900-1800), in the acting team's frame.
opp_point <- function(z) {
  rc <- which(OPP_ZONE == z, arr.ind = TRUE)
  if (!nrow(rc)) return(c(jit(1, 150, 750), jit(1, 1000, 1700)))
  r <- rc[1, "row"]; c0 <- rc[1, "col"]
  c(jit(1, 300 * (c0 - 1) + 25, 300 * c0 - 25),
    jit(1, 900 + 300 * (r - 1) + 25, 900 + 300 * r - 25))
}
#' A point inside an own-half zone (y 0-900).
own_point <- function(z) {
  rc <- which(OWN_ZONE == z, arr.ind = TRUE)
  if (!nrow(rc)) return(c(jit(1, 150, 750), jit(1, 150, 750)))
  r <- rc[1, "row"]; c0 <- rc[1, "col"]
  c(jit(1, 300 * (c0 - 1) + 25, 300 * c0 - 25),
    jit(1, 900 - 300 * r + 25, 1200 - 300 * r - 25))
}

# ---- fill in the zones this file never coded --------------------------
# The source .dvw codes no serve zone at all and a landing zone on only 69 of
# 205 attacks, which left every generated serve in zone 6 and the lane, depth
# and tendency layers with nothing to separate. So the gaps are filled with a
# plausible spread first, and the filled zones are what gets written into the
# fixture -- the file then states the zone its own coordinates fall in, which
# is what makes the round-trip check in tests/verify.R cover every play rather
# than the third that happened to be coded.
SZ <- p$start_zone; EZ <- p$end_zone

serve_w <- c(`1` = .16, `6` = .16, `5` = .14, `9` = .14, `8` = .13, `7` = .12,
             `2` = .05, `3` = .05, `4` = .05)
si <- which(p$skill == "S" & is.na(EZ))
EZ[si] <- as.integer(sample(names(serve_w), length(si), TRUE, prob = serve_w))
si2 <- which(p$skill == "S" & is.na(SZ))
SZ[si2] <- as.integer(sample(c("1", "6", "5"), length(si2), TRUE, prob = c(.5, .3, .2)))

#' A landing zone for an attack with none coded, biased by the approach: a pin
#' hitter's two real options are line and cross, and depth skews deep.
sample_landing <- function(start) {
  col <- if (!is.na(start) && start == 4) sample(1:3, 1, prob = c(.40, .25, .35))
         else if (!is.na(start) && start == 2) sample(1:3, 1, prob = c(.35, .25, .40))
         else sample(1:3, 1, prob = c(.30, .40, .30))
  row <- sample(1:3, 1, prob = c(.20, .35, .45))
  OPP_ZONE[row, col]
}
ai <- which(p$skill == "A" & is.na(EZ))
for (i in ai) EZ[i] <- sample_landing(SZ[i])

#' travelPath for one play, or NULL where the format would carry none.
#' Receptions are resolved later, since they need their own serve's landing.
path_for <- function(i) {
  sk <- p$skill[i]; sz <- SZ[i]; ez <- EZ[i]; ef <- p$effect[i]
  if (sk == "S") {
    st <- own_point(if (is.na(sz)) 6 else sz)
    # the serve is struck from behind the endline, so y is negative
    a <- c(st[1], jit(1, -70, -8))
    b <- if (is.na(ez)) opp_point(6) else opp_point(ez)
    if (!is.na(ef) && ef == "=")   # an error lands out: long, wide, or in the net
      b <- switch(sample(3, 1),
                  c(b[1], jit(1, 1810, 1890)),
                  c(if (b[1] < 450) jit(1, -80, -10) else jit(1, 910, 980), b[2]),
                  c(b[1], jit(1, 700, 870)))
    return(list(a, b))
  }
  if (sk == "R") return(NULL)   # resolved in the reception pass below
  if (sk == "A") {
    fr <- !is.na(sz) && sz %in% c(2, 3, 4)
    a <- own_point(if (is.na(sz)) 4 else sz)
    a[2] <- if (fr) jit(1, 840, 900) else jit(1, 560, 760)
    b <- if (is.na(ez)) opp_point(5) else opp_point(ez)
    if (!is.na(ef) && ef == "=")
      b <- switch(sample(3, 1),
                  c(b[1], jit(1, 1810, 1900)),
                  c(if (b[1] < 450) jit(1, -90, -10) else jit(1, 910, 990), b[2]),
                  c(b[1], jit(1, 820, 890)))
    if (!is.na(ef) && ef == "/") b <- c(b[1], jit(1, 905, 1000))  # blocked straight down
    return(list(a, b))
  }
  if (sk == "E") {
    # the set: taken in the setter's own half, nearer the net the better the pass
    off <- switch(p$rec_effect[i] %||% "-", "#" = jit(1, 20, 120), "+" = jit(1, 60, 200),
                  "!" = jit(1, 150, 320), "-" = jit(1, 220, 450), jit(1, 120, 400))
    # A setter in system works right of centre; a broken pass drags the contact
    # anywhere, which is what puts sets in the left lane at all.
    oos <- (p$rec_effect[i] %||% "-") %in% c("-", "/", "!")
    lane <- if (oos && runif(1) < 0.45) jit(1, 60, 820) else
      switch(p$target[i] %||% "F", "F" = jit(1, 480, 760), "C" = jit(1, 330, 600),
             "B" = jit(1, 560, 820), "P" = jit(1, 300, 620), jit(1, 350, 650))
    a <- c(lane, COURT$NET - off)
    # the end of the set is the attacker's contact point
    b <- switch(p$target[i] %||% "F", "F" = c(jit(1, 40, 240), jit(1, 850, 900)),
                "C" = c(jit(1, 330, 570), jit(1, 860, 900)),
                "B" = c(jit(1, 660, 870), jit(1, 850, 900)),
                "P" = c(jit(1, 330, 570), jit(1, 620, 740)),
                c(jit(1, 300, 600), jit(1, 860, 900)))
    return(list(a, b))
  }
  NULL   # blocks, digs and free balls: left without coordinates, as files often do
}

paths <- lapply(seq_len(nrow(p)), path_for)

# ---- receptions: place the receiver where the serve actually arrived ----
# .vsm coordinates are per acting team, so the one physical point where the
# ball crossed is (x, y) in the server's frame and (900 - x, 1800 - y) in the
# receiver's. Deriving the reception contact that way rather than drawing it
# fresh makes the two frames consistent, which is the property tests/verify.R
# checks when it round-trips the reception zone through zone_of(own).
by_rally <- split(seq_len(nrow(p)), paste(p$set_no, p$rally_no, sep = "|"))
for (ii in by_rally) {
  ii <- ii[order(p$seq[ii])]
  srv <- ii[p$skill[ii] == "S"]
  if (!length(srv)) next
  sp <- paths[[srv[1]]]
  if (is.null(sp)) next
  land <- sp[[2]]
  for (i in ii[p$skill[ii] == "R"]) {
    ef <- p$effect[i]
    a <- c(COURT$W - land[1], COURT$L - land[2])
    if (a[1] < 5 || a[1] > COURT$W - 5 || a[2] < 5 || a[2] > COURT$NET - 5) next
    SZ[i] <- zone_of(a[1], a[2], "own")
    if (!is.na(ef) && ef == "=") { paths[[i]] <- list(a, a); next }  # aced where it landed
    # the pass result: tight and central when graded well, off the net when not
    off <- switch(ef %||% "-", "#" = jit(1, 20, 110), "+" = jit(1, 60, 190),
                  "!" = jit(1, 170, 330), "-" = jit(1, 250, 480), jit(1, 300, 600))
    xw <- switch(ef %||% "-", "#" = jit(1, 380, 520), "+" = jit(1, 320, 580),
                 jit(1, 150, 760))
    b <- if (!is.na(ef) && ef == "/") c(xw, jit(1, 930, 1080))   # overpass
         else c(xw, COURT$NET - off)
    paths[[i]] <- list(a, b)
  }
}

# ---- rosters ----------------------------------------------------------
roster_json <- function(sd) {
  r <- m$rosters[[sd]]
  lapply(seq_len(nrow(r)), function(i) list(
    shirtNumber = r$num[i],
    firstName = "",
    lastName = r$last[i]))
}
# one libero per side, picked the way the app infers it: plays, never attacks
libero_of <- function(sd) {
  q <- p[p$side == sd, ]
  cand <- setdiff(unique(q$num[q$skill == "R"]), unique(q$num[q$skill == "A"]))
  if (length(cand)) cand[1] else integer()
}

combos <- lapply(names(m$calls), function(k) list(
  code = k, name = m$calls[[k]]$name, targetAttacker = m$calls[[k]]$target))

# ---- events, grouped back into sets and rallies -----------------------
sets_out <- list()
for (sn in sort(unique(p$set_no))) {
  idx_set <- which(p$set_no == sn)
  events <- list()
  for (rn in sort(unique(p$rally_no[idx_set]))) {
    ii <- idx_set[p$rally_no[idx_set] == rn]
    ii <- ii[order(p$seq[ii])]
    if (!length(ii)) next
    plays <- lapply(ii, function(i) {
      out <- list(team = p$tag[i], player = p$num[i], skill = p$skill[i])
      if (!is.na(p$effect[i])) out$effect <- p$effect[i]
      if (!is.na(p$hit_type[i])) out$hitType <- p$hit_type[i]
      if (!is.na(p$skill_type[i])) out$skillType <- p$skill_type[i]
      cl <- if (p$skill[i] == "E") p$setter_call[i] else p$call[i]
      if (!is.na(cl)) out$combination <- cl
      if (!is.na(p$target[i])) out$targetAttacker <- p$target[i]
      if (!is.na(p$blockers[i])) out$players <- p$blockers[i]
      if (!is.na(SZ[i])) out$startZone <- SZ[i]
      if (!is.na(EZ[i])) out$endZone <- EZ[i]
      if (!is.na(p$end_sub_zone[i])) out$endSubZone <- p$end_sub_zone[i]
      tp <- paths[[i]]
      if (!is.null(tp))
        out$travelPath <- lapply(tp, function(q) list(x = round(q[1], 1), y = round(q[2], 1)))
      out
    })
    # lineup: zones 1..6 per side, plus the setter's zone, which is the field
    # the whole rotation layer reads
    lu <- list()
    for (sd in c("home", "away")) {
      j <- ii[p$side[ii] == sd]
      if (!length(j)) j <- ii[1]
      cols <- if (p$side[j[1]] == sd) paste0("lu", 1:6) else paste0("olu", 1:6)
      pos <- as.integer(unlist(p[j[1], cols, with = FALSE]))
      sa <- if (p$side[j[1]] == sd) p$setter_at[j[1]] else p$opp_setter_at[j[1]]
      lu[[sd]] <- list(positions = setNames(as.list(pos), as.character(1:6)),
                       setterAt = sa)
    }
    sc <- strsplit(p$score[ii[1]], "-", fixed = TRUE)[[1]]
    pw <- p$tag[ii][!is.na(p$point_won[ii]) & p$point_won[ii]]
    ex <- list(plays = plays)
    if (length(pw)) ex$point <- pw[1]
    events[[length(events) + 1L]] <- list(
      lineup = lu,
      score = list(home = as.integer(sc[1]), away = as.integer(sc[2])),
      exchange = ex)
  }
  sets_out[[length(sets_out) + 1L]] <- list(number = sn, events = events)
}

out <- list(
  startDate = "2026-10-09T18:00:00",
  comment = "SYNTHETIC fixture. Events are real, coordinates are generated inside each coded zone. Do not read any number off this file as a measurement.",
  team = list(
    home = list(name = m$teams$home$name, shortCode = "HOM",
                players = roster_json("home"), libero = as.list(libero_of("home"))),
    away = list(name = m$teams$away$name, shortCode = "AWY",
                players = roster_json("away"), libero = as.list(libero_of("away")))
  ),
  attackCombinations = combos,
  scout = list(sets = sets_out)
)

writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "null"), dst)
cat(sprintf("wrote %s: %d sets, %d plays, %d with coordinates\n",
            dst, length(sets_out), nrow(p), sum(!vapply(paths, is.null, logical(1)))))
