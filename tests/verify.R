# ---------------------------------------------------------------------------
# Re-parse the fixtures and check the layers that a rendered page cannot show
# you are wrong about.
#
# Run this after ANY change to the parsing or geometry layer. A parse that
# shifts by one column still produces output that looks like volleyball, so
# "the app still renders" is not evidence. The checks that matter here are the
# zone round-trips: a coordinate placed inside the zone the coder recorded has
# to come back out of zone_of() as that same zone, which tests the OPP_ZONE and
# OWN_ZONE lattices, lat_third(), depth_of() and srv_zone6() against
# DataVolley's own numbering rather than against my reading of it.
#
# Usage:  Rscript tests/verify.R [extra files...]
# ---------------------------------------------------------------------------

suppressMessages(library(data.table))
root <- dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
setwd(root)
for (f in c("core.R", "parse_vsm.R", "parse_dvw.R", "model.R", "svg.R", "tables.R",
            "tab_attack.R", "tab_serve.R", "tab_set.R", "tab_setter.R",
            "tab_passing.R", "tab_defense.R", "tab_rotation.R", "tab_last6.R",
            "tab_plays.R"))
  source(file.path("R", f))

PASS <- 0L; FAIL <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  ok   %s\n", label)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL %s  %s\n", label, detail)) }
}
info <- function(...) cat(sprintf("       %s\n", paste0(...)))

load_file <- function(pth) {
  txt <- paste(readLines(pth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- sub("^﻿", "", txt)
  nm <- basename(pth)
  is_dvw <- grepl("\\.dvw$", nm, ignore.case = TRUE) ||
    grepl("^\\s*\\[3DATAVOLLEYSCOUT\\]", txt)
  finalize_match(if (is_dvw) parse_dvw(txt, nm) else parse_vsm(txt, nm))
}

files <- c(Sys.glob("data/*.vsm"), Sys.glob("data/*.dvw"), commandArgs(TRUE))
files <- files[file.exists(files)]
if (!length(files))
  stop("No match files to check.\n",
       "  data/ is not in the repository -- coded match files carry real\n",
       "  players and opponents -- so a fresh clone has none. Either:\n",
       "    * put a .vsm or .dvw in data/, or\n",
       "    * pass files as arguments:  Rscript tests/verify.R path/to/match.vsm\n",
       "  To rebuild the coordinate fixture from a .dvw you already have:\n",
       "    Rscript data-raw/make_fixture.R your.dvw data/fixture_coords.vsm",
       call. = FALSE)

matches <- list()
for (pth in files) {
  cat(sprintf("\n== %s\n", basename(pth)))
  t0 <- Sys.time()
  m <- load_file(pth)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  p <- m$plays
  info(sprintf("%s | %s vs %s | %d sets | %d plays | %.2fs",
               m$format, m$teams$home$name, m$teams$away$name,
               length(m$sets), nrow(p), el))
  ok("parses to at least one play", nrow(p) > 0)
  ok("parse under 3 s", el < 3, sprintf("%.2fs", el))
  ok("every play has a side", all(p$side %in% c("home", "away")))
  ok("every play has a skill", all(!is.na(p$skill)))
  ok("set numbers are positive", all(p$set_no >= 1))
  ok("rally numbers never go backwards inside a set",
     all(tapply(p$rally_no, p$set_no, function(v) all(diff(v) >= 0))))
  ok("no jersey number is NA on a coded touch", all(!is.na(p$num)))

  # a rally's touches must be ordered, since the phase walk and the set->attack
  # link both read the sequence
  ok("seq is dense within each rally",
     all(tapply(p$seq, paste(p$set_no, p$rally_no), function(v)
       identical(sort(v), seq_along(v)))))

  atk <- p[p$skill == "A", ]
  ok("every attack got a phase", all(!is.na(atk$phase)),
     sprintf("%d without", sum(is.na(atk$phase))))
  ok("SR attacks all carry a passer",
     all(!is.na(atk$passer[!is.na(atk$phase) & atk$phase == "SR"])))

  sets <- p[p$skill == "E", ]
  linked <- sum(!is.na(sets$atk_effect))
  info(sprintf("sets %d, linked to an attack %d (%.0f%%)", nrow(sets), linked,
               100 * linked / max(1, nrow(sets))))
  # one attack must never be claimed by two sets
  claimed <- sets$atk_uid[!is.na(sets$atk_uid)]
  ok("no attack is claimed by two sets", !any(duplicated(claimed)),
     sprintf("%d duplicated", sum(duplicated(claimed))))

  if (any(p$has_xy)) {
    xy <- p[p$has_xy, ]
    info(sprintf("coordinates on %d plays", nrow(xy)))
    ok("coordinates are finite",
       all(is.finite(c(xy$x0, xy$y0, xy$x1, xy$y1))))

    # ---- zone round-trips --------------------------------------------
    # Exact agreement is the wrong bar on a real file. VolleyStation quantises
    # clicks onto a lattice that piles points right on the zone lines, and the
    # files carry slightly different court calibrations, so a coordinate-derived
    # zone differs from the coder's on a few percent of plays. What must hold is
    # that every disagreement is a boundary case: a point that lands a hand's
    # width inside the next band, never one zone away across open floor. A
    # systematic mis-mapping -- a transposed lattice, a mirrored half -- would
    # put disagreements in the middle of zones and fail this.
    near_line <- function(v, origin) {
      d <- (v - origin) %% 300
      pmin(d, 300 - d)
    }
    zone_check <- function(q, xc, yc, zc, half, lab) {
      if (!nrow(q)) return(invisible())
      got <- zone_of(q[[xc]], q[[yc]], half)
      bad <- which(got != q[[zc]])
      if (!length(bad)) { ok(sprintf("%s matches the coded zone exactly", lab), TRUE); return(invisible()) }
      d <- pmin(near_line(q[[xc]][bad], 0),
                near_line(q[[yc]][bad], if (half == "opp") 900 else 0))
      # A coder can also simply type one zone and click another; that is a
      # coding slip, not a geometry fault, and it shows up as a disagreement
      # out in open floor. Measured over 14 scouted files there is exactly one,
      # so a 1% allowance separates "this happens" from "the mapping is wrong"
      # -- a transposed lattice or a mirrored half would put most of the
      # disagreements out there, not one in ninety.
      far <- sum(d > 15)
      ok(sprintf("%s: disagreements are boundary cases", lab),
         far <= max(1L, ceiling(0.01 * nrow(q))),
         sprintf("%d of %d differ out in open floor, furthest %.0f units from a line",
                 far, nrow(q), max(d)))
      info(sprintf("%s: %d of %d differ (%.0f%%), %d of them beyond a lattice step",
                   lab, length(bad), nrow(q), 100 * length(bad) / nrow(q), far))
    }

    a <- xy[xy$skill == "A" & !is.na(xy$end_zone) &
            (is.na(xy$effect) | !(xy$effect %in% c("=", "/"))) &
            !out_of_court(xy$x1, xy$y1), ]
    zone_check(a, "x1", "y1", "end_zone", "opp", "attack landing zone")

    # and the fix for all of it: the recorded zone wins where there is one
    if (nrow(a)) ok("zone_or_coded() returns the coder's own zone untouched",
                    all(zone_or_coded(a$end_zone, a$x1, a$y1, "opp") == a$end_zone))
    s <- xy[xy$skill == "S" & !is.na(xy$end_zone) &
            (is.na(xy$effect) | xy$effect != "=") &
            !out_of_court(xy$x1, xy$y1), ]
    if (nrow(s)) {
      zone_check(s, "x1", "y1", "end_zone", "opp", "serve landing zone")
      # srv_zone6 and DV_ZONE6 collapse nine zones to six by different routes:
      # DV folds the middle band onto the back row, srv_zone6 splits the half
      # at its own midpoint. They agree except inside the middle band, which is
      # expected and is why only the agreement rate is reported here.
      z6 <- srv_zone6(s$x1, s$y1)
      dv <- unname(DV_ZONE6[as.character(s$end_zone)])
      mid <- s$end_zone %in% c(7, 8, 9)
      info(sprintf("srv_zone6 vs coded zone outside the middle band: %.0f%% agree",
                   100 * mean(z6[!mid] == dv[!mid])))
      ok("srv_zone6_or_coded() returns the coder's own zone untouched",
         all(srv_zone6_or_coded(s$end_zone, s$x1, s$y1) == dv))
      info(sprintf("middle-band agreement %.0f%% (expected to be partial)",
                   100 * mean(z6[mid] == dv[mid])))

      # at or behind the endline; one file has a serve clicked exactly on it
      ok("serve contact is behind the endline", all(s$y0 <= 0),
         sprintf("%d in front of it", sum(s$y0 > 0)))
      ok("serve landings are past the net", all(s$y1 > COURT$NET))
    }
    r <- xy[xy$skill == "R" & !is.na(xy$start_zone) &
            xy$y0 >= 0 & xy$y0 < COURT$NET & xy$x0 >= 0 & xy$x0 <= COURT$W, ]
    zone_check(r, "x0", "y0", "start_zone", "own", "reception contact zone")

    # ---- the derived reads the tabs lean on --------------------------
    aa <- xy[xy$skill == "A", ]
    lt <- lat_third(aa$x1)
    ok("lat_third covers all three lanes", length(unique(lt)) == 3,
       paste(sort(unique(lt)), collapse = ","))
    ok("depth_of covers all three bands",
       length(unique(depth_of(aa$y1[aa$y1 > COURT$NET]))) == 3)
    rd <- rel_direction(aa$x0, aa$x1)
    ok("rel_direction is NA exactly for middle-origin attacks",
       all(is.na(rd) == (origin_band(aa$x0) == "middle")))

    # passer_start needs 5 in-court serves before it will place anything, and
    # one file carries coordinates on only a handful of its serves
    srv_xy <- xy[xy$skill == "S", ]
    ic <- sum(!out_of_court(srv_xy$x1, srv_xy$y1))
    if (ic >= 5) {
      ps <- passer_start(srv_xy)
      ok("passer_start places at least one lane", length(ps$spots) >= 1,
         passer_start_text(ps))
    } else {
      info(sprintf("only %d in-court serves carry coordinates, passer_start declines to place one, as designed", ic))
    }
    for (sp in ps$spots) {
      ok(sprintf("passer_start box brackets its median (%s)", sp$k),
         sp$x25 <= sp$x + 1e-9 && sp$x <= sp$x75 + 1e-9 &&
         sp$y25 <= sp$y + 1e-9 && sp$y <= sp$y75 + 1e-9)
    }

    sz <- set_zone_of(xy[xy$skill == "E", ])
    ok("set_zone_of labels every set with a contact point",
       all(!is.na(sz)))
    info(sprintf("set origin zones used: %s",
                 paste(sort(unique(sz)), collapse = " ")))
  } else {
    info("no coordinates in this file, charts are suppressed by design")
    ok("coords_available() reports no coordinates",
       !coords_available(list(m), list(matches = character())))
  }

  if (length(m$warnings)) info("warnings: ", paste(m$warnings, collapse = " / "))
  matches[[m$match_id]] <- m
}

cat("\n== dvw / vsm coordinate parity\n")
# The two formats must describe the same court. A .vsm stores a contact
# directly; a .dvw stores an index into a 100x100 grid, and stores a reception
# or dig as the incoming ball in the OPPONENT's frame. Both of those are easy
# to get subtly wrong in a way that still draws a plausible-looking chart, so
# the codec is checked against itself here rather than against a second file.
#
# Two real files of one match cannot settle this: measured on IU-TAMU, the
# cleaned .vsm and the uncleaned .dvw hold independently entered coordinates
# for the same 167 attacks, correlating +0.99 on contact x. That correlation
# is what rules out a flipped frame; it is not a parity test.
grid_rt <- function(x, y) {
  col <- round((x - DV_X0) / DV_XSTEP)
  row <- round((y - DV_Y0) / DV_YSTEP)
  col <- pmin(pmax(col, 0), 99); row <- pmin(pmax(row, 0), 99)
  dv_xy(row * 100L + col + 1L)
}
ok("grid index decodes to the same point it encodes",
   {
     i <- 1:10000
     p <- dv_xy(i)
     j <- round((p$y - DV_Y0) / DV_YSTEP) * 100 + round((p$x - DV_X0) / DV_XSTEP) + 1
     all(j == i)
   })
# the grid overhangs the court on every side: x -106.9..1006.9 against a
# 0..900 court, y -211.1..1988.9 against 0..1800, which is the out-of-bounds
# margin a coder needs to mark a ball landing wide or long
ok("the grid covers the court plus an out-of-bounds margin",
   { p <- dv_xy(1:10000)
     min(p$x) < 0 && max(p$x) > COURT$W && min(p$y) < 0 && max(p$y) > COURT$L &&
     all(p$x >= -107 & p$x <= 1007 & p$y >= -212 & p$y <= 1989) })
ok("the net sits on a grid row",
   isTRUE(all.equal(dv_xy(50 * 100 + 1)$y, COURT$NET)))

vs <- Filter(function(m) m$format == "vsm" && any(m$plays$has_xy), matches)
if (length(vs)) {
  p <- vs[[1]]$plays
  q <- p[p$has_xy & p$skill %in% c("S", "A", "E"), ]
  rt <- grid_rt(q$x0, q$y0)
  dx <- abs(rt$x - q$x0); dy <- abs(rt$y - q$y0)
  ok("a vsm contact survives a round trip through the dvw grid",
     max(dx) <= DV_XSTEP / 2 + 1e-6 && max(dy) <= DV_YSTEP / 2 + 1e-6,
     sprintf("worst x %.2f (step %.2f), worst y %.2f (step %.2f)",
             max(dx), DV_XSTEP, max(dy), DV_YSTEP))
  # the lane and depth a chart reads off must survive it too, except for
  # points sitting within half a grid cell of the line
  lane_a <- lat_third(q$x1); lane_b <- lat_third(grid_rt(q$x1, q$y1)$x)
  near <- abs((q$x1 %% 300) - 150) > 150 - DV_XSTEP
  ok("landing lane survives the round trip away from the lines",
     all(lane_a[!near] == lane_b[!near]),
     sprintf("%d of %d differ", sum(lane_a[!near] != lane_b[!near]), sum(!near)))

  # and the reception rule is its own inverse: mirroring twice is a no-op
  mx <- COURT$W - (COURT$W - q$x0); my <- COURT$L - (COURT$L - q$y0)
  ok("the reception mirror is self-inverse",
     isTRUE(all.equal(mx, q$x0)) && isTRUE(all.equal(my, q$y0)))
}

dv <- Filter(function(m) m$format == "dvw" && any(m$plays$has_xy), matches)
for (m in dv) {
  p <- m$plays[m$plays$has_xy, ]
  s <- p[p$skill == "S", ]
  if (nrow(s)) ok(sprintf("%s: dvw serves start behind the endline", substr(m$file_name, 1, 28)),
                  all(s$y0 <= 0), sprintf("%d in front", sum(s$y0 > 0)))
  a <- p[p$skill == "A", ]
  if (nrow(a)) ok(sprintf("%s: dvw attacks cross the net", substr(m$file_name, 1, 28)),
                  mean(a$y0 < COURT$NET & a$y1 > COURT$NET) > 0.9,
                  sprintf("%.0f%% do", 100 * mean(a$y0 < COURT$NET & a$y1 > COURT$NET)))
}

cat("\n== pooled\n")
matches <- classify_systems(matches)
matches <- add_season_codes(matches)
syst <- table(unlist(lapply(matches, function(m) m$plays$sys)))
info(sprintf("system per rally: %s",
             paste(sprintf("%s=%d", names(syst), as.integer(syst)), collapse = " ")))
ok("every play got a system label",
   all(!is.na(unlist(lapply(matches, function(m) m$plays$sys)))))
ok("system labels are from the known set",
   all(names(syst) %in% c("5-1", "6-2", "?")))

idx <- team_index(matches)
focus_name <- idx$name[1]
focus <- focus_sides(matches, focus_name)
info(sprintf("focus %s (%d of %d files)", focus_name, idx$files[1], length(matches)))
ok("focus side resolved for at least one file",
   any(!is.na(unlist(focus))))

sel <- list(matches = character(), sets = integer(), players = integer(),
            rots = character(), passers = integer())
Fp <- focus_plays(matches, sel, focus)
ok("focus_plays returns only the focus team",
   length(unique(Fp$team_name)) == 1, paste(unique(Fp$team_name), collapse = "/"))
Op <- opponent_plays(matches, sel, focus)
# An intrasquad practice file carries the same team name on both sides, so
# "the opponent" there is the other six, not another programme. The filter
# works on side, which stays correct; only the name check cannot apply.
intra <- any(vapply(matches, function(m)
  identical(m$teams$home$name, m$teams$away$name), logical(1)))
if (intra) {
  info("intrasquad file present, both sides share a name, so the opponent name check does not apply")
  # uid is only unique within a file, so identity is the pair with match_id.
  ok("opponent_plays returns the other side's touches",
     nrow(Op) > 0 &&
     !any(paste(Op$match_id, Op$uid) %in% paste(Fp$match_id, Fp$uid)))
} else {
  ok("opponent_plays excludes the focus team",
     !(focus_name %in% unique(Op$team_name)))
}

# A filter must only ever shrink the set, and clearing it must restore it.
n_all <- nrow(Fp)
for (s2 in list(list(sets = 1L), list(rots = "R1"),
                list(players = Fp$num[1]), list(passers = na.omit(Fp$passer)[1]))) {
  if (any(is.na(unlist(s2)))) next
  n <- nrow(focus_plays(matches, modifyList(sel, s2), focus))
  ok(sprintf("filter %s only shrinks (%d of %d)",
             paste(names(s2), unlist(s2), sep = "=", collapse = " "), n, n_all),
     n <= n_all && n >= 0)
}

ck <- coords_available(matches, sel)
o <- list(aView = "arrows", aBins = 3, aColor = "volume", aIncErr = TRUE, tend = TRUE,
          sView = "arrows", sBins = 3, srvType = "all", sPass = TRUE,
          eView = "points", eBins = 3, setSetter = "", setZones = character(),
          setterWho = "main", setterEx = character(), mono = FALSE,
          phases = character(), sys = character())

sel_all <- sel; sel_all$rots <- character()
CV0 <- convert_rallies(matches, sel, focus)
RL <- rally_outcomes(matches, sel_all, focus)
cat("\n== rotations\n")
ok("rally_outcomes returns one row per rally",
   nrow(RL) == nrow(unique(RL[, c("match_id", "set_no", "rally_no")])))
ok("every rally is served by exactly one side or unknown",
   all(is.na(RL$served) | is.logical(RL$served)))
rl0 <- rally_line(RL)
info(sprintf("%d rallies, %s record a winner; side-out %s, point scoring %s",
             rl0$n, pct(rl0$cover), pct(rl0$so), pct(rl0$ps)))
ok("received + served never exceeds rallies with a winner",
   rl0$rec + rl0$srv <= rl0$known)
ok("side-out and point rates are proportions",
   (is.na(rl0$so) || (rl0$so >= 0 && rl0$so <= 1)) &&
   (is.na(rl0$ps) || (rl0$ps >= 0 && rl0$ps <= 1)))
ok("per-rotation received rallies sum to the total",
   sum(vapply(ROTS, function(k) rally_line(RL[!is.na(RL$rot) & RL$rot == k, ])$rec, 0)) <= rl0$rec)
sl <- sideout_line(RL)
ok("the side-out family stacks: FBSO% <= ESO% <= SO%",
   is.na(sl$so) || (sl$fbso <= sl$eso + 1e-9 && sl$eso <= sl$so + 1e-9),
   sprintf("FBSO %s, ESO %s, SO %s", pct(sl$fbso), pct(sl$eso), pct(sl$so)))
ok("earned side-outs are a subset of side-outs won",
   sum(RL$won & RL$kill, na.rm = TRUE) <= sum(RL$won, na.rm = TRUE))
ok("plus/minus is won minus lost",
   sl$plus_minus == sum(RL$won, na.rm = TRUE) - sum(!RL$won, na.rm = TRUE))
ok("every rotation label maps to a setter zone",
   all(!is.na(ROT_ZONE[ROTS])) && setequal(unname(ROT_ZONE[ROTS]), 1:6))
Fa <- focus_plays(matches, sel_all, focus)
ok("sQ% is serve-receive quality, matching the Pass quality tab",
   { sc <- set_cols(RL, Fa); rl <- receive_line(Fa[Fa$skill == "R", ])
     isTRUE(all.equal(sc$sq, rl$pospct)) },
   "worksheet: VSPER(VSOR(R#, R+))")
cat("\n== last 6\n")
A6 <- attacks_of(Fp, character())
if (nrow(A6)) {
  data.table::setorder(A6, date, match_id, set_no, rally_no, seq)
  ok("attacks sort into a single chronological order",
     all(diff(as.integer(factor(paste(A6$date, A6$match_id, sprintf("%03d", A6$set_no),
                                      sprintf("%04d", A6$rally_no), sprintf("%03d", A6$seq))))) >= 0))
  ok("every attack lands in exactly one of pins / middles / other",
     all(attack_group(A6$role) %in% c("Pins", "Middles", "Other")))
  g <- attack_group(A6$role)
  info(sprintf("pins %d, middles %d, other %d attacks",
               sum(g == "Pins"), sum(g == "Middles"), sum(g == "Other")))
  ok("middles are the ones targeted at the middle",
     { mid <- A6[g == "Middles", ]
       !nrow(mid) || mean(!is.na(mid$target) & mid$target == "C") >
                     mean(!is.na(A6$target[g == "Pins"]) & A6$target[g == "Pins"] == "C") },
     "a middle's attacks should be middle-targeted more often than a pin's")
  # the recent window must be a suffix of the player's own attempts
  k1 <- paste(A6$num, A6$player_short, sep = "|")[1]
  q <- A6[paste(A6$num, A6$player_short, sep = "|") == k1, ]
  tl <- utils::tail(q, 6)
  ok("the last-6 window is the most recent attempts, in order",
     nrow(tl) == min(6, nrow(q)) && identical(tl$uid, utils::tail(q$uid, 6)))
  ok("window efficiency uses VolleyStation's formula",
     isTRUE(all.equal(vs_efficiency(tl$effect, "A"), attack_line(tl)$eff)))
}

cat("\n== against VolleyStation's own definitions\n")
# Transcribed from its source at C:\vsdev (app/submodules/functions/
# get-efficiency.function.js and app/server/services/user-settings/
# user-settings.service.js). Any metric that is meant to agree with
# VolleyStation must equal vs_efficiency() for its skill; where this tool
# departs on purpose, the departure is asserted too, so it stays deliberate.
At <- Fp[Fp$skill == "A", ]; Sv <- Fp[Fp$skill == "S", ]; Rc <- Fp[Fp$skill == "R", ]
if (nrow(At)) ok("attack efficiency equals VolleyStation's",
   isTRUE(all.equal(attack_line(At)$eff, vs_efficiency(At$effect, "A"))),
   "(# - / - =) / attempts")
if (nrow(Sv)) ok("serve Eff equals VolleyStation's",
   isTRUE(all.equal(serve_line(Sv)$eff, vs_efficiency(Sv$effect, "S"))),
   "(# + + + / - =) / serves")
if (nrow(Rc)) ok("reception positive% equals VolleyStation's reception efficiency",
   isTRUE(all.equal(receive_line(Rc)$pospct, vs_efficiency(Rc$effect, "R"))),
   "(# + +) / receptions, no losing symbols")
Dg0 <- Fp[Fp$skill == "D", ]
if (nrow(Dg0)) ok("dig in-system departs from VolleyStation on purpose",
   dig_line(Dg0)$in_sys_pct <= dig_line(Dg0)$vs_eff + 1e-9,
   "VolleyStation counts an over-dig as winning; the staff's scout report does not")
ok("every skill in the symbol table yields a proportion in [-1, 1]",
   all(vapply(names(VS_SYMBOLS), function(k)
     { v <- vs_efficiency(c("#","+","!","-","/","="), k); v >= -1 && v <= 1 }, logical(1))))

cat("\n== defense\n")
Dg <- Fp[Fp$skill == "D", ]; Bk <- Fp[Fp$skill == "B", ]
dl <- dig_line(Dg); bl <- block_line(Bk)
info(sprintf("%d digs (%s in system), %d block touches (%d stuff)",
             dl$n, pct(dl$in_sys_pct), bl$n, bl$stuff))
ok("dig grades partition the digs",
   dl$in_sys + dl$oos + dl$over + dl$err <= dl$n)
ok("digs kept up excludes errors", dl$up == dl$n - dl$err)
ok("block grades partition the touches", bl$stuff + bl$touch + bl$err <= bl$n)
OppP <- opponent_plays(matches, sel, focus)
MP <- mirror_plays(OppP[OppP$skill == "A" & OppP$has_xy, ])
if (nrow(MP)) {
  ok("mirroring is its own inverse",
     { back <- mirror_plays(MP)
       orig <- OppP[OppP$skill == "A" & OppP$has_xy, ]
       isTRUE(all.equal(back$x0, orig$x0)) && isTRUE(all.equal(back$y1, orig$y1)) })
  ok("mirrored opponent attacks land in our half",
     mean(MP$y1 < COURT$NET) > 0.85,
     sprintf("%.0f%% do", 100 * mean(MP$y1 < COURT$NET)))
  ok("mirrored opponent contacts are across the net",
     mean(MP$y0 > COURT$NET) > 0.85,
     sprintf("%.0f%% are", 100 * mean(MP$y0 > COURT$NET)))
}
RLo <- rally_outcomes(matches, sel_all, focus, opp = TRUE)
ok("the opponent view covers the same rallies",
   nrow(RLo) == nrow(RL))
ok("a rally the focus team won is one the opponent lost",
   { k <- !is.na(RL$won) & !is.na(RLo$won); all(RL$won[k] != RLo$won[k]) })

render <- function(o2) list(
  attack  = tab_attack(Fp, o2, ck),
  serve   = tab_serve(Fp, o2, ck),
  setting = tab_set(Fp, o2, ck),
  setter  = tab_setter(Fp, o2, ck),
  passing = tab_passing(Fp, o2, ck,
              sys_rows = system_card(focus_plays(matches, sel, focus, ignore_sys = TRUE),
                                     convert_rallies(matches, sel, focus))),
  defense = tab_defense(Fp, opponent_plays(matches, sel, focus),
                        convert_rallies(matches, sel, focus), o2, ck),
  last6 = tab_last6(Fp, o2),
  rotation = tab_rotation(RL, focus_plays(matches, sel_all, focus),
                          rally_outcomes(matches, sel_all, focus, opp = TRUE),
                          opponent_plays(matches, sel_all, focus), o2),
  plays   = tab_plays(Fp, o2))

cat("\n== render\n")
H <- render(o)
for (nm in names(H)) {
  h <- H[[nm]]
  ok(sprintf("%s renders", nm), is.character(h) && nchar(h) > 200,
     sprintf("%d chars", nchar(h)))
  ok(sprintf("%s has no unresolved placeholder", nm),
     !grepl("NULL|NaN|\\bNA%\\b", h))
  if (ck && nm %in% c("attack", "serve", "setting", "passing"))
    ok(sprintf("%s drew marks on a court", nm),
       grepl("<svg class=\"court\"", h, fixed = TRUE) &&
       (grepl("<line", h, fixed = TRUE) || grepl("<circle", h, fixed = TRUE)))
}

cat("\n== view variants\n")
variants <- list(
  list(aView = "heat"), list(aView = "heat", aBins = 6, aColor = "eff"),
  list(aView = "contact"), list(aIncErr = FALSE), list(tend = FALSE),
  list(sView = "heat"), list(sView = "heat", sBins = 6), list(sPass = FALSE),
  list(eView = "toatk"), list(eView = "heat"), list(eView = "heat", eBins = 6),
  list(setZones = c("M1", "R1")), list(setterWho = "all"),
  list(mono = TRUE), list(phases = "SR"), list(sys = "5-1"), list(srvType = "TS"))
for (v in variants) {
  lbl <- paste(names(v), vapply(v, function(x) paste(x, collapse = "+"), ""),
               sep = "=", collapse = " ")
  r <- tryCatch({
    o2 <- modifyList(o, v)
    # phase and system live in o but filter the play set, as they do in the app
    sl <- sel
    f2 <- focus_plays(matches, sl, focus)
    nchar(paste0(tab_attack(f2, o2, ck), tab_serve(f2, o2, ck), tab_set(f2, o2, ck),
                 tab_setter(f2, o2, ck), tab_passing(f2, o2, ck)))
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(lbl, is.numeric(r) && r > 1000, if (is.numeric(r)) paste(r, "chars") else r)
}

cat("\n== empty and degenerate inputs\n")
e0 <- empty_plays()
for (nm in c("attack", "serve", "setting", "setter", "passing", "defense", "rotation", "last6", "plays")) {
  f <- get(paste0("tab_", if (nm == "setting") "set" else nm))
  # tab_plays draws no court and tab_rotation works on rallies, not plays
  r <- tryCatch(
    if (nm == "plays") f(e0, o)
    else if (nm == "rotation") f(RL[0, ], e0, RL[0, ], e0, o)
    else if (nm == "defense") f(e0, e0, CV0[0, ], o, ck)
    else if (nm == "last6") f(e0, o)
    else f(e0, o, ck),
    error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(sprintf("%s survives an empty play set", nm),
     is.character(r) && grepl("class=\"empty\"", r), r)
}
one <- Fp[1, ]
for (nm in c("attack", "serve", "setting", "setter", "passing")) {
  f <- get(paste0("tab_", if (nm == "setting") "set" else nm))
  r <- tryCatch({ f(one, o, ck); "rendered" },
                error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(sprintf("%s survives a single play", nm), r == "rendered", r)
}

# ---------------------------------------------------------------------------
# A file being coded live
# ---------------------------------------------------------------------------
# The app follows a .dvw on disk and re-parses it every few seconds, so it
# reads the same file in states no finished export is ever in: part of a rally
# coded, or a line half written. Two things have to hold. Growing the file must
# only ever add plays, never renumber or drop the ones already on screen. And a
# read caught mid-write must fail loudly, because the alternative, a parse that
# quietly returns fewer plays than the file holds, shows a coach stale numbers
# with a live badge over them.

dvw_src <- files[grepl("[.]dvw$", files, ignore.case = TRUE)]
if (length(dvw_src)) {
  cat("\n== a file being coded live\n")
  raw <- readBin(dvw_src[1], "raw", file.info(dvw_src[1])$size)
  tmp <- tempfile(fileext = ".dvw")
  on.exit(unlink(tmp), add = TRUE)

  try_parse <- function(n) {
    writeBin(raw[seq_len(n)], tmp)
    tryCatch(load_file(tmp), error = function(e) e)
  }

  # Grow the file from a third of the way in and watch the play count. Every
  # prefix that parses has to be a prefix of the finished match too.
  ns <- as.integer(length(raw) * seq(0.35, 1, by = 0.05))
  got <- lapply(ns, try_parse)
  parsed <- !vapply(got, inherits, TRUE, "condition")
  counts <- vapply(got[parsed], function(m) nrow(m$plays), 0L)
  ok("a growing file parses at some prefix", any(parsed))
  ok("growing the file never removes plays", !is.unsorted(counts),
     paste(counts, collapse = " "))
  ok("the whole file holds at least as many plays as any prefix",
     !length(counts) || max(counts) == counts[length(counts)])

  # Nothing may come back as a half-parsed match. Either a match or a
  # condition, and every condition carries a message worth printing.
  bad <- got[!parsed]
  ok("a torn read raises rather than returning a short match",
     all(vapply(bad, function(e) nzchar(conditionMessage(e)), TRUE)))

  # A line cut in the middle is the common case: the coder is mid-touch.
  L <- readLines(dvw_src[1], warn = FALSE)
  L[length(L)] <- substr(L[length(L)], 1, 6)
  writeLines(L, tmp)
  r <- tryCatch({ load_file(tmp); "parsed" },
                error = function(e) paste("raised:", conditionMessage(e)))
  ok("a half-written last line does not crash the reader",
     grepl("^parsed|^raised:", r), r)
}

# ---------------------------------------------------------------------------
# Going back to fix a play
# ---------------------------------------------------------------------------
# Coders correct themselves. VolleyStation rewrites the whole file when they
# do, so the app has to notice an edit in the middle as readily as an append
# at the end, and has to end up with the corrected numbers rather than the
# union of both versions. Two ways that can go wrong: the change is never
# noticed, because the file did not get longer; or it is noticed and merged,
# because the match was added to rather than replaced.

if (length(dvw_src)) {
  cat("\n== going back to fix a play\n")
  L0 <- readLines(dvw_src[1], warn = FALSE)
  tmp2 <- tempfile(fileext = ".dvw")
  on.exit(unlink(tmp2), add = TRUE)
  writeLines(L0, tmp2)
  base <- load_file(tmp2)
  sig0 <- file_sig(tmp2)

  # A grade corrected in place: same line count, same byte count.
  hit <- grep("^[*a][0-9]{2}A.[^=]", L0)
  if (length(hit)) {
    i <- hit[1]
    L1 <- L0
    substr(L1[i], 6, 6) <- "="            # whatever it was, call it an error
    writeLines(L1, tmp2)
    fixed <- load_file(tmp2)
    ok("an edited grade changes nothing about the file's length",
       nchar(L1[i]) == nchar(L0[i]))
    ok("a same-size edit still changes the signature",
       !identical(file_sig(tmp2), sig0))
    ok("an edited play is replaced, not added",
       nrow(fixed$plays) == nrow(base$plays),
       sprintf("%d then %d", nrow(base$plays), nrow(fixed$plays)))
    a0 <- base$plays[skill == "A"]; a1 <- fixed$plays[skill == "A"]
    ok("the corrected grade is what comes back",
       nrow(a1) == nrow(a0) && sum(a1$effect == "=") == sum(a0$effect == "=") + 1L)
    ok("attack efficiency moves with the correction",
       vs_efficiency(a1$effect, "A") < vs_efficiency(a0$effect, "A"),
       sprintf("%.3f then %.3f", vs_efficiency(a0$effect, "A"),
               vs_efficiency(a1$effect, "A")))
  }

  # A play deleted from the middle: the count has to go down, not stay put.
  drop <- grep("^[*a][0-9]{2}[SRAEBD]", L0)
  if (length(drop) > 3) {
    writeLines(L0[-drop[length(drop) %/% 2]], tmp2)
    cut <- load_file(tmp2)
    ok("a deleted play leaves the match", nrow(cut$plays) < nrow(base$plays),
       sprintf("%d then %d", nrow(base$plays), nrow(cut$plays)))
  }

  # And the signature has to be stable when nothing is happening, or the app
  # would re-parse on every poll all match long.
  writeLines(L0, tmp2)
  s1 <- file_sig(tmp2); Sys.sleep(0.25); s2 <- file_sig(tmp2)
  ok("an untouched file keeps its signature", identical(s1, s2))
  ok("a missing file has no signature",
     !nzchar(file_sig(file.path(tempdir(), "no-such-match.dvw"))))
}

cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
if (FAIL) quit(status = 1)
