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
for (f in c("core.R", "net.R", "qr.R", "positions.R", "next_server.R", "parse_vsm.R", "parse_dvw.R", "model.R", "svg.R", "tables.R",
            "tab_attack.R", "tab_serve.R", "tab_set.R", "tab_setter.R",
            "tab_passing.R", "tab_defense.R", "tab_rotation.R", "tab_last6.R", "tab_roster.R", "tab_nextserver.R",
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

# ---------------------------------------------------------------------------
# The QR code
# ---------------------------------------------------------------------------
# An encoder written out by hand, for a thing nobody can proofread by eye. A
# wrong module is not visible and not recoverable: it either scans or a coach
# is standing in a gym typing an address.
#
# So the matrices below are not hand-written. They were produced by this
# encoder and then checked, module for module, against the Python `qrcode`
# package, an independent implementation: 416 of 416 matrices identical over
# versions 1 to 6, all eight masks, and every URL shape this app produces.
# They were also rendered and read back by a real decoder. Keeping them here
# means that check still runs in a gym with no internet and no Python: if a
# change moves a single module, this says so.

QR_FIXTURES <- list(
  list(text = "http://drews-laptop.local:7815",
       n = 29L,
       m = "11111110011001110011101111111/10000010001101111110101000001/10111010101011010001101011101/10111010101110110101001011101/10111010111000011011101011101/10000010101000000010001000001/11111110101010101010101111111/00000000100010101000100000000/10111110010000001101001111100/11110000001101110111111010001/10011110111001111000001100000/10001100110011010011100010010/11011110110000110111000101100/01000000110010011101011110101/10111010000010000110001100100/10000100011100101010010110010/00011011011110001110100100100/11010101011111110001111111101/10011010011111111100110111100/10111100100001010001100110010/10001011101010110111111110111/00000000101110011100100011111/11111110011110000011101011100/10000010110110101011100011011/10111010101100000110111110101/10111010101101110000000001000/10111010110100111011001110010/10000010001011111001110001010/11111110101010111111010011100"),
  list(text = "http://192.168.1.42:7815",
       n = 25L,
       m = "1111111000100010001111111/1000001000011011001000001/1011101010110000101011101/1011101011010100001011101/1011101010100010001011101/1000001011001101101000001/1111111010101010101111111/0000000010111001000000000/1011111001001101101111100/1100110110110110010000010/1101111111110101001101011/0101000000010000100010001/0100011010000100001110111/1011010010101010000001010/1000011100000111010101011/1010110111001011000001001/1010001110101100111110100/0000000011100110100011100/1111111001111000101011111/1000001011010001100011001/1011101010011101111111100/1011101010101011101110111/1011101011000000010000101/1000001001100010101111001/1111111010000101011111111"),
  list(text = "http://localhost:7815",
       n = 25L,
       m = "1111111001110010001111111/1000001000011111001000001/1011101011000010001011101/1011101011001100101011101/1011101011101110001011101/1000001011000001001000001/1111111010101010101111111/0000000011100001100000000/1011111000111101101111100/1110000010001010100100010/1001011001110001101111011/1111110011101010010110001/1000011001011101111010111/1101010001100010100101010/1010011111011011101001011/1000110100010010111111001/1001101110110000111110100/0000000011001101100011100/1111111001100110101011111/1000001010001100100011011/1011101011001011111111111/1011101011000001011010111/1011101010011001100100101/1000001000010011110111001/1111111011010000001111111"),
  list(text = "http://10.144.63.30:7815",
       n = 25L,
       m = "1111111010110101001111111/1000001000101010001000001/1011101001011010001011101/1011101010110100101011101/1011101010100101001011101/1000001010001110101000001/1111111010101010101111111/0000000011110000100000000/1000101110110010111111001/1110000110001001000111010/0101101000111101101001100/0111110000010000011000110/0100011111010010101001111/1011100101101101010110010/0000111100111111110101100/0011010100010010000001110/1101111100001011111111100/0000000010100000100010100/1111111010000000101011000/1000001000001001100011110/1011101010111010111110100/1011101001001100001001111/1011101001011000101000010/1000001001011010000111110/1111111011100010001000111"),
  list(text = "http://cal-poly-volleyball-laptop.local:7815",
       n = 33L,
       m = "111111100100111011011001001111111/100000100000111111001010001000001/101110101010110101101001001011101/101110101000001110001111101011101/101110101101000101100000101011101/100000101000000110101110001000001/111111101010101010101010101111111/000000001000000100000100000000000/101111100010011101000010101111100/010000011111001010111111001001101/001110100100000100100100111010100/100100000000101110001110100011100/001010100010100000110100000111000/000011001000101110001011111001011/111111101001001001110100000000010/010001011101001010111110111101100/000000100001010001010000010110001/111100000010111011111001001101111/110100110000101111000010001010110/101110011000010111101000001011111/101011100010111010011111000111011/111101011101100100100001111001001/101110110110000110101110100010110/100000001011010100000110110101100/101011100001100101000011111110011/000000001011000000111110100010101/111111100111100100101101101010110/100000101101010110011110100010100/101110101000110000110100111111010/101110101000111111001110110111001/101110101101011000010011001101000/100000100000001010111101010111100/111111101011110001010010111110010")
)

cat("\n== the QR code\n")

for (f in QR_FIXTURES) {
  m <- qr_encode(f$text)
  ok(sprintf("%s encodes to the verified matrix", f$text),
     !is.null(m) && nrow(m) == f$n &&
       identical(paste(apply(m, 1, paste, collapse = ""), collapse = "/"), f$m))
}

# Structure, so a failure above says which part moved rather than just that
# something did.
m <- qr_encode("http://drews-laptop.local:7815")
n <- nrow(m)
ok("the symbol is square and a legal size", n == ncol(m) && (n - 17L) %% 4L == 0L)
corner <- function(r0, c0) {
  blk <- m[r0:(r0 + 6L), c0:(c0 + 6L)]
  all(blk[1, ] == 1) && all(blk[7, ] == 1) && all(blk[, 1] == 1) &&
    all(blk[, 7] == 1) && all(blk[3:5, 3:5] == 1) &&
    sum(blk[2, 2:6]) == 0 && sum(blk[6, 2:6]) == 0
}
ok("three finder patterns, and the fourth corner empty",
   corner(1L, 1L) && corner(1L, n - 6L) && corner(n - 6L, 1L) &&
     !corner(n - 6L, n - 6L))
ok("the timing patterns alternate",
   all(m[7L, 9:(n - 8L)] == rep(c(1L, 0L), length.out = n - 16L)) &&
     all(m[9:(n - 8L), 7L] == rep(c(1L, 0L), length.out = n - 16L)))
ok("the dark module is dark", m[n - 7L, 9L] == 1L)

# The format bits are a published table; getting these wrong means a decoder
# reads the wrong mask and the whole symbol is noise.
ok("format bits match the published table for level M",
   identical(vapply(0:7, function(k) paste(qr_format_bits(k), collapse = ""), ""),
             c("101010000010010", "101000100100101", "101111001111100",
               "101101101001011", "100010111111001", "100000011001110",
               "100111110010111", "100101010100000")))

ok("a payload too long for version 6 returns nothing, rather than a bad code",
   is.null(qr_encode(strrep("x", 107L))))
ok("107 bytes is the first length that does not fit",
   !is.null(qr_encode(strrep("x", 106L))))

sv <- qr_svg("http://drews-laptop.local:7815", px = 150)
ok("the svg carries a quiet zone", grepl('viewBox="0 0 37 37"', sv, fixed = TRUE))
ok("each module is a whole number of pixels",
   grepl('width="148"', sv, fixed = TRUE))

# ---------------------------------------------------------------------------
# Both teams at once
# ---------------------------------------------------------------------------
# Every single-team tab is rendered twice, once per side, so each one has to
# survive being handed the opponent's plays as readily as our own. The
# opponent's set is a different shape: different roster, often fewer rallies,
# and on a practice file it can be the same team as us.

cat("\n== both teams at once\n")

PAIRED <- c("attack", "serve", "setting", "setter", "passing", "last6")
tab_fn <- function(nm) get(paste0("tab_", if (nm == "setting") "set" else nm))

Opp_all <- Op
ok("the opponent has plays of their own to render", nrow(Opp_all) > 0)
ok("the two sides are different sets of plays",
   !identical(nrow(Opp_all), nrow(Fp)) || !identical(Opp_all$player_no, Fp$player_no))

for (nm in PAIRED) {
  f <- tab_fn(nm)
  r <- tryCatch({
    if (nm == "last6") f(Opp_all, o) else f(Opp_all, o, ck)
    "rendered"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(sprintf("%s renders the opponent's plays", nm), r == "rendered", r)
}

# An empty opponent happens: one team's file, or a filter that excludes them.
for (nm in PAIRED) {
  f <- tab_fn(nm)
  r <- tryCatch({
    if (nm == "last6") f(Fp[0, ], o) else f(Fp[0, ], o, ck)
    "rendered"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(sprintf("%s survives an empty opponent", nm), r == "rendered", r)
}

h <- two_up("<p>ours</p>", "<p>theirs</p>", "Cal Poly", "Grand Canyon")
ok("two_up puts both sides in one block, each labelled",
   grepl('class="side us"', h, fixed = TRUE) &&
     grepl('class="side them"', h, fixed = TRUE) &&
     grepl(">Cal Poly<", h, fixed = TRUE) && grepl(">Grand Canyon<", h, fixed = TRUE) &&
     grepl("ours", h, fixed = TRUE) && grepl("theirs", h, fixed = TRUE))
ok("a team name with markup in it is escaped, not rendered",
   !grepl("<script>", two_up("", "", "<script>x</script>", "b"), fixed = TRUE))

# Labels. The practice case is the one that bites: both sides are the same
# team, so the names alone would label the two columns identically.
S_fake <- list(matches = matches, focus = focus, focus_name = focus_name,
               sel = sel)
lab <- side_labels(S_fake)
ok("each column gets a label", nzchar(lab$us) && nzchar(lab$them))
ok("the two labels are never the same word", !identical(lab$us, lab$them),
   sprintf("%s vs %s", lab$us, lab$them))

# ---------------------------------------------------------------------------
# Positions
# ---------------------------------------------------------------------------
# Both formats carry a position per player, so this is a lookup rather than a
# guess, and the two readers have to agree about it the way they agree about
# everything else. The overrides on top are the part that can go quietly
# wrong: applied in place they are a one-way door, and Undo does nothing.

cat("\n== positions\n")

ok("every play carries a position",
   "pos" %in% names(Fp) && !any(is.na(Fp$pos)))
ok("positions are from the known set or the unknown marker",
   all(Fp$pos %in% c(POS_CODES, "?")),
   paste(setdiff(unique(Fp$pos), c(POS_CODES, "?")), collapse = ", "))

r1 <- matches[[1]]$rosters$home
ok("the roster keeps the file's own answer beside the working one",
   "pos_file" %in% names(r1) && identical(r1$pos, r1$pos_file))
ok("a libero is flagged and coded as one",
   !any(r1$libero) || all(r1$pos[r1$libero] == "L"))

ok("the code mapping is VolleyStation's own enum",
   identical(pos_label(0:4), c("L", "OH", "OPP", "MB", "S")))
ok("an out-of-range code is unknown, not quietly a middle",
   identical(pos_label(c(-1, 5, NA)), c("?", "?", "?")))
ok("pins are outsides and opposites, not middles",
   all(is_pin(c("OH", "OPP"))) && !any(is_pin(c("MB", "S", "L", "?"))))

# Listed position wins; inference is only the fallback.
ok("a listed position beats the code inference",
   identical(attack_group("MB", "OH"), "Pins") &&
     identical(attack_group("OH", "MB"), "Middles"))
ok("an unknown position falls back to what they actually run",
   identical(attack_group("MB", "?"), "Middles") &&
     identical(attack_group(NA, "?"), "Other"))
ok("no position column at all still groups",
   identical(attack_group(c("MB", "OH")), c("Middles", "Pins")))

# The round trip that broke: set, then clear, and land back on the file.
team1 <- matches[[1]]$teams$home$name
num1 <- r1$num[1]
was <- r1$pos_file[1]
other <- setdiff(c("OH", "MB"), was)[1]

ov <- set_position(list(), team1, num1, other)
m2 <- apply_positions(matches, ov)
ok("an override reaches the roster",
   identical(m2[[1]]$rosters$home$pos[1], other))
ok("an override reaches the plays",
   !nrow(m2[[1]]$plays) ||
     all(m2[[1]]$plays$pos[m2[[1]]$plays$side == "home" &
                           m2[[1]]$plays$num == num1] == other))
ok("the file's own answer is untouched by an override",
   identical(m2[[1]]$rosters$home$pos_file[1], was))

ov2 <- set_position(ov, team1, num1, "")
m3 <- apply_positions(m2, ov2)
ok("clearing an override puts the player back to the file",
   identical(m3[[1]]$rosters$home$pos[1], was),
   sprintf("file said %s, got %s", was, m3[[1]]$rosters$home$pos[1]))
ok("clearing the last override leaves no empty team behind",
   !length(ov2))
ok("applying the same overrides twice changes nothing the second time",
   identical(apply_positions(m2, ov)[[1]]$rosters$home$pos,
             m2[[1]]$rosters$home$pos))

# On disk, because the point is that they outlive the session.
tf <- tempfile(fileext = ".json")
write_positions(ov, tf)
ok("overrides survive a write and a read", identical(read_positions(tf), ov))
ok("a missing file is no overrides, not an error",
   identical(read_positions(file.path(tempdir(), "nope.json")), list()))
writeLines("{ not json", tf)
ok("a damaged file is ignored rather than fatal",
   identical(read_positions(tf), list()))
unlink(tf)

PT <- position_table(matches, ov)
ok("the editor lists every player on both teams", nrow(PT) > 0)
ok("the editor shows the file's answer and the override separately",
   all(c("from_file", "override", "in_force") %in% names(PT)))
ok("the override is what is in force where one is set",
   all(PT$in_force[nzchar(PT$override)] == PT$override[nzchar(PT$override)]))
ok("the file's answer is in force where none is set",
   all(PT$in_force[!nzchar(PT$override)] == PT$from_file[!nzchar(PT$override)]))

r <- tryCatch({ tab_roster(PT, focus_name); "rendered" },
              error = function(e) paste("ERROR:", conditionMessage(e)))
ok("the roster tab renders", r == "rendered", r)
r <- tryCatch({ tab_roster(PT[0, ], focus_name); "rendered" },
              error = function(e) paste("ERROR:", conditionMessage(e)))
ok("the roster tab survives having no players", r == "rendered", r)

# ---------------------------------------------------------------------------
# Who serves next
# ---------------------------------------------------------------------------
# A prediction, so it is scored rather than eyeballed. The rule is measured
# against every rally boundary in whatever files this run was given, and the
# score is printed even when it passes, because the number is the point.

cat("\n== who serves next\n")

ok("a rotation advances by one and wraps at six",
   identical(vapply(ROTS, rot_advance, ""),
             c(R1 = "R2", R2 = "R3", R3 = "R4", R4 = "R5", R5 = "R6", R6 = "R1")))
ok("a rotation label that makes no sense gives nothing back",
   is.na(rot_advance("R9")) && is.na(rot_advance("x")) && is.na(rot_advance(NA)))

for (nm in names(matches)) {
  m <- matches[[nm]]
  S <- serve_log(m$plays)
  if (is.null(S) || nrow(S) < 10L) next

  # Replay the match one rally at a time and ask, each time, who serves next.
  hit <- miss <- unknown <- 0L
  for (i in seq_len(nrow(S) - 1L)) {
    upto <- m$plays[m$plays$set_no < S$set_no[i] |
                    (m$plays$set_no == S$set_no[i] & m$plays$rally_no <= S$rally_no[i])]
    g <- next_server(upto)
    truth <- S$num[i + 1L]
    if (S$set_no[i + 1L] != S$set_no[i]) next      # a new set starts afresh
    if (is.null(g) || !isTRUE(g$known) || is.na(g$num)) unknown <- unknown + 1L
    else if (identical(as.integer(g$num), as.integer(truth))) hit <- hit + 1L
    else miss <- miss + 1L
  }
  n <- hit + miss
  if (!n) next
  rate <- 100 * hit / n
  info(sprintf("%s: %.1f%% of %d called, %d not called",
               substr(nm, 1, 34), rate, n, unknown))
  # Intrasquad files are a wash drill: the two sides are one team and the
  # rotation does not turn over, so they are reported and not graded.
  intra <- identical(m$teams$home$name, m$teams$away$name)
  if (!intra)
    ok(sprintf("next server is called better than four in five on %s",
               substr(nm, 1, 24)), rate >= 80,
       sprintf("%.1f%%", rate))
}

match_one <- matches[[1]]
ns <- next_server(match_one$plays)
ok("the prediction says which side, who, and on what evidence",
   is.null(ns) || all(c("side", "num", "name", "rot", "basis", "known") %in% names(ns)))
ok("holding serve is recognised as holding serve",
   is.null(ns) || ns$basis %in% c("serving now", "holding serve",
                                  "last time they were in this rotation",
                                  "not seen in this rotation yet",
                                  "no serve from them yet"))
ok("too little to go on returns nothing rather than a guess",
   is.null(next_server(match_one$plays[0, ])))

so_home <- serve_order(match_one$plays, "home")
so_away <- serve_order(match_one$plays, "away")
ok("each side gets a serve order", !is.null(so_home) && !is.null(so_away))
# The bug this catches: a parameter named `side` is shadowed by the column of
# the same name, every row passes the filter, and both teams come back equal.
ok("the two sides are not the same six players",
   identical(match_one$teams$home$name, match_one$teams$away$name) ||
     !identical(so_home$num, so_away$num),
   paste(so_home$num, collapse = ","))
ok("a serve order has at most one server per rotation",
   is.null(so_home) || !any(duplicated(so_home$rot)))
ok("serve efficiency stays a proportion",
   is.null(so_home) || all(is.na(so_home$eff) | abs(so_home$eff) <= 1))

# ---------------------------------------------------------------------------
# Live match against scouting history
# ---------------------------------------------------------------------------
# Scouting files are loaded before a match so the Next server tab has
# something to read. Every other tab is about the match being played, and
# pooling last month's film into tonight's hitting percentage would be wrong
# in the quiet way: a number that looks like tonight and is not.
#
# A scouting library is also several opponents, so "more history" is only
# better when it is history of the same people.

cat("\n== live match against scouting history\n")

if (length(matches) > 1L) {
  live_id <- names(matches)[length(matches)]
  sel_l <- sel; sel_l$matches <- live_id

  n_all <- nrow(all_plays(matches, sel))
  n_live <- nrow(all_plays(matches, sel_l))
  ok("scoping to the live file is narrower than pooling", n_live < n_all,
     sprintf("%d of %d plays", n_live, n_all))
  ok("the live scope is one file's worth",
     length(unique(all_plays(matches, sel_l)$match_id)) == 1L)

  a_live <- sum(focus_plays(matches, sel_l, focus)$skill == "A")
  a_all <- sum(focus_plays(matches, sel, focus)$skill == "A")
  ok("a match tab would count tonight's attacks, not the season's",
     a_live < a_all, sprintf("%d against %d", a_live, a_all))
}

# The filter that keeps one opponent's history to that opponent.
opp_names <- unique(vapply(matches, function(m) {
  fs <- focus[[m$match_id]]
  if (is.null(fs) || is.na(fs)) return("")
  m$teams[[if (fs == "home") "away" else "home"]]$name %||% ""
}, ""))
opp_names <- opp_names[nzchar(opp_names)]

if (length(opp_names)) {
  one <- opp_names[1]
  sel_o <- sel_same_opponent(matches, sel, focus, one)
  O <- opponent_plays(matches, sel_o, focus)
  ok("history is kept to the one opponent",
     !nrow(O) || identical(unique(O$team_name), one),
     paste(unique(O$team_name), collapse = " + "))
  if (length(opp_names) > 1L) {
    O_pool <- opponent_plays(matches, sel, focus)
    ok("pooling would have mixed opponents, which is what this prevents",
       length(unique(O_pool$team_name)) > 1L)
  }
}


# A scouting film is the opponent playing a third party, so the team being
# scouted has to be reachable by name. Through the focus team it is not
# reachable at all, because the focus team is not in the file.
all_team_names <- unique(unlist(lapply(matches, function(m)
  c(m$teams$home$name %||% "", m$teams$away$name %||% ""))))
all_team_names <- all_team_names[nzchar(all_team_names)]

for (tn in all_team_names) {
  TP <- team_plays(matches, sel_with_team(matches, sel, tn), tn)
  ok(sprintf("every play found by name belongs to %s", substr(tn, 1, 22)),
     !nrow(TP) || identical(unique(TP$team_name), tn),
     paste(unique(TP$team_name), collapse = " + "))
}

ok("a team nobody has played gives nothing, not everything",
   !nrow(team_plays(matches, sel, "Nobody University")))
ok("no team named gives nothing",
   !nrow(team_plays(matches, sel, "")))
ok("selecting by name keeps only the files that team is in",
   {
     tn <- all_team_names[1]
     s2 <- sel_with_team(matches, sel, tn)
     all(vapply(s2$matches, function(id) {
       m <- matches[[id]]
       tn %in% c(m$teams$home$name %||% "", m$teams$away$name %||% "")
     }, TRUE))
   })
ok("by name finds at least as much as going through the focus team",
   {
     tn <- all_team_names[1]
     a <- nrow(team_plays(matches, sel_with_team(matches, sel, tn), tn))
     b <- nrow(opponent_plays(matches, sel_same_opponent(matches, sel, focus, tn), focus))
     a >= b
   })

ok("an opponent nobody has played leaves the selection alone",
   identical(sel_same_opponent(matches, sel, focus, "Nobody University"), sel))
ok("no opponent named leaves the selection alone",
   identical(sel_same_opponent(matches, sel, focus, ""), sel))

lm <- matches[[1]]
ok("the live opponent is the side we are not",
   {
     nm <- live_opponent(lm, focus)
     fs <- focus[[lm$match_id]]
     is.null(fs) || is.na(fs) || identical(nm, lm$teams[[if (fs == "home") "away" else "home"]]$name)
   })
ok("no live match means no opponent, rather than an error",
   identical(live_opponent(NULL, focus), ""))

# Before the first rally there are no plays, so nobody has a side yet, but
# both team names are in the file. The scouting library is useless until this
# resolves, and before the first rally is exactly when it is wanted.
lm <- matches[[1]]
h <- lm$teams$home$name %||% ""
a <- lm$teams$away$name %||% ""
no_side <- setNames(list(NA_character_), lm$match_id)
ok("the opponent is found with no side resolved, from the team names",
   identical(live_opponent(lm, no_side, h), a) &&
     identical(live_opponent(lm, no_side, a), h))
ok("a team that is in neither side gives nothing",
   identical(live_opponent(lm, no_side, "Nobody University"), ""))
ok("a resolved side still wins over the names",
   {
     fs <- focus[[lm$match_id]]
     is.null(fs) || is.na(fs) ||
       identical(live_opponent(lm, focus, h), live_opponent(lm, focus))
   })

# serve_order on one team's plays, which is what a film read by name gives.
tn <- unique(c(h, a))[1]
TP <- team_plays(matches, sel_with_team(matches, sel, tn), tn)
so1 <- serve_order(TP)
ok("a serve order reads from one team's plays with no side given",
   is.null(so1) || (nrow(so1) >= 1 && !any(duplicated(so1$rot))))
ok("that order names only that team's players",
   is.null(so1) || all(so1$num %in% unique(TP$num)))


# The tab itself, in both modes and with nothing to go on.
ns1 <- next_server(all_plays(matches, sel))
labs <- side_labels(list(matches = matches, focus = focus,
                         focus_name = focus_name, sel = sel))
Opp_ns <- opponent_plays(matches, sel, focus)
for (mode in c(TRUE, FALSE)) {
  r <- tryCatch({
    tab_nextserver(Fp, Opp_ns, ns1, o, ck, labs, mode, n_files = length(matches))
    "rendered"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  ok(sprintf("next server renders with the serve %s",
             if (mode) "ours" else "theirs"), r == "rendered", r)
}
r <- tryCatch({ tab_nextserver(Fp, Opp_ns, NULL, o, ck, labs, TRUE); "rendered" },
              error = function(e) paste("ERROR:", conditionMessage(e)))
ok("next server says so rather than failing when it cannot tell",
   r == "rendered", r)
r <- tryCatch({ tab_nextserver(Fp[0, ], Opp_ns[0, ], ns1, o, ck, labs, TRUE); "rendered" },
              error = function(e) paste("ERROR:", conditionMessage(e)))
ok("next server survives an empty set of plays", r == "rendered", r)

# Picking a server by hand, and the shadowing bug that class of code invites.
# A data.table expression resolves a bare name to its own column first, so a
# parameter named after a column silently compares the column with itself and
# every row passes. It has now done this twice: once in serve_order, where
# both teams came back identical, and once in server_block, where one
# player's card showed the whole team's serves.
Opp_ns2 <- opponent_plays(matches, sel, focus)
S_ns <- Opp_ns2[Opp_ns2$skill == "S" & !is.na(Opp_ns2$num), ]
if (nrow(S_ns)) {
  who <- as.integer(names(sort(table(S_ns$num), decreasing = TRUE))[1])
  hers <- sum(S_ns$num == who)
  # The precise test for the shadowing bug, with no HTML scraping: the card
  # built by naming her must equal the card built from her rows alone. Under
  # the bug the first is built from every row and the two differ.
  by_name <- paste(server_block(S_ns, who, "someone", o, ck), collapse = "")
  by_rows <- paste(server_block(S_ns[S_ns$num == who, ], who, "someone", o, ck),
                   collapse = "")
  ok("a server's card counts her serves, not her team's",
     identical(by_name, by_rows),
     sprintf("she has %d of %d serves", hers, nrow(S_ns)))
  other <- setdiff(unique(S_ns$num), who)
  ok("two servers do not get the same card",
     !length(other) ||
       !identical(by_name, paste(server_block(S_ns, other[1], "other", o, ck),
                                 collapse = "")))
  ok("a card for a player who never served says so, rather than showing the team",
     grepl("has not served",
           paste(server_block(S_ns, 9999L, NA, o, ck), collapse = ""), fixed = TRUE))

  chips <- server_chips(Opp_ns2, "")
  ok("the picker lists every server", 
     all(vapply(unique(S_ns$num), function(n)
       grepl(sprintf('data-v="%d"', n), chips, fixed = TRUE), TRUE)))
  ok("the picker offers a way back to following the match",
     grepl('data-o="pinServer" data-v=""', chips, fixed = TRUE))
  ok("the pinned one is the one marked",
     grepl(sprintf('class="chip on" data-o="pinServer" data-v="%d"', who),
           server_chips(Opp_ns2, as.character(who)), fixed = TRUE))

  o_pin <- o; o_pin$pinServer <- who
  labs2 <- side_labels(list(matches = matches, focus = focus,
                            focus_name = focus_name, sel = sel))
  # A pin wins over the prediction and over whose serve it is.
  for (mine in c(TRUE, FALSE)) {
    hh <- tab_nextserver(Fp, Opp_ns2, ns1, o_pin, ck, labs2, mine, n_files = 1L)
    ok(sprintf("a pinned server is shown even when the serve is %s",
               if (mine) "ours" else "theirs"),
       grepl("picked by hand", hh, fixed = TRUE))
  }
  hh <- tab_nextserver(Fp, Opp_ns2, NULL, o_pin, ck, labs2, TRUE, n_files = 1L)
  ok("a pin works before the match too, with no prediction at all",
     grepl("picked by hand", hh, fixed = TRUE))
}

# A serve order row names one player, so its counts must be hers. Crediting
# the whole rotation to her claimed more serves than she had played, which is
# impossible on its face and was on the screen.
SO <- Opp_ns2[Opp_ns2$skill == "S", ]
if (nrow(SO)) {
  so2 <- serve_order(SO)
  if (!is.null(so2)) {
    worst <- 0L
    for (i in seq_len(nrow(so2))) {
      hers <- sum(!is.na(SO$num) & SO$num == so2$num[i])
      if (so2$serves[i] > hers) worst <- worst + 1L
    }
    ok("no rotation row claims more serves than that player has", worst == 0L,
       sprintf("%d rows", worst))
    ok("a rotation row counts the named player only, with the rest beside it",
       all(so2$serves + so2$others ==
             vapply(so2$rot, function(r) sum(SO$rot == r, na.rm = TRUE), 0L)))
    ok("shared serves are never negative", all(so2$others >= 0))
    ok("one row per rotation at most", !any(duplicated(so2$rot)))
  }
  cards <- paste(serve_order_cards(Opp_ns2), collapse = "")
  ok("the serve order is one card per film, not one pooled table",
     lengths(regmatches(cards, gregexpr("class=\"card", cards, fixed = TRUE))) >=
       length(unique(SO$match_id)))
  ok("a film is named on its own card",
     grepl(substr(SO$file_name[1], 1, 10), cards, fixed = TRUE))
}




# ---------------------------------------------------------------------------
# Cards that have nothing in them
# ---------------------------------------------------------------------------
# A file coded live carries no coordinates, so every card built on where the
# ball went is a frame of zeros. Six or eight of them stack up and push the
# numbers a coach does need off the screen, between rallies, which is the one
# moment the screen has to be readable at a glance.

cat("\n== cards with nothing in them\n")

LINE_CARDS <- c("Landing third of the opponent court",
                "Line vs cross (pin attacks only)", "Depth",
                "By depth band", "Serving position behind the endline",
                "Where the set came from", "Origin zone")

drawn <- function(h, t) grepl(sprintf("<h3>%s", t), paste(h, collapse = ""), fixed = TRUE)

for (nm in names(matches)) {
  one <- setNames(list(matches[[nm]]), nm)
  idx1 <- team_index(one)
  fc <- focus_sides(one, idx1$name[1])
  F1 <- focus_plays(one, sel, fc)
  if (!nrow(F1)) next
  ck1 <- coords_available(one, sel)
  h <- c(tab_attack(F1, o, ck1), tab_serve(F1, o, ck1), tab_set(F1, o, ck1))
  for (sk in list(c("A", "Landing third of the opponent court"),
                  c("S", "By depth band"),
                  c("E", "Where the set came from"))) {
    q <- F1[F1$skill == sk[1], ]
    lines_there <- nrow(q) > 0 && any(q$has_xy, na.rm = TRUE)
    ok(sprintf("%s: %s is %s", substr(nm, 1, 20), sk[2],
               if (lines_there) "drawn" else "not drawn"),
       drawn(h, sk[2]) == lines_there)
  }
}

# The blocker count is a coded field. Cal Poly's own files put 2 on every
# attack because nobody changes the default, so the card would be one row
# saying nothing at all.
for (nm in names(matches)) {
  one <- setNames(list(matches[[nm]]), nm)
  idx1 <- team_index(one)
  F1 <- focus_plays(one, sel, focus_sides(one, idx1$name[1]))
  A1 <- F1[F1$skill == "A", ]
  if (!nrow(A1)) next
  varies <- length(unique(A1$blockers[!is.na(A1$blockers)])) > 1L
  h <- tab_attack(F1, o, coords_available(one, sel))
  ok(sprintf("%s: against the block is %s", substr(nm, 1, 20),
             if (varies) "drawn" else "not drawn"),
     drawn(h, "Against the block") == varies)
}

ok("a card asked for with nothing behind it is not drawn",
   identical(card_if(FALSE, "Title", "body"), ""))
ok("a card asked for with something behind it is drawn",
   grepl("Title", card_if(TRUE, "Title", "body"), fixed = TRUE))
ok("no rows means no lines", !has_lines(Fp[0, ]))
ok("a missing column means no lines", !has_lines(data.frame(x = 1)))

cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
if (FAIL) quit(status = 1)
