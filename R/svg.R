# ---------------------------------------------------------------------------
# Court plots, as inline SVG.
#
# Drawn as SVG rather than with ggplot2 on purpose: the trajectory charts carry
# a <title> per shot (hover a line, read the hitter, set, score, call and
# outcome), they stay vector-crisp when a coach prints a sheet, and the whole
# thing is text so it costs nothing to re-render on every filter change.
#
# World coordinates are the file's own: x 0-900, y 0-1800, net at 900. SY()
# flips y for the screen, since y grows away from the viewer's baseline while
# SVG y grows downward.
# ---------------------------------------------------------------------------

SY <- function(y, flip = FALSE) if (flip) y else (COURT$L - y)
SX <- function(x, flip = FALSE) if (flip) (COURT$W - x) else x
SYtop <- function(a, b, flip = FALSE) pmin(SY(a, flip), SY(b, flip))

out_color <- function(st, mono = FALSE) if (mono) MONO[[st]]$col else unname(OUTCOME[st])

#' The court frame, with whatever layers were handed in drawn on top of it.
#'
#' Two independent containment mechanisms, because relying on either alone has
#' failed in testing: an explicit CSS aspect-ratio (browsers have been observed
#' to infer the wrong height from a viewBox under print rendering) and an SVG
#' clipPath matching the viewBox exactly. The clipPath is the authoritative one
#' -- it clips in the SVG's own coordinate system whatever size the browser
#' renders the outer box at.
court_svg <- function(id, x_min, x_max, y_min, y_max,
                      cells = "", marks = "", labels = "",
                      halves = "both", scale = 1, alt = "court plot",
                      mono = FALSE, coords_ok = TRUE, fill = FALSE) {
  if (!coords_ok) return("")
  vb <- sprintf("%s %s %s %s", x_min, SYtop(y_min, y_max), x_max - x_min, y_max - y_min)
  s <- scale
  ar <- sprintf("%.4f", (x_max - x_min) / (y_max - y_min))
  clip_id <- paste0("clip", id)
  sizing <- if (fill) ' preserveAspectRatio="xMidYMid slice"'
            else sprintf(' style="aspect-ratio:%s"', ar)

  heads <- paste0(vapply(names(OUTCOME), function(k) {
    col <- out_color(k, mono)
    shape <- if (mono) MONO[[k]]$head else "arrow"
    body <- if (shape == "cross")
      sprintf('<path d="M1,1 L7,7 M7,1 L1,7" stroke="%s" stroke-width="1.9" fill="none"/>', col)
    else if (shape == "bar")
      sprintf('<path d="M5.4,0.4 L7.2,0.4 L7.2,7.6 L5.4,7.6 z" fill="%s"/>', col)
    else
      sprintf('<path d="M0,0.6 L7.5,4 L0,7.4 z" fill="%s"/>', col)
    ref <- if (shape == "cross") 4 else 6.5
    sprintf('<marker id="ah-%s%s" viewBox="0 0 8 8" refX="%s" refY="4" markerWidth="5" markerHeight="5" orient="auto">%s</marker>',
            k, id, ref, body)
  }, ""), collapse = "")

  L <- c(
    sprintf('<svg class="court" viewBox="%s"%s role="img" aria-label="%s">', vb, sizing, esc(alt)),
    sprintf('<defs><clipPath id="%s"><rect x="%s" y="%s" width="%s" height="%s"/></clipPath>',
            clip_id, x_min, SYtop(y_min, y_max), x_max - x_min, y_max - y_min),
    sprintf('<marker id="ah-tend%s" viewBox="0 0 8 8" refX="5.6" refY="4" markerWidth="3.4" markerHeight="3.4" orient="auto"><path d="M0,0.8 L7.2,4 L0,7.2 z" fill="%s"/></marker>',
            id, if (mono) "#000" else "var(--accent)"),
    heads, "</defs>",
    sprintf('<g clip-path="url(#%s)">', clip_id)
  )

  # playable surface(s)
  halves_l <- list()
  if (y_min < COURT$NET && halves != "opp")
    halves_l[[length(halves_l) + 1L]] <- c(max(y_min, 0), min(y_max, COURT$NET))
  if (y_max > COURT$NET && halves != "own")
    halves_l[[length(halves_l) + 1L]] <- c(max(y_min, COURT$NET), min(y_max, COURT$L))
  for (h in halves_l)
    L <- c(L, sprintf('<rect x="0" y="%s" width="%s" height="%s" fill="var(--wood)"/>',
                      SYtop(h[1], h[2]), COURT$W, h[2] - h[1]))

  if (nzchar(cells)) L <- c(L, cells)

  L <- c(L, sprintf('<g fill="none" stroke="var(--wood-line)" stroke-width="%s">', 3 * s))
  for (h in halves_l)
    L <- c(L, sprintf('<rect x="0" y="%s" width="%s" height="%s"/>',
                      SYtop(h[1], h[2]), COURT$W, h[2] - h[1]))
  L <- c(L, "</g>")

  L <- c(L, sprintf('<g fill="none" stroke="var(--wood-line-2)" stroke-width="%s" stroke-dasharray="%s %s">',
                    2 * s, 14 * s, 10 * s))
  for (y in c(COURT$NET - COURT$THREE, COURT$NET + COURT$THREE))
    if (y > y_min && y < y_max)
      L <- c(L, sprintf('<line x1="0" y1="%s" x2="%s" y2="%s"/>', SY(y), COURT$W, SY(y)))
  L <- c(L, "</g>")

  if (y_min < COURT$NET + 1 && y_max > COURT$NET - 1)
    L <- c(L, sprintf('<line x1="-40" y1="%s" x2="%s" y2="%s" stroke="var(--ink)" stroke-width="%s"/>',
                      SY(COURT$NET), COURT$W + 40, SY(COURT$NET), 5 * s))

  if (nzchar(marks))  L <- c(L, marks)
  if (nzchar(labels)) L <- c(L, labels)
  paste0(paste(c(L, "</g>", "</svg>"), collapse = ""))
}

#' n x n cell layer over a half, shaded by volume or by hitting efficiency.
cell_layer <- function(cells, cols, rows_n, rect, mode) {
  ns <- vapply(cells, function(c0) nrow(c0$rows), numeric(1))
  mx <- max(1, ns)
  cw <- (rect$x1 - rect$x0) / cols; ch <- (rect$y1 - rect$y0) / rows_n
  out <- character()
  for (c0 in cells) {
    n <- nrow(c0$rows)
    x <- rect$x0 + c0$c * cw; y_top <- rect$y1 - c0$r * ch
    fill <- "none"; op <- 0
    st <- NULL
    if (n) {
      st <- attack_line(c0$rows)
      if (identical(mode, "volume")) {
        fill <- "var(--accent)"; op <- 0.07 + 0.46 * (n / mx)
      } else {
        e <- st$eff
        fill <- if (!is.na(e) && e >= 0) "var(--kill)" else "var(--err)"
        op <- 0.09 + 0.45 * min(1, abs(e %||% 0) / 0.5)
      }
    }
    out <- c(out, sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" fill-opacity="%.3f" stroke="var(--rule)" stroke-width="1.5"/>',
                          x, SY(y_top), cw, ch, fill, op))
    if (n) {
      lbl <- if (identical(mode, "volume")) as.character(n) else eff(st$eff)
      sub <- if (identical(mode, "volume")) paste(pct(st$kpct), "k") else paste(n, "att")
      out <- c(out,
        sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="30" fill="var(--ink)" opacity=".82">%s</text>',
                x + cw / 2, SY(y_top - ch / 2) + 9, lbl),
        sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="19" fill="var(--ink-2)" opacity=".7">%s</text>',
                x + cw / 2, SY(y_top - ch / 2) + 36, sub))
    }
  }
  paste(out, collapse = "")
}

zone_number_layer <- function(half, rect) {
  cw <- COURT$W / 3; ch <- (rect$y1 - rect$y0) / 3
  out <- character()
  for (r in 0:2) for (c in 0:2) {
    cx <- c * cw + cw / 2; cy <- rect$y1 - r * ch - ch / 2
    z <- zone_of(cx, cy, half)
    out <- c(out, sprintf('<text x="%s" y="%s" font-family="ui-monospace,monospace" font-size="22" fill="var(--wood-line)">%s</text>',
                          c * cw + 14, SY(rect$y1 - r * ch - 14) + 20, z))
  }
  paste(out, collapse = "")
}

#' Trajectory arrows, contact -> landing, one per shot.
arrow_layer <- function(rows, id, scale = 1, mono = FALSE, extra = NULL) {
  rows <- rows[rows$has_xy, ]
  if (!nrow(rows)) return("")
  s <- scale
  st <- land_status(rows$effect)
  col <- vapply(st, out_color, "", mono = mono)
  w <- (if (mono) vapply(st, function(k) MONO[[k]]$w, 0) else ifelse(st == "kill", 3.2, 2.2)) * s
  # in mono, opacity would reintroduce the greyscale ambiguity the dash and
  # endpoint shape exist to avoid, so lines stay fully opaque
  op <- if (mono) rep(1, nrow(rows)) else ifelse(st == "kill", .85, .6)
  dash <- if (mono) vapply(st, function(k) {
    d <- MONO[[k]]$dash
    if (!nzchar(d)) "" else sprintf(' stroke-dasharray="%s"',
      paste(sprintf("%.1f", as.numeric(strsplit(d, " ")[[1]]) * s), collapse = " "))
  }, "") else rep("", nrow(rows))

  callf <- ifelse(!is.na(rows$call),
                  paste0(rows$call, ifelse(is.na(rows$call_name), "", paste0(" ", rows$call_name))),
                  ifelse(rows$skill == "S", "serve", ""))
  tip <- sprintf("%s #%s · set %s · %s · %s · %s",
                 rows$player_short, rows$num, rows$set_no, rows$score, callf, st)
  if (!is.null(extra)) tip <- paste0(tip, " · ", extra(rows))

  paste0(sprintf('<g><title>%s</title><line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"%s opacity="%s" marker-end="url(#ah-%s%s)"/><circle cx="%s" cy="%s" r="%s" fill="%s" opacity="%s"/></g>',
                 esc(tip), SX(rows$x0), SY(rows$y0), SX(rows$x1), SY(rows$y1),
                 col, w, dash, op, st, id,
                 SX(rows$x0), SY(rows$y0), (if (mono) 4 else 5) * s, col,
                 if (mono) 1 else .9),
         collapse = "")
}

#' Landing (or contact) dots only.
dot_layer <- function(rows, xcol, ycol, scale = 1, r = 7) {
  rows <- rows[rows$has_xy, ]
  if (!nrow(rows)) return("")
  st <- land_status(rows$effect)
  col <- unname(OUTCOME[st])
  tip <- sprintf("%s #%s · set %s · %s%s%s",
                 rows$player_short, rows$num, rows$set_no, rows$score,
                 ifelse(is.na(rows$target), "",
                        paste0(" · to ", ifelse(is.na(TARGET[rows$target]),
                                                     rows$target, TARGET[rows$target]))),
                 ifelse(is.na(rows$atk_effect), "", paste0(" · attack ", rows$atk_effect)))
  paste0(sprintf('<g><title>%s</title><circle cx="%s" cy="%s" r="%s" fill="%s" fill-opacity=".55" stroke="%s" stroke-width="%s"/></g>',
                 esc(tip), SX(rows[[xcol]]), SY(rows[[ycol]]), r * scale, col, col, 1.6 * scale),
         collapse = "")
}

#' Bold summary lines showing where shots actually concentrate.
#'
#' Deliberately NOT a mean line: averaging a hitter who splits line and cross
#' produces a straight-down-the-middle arrow they never hit -- one front quick
#' in this data sends 17 of 32 right while the mean points at the middle third
#' where only 22% land. So instead: bucket the landings, take the densest
#' buckets, and draw the median shot within each. That is a shot they genuinely
#' hit, and its share is honest.
tendency_layer <- function(rows, id, scale = 1, top_n = 2, kind = "attack",
                           no_label = FALSE, mono = FALSE) {
  is_srv <- identical(kind, "serve")
  # serves: drop net errors (never crossed)
  xy <- rows[rows$has_xy & (!is_srv | rows$y1 >= COURT$NET), ]
  if (nrow(xy) < 6) return("")              # too few to claim a pattern
  deep_cut <- if (is_srv) (COURT$NET + COURT$L) / 2 else 1500
  s <- scale

  # Bucket by lane first, then a coarse near/deep split. A full 3x3 zone grid
  # splinters a clear "she goes right" tendency across two or three cells and
  # reports 20% where the real pattern is 40%. Serves bucket by lane only: a
  # short/deep split cuts a server's main lane in two whenever her serves land
  # near the midline.
  lane <- lat_third(xy$x1)
  deep <- if (is_srv) rep("all", nrow(xy)) else ifelse(xy$y1 >= deep_cut, "deep", "near")
  key <- paste(lane, deep, sep = "|")
  sp <- split(seq_len(nrow(xy)), key)
  sp <- sp[order(-vapply(sp, length, 0))]
  sp <- sp[vapply(sp, function(g) length(g) >= 3 && length(g) / nrow(xy) >= 0.15, logical(1))]
  if (!length(sp)) return("")
  sp <- sp[seq_len(min(top_n, length(sp)))]

  out <- character()
  for (i in seq_along(sp)) {
    g <- xy[sp[[i]], ]
    x0 <- med(g$x0); y0 <- med(g$y0); x1 <- med(g$x1); y1 <- med(g$y1)
    share <- nrow(g) / nrow(xy)
    st <- if (is_srv) serve_line(g) else attack_line(g)
    w <- (if (i == 1) 11 else 8) * s
    tip <- if (is_srv)
      sprintf("%s of serves land here · %s of %s · %s ace · %s eff",
              pct(share), nrow(g), nrow(xy), pct(st$acepct), eff(st$eff))
    else
      sprintf("%s of attempts land here · %s of %s · %s kill · %s",
              pct(share), nrow(g), nrow(xy), pct(st$kpct), eff(st$eff))
    lw <- 120 * s; lh <- 52 * s
    dx0 <- SX(x0); dx1 <- SX(x1)
    lx <- clamp(dx1, 60, COURT$W - 60); ly <- SY(y1) - 30 * s
    acc <- if (mono) "#000" else "var(--accent)"
    g0 <- paste0(
      sprintf('<g><title>%s</title>', esc(tip)),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#ffffff" stroke-width="%s" stroke-linecap="round" opacity=".85"/>',
              dx0, SY(y0), dx1, SY(y1), w + 5 * s),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s" stroke-linecap="round" opacity="%s" marker-end="url(#ah-tend%s)"/>',
              dx0, SY(y0), dx1, SY(y1), acc, w,
              if (mono) 1 else (if (i == 1) .95 else .6), id),
      sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>', dx0, SY(y0), 7 * s, acc))
    if (no_label) {
      out <- c(out, paste0(g0, "</g>"))
    } else {
      out <- c(out, paste0(g0,
        sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="%s" fill="%s" opacity="%s"/>',
                lx - lw / 2, ly - lh, lw, lh, 8 * s, acc,
                if (mono) 1 else (if (i == 1) .95 else .75)),
        sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="%s" font-weight="700" fill="#ffffff">%s</text>',
                lx, ly - lh / 2 + 13 * s, 34 * s, pct(share)),
        "</g>"))
    }
  }
  paste(out, collapse = "")
}

passer_start_layer <- function(ps, scale = 1, mono = FALSE) {
  if (!length(ps$spots)) return("")
  s <- scale
  col <- if (mono) "#000" else "#7a3fb0"
  out <- character()
  for (i in seq_along(ps$spots)) {
    sp <- ps$spots[[i]]
    xa <- min(SX(sp$x25), SX(sp$x75)); xb <- max(SX(sp$x25), SX(sp$x75))
    ya <- min(SY(sp$y25), SY(sp$y75)); yb <- max(SY(sp$y25), SY(sp$y75))
    r <- (if (i == 1) 15 else 12) * s
    tip <- sprintf("Passer start, %s: %s m off the net · %s of %s in-court serves (%s) · box = middle half of them",
                   LANE_NAME[[sp$k]], m1(sp$y - COURT$NET), sp$n, ps$n, pct(sp$share))
    out <- c(out, paste0(
      sprintf('<g><title>%s</title>', esc(tip)),
      sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" fill-opacity="%s" stroke="%s" stroke-width="%s" stroke-dasharray="%s %s" opacity="%s"/>',
              xa, ya, max(xb - xa, 6), max(yb - ya, 6), col, if (mono) 0 else .08,
              col, 3 * s, 8 * s, 6 * s, if (i == 1) .9 else .6),
      sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s" stroke="%s" stroke-width="%s"/>',
              SX(sp$x), SY(sp$y), r, if (i == 1) col else "#fff", col, 2.5 * s),
      sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="%s" font-weight="700" fill="%s">P</text>',
              SX(sp$x), SY(sp$y) + 6 * s, 17 * s, if (i == 1) "#fff" else col),
      "</g>"))
  }
  paste(out, collapse = "")
}

#' Set contact points, coloured by the attacker the set was meant for.
set_target_layer <- function(rows, r = 9) {
  if (!nrow(rows)) return("")
  TCOL <- c(F = "var(--accent)", C = "var(--kill)", B = "var(--blocked)",
            P = "var(--err)", S = "var(--inplay)")
  col <- unname(TCOL[rows$target]); col[is.na(col)] <- "var(--inplay)"
  who <- ifelse(is.na(TARGET[rows$target]), "?", TARGET[rows$target])
  tip <- sprintf("%s #%s · set %s · %s · to %s%s · %s m off net%s",
                 rows$player_short, rows$num, rows$set_no, rows$score, who,
                 ifelse(is.na(rows$atk_player), "", paste0(" (", rows$atk_player, ")")),
                 vapply(COURT$NET - rows$y0, m1, ""),
                 ifelse(is.na(rows$atk_effect), "", paste0(" · attack ", rows$atk_effect)))
  paste0(sprintf('<g><title>%s</title><circle cx="%s" cy="%s" r="%s" fill="%s" fill-opacity=".5" stroke="%s" stroke-width="2"/></g>',
                 esc(tip), rows$x0, SY(rows$y0), r, col, col),
         collapse = "")
}

#' Set -> the attack it produced.
set_to_attack_layer <- function(rows, id) {
  rows <- rows[!is.na(rows$atk_x) & !is.na(rows$atk_y), ]
  if (!nrow(rows)) return("")
  st <- land_status(rows$atk_effect)
  col <- unname(OUTCOME[st])
  tip <- sprintf("%s → %s · %s · %s", rows$player_short,
                 ifelse(is.na(rows$atk_player), "?", rows$atk_player),
                 ifelse(is.na(rows$atk_call), "", rows$atk_call), st)
  paste0(sprintf('<g><title>%s</title><line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="2.4" opacity=".62" marker-end="url(#ah-%s%s)"/><circle cx="%s" cy="%s" r="5" fill="%s"/></g>',
                 esc(tip), rows$x0, SY(rows$y0), rows$atk_x, SY(rows$atk_y),
                 col, st, id, rows$x0, SY(rows$y0), col),
         collapse = "")
}

#' The rotation wheel: the six rotations in the order a team travels them.
#'
#' Laid out as the cycle rather than as a bar chart on purpose. The question
#' this tab answers is where to start, and the thing that makes a starting
#' rotation good or bad is partly what follows it -- a strong rotation is worth
#' less sitting in front of two weak ones. A ring shows that; a sorted bar
#' chart hides it.
#'
#' `vals` is a named vector of rates keyed R1..R6, `ref` the team rate each is
#' judged against, `labs` an optional second line per rotation.
rotation_wheel <- function(vals, ref = NA_real_, labs = NULL, id = "rw") {
  cx <- 185; cy <- 180; R <- 118; r <- 44
  out <- c(sprintf('<svg class="court" viewBox="0 0 370 360" role="img" aria-label="side-out rate by rotation">'))
  # direction of travel
  out <- c(out, sprintf('<circle cx="%s" cy="%s" r="%s" fill="none" stroke="var(--rule)" stroke-width="2" stroke-dasharray="6 7"/>', cx, cy, R))
  out <- c(out, sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="11" fill="var(--ink-3)">rotation order ↻</text>', cx, cy - 4))
  if (!is.na(ref))
    out <- c(out, sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="ui-monospace,monospace" font-size="13" fill="var(--ink-2)">team %s</text>', cx, cy + 14, pct(ref)))

  for (i in 1:6) {
    k <- paste0("R", i)
    ang <- -pi / 2 + (i - 1) * (2 * pi / 6)        # R1 at the top, clockwise
    x <- cx + R * cos(ang); y <- cy + R * sin(ang)
    v <- vals[[k]] %||% NA_real_
    if (is.null(v)) v <- NA_real_
    good <- !is.na(v) && !is.na(ref) && v > ref
    col <- if (is.na(v)) "var(--rule)" else if (good) "var(--kill)" else "var(--err)"
    op <- if (is.na(v) || is.na(ref)) 0.08 else min(0.42, 0.08 + 2.2 * abs(v - ref))
    out <- c(out,
      sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s" fill-opacity="%.3f" stroke="%s" stroke-width="2.5"/>',
              x, y, r, col, op, col),
      sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-family="ui-monospace,monospace" font-size="15" font-weight="700" fill="var(--ink)">%s</text>',
              x, y - 9, k),
      sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-family="ui-monospace,monospace" font-size="19" fill="var(--ink)">%s</text>',
              x, y + 12, if (is.na(v)) "—" else pct(v)))
    if (!is.null(labs) && !is.na(labs[[k]] %||% NA))
      out <- c(out, sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-family="ui-monospace,monospace" font-size="10.5" fill="var(--ink-3)">%s</text>',
                            x, y + 27, esc(labs[[k]])))
  }
  paste0(paste(c(out, "</svg>"), collapse = ""))
}

# ----------------------------------------------- clickable set-origin zones

# Zones are lane x distance from the net, because those are the two things that
# actually change what a setter can run.
SET_LANES <- list(list(k = "L", l = "Left",   lo = 0,   hi = 300),
                  list(k = "M", l = "Middle", lo = 300, hi = 600),
                  list(k = "R", l = "Right",  lo = 600, hi = 900))
SET_BANDS <- list(list(k = "0", l = "≤1.5 m", lo = 0,   hi = 150),
                  list(k = "1", l = "1.5–3 m", lo = 150, hi = 300),
                  list(k = "2", l = ">3 m",         lo = 300, hi = 1e5))

set_zone_of <- function(rows) {
  out <- rep(NA_character_, nrow(rows))
  ok <- rows$has_xy
  if (!any(ok)) return(out)
  x <- rows$x0[ok]; d <- COURT$NET - rows$y0[ok]
  lane <- ifelse(x < 300, "L", ifelse(x < 600, "M", "R"))
  band <- ifelse(d < 150, "0", ifelse(d < 300, "1", "2"))
  out[ok] <- paste0(lane, band)
  out
}

#' Zone overlay drawn in court coordinates. Each cell is a Shiny input: the
#' click handler lives in www/app.js and posts the zone key back.
set_zone_grid <- function(selected = character()) {
  out <- character()
  for (b in SET_BANDS) for (l in SET_LANES) {
    key <- paste0(l$k, b$k)
    y_hi <- COURT$NET - b$lo
    y_lo <- max(COURT$NET - min(b$hi, 450), 450)
    on <- key %in% selected
    out <- c(out, sprintf('<rect class="setzone%s" data-zone="%s" x="%s" y="%s" width="%s" height="%s" fill="%s" fill-opacity="%s" stroke="var(--accent)" stroke-width="2.5" stroke-dasharray="9 6"><title>%s %s</title></rect>',
                          if (on) " on" else "", key, l$lo, SY(y_hi), l$hi - l$lo,
                          abs(SY(y_lo) - SY(y_hi)),
                          if (on) "var(--accent)" else "#ffffff",
                          if (on) ".16" else "0", l$l, b$l))
  }
  paste(out, collapse = "")
}
