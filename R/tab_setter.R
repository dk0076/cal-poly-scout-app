# ---------------------------------------------------------------------------
# SETTER
#
# Distribution and efficiency only -- no contact points, so this tab works
# identically on a .dvw with no coordinates at all.
#
# Who counts as a setter is the coach's call, so every player who set appears
# and any of them can be switched off. A .vsm records a set for whoever
# touched second, so a libero bump-set and an outside's emergency ball do show
# up as one-set "setters" with a -1.000 efficiency -- that is noise, but it is
# visible noise, and hiding it automatically meant a player you wanted to look
# at could not be selected at all.
# ---------------------------------------------------------------------------

tab_setter <- function(F, o, coords_ok) {
  Eall <- F[F$skill == "E", ]
  if (!nrow(Eall)) return(empty("No sets in the current filter."))

  skey <- paste(Eall$num, Eall$player_short, sep = "|")
  roster <- by_size(crosstab(Eall, skey, setter_eff), "n")

  # Who counts as a setter is the coach's call, not a rule. This used to hide
  # anyone under 5% of the team's sets and every libero, which meant a player
  # you wanted to look at could not be selected at all. Now every player who
  # set appears, all on by default, and any of them can be toggled.
  # the "All" chip posts an empty string rather than an empty vector
  dropped_keys <- o$setterEx %||% character()
  dropped_keys <- dropped_keys[nzchar(dropped_keys)]
  E <- Eall[!(skey %in% dropped_keys), ]
  if (!nrow(E)) return(empty("Every setter is switched off. Click a name below to bring one back."))
  cut <- nrow(Eall) - nrow(E)

  chips <- viewbar(
    '<span class="tiny" style="align-self:center;margin:0 6px 0 0">Setters — click to include or drop</span>',
    sprintf('<button class="chip %s" data-o="setterEx" data-v="">All</button>',
            if (!length(dropped_keys)) "on" else ""),
    paste0(vapply(roster, function(g) {
      parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
      off <- g$key %in% dropped_keys
      sprintf('<button class="chip %s" data-setx="%s" title="%s"%s>%s %s <span class="n">%s</span></button>',
              if (off) "" else "on", esc(g$key),
              if (off) "excluded" else "included",
              if (off) ' style="opacity:.5;text-decoration:line-through"' else "",
              esc(parts[2]), thin(paste0("#", parts[1])), g$stat$n)
    }, ""), collapse = ""))

  t0 <- setter_eff(E)
  TKEYS <- list(c("F", "Out"), c("C", "Mid"), c("B", "Opp"), c("P", "Pipe"))
  share <- function(rs, k)
    if (nrow(rs)) pct(sum(!is.na(rs$target) & rs$target == k) / nrow(rs)) else "—"

  H <- hdr("Setter", sprintf(
    "%s sets · %s linked to the attack they produced%s · distribution and efficiency only, no contact points",
    nrow(E), t0$linked,
    if (cut) sprintf(" · %s set%s by %s player%s switched off",
                     cut, if (cut == 1) "" else "s", length(dropped_keys),
                     if (length(dropped_keys) == 1) "" else "s") else ""))
  H <- c(H, chips, kpis(list(
    list(k = "Sets", v = nrow(E)),
    list(k = "Assist%", v = pct(t0$assist)),
    list(k = "Atk hit off sets", v = eff(t0$hit)),
    list(k = "Set eff", v = eff(t0$seff)),
    list(k = "Ball-handling err", v = t0$bhe)
  )))

  by_s <- roster[!vapply(roster, function(g) g$key %in% dropped_keys, logical(1))]
  H <- c(H, card("By setter", c(tbl_generic(
    c("Setter", "Sets", "BHE", "Linked", "Assist%", "Atk K%", "Atk hit", "SO hit",
      "Trans hit", "Out", "Mid", "Opp", "Pipe", "Set eff"),
    lapply(by_s, function(g) {
      parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
      so <- setter_eff(g$rows[!is.na(g$rows$phase) & g$rows$phase == "SR", ])
      tr <- setter_eff(g$rows[!is.na(g$rows$phase) & g$rows$phase %in% c("FT", "TR"), ])
      c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), g$stat$n, g$stat$bhe,
        pct(g$stat$linked / g$stat$n), pct(g$stat$assist), pct(g$stat$kpct),
        eff_td(g$stat$hit),
        if (so$linked) eff_td(so$hit) else "—",
        if (tr$linked) eff_td(tr$hit) else "—",
        vapply(TKEYS, function(k) share(g$rows, k[1]), ""),
        eff_td(g$stat$seff))
    })),
    tiny("SO hit and Trans hit are the hitting efficiency of the attack off that set, split by the phase of that attack. A set carries no phase of its own; it inherits the one from the swing it produced."))))

  L <- list(); R <- list()

  by_t <- lapply(c(TKEYS, list(c(NA_character_, "Other / none"))), function(kl) {
    k <- kl[1]; lab <- kl[2]
    rs <- if (is.na(k))
      E[is.na(E$target) | !(E$target %in% vapply(TKEYS, `[`, "", 1)), ]
      else E[!is.na(E$target) & E$target == k, ]
    s <- setter_eff(rs)
    c(lab, s$n, pct(if (nrow(E)) s$n / nrow(E) else NA), pct(s$kpct), eff_td(s$hit),
      pct(if (nrow(rs)) sum(!is.na(rs$phase) & rs$phase == "SR") / nrow(rs) else NA))
  })
  L <- c(L, card("Where the ball goes, and what it returns", c(tbl_generic(
    c("Target", "Sets", "Share", "Atk K%", "Atk hit", "Of which side-out"), by_t),
    tiny("The gap between share and efficiency is the usable finding: a target carrying a large share at a low return, or a small share at a high one."))))

  GRADE <- list(c("#", "Perfect #"), c("+", "Positive +"), c("!", "Poor !"),
                c("-", "Negative -"), c("/", "Overpass /"))
  gr <- Filter(Negate(is.null), lapply(GRADE, function(gl) {
    rs <- E[!is.na(E$rec_effect) & E$rec_effect == gl[1], ]
    if (!nrow(rs)) return(NULL)
    s <- setter_eff(rs)
    c(gl[2], s$n, vapply(TKEYS, function(k) share(rs, k[1]), ""),
      pct(s$kpct), eff_td(s$hit))
  }))
  no_pass <- E[!is.na(E$phase) & E$phase != "SR", ]
  if (nrow(no_pass)) {
    s <- setter_eff(no_pass)
    gr <- c(gr, list(tot_row(c("Transition (no pass)", s$n,
                               vapply(TKEYS, function(k) share(no_pass, k[1]), ""),
                               pct(s$kpct), eff_td(s$hit)))))
  }
  L <- c(L, if (length(gr))
    card("Distribution by the pass that fed it", c(tbl_generic(
      c("Pass", "Sets", vapply(TKEYS, `[`, "", 2), "Atk K%", "Atk hit"), gr),
      tiny("Whether the middle stays live once the pass degrades. Sets with no reception in front of them are grouped as transition.")))
    else card("Distribution by the pass that fed it",
              empty("No receptions linked to sets in this filter.")))

  ph_rows <- Filter(Negate(is.null), lapply(list(c("SR", "Side-out"), c("TR", "Transition"),
                                                c("FT", "First ball, serving")), function(kl) {
    rs <- E[!is.na(E$phase) & E$phase == kl[1], ]
    if (!nrow(rs)) return(NULL)
    s <- setter_eff(rs)
    c(kl[2], s$n, vapply(TKEYS, function(k) share(rs, k[1]), ""),
      pct(s$kpct), eff_td(s$hit))
  }))
  if (length(ph_rows))
    L <- c(L, card("By phase", tbl_generic(
      c("Phase", "Sets", vapply(TKEYS, `[`, "", 2), "Atk K%", "Atk hit"), ph_rows)))

  R <- c(R, card("By rotation", c(tbl_generic(
    c("Rot", "Sets", vapply(TKEYS, `[`, "", 2), "Atk K%", "Atk hit", "Set eff"),
    lapply(by_key(crosstab(E, E$rot, setter_eff)), function(g)
      c(sprintf("<b>%s</b>", esc(g$key)), g$stat$n,
        vapply(TKEYS, function(k) share(g$rows, k[1]), ""),
        pct(g$stat$kpct), eff_td(g$stat$hit), eff_td(g$stat$seff)))),
    tiny("R1–R6 are sequence labels: R1 is the setter in zone 1, then 6, 5, 4, 3, 2. R1–R3 are setter-back-row rotations, so a middle share that collapses across R4–R6 is a front-row-setter effect, not a preference."))))

  lk <- E[!is.na(E$atk_num), ]
  R <- c(R, card("Who gets the ball", tbl_generic(
    c("Hitter", "Sets", "Share", "Atk K%", "Atk hit"),
    lapply(by_size(crosstab(lk, paste(lk$atk_num,
                                      ifelse(is.na(lk$atk_player),
                                             paste0("#", lk$atk_num), lk$atk_player),
                                      sep = "|"), setter_eff), "n"), function(g) {
      parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
      c(sprintf("<b>%s</b> %s", parts[1], esc(parts[2])), g$stat$n,
        pct(if (t0$linked) g$stat$n / t0$linked else NA),
        pct(g$stat$kpct), eff_td(g$stat$hit))
    }))))

  paste0(c(H, cols2(L, R, 520),
           note("Atk K% and Atk hit use VolleyStation's own attack formulas, applied to the attacks that followed these sets. Assist% and Set eff are conventions defined by this tool, not VolleyStation report columns: Assist% is kills per set attempted, Set eff is (kills − errors − blocks − ball-handling errors) / sets attempted, so it charges the setter for her own errors and for sets that produced no attack. Read them within one file, not against published numbers from elsewhere.")),
         collapse = "")
}
