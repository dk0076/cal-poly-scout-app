# ---------------------------------------------------------------------------
# What the evaluation characters mean.
#
# Every rate in the app is built from this mapping, so it is also shown in the
# Notes tab rather than buried here. The DataVolley defaults, matching the
# HTML report these numbers were checked against.
# ---------------------------------------------------------------------------

DEFS <- list(
  Serve = c("#" = "ace", "+" = "neutral", "!" = "neutral",
            "/" = "neutral", "-" = "neutral", "=" = "err"),
  Reception = c("#" = "perfect", "+" = "positive", "!" = "neutral",
                "-" = "neutral", "/" = "overpass", "=" = "err"),
  Attack = c("#" = "kill", "+" = "neutral", "!" = "neutral",
             "-" = "neutral", "/" = "blocked", "=" = "err"),
  Block = c("#" = "stuff", "+" = "neutral", "!" = "neutral",
            "-" = "neutral", "/" = "neutral", "=" = "err"),
  Dig = c("#" = "good", "+" = "good", "!" = "good",
          "-" = "good", "/" = "neutral", "=" = "err"),
  Set = c("#" = "neutral", "+" = "neutral", "!" = "neutral",
          "-" = "neutral", "/" = "neutral", "=" = "err"),
  Freeball = c("#" = "neutral", "+" = "neutral", "!" = "neutral",
               "-" = "neutral", "/" = "neutral", "=" = "err"))

# Named by symbol, not by position: the serve scale puts "/" ahead of "-",
# which is the opposite way round from every other skill.
DEF_TEXT <- list(
  Serve = c("#" = "Ace", "+" = "Positive, opponent limited",
            "!" = "OK, no quick available", "/" = "Positive, no attack possible",
            "-" = "Negative, opponent in system", "=" = "Error"),
  Reception = c("#" = "Perfect pass (4)", "+" = "Good pass (3)", "!" = "Okay pass (2)",
                "-" = "Poor pass (1)", "/" = "Overpass (0)", "=" = "Error (0)"),
  Attack = c("#" = "Kill", "+" = "Positive, opponent out of system",
             "!" = "Blocked for reattack", "-" = "Poor, opponent in system",
             "/" = "Blocked for a point", "=" = "Error"),
  Block = c("#" = "Winning block", "+" = "Block continues, in our favour",
            "!" = "Block touch", "-" = "Block continues, opponent favour",
            "/" = "Invasion / net touch", "=" = "Error"),
  Dig = c("#" = "Perfect dig", "+" = "Good dig", "!" = "Playable dig",
          "-" = "Poor dig", "/" = "Straight back over", "=" = "Error"),
  Set = c("#" = "Perfect set", "+" = "Good set", "!" = "OK set",
          "-" = "Poor set", "/" = "Set back over", "=" = "Error"),
  Freeball = c("#" = "Perfect", "+" = "Good", "!" = "OK",
               "-" = "Poor", "/" = "Straight back", "=" = "Error"))

# Passing grades on the 0 to 4 scale. Editable in the sidebar, because a
# program that grades its own way should get its own averages.
GRADE_DEFAULT <- c("#" = 4, "+" = 3, "!" = 2, "-" = 1, "/" = 0, "=" = 0)

SERVE_TYPES <- c(M = "Jump float", H = "Standing float", Q = "Jump spin",
                 T = "Topspin", N = "Other")
RALLY_END <- c(O = "Off the block, out", F = "Floor", U = "Unplayable")
SET_TARGETS <- c(F = "outside", C = "middle", B = "right side",
                 P = "back row", S = "setter")

# The house code guide. A description in the scout file wins, but the grouping
# always comes from here, because the file's own table has nowhere to put it.
# `confirmed` marks the readings somebody checked against video.
HOUSE_COMBOS <- read.csv(text = "
code,desc,group,confirmed
LT,Quick in 3,Middle,FALSE
LG,Quick far from setter,Middle,FALSE
LP,Push,Middle,FALSE
L2,Back 1,Middle,TRUE
LS,Slide,Middle,FALSE
PR,Red,Opposite,FALSE
IR,Red,Opposite,FALSE
V2,Out of system 5,Opposite,FALSE
I5,5,Opposite,FALSE
P2,2 ball opposite,Opposite,TRUE
PG,Go,Outside,FALSE
V4,Out of system 4 high ball to the outside,Outside,TRUE
P4,2 ball outside,Outside,FALSE
PH,D ball,Back row,TRUE
IP,Pipe,Back row,FALSE
VP,Out of system pipe,Back row,TRUE
ID,40,Back row,TRUE
I0,In system D ball,Back row,FALSE
P7,30,Back row,FALSE
I9,40,Back row,FALSE
PO,Overpass,Other,FALSE
PD,Setter dump,Other,FALSE
PP,Setter dump,Other,FALSE
XM,Push,Middle,FALSE
X1,Front 1,Middle,FALSE
X7,Gap,Middle,FALSE
X2,Back 1,Middle,FALSE
CB,Slide setter in 2,Middle,FALSE
CF,Slide setter in 3,Middle,FALSE
X5,Go,Outside,FALSE
V5,Out of system 4,Outside,FALSE
X6,Red,Opposite,FALSE
V6,Out of system 5,Opposite,FALSE
X8,D ball,Opposite,FALSE
XP,Bic,Back row,FALSE
XB,40,Back row,FALSE
", stringsAsFactors = FALSE)

#' Describe an attack code.
#'
#' A description in the scout file wins, since that is what the scout actually
#' meant by it. The grouping comes from the house guide either way, because
#' the file's own table has no column for it and everything would otherwise
#' land in one heap.
combo_info <- function(codes, file_combos) {
  out <- data.frame(code = codes, call = NA_character_, group = "",
                    source = "none", confirmed = FALSE, stringsAsFactors = FALSE)
  j <- match(codes, HOUSE_COMBOS$code)
  known <- !is.na(j)
  out$group[known] <- HOUSE_COMBOS$group[j[known]]
  out$confirmed[known] <- HOUSE_COMBOS$confirmed[j[known]]

  if (!is.null(file_combos)) {
    i <- match(codes, file_combos$code)
    hit <- !is.na(i) & nzchar(file_combos$desc[i]) & file_combos$desc[i] != codes
    hit[is.na(hit)] <- FALSE
    out$call[hit] <- file_combos$desc[i[hit]]
    out$source[hit] <- "file"
  }
  use <- out$source == "none" & known
  out$call[use] <- HOUSE_COMBOS$desc[j[use]]
  out$source[use] <- "guide"
  out$call[out$source == "none"] <- "not in the guide"
  out$group[!nzchar(out$group)] <- "Ungrouped"
  out
}
