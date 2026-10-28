# Statbook

A scout report for coded volleyball, in Shiny. Drop one or more DataVolley
`.dvw` files in, pick the sets worth counting, and read the box scores.

This is the R port of `scout_report_v26_digefficiency.html`, the browser-side
report the numbers were worked out in. Every counter in it is here.

```r
shiny::runApp("statbook")
```

Needs `shiny` and `bslib`. The statistics are plain base R: no `data.table`,
no `ggplot2`, no `DT`.

## Layout

| File | What is in it |
| --- | --- |
| [app.R](app.R) | the UI and the server |
| [R/parse.R](R/parse.R) | reading a `.dvw`, and telling a played-out set from a drill block |
| [R/codes.R](R/codes.R) | what each evaluation character means, and the attack-code guide |
| [R/stats.R](R/stats.R) | rallies and counters |
| [R/tables.R](R/tables.R) | the tables for the screen |

Shiny sources everything in `R/` on startup, so there is nothing to install
and no build step.

## What it computes

Eight tabs: overview, attacking, serve and pass, setting, defense, one
player, trends, and the notes that say what the file itself left unclear.

Load several files at once and the sets pool into one running report. Each
set is classified as a played-out set, a scored block or drill work, so you
can count only the sets that were really played. Practice players can be left
out entirely: their touches leave the stream, so the rallies around them keep
their shape. The pass weights are editable, and every table follows.

| Stat | Definition |
| --- | --- |
| Attack `Eff` | (kills − errors − blocked) ÷ attempts, the NCAA convention |
| `oPGP` | the average pass the receiver managed off that server, 0 to 4, so **lower is better** |
| `PS%` | share of graded serves that held the receiver to a 2 or worse, aces counted as 0 |
| Pass `AVG` | perfect 4, good 3, okay 2, poor 1, overpass and error 0, and the weights are editable |
| `PGP%` | passes graded 3 or 4 |
| Sideout % | rallies received and won |
| First-ball sideout | won on the first swing off the reception, so the rally never turned |
| Rotation | sideout and point scoring by the setter's position at the start of the rally |
| Setter distribution | every attack credited to the setter who fed it, meaning the last set by that team before the swing |
| Assists | a kill credited back to the set that fed it |
| Cover | a dig of your own attack off the opponent's block |
| Dig `Conv%` | share of digs that led to a swing by that team in the same rally |
| Dig `Eff` | (converted − errors − balls sent straight back over) ÷ attempts |

Most of those are a count of rows matching a condition. Six are not: a serve
is judged by the pass it forced, a dig by the swing it set up, an assist and a
cover by the touch beside them, an attack by the setter behind it and by
whether the rally had turned over yet. Those come out of one pass over the
rallies in `rally_context()`.

## Checking the numbers

The reference implementation is the JavaScript inside the original HTML file.
It is pulled out, run under node, and diffed against this one fact by fact:
every counter for every player, both team lines, the attack codes, the
rotation splits, the setters' distributions, the tagged plays and the serve
types.

The last run: **203 files, 236,230 facts, no disagreement**, plus 35 files
that are not scout files at all and that both readers refuse. Four files
differ in the comparison and in neither case does a statistic change:

- three have a player whose only touch is a freeball. She gets an all-zero
  row in the HTML's internals and no row here. Neither counts freeballs
  toward anything.
- one has 2 point codes in 320 rallies, and 2/320 lands exactly on a rounding
  tie that JavaScript breaks upward and R breaks downward. Only the
  comparison script prints that number.

## What the HTML does that this does not

- The printed layout. The HTML prints a player card per player to paper; here
  you pick a player on screen.
- The SVG charts are redrawn as base-R plots, so they look different.
- Renaming the two sides by hand. The names come from the file.
- An uploaded roster CSV overriding the names in the file.
