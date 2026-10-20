# Cal Poly Scout App

Opponent and self scouting off coded match files, in R/Shiny. Reads
VolleyStation `.vsm` and DataVolley `.dvw`, in any mix.

By **Andrew King**, Statistics and Data Science, Cal Poly. Built with AI
assistance. How it got here: [STORY.md](STORY.md).

## Running it

R 4.5 with `shiny` and `data.table`. Nothing to install, no build step.

```bash
Rscript -e "shiny::runApp('scoutR', launch.browser = TRUE)"
```

Or open `app.R` in RStudio and click Run App, then drop files on the opening
screen.

## Match files are not in this repository

`data/` is gitignored, along with every `.vsm`, `.dvw` and `.vss`. Coded files
carry real players and opponents. A fresh clone has no sample and no fixtures,
and the app says so rather than offering a button that errors.

To run the checks, pass your own files:

```bash
Rscript tests/verify.R path/to/match.vsm path/to/another.dvw
```

## The tabs

| Tab | What it answers |
| --- | --- |
| **Attack direction** | Where each hitter puts the ball and what it returns. Trajectories, landing grid, lanes, line/cross, depth, middle routes, serve-receive vs transition. |
| **Serve direction** | Contact to landing. By type, server, target zone, depth, and where to stand a passer against each server. |
| **Set origin** | Where the setter touched it and what she could run from there. Click zones of her own half to cut by lane and distance off the net. |
| **Setter** | Distribution and efficiency. Every player who set is listed and any can be switched off. |
| **Pass quality** | Sorted worst first, because this page is for picking who to serve. Pass average, earned FBSO, 5-1 vs 6-2. |
| **Defense** | What they manage against us, whether we touch it, and whether we win the rally after. Court drawn from our baseline. |
| **Rotations** | SO%, ESO%, FBSO%, PS%, +/-, aces and errors per rotation, both teams, then the same by set. |
| **Last 6** | Who is swinging well now. Recent attempts as chips, split into pins and middles. |
| **Plays & export** | The raw touches, and a CSV of every filtered play. |

The left rail filters everything at once: matches, which side is your team,
sets, rotation, system, phase, passer, players.

## Worth knowing

**Shot charts only work on files coded with lines.** No coordinates, no charts.
The tables still work. Either format can carry them.

**Rotation labels are sequence-based.** R1 is the setter in zone 1, then 6, 5,
4, 3, 2, so R1 to R3 are the setter-back-row rotations.

**Some touches never reach a rate.** Practice players #98 and #99, and any play
tagged `00`, which means the coder marked it to revisit. Both still show on the
Plays tab, and the count appears in a banner.

**Two defensive fields are not being coded.** Every Cal Poly file records 2
blockers on every attack, and digs are never graded `#`. The Defense tab says
so rather than printing a table built on a constant.

## Metrics are defined against VolleyStation

Not reconciled afterwards. VolleyStation computes every skill efficiency with
one formula:

```
efficiency = (winning symbols - losing symbols) / total
```

That symbol table is transcribed into `VS_SYMBOLS` in `R/core.R`, and
`vs_efficiency(effect, skill)` applies it. `tests/verify.R` asserts on real
data that attack efficiency, serve `Eff` and reception positive% each equal
VolleyStation's, and asserts the one deliberate departure: dig "in system"
follows the staff's scout report, which excludes the over-dig VolleyStation
counts as winning.

Define any new metric through `vs_efficiency()` where VolleyStation has an
opinion. Where it does not, say so in the column note.

Conventions that are this program's own and should be read within one file:
`Assist%`, `Set eff`, earned `FBSO%`, `Convert%`, and the 0 to 4 pass and serve
scales in `PASS_SCALE` / `SERVE_SCALE`.

## Layout

```
app.R              UI, server, filter rail, file intake
R/core.R           geometry, zones, season codes, VolleyStation symbol table
R/parse_vsm.R      VolleyStation JSON reader
R/parse_dvw.R      DataVolley text reader
R/model.R          rally linking, roles, 5-1/6-2, stat lines, filters
R/svg.R            court plots, as inline SVG
R/tables.R         KPI strips, cards, table builder
R/tab_*.R          one file per tab
tests/verify.R     the check suite
tests/parity.R     compare one match exported two ways
tools/             .vss lint and diff
data-raw/          fixture generator
```

Charts are inline SVG, not ggplot2: each shot carries a `<title>`, so hovering
gives the hitter, set, score, call and outcome, and it stays vector-crisp in
print.

## Checking it

```bash
Rscript tests/verify.R "../SCOUT/cleaned scout"/*.vsm
```

Parser invariants, the phase walk, set-to-attack linking, zone round-trips,
every tab in every view state, and the empty and single-play cases. Run it
after any change to parsing or geometry: a parse that shifts by one column
still produces output that looks like volleyball.

To compare one match exported both ways:

```bash
Rscript tests/parity.R match.vsm match.dvw
```

Two parser bugs came out of running that. Rallies in a `.dvw` were derived only
from score rows, which collapsed whole sets into one rally on files coded
without a score; and phase was computed in two places that had drifted apart.
Both are fixed and asserted.

### Still owed

No rate has been compared against VolleyStation's rendered report output. It
computes at display time, so that one needs a human running the app.
