# Cal Poly Scout App

Opponent and self scouting off coded match files, in R/Shiny. A port of the
`scout_vsm` single-file HTML tool.

By **Andrew King**, Statistics and Data Science, Cal Poly. Built with AI
assistance.

## Match files are not in this repository

`data/` is gitignored, and so is every `.vsm`, `.dvw` and `.vss`. Coded match
files carry real players, real opponents and real scouting information, none of
which is ours to publish. A fresh clone therefore has no sample and no test
fixtures, and the app will say so rather than offering a button that errors.

To use it, put your own `.vsm` or `.dvw` files somewhere and drop them on the
app. To run the checks, pass files as arguments:

```bash
Rscript tests/verify.R path/to/match.vsm path/to/another.dvw
```

To rebuild the coordinate fixture the charts can be demonstrated with, from a
`.dvw` you already hold:

```bash
Rscript data-raw/make_fixture.R your-match.dvw data/fixture_coords.vsm
```

Reads VolleyStation `.vsm` and DataVolley `.dvw`, in any mix, as many files as
you like. Nothing leaves the machine: the files are parsed in the R process
running on your own computer.

## Running it

R 4.5 with `shiny` and `data.table` (both already installed here). From the
folder above this one:

```bash
Rscript -e "shiny::runApp('scoutR', launch.browser = TRUE)"
```

Or open `scoutR/app.R` in RStudio and click Run App. There is nothing to
install and no build step.

To try it without a match file, click **Load the bundled sample** on the
opening screen.

## What is in it

Six tabs, ported from the original:

| Tab | What it answers |
| --- | --- |
| **Attack direction** | Where each hitter puts the ball and what it returns. Trajectories, landing grid, contact points, tendency lines, lane / line-seam-cross / depth splits, middle routes, SR vs transition by rotation and by hit type, hard vs tip, and against the block. |
| **Serve direction** | Contact behind the endline to landing point. By type, by server, by target zone, by depth band, by serving position, plus passer-start marks: against this server, where should our passer stand. |
| **Set origin** | Where the setter touched it and what she could run from there. Contact points coloured by target, set→hitter arrows, contact grid, and a click-the-zones explorer that cuts by lane and distance off the net. |
| **Setter** | Distribution and efficiency, no coordinates needed, so it works identically on a `.dvw`. By setter, by target, by the pass that fed it, by phase, by rotation, by hitter. Every player who set is listed and any of them can be switched off; there is no automatic hiding. |
| **Pass quality** | Sorted worst positive% first, because the use of this page before a match is picking who to serve. Includes earned FBSO and a 5-1 vs 6-2 comparison with Wilson intervals. |
| **Defense** | How we are defending, kept as three separate questions: what they manage against us (their attack efficiency, by call and by where it lands in our court), whether we touch it (block touches and stuffs, digs kept up and in system), and whether we win the rally afterwards (transition conversion). The court is drawn from our baseline, their attacks mirrored into our frame. |
| **Rotations** | Rotational analysis to the staff's own layout, for both teams side by side: SO%, ESO%, FBSO%, PS%, +/-, aces and errors per rotation, then the same questions by set with Eff, K%, E+B% and sQ%. Rotation labels carry the setter's court zone. sQ% is serve-receive quality, defined from the worksheet's own `VSPER(VSOR(R#, R+))`. |
| **Last 6** | Who is swinging well *now*, after this program's own "MATCH Last 6 Attempts Viewer" worksheet. A row per hitter: attempts, overall efficiency, efficiency across the recent window with an arrow against the overall, then the attempts themselves oldest to newest as colour-coded chips showing call and grade. Split into pins and middles, because a middle on six quicks and a pin on six high balls are not comparable. Window switches to 10 or 15. |
| **Plays & export** | The raw touches behind every other number, and a CSV of every filtered play. |

The left rail filters everything at once: matches, which side of each file is
your team, sets, rotation, offensive system, attack phase, who passed the ball,
and players.

### Three things worth knowing before you read a number off it

**Shot charts only work on files coded with lines.** No coordinates, no
charts, the tables still work. Either format can carry them.

**The dvw coordinate decoding is verified.** On a match held in both formats,
the plays that neither pass re-cleaned decode identically (R² = 1.000), so the
differences elsewhere are the cleaning, not the parser.

A caution for anyone extending this: regress per skill, never pooled. Serves
and attacks contact at very different heights, and pooling two populations that
each sit on their own line invents a scale error that is not there.
`tests/parity.R` does it per skill for that reason.

**The two formats store receptions and digs differently**, so the parser
mirrors those two skills to make them agree, and `tests/verify.R` checks it.

**Two defensive fields are not being coded in this program's own files.**
Every Cal Poly file to hand records `2` blockers on every single attack, 369
of 369 on one, 370 of 370 on another, so the blocker count is a constant, not
a measurement. Digs are graded only `+`, `/` and `=`, never `#`, so "in
system" collapses to "kept up". The Defense tab detects both and says so
rather than printing a confident-looking table built on a constant. Scouted
opponent files do carry the full range of both, so the tables fill in there.
Coding those two fields would make the blocking read the most useful table on
that page.

**Some touches are excluded from every rate.** Practice players #98 and #99
are not players, and they are not rare, 1447 touches across 15 files here,
663 of them attacks, which moved one file's hitting from .081 to .095 once
they came out. Plays whose custom tag contains `00` are excluded too: that tag
means the coder marked the play to revisit, so it is not final. Both are
excluded rather than deleted, they still appear on the Plays tab, and the
count is reported in a banner so the exclusion is visible. `Z` is the opposite
of `00`, a play flagged *for* the coach, and is never excluded.

**Intrasquad files have the same team name on both sides.** All the Cal-Cal
practice files do. The focus picker shows one entry, so use the per-file side
dropdown in the rail to choose which six you are looking at, it labels them
"home side" and "away side" when the names match.

**Rotation labels are sequence-based.** R1 is the setter in zone 1, then 6, 5,
4, 3, 2, so R1–R3 are the setter-back-row rotations and R4–R6 the front-row
ones, which is the convention coaches use. Labelling by raw zone number instead
was the original tool's bug: it put the front-row-setter rotations at S2/S3/S4
and read as though the opposite vanished there.

### Working rule: every new metric is cross-checked against VolleyStation

Not reconciled afterwards, defined against it in the first place. VolleyStation
computes every skill efficiency with one formula, in
`app/submodules/functions/get-efficiency.function.js` of the readable build at
`C:\vsdev`:

```
efficiency = (count of winning symbols - count of losing symbols) / total
```

with the default symbol table from `user-settings.service.js`. That table is
transcribed into `VS_SYMBOLS` in `R/core.R`, and `vs_efficiency(effect, skill)`
applies it. `tests/verify.R` asserts, on real data, that this tool's attack
efficiency, serve `Eff` and reception positive% each equal VolleyStation's, and
asserts the one place it departs on purpose, so the departure stays deliberate.

Define a new metric through `vs_efficiency()` where VolleyStation has an opinion.
Where it does not, say so in the column note rather than letting it look official.

### Which numbers match VolleyStation and which do not

Said on the page too, but worth having in one place:

- **Reproduce VolleyStation's own report columns**, so they can be read against
  it: attack kill% and efficiency, serve `Eff` = (aces + no-attack + positive −
  errors) ÷ serves, serve `Op`, and "positive reception" = `#` or `+`.
- **This staff's own 0–4 scales**: pass average (`# 4, + 3, ! 2, - 1,
  overpass 1, error 0`) and serve average (`# 4, + 2, ! 1.5, - 1, overpass 3,
  error -1`). Both are in `PASS_SCALE` / `SERVE_SCALE` in `R/core.R`. The
  overpass weight on the pass scale is the staff's direct call, the same as a
  negative pass, settling a disagreement between their worksheet (0.5) and
  their HTML report (0).
- **Conventions defined by this tool**, so they should be read within one file
  and not against published figures: `Assist%` (kills per set attempted),
  `Set eff` (kills − errors − blocks − ball-handling errors ÷ sets attempted,
  which charges the setter for sets that produced no attack), earned `FBSO%`,
  and `Convert%`.

## Layout

```
app.R              UI, server, the filter rail, file intake
R/core.R           court geometry, zone lattices, season codes, formatting
R/parse_vsm.R      VolleyStation JSON reader
R/parse_dvw.R      DataVolley plain-text reader
R/model.R          rally linking, roles, 5-1/6-2 classifier, stat lines, filters
R/svg.R            court plots and their layers, as inline SVG
R/tables.R         KPI strips, cards, the table builder
R/tab_*.R          one file per tab
www/styles.css     Cal Poly green and gold, print-aware
www/app.js         one delegated click handler for every rendered control
data/              fixtures (see below)
data-raw/          the fixture generator
tests/verify.R     the check suite
```

Charts are inline SVG rather than ggplot2 on purpose: each shot carries a
`<title>`, so hovering a line gives you the hitter, set, score, call and
outcome; it stays vector-crisp when a coach prints the page; and re-rendering
on a filter change costs nothing because it is just text.

## Checking it still works

```bash
Rscript tests/verify.R
```

91 checks over the two bundled fixtures: parser invariants, the phase walk,
set→attack linking (including that no attack is ever claimed by two sets), zone
round-trips, every tab in every view state, and the empty and single-play
cases. Run it after any change to the parsing or geometry layer, a parse that
shifts by one column still produces output that looks like volleyball, so "the
app still renders" is not evidence.

To compare one match exported two ways, field by field:

```bash
Rscript tests/parity.R "../cleaned practice/2026-09-15 Cal-Cal.vsm" "../cleaned practice/2026-09-15 Cal-Cal.dvw"
```

It aligns the plays, then reports every count, rate and coded field as
identical or not, and treats a coordinate gap inside half a grid cell as the
`.dvw` grid rather than a disagreement. Two parser bugs came out of running it
and are described below.

`tests/verify.R` takes extra files as arguments, which is the better check:

```bash
Rscript tests/verify.R "../drive vsm"/*.vsm
```

That runs 223 checks across the eleven real practice files plus the fixtures.

## The fixtures, and what they do and do not prove

`data/sample_match.dvw` is a real coded match. `data/fixture_coords.vsm` is
**synthetic**: its events are that same real match, but every coordinate is
generated, placed inside the zone the coder actually recorded.

That fixture exists because **no file anywhere on this machine has ball
coordinates**, not the eleven real practice `.vsm` files, not the `.dvw`
match files. Without it the entire coordinate half of the app would be
untested. Generating coordinates inside the coded zones makes the round-trip
checks in `tests/verify.R` meaningful: a point placed in zone 7 has to come
back out of `zone_of()` as zone 7, which tests the zone lattices,
`lat_third()`, `depth_of()` and `srv_zone6()` against DataVolley's own
numbering rather than against anyone's reading of it.

Regenerate it with:

```bash
Rscript data-raw/make_fixture.R data/sample_match.dvw data/fixture_coords.vsm
```

### What has been checked

**846 checks across every coded file in this season folder**, the scouted
opponent `.vsm` (which do carry coordinates), the practice `.vsm`, the `.dvw`
match and practice files, and the two bundled fixtures. All pass.

**Formulas reconciled against the staff's own numbers.** Set 5 of
`drive vsm/2026-08-08 Cal-Cal.vsm`, against `Spreadsheets/Scout Report.pdf`:

| | report | scoutR |
| --- | --- | --- |
| Attempts / kills / errors / blocked | 55 / 18 / 5 / 5 | same |
| Hitting %, kill % | .145, 33% | same |
| Serves, serve errors | 44, 8 | same |
| Positive reception % | 50.0% | same |
| Blocks | 5 | same |
| Rallies | 44 | same |
| Pass average | 2.24 | 2.26 |
| Aces | 3 | 4 |

The attack definitions were separately confirmed against VQL in
`coachs spreadsheets/1. Cal Poly Practice Totals.vss`: `Eff = VSEFF(A)`,
`K% = VSPER(A#)`, `EB% = VSPER(A/ or A=)`, all matching.

The last two rows are explained, not outstanding:

- **Pass average.** The report weights a reception overpass **0**; the staff's
  own worksheet weights it **0.5**. Two scales for one metric. scoutR follows
  the worksheet, which gives 2.26; on the report's weights the same receptions
  give 2.2353, i.e. its 2.24. One constant, `PASS_SCALE` in `R/core.R`, if the
  staff settles the other way.
- **Aces.** Set 5 rally 5 has a serve graded `#` *and then the rally carries
  on*, a set, a kill, a block. The grade and the touches contradict each
  other. scoutR counts the grade as coded, the way VolleyStation's own serve
  formulas do, and raises a warning naming the rally; the report silently drops
  it. Four such serves exist in that one file. They are coding slips to fix at
  source, not a formula difference.

**Parsing reconciled against VolleyStation's own database.** VolleyStation Pro
keeps an embedded MongoDB. Read against a copy of it, scoutR's parse of the
exported `2026-08-08 Cal-Cal.vsm` matches the app's own stored match exactly,
1428 plays, and every skill count identical (A 304, B 117, D 162, E 356, R 216,
S 273), the same again for set 5 alone. The export is lossless and the parser
is faithful to it.

That database cannot settle formulas, though, and it is worth saying why: it
stores coded touches, not statistics. Matches hold the same structure the
`.vsm` carries, the `reports` collection holds clip-reel definitions, and the
stored worksheets hold VQL formulas with no cached results. VolleyStation
computes its numbers at display time, so the only written-down form of its
formulas is the VQL in the worksheets, which is what the attack check above
used.

### Two bugs reading one match both ways found

Three matches in `cleaned practice/` exist as both a `.vsm` and a `.dvw` of the
same cleaning pass. Comparing them play by play found two faults, both of which
had been producing wrong serve-receive splits in silence.

**Rally boundaries in a `.dvw`.** Rallies were derived only from the score
rows, and a practice file is routinely coded with no score, one here has six
sets and four score rows, so five of its six sets read as a single rally each.
The serve-receive test resets per rally, so 101 side-out attacks were being
called transition. Rallies are now cut at each serve, the same rule the `.vsm`
path uses, which took that file from 74 rallies to 332 against the `.vsm`'s 334
and raised phase agreement between the two formats from 71% to 100%.

**Two implementations of one rule.** The `.vsm` parser derived phase inline
while the `.dvw` path derived it in `finalize_match()`. They had drifted to
86% agreement. There is now one implementation, used by both.

What is left after those: across the three matched pairs, 3726 plays, every
count and rate is identical and so is every coded field on attacks, serves,
receptions, digs and free balls. Eight plays disagree on a zone, block start
and end zones, one dig, one set's end zone, fields the analysis never reads,
where the two formats record different things on the same row.

### Two bugs the VolleyStation reconciliation found

**Rally segmentation.** One exchange is meant to be one rally, but 17 of these
files drop a whole set into a single exchange, one holds 245 plays and 44
serves. VolleyStation's own database stores it the same way, so this is how the
match was coded, not something the export did. Taken at face value the entire set is one rally, and because the
serve-receive test resets per rally, every attack after the first read as
transition: the SR/transition splits on those files were quietly meaningless.
Rallies are now recovered by splitting at each serve, which turns that set into
exactly the 44 rallies the report counts. Where this happens the file still
records only one lineup per exchange, so the app warns that rotation cuts on
those sets are the starting rotation only.

**Zones from coordinates.** A zone derived from a coordinate disagreed with the
coder's own entry on 4% of attack landings and 12–17% of reception contacts.
Every disagreement sat within ~7 units of a zone line: VolleyStation quantises
clicks onto a lattice that piles points right on the boundary, and the files
carry slightly different court calibrations, one puts x=303 in the middle
third and x=307 in the left, which no single threshold satisfies. So the
coordinate is the wrong source for a question the file already answers. The app
now uses the recorded zone wherever there is one and computes only as a
fallback.

### What is still not reachable here

The setter-call card has no real data behind it: these files carry combinations
on attacks but none on sets, so every `.vsm` to hand leaves that card empty. It
works on the fixture; it has never met a real example.

## Not ported

The original's remaining tabs: Player, Opponent attacks, Projected lineup, Game
plan, and the print-ready Scouting sheet (headshots, server and attacker
strips, lineup boxes). The machinery they sit on, parsers, filters, stat
lines, court layers, roster and role inference, starting and projected lineups
is all here, so they are additive work rather than new foundations.
