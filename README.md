# Cal Poly Scout App

Opponent and self scouting off coded match files, in R/Shiny. Reads
VolleyStation `.vsm` and DataVolley `.dvw`, in any mix.

By **Andrew King**, Statistics and Data Science, Cal Poly. Built with AI
assistance. How it got here: [STORY.md](STORY.md).

## Running it

R 4.5 with `shiny` and `data.table`. Nothing to install, no build step.

Double-click **scout.bat**, or:

```bash
Rscript run.R
```

Either serves it to the whole room and prints the addresses. The batch file
finds R itself, which matters because R's installer does not put `Rscript` on
the PATH, so the command above fails on a machine where R is plainly
installed. Open `app.R` in RStudio and click Run App for just this machine.

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
| **Attacking** | Where each hitter puts the ball and what it returns. Trajectories, landing grid, lanes, line/cross, depth, middle routes, serve-receive vs transition. |
| **Serve direction** | Contact to landing. By type, server, target zone, depth, and where to stand a passer against each server. |
| **Set origin** | Where the setter touched it and what she could run from there. Click zones of her own half to cut by lane and distance off the net. |
| **Setter** | Distribution and efficiency. Every player who set is listed and any can be switched off. |
| **Pass quality** | Sorted worst first, because this page is for picking who to serve. Pass average, earned FBSO, 5-1 vs 6-2. |
| **Defense** | What they manage against us, whether we touch it, and whether we win the rally after. Court drawn from our baseline. |
| **Rotations** | SO%, ESO%, FBSO%, PS%, +/-, aces and errors per rotation, both teams, then the same by set. |
| **Last 6** | Who is swinging well now. Recent attempts as chips, split into pins and middles. |
| **Next server** | Follows the match. Our serve shows their passers, weakest first; their serve shows the player about to serve, chart first. |
| **Plays & export** | The raw touches, and a CSV of every filtered play. |
| **Roster** | Who plays where. Set before coding, since every mention of pins and middles reads it. |

Attacking, Serve, Set origin, Setter, Pass quality and Last 6 show both teams
at once, side by side, ours in green and theirs in red. Nothing to switch
between, and the comparison is the point. The **One team** button gives a
single side the full width, and below 1180px the two stack rather than
squeeze. Rotations and Defense already had both teams by construction.

The team picker in the header no longer decides whose numbers you see. It
says which team is yours, which is what sets the colours and the order.

**What is shared and what is yours.** The match being followed and the
roster belong to everyone: correct a position on the laptop and every screen
already watching changes, with nobody reloading. Which tabs you want, and
the filters and view options, are yours alone, so one coach cutting to R1
does not cut the whole bench to R1.

**Each device picks its own tabs.** The **Tabs** button hides the ones a
coach does not want, and the choice is remembered on that device. One laptop
serves every iPad on the bench, so this is kept in the browser rather than on
the server: the coach watching serve and pass and the coach watching attack
and defence are looking at the same session. Hiding the tab you are on moves
you to the first one still showing, and hiding every tab is ignored rather
than obeyed.

The left rail filters everything at once: matches, which side is your team,
sets, rotation, system, phase, passer, players.

## Following a match as it is coded

VolleyStation appends to the `.dvw` while you code, so the app can follow one.
Paste the file's path into the box under the drop zone and press Watch. It
re-reads about four tenths of a second after each change and keeps your
filters, tab and scroll position.

**Going back to fix a play works.** Every refresh is a full re-read, and the
match is replaced rather than added to, so a corrected grade, a deleted touch
or a renumbered rally all land the same way an appended one does. A correction
that leaves the file exactly as long is still caught, because the app compares
timestamp and size as a pair rather than watching the file grow. The badge in the header carries the time of the last
refresh, so a feed that has stopped reads as a stale clock rather than as
numbers that quietly stop moving.

A dropped or browsed file cannot be followed. Shiny copies an upload to a temp
path, so the original can grow all it likes and the app never sees it.

The rally in progress has no score row until the point ends, so SO%, ESO%,
FBSO% and PS% lag the court by one rally. The Rotations tab says so.

Polling ten times a second costs about 19% of one core while someone is
coding. After ten seconds of no change it drops to twice a second, about 2%,
and picks straight back up on the next touch. `WATCH_POLL` in `app.R` is the
knob.

## On the bench, on iPads

The same shape as VolleyStation's own sharing. The laptop coding the match
double-clicks `scout.bat`, and the iPads open the address it prints. Any router,
no internet: the router does not have to reach anything, only to be a
network, and every script, stylesheet and font the page loads is served from
this machine. All that matters is that the devices are on the same router.

```
  on the tablets      http://drews-laptop.local:7815
  or by address       http://192.168.1.42:7815
  on this laptop      http://localhost:7815

  network             MatchRouter (Private)
  firewall rule       in place

  Ready for the tablets.
```

Use the first line. It is the laptop's own name, so it survives changing
routers, which the address does not. iPads resolve it natively.

The app shows that address as a QR code, on the opening screen and behind
the Connect button once a match is loaded, so a tablet is pointed at the
laptop rather than typed at. The encoder is in `R/qr.R`, written out rather
than taken from a package because a missing package cannot be installed in a
gym with no internet. It was checked module for module against an
independent implementation, 416 of 416 matrices over versions 1 to 6 and all
eight masks, and the matrices are kept in `tests/verify.R` so that check
still runs offline.

It checks the two things that stop it working and prints the command to fix
each: a network Windows has filed as Public, and a missing firewall rule.
The rule is once ever, the network is once per router. Campus and venue
Wi-Fi is the case no setting fixes, since those networks keep their clients
from seeing each other at all.

The full match-day sheet, written to be read on the laptop with no internet,
is [BENCH.md](BENCH.md).

**One file, one poller, however many people are watching.** The watched file
belongs to the app, not to a tab, so anyone can press Watch and everyone sees
the same match, and a tablet that joins in the third set has the match
already. Uploads stay per viewer: an iPad can open last week's file without
taking the bench off the live one. Measured with two viewers on one coding
file, 14% of one core, against 19% for a single viewer before this was
shared.

Below 900px the filter rail opens over the page from a Filters button rather
than sitting beside it, so the numbers are the first thing on the screen.

## Worth knowing

**Shot charts only work on files coded with lines.** No coordinates, no charts.
The tables still work. Either format can carry them.

The opening screen asks for the two separately, and the rail groups them
under Tonight and Scouting films. Before the first rally the Next server tab
shows the opponent's serve order and passers off the films; after it, it
follows the match.

**Scouting files and the live match are kept apart.** Load the opponent's
previous matches before the game and they feed the Next server tab only:
where their #12 serves, which of their passers to go at. Every other tab is
scoped to the file being coded, because pooling last month's film into
tonight's hitting percentage produces a number that looks like tonight and is
not. Picking matches by hand in the rail overrides this.

The history is also kept to the team you are playing, and found by team
name rather than through your own side. A scouting film is the opponent
playing somebody else, with your team not in it anywhere, so asking for
"our opponent in that file" finds nothing and discards the whole library.
On a real set of three files that was the difference between 66 serves with
no coordinates and 193 serves with 127 charted.

The team picker is remembered in `data/our_team.txt`. The default picks the
team appearing in the most files, which is right until opponent films are
loaded: three of theirs against one of yours and the app would decide you
are Grand Canyon.

**A serve order is per film, never pooled.** A rotation label means their
setter is in that zone, which compares across matches only while the lineup
does. On four UCSB films the same label picked a different server in
different films, and pooling produced rows claiming more serves than the
player named had played. Each film gets its own table, most recent first,
and each says what share of its rotations was served by somebody other than
the named player: 1% on one of those films and 57% on another, which is the
difference between an order you can use and a label.

**Who serves next is worked out, not read off the lineup.** Three rules were
measured against real matches: zone 1 of the lineup and "same server if they
held" both managed 72% on side-outs, and the rule this uses, whoever served
the last time that side was in the rotation they are about to be in, managed
96.7%. The difference is serving substitutes. Per match it called 100% on one
scouted opponent and 98% on another, and the tab says which rule answered.

**Positions come from the file.** Both formats store one per player, using
VolleyStation's own codes, so pins and middles are read rather than guessed.
The Roster tab shows what the file said and lets a staff correct it where the
file is wrong or blank, which is usual for an opponent typed from a lineup
sheet. A correction is kept by team and jersey number in ,
so it holds through live refreshes, a restart, and the next file off the same
roster. Where a roster carries no position at all, the pins and middles split
falls back to what each hitter actually runs.

**Rotation labels are sequence-based.** R1 is the setter in zone 1, then 6, 5,
4, 3, 2, so R1 to R3 are the setter-back-row rotations.

**Some touches never reach a rate.** Practice players #98 and #99, and any play
tagged `00`, which means the coder marked it to revisit. Both still show on the
Plays tab, and the count appears in a banner.

**Two defensive fields are not being coded.** Every Cal Poly file records 2
blockers on every attack, and digs are never graded `#`. The Defense tab says
so rather than printing a table built on a constant.

**A printed scout sheet will show fewer serves than this does.** The sheets are
built from the chart, so they count only plays coded with a line. Checked
against the staff's own UCSB sheet over four films: all nine servers agree
exactly on serves, aces and efficiency once uncharted serves are set aside,
and the attacker page agrees without any adjustment. The Next server tab
counts every serve and prints the charted count beside it, so the two
reconcile on sight.

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
scout.bat          double-click this at a match
run.R              serve it on any router, printing the address
BENCH.md           the match-day sheet
app.R              UI, server, shared file watch, filter rail, file intake
R/core.R           geometry, zones, season codes, VolleyStation symbol table
R/net.R            the addresses this machine answers on
R/qr.R             a QR encoder, so a tablet can be pointed at the screen
R/positions.R      position overrides, kept by team and jersey
R/next_server.R    who serves next, and each side's serve order
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
