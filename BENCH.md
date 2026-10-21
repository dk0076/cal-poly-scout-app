# Running it at a match

No internet needed, for the app or for this page. Any router works. All that
matters is that the laptop and the tablets are on the same one.

## Once, ever

In an **administrator** PowerShell, so Windows stops asking:

```bash
netsh advfirewall firewall add rule name=scoutR dir=in action=allow protocol=TCP localport=7815 profile=private
```

This covers every private network, not just the one you are on. You never
need to run it again.

## Once per new router

Windows files a network it has not seen as Public, and refuses the tablets on
a Public network. Tell it the router is yours, in an **administrator**
PowerShell:

```bash
Set-NetConnectionProfile -InterfaceAlias "Wi-Fi" -NetworkCategory Private
```

Or do it without a terminal: Settings, Network and internet, Wi-Fi, click the
network, choose Private network.

## Every match

1. Laptop and iPads on the same router.
2. On the laptop, open the scoutR folder and double-click **scout.bat**.
3. A black window opens and prints what to type. Read the first line to
   whoever has an iPad. Leave the window open: closing it stops the app.
4. Press Watch on the laptop and paste the path of the `.dvw` VolleyStation
   is writing. Every tablet follows it from that moment, including ones that
   join later.

Pin scout.bat to the taskbar or make a desktop shortcut for it, so match day
is one click.

## Before the first match off a new roster

Open the **Roster** tab. Both file formats already carry a position for every
player, so it arrives filled in and usually needs nothing. Correct one where
the file is wrong or blank, which is normal for an opponent typed from a
lineup sheet. Undo puts a player back to what the file says.

Corrections reach the iPads as you make them, with nobody reloading. This is
worth two minutes before coding, because every mention of pins and middles on
the other tabs reads it. A correction is kept by team and jersey
number, so it holds through the match, a restart, and the next file off the
same roster.

## Setting up each iPad

Press **Tools**, then **Tabs**, and turn off what that coach does not want.
Each iPad remembers its own set, so one can sit on serve and pass while
another sits on attacking and defence, both reading the same live match.
Show all puts them back.

## Scouting files, before the game

Load the opponent's previous matches with Browse, then point the watch at
tonight's file. The previous matches feed the **Next server** tab and nothing
else: every other tab counts tonight only, so a hitting percentage is
tonight's.

The Next server tab follows the match. When we serve it lists their passers
worst first, which is who to serve. When they serve it shows the player about
to serve, her chart first, then where she goes and how she has been serving
lately. The line under the heading says which files it is reading.

## Starting over

Press **Cal Poly Scout** in the top left. It clears the loaded files and goes
back to the opening screen, ready for a different one. It asks first, because
it also stops the live file for everyone watching, so a stray tap on a tablet
cannot take the bench off the match.

The small number beside it, like v1.12, is the version. It goes up every time
the app changes, so it is worth reading out if something looks wrong. Hover
it for the exact build.

## Getting the iPads on

The opening screen shows a QR code. Point an iPad camera at the laptop
screen and tap the link that appears. Once a match is loaded the code moves
behind the **Connect** button in the header, so a tablet arriving at the end
of the second set can still be pointed at it.

The code holds the laptop name address, the one that survives changing
routers, so it keeps working at the next gym with no change.

## The address

```
  on the tablets      http://drews-laptop.local:7815
  or by address       http://10.0.0.14:7815
  on this laptop      http://localhost:7815
```

Use the first one. It is the laptop's own name and it does not change when
you change routers, which the numbers do. iPads resolve it natively.

A few routers refuse to pass names like that. Then use the second line, and
expect it to be different at the next gym.

## When it says it is not ready

It checks before serving and tells you which of the two things is wrong:

```
  network             MatchRouter (Private)
  firewall rule       in place

  Ready for the tablets.
```

Anything other than that, it prints the command to fix it.

## What will not work, ever

Campus and venue Wi-Fi. Eduroam and guest networks keep their clients from
seeing each other, so the tablets cannot reach the laptop whatever the
address says and whatever you change on the laptop. This is the one case a
setting cannot fix. Bring the router.

## If a tablet still cannot open it

In order, because this is roughly how often each is the cause:

1. The tablet is on a different network. Phones and iPads switch back to a
   remembered one by themselves.
2. The network is Public. The black window says so at the top.
3. The firewall rule is missing. It says so too.
4. The router does not pass `.local` names. Use the numeric address.
5. The laptop went to sleep, or the black window got closed. Either stops
   the app.
