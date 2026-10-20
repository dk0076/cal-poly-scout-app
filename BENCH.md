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
2. On the laptop, in the scoutR folder:

```bash
Rscript run.R
```

3. It prints what to open. Read the first line to whoever has an iPad.
4. Press Watch on the laptop and paste the path of the `.dvw` VolleyStation
   is writing. Every tablet follows it from that moment, including ones that
   join later.

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

`run.R` checks before serving and tells you which of the two things is wrong:

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
2. The network is Public. `run.R` says so at the top.
3. The firewall rule is missing. `run.R` says so too.
4. The router does not pass `.local` names. Use the numeric address.
5. The laptop went to sleep. The app stops with it.
