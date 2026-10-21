# ---------------------------------------------------------------------------
# Serve scoutR to the room.
#
# At a match the laptop codes into VolleyStation and the bench reads off iPads
# on the same router, which is the setup VolleyStation's own sharing assumes.
# This does the same thing: bind every interface rather than loopback, then
# print the addresses to type into Safari.
#
#   Rscript run.R          serve on the network, port 7815
#   Rscript run.R 8080     a different port
#
# Any router. Nothing about a network is stored or assumed: the addresses are
# read fresh at every start, so moving between the practice gym, the match
# router and a phone hotspot needs no change here. All that matters is that
# the laptop and the tablets are on the same one.
#
# No internet either. The router does not have to reach anything, only to be
# a network. Every script, stylesheet and font the page loads is served by
# this process out of the R library on this machine, which is checked rather
# than assumed: a page load makes 13 requests and all 13 come back here.
#
# What does change between routers is the laptop's address, which is why the
# computer's own name is printed first. A router hands out a new IP every time
# it feels like it; the name stays put.
# ---------------------------------------------------------------------------

port <- suppressWarnings(as.integer(commandArgs(TRUE)[1]))
if (is.na(port)) port <- 7815L

RULE <- "scoutR"
win <- .Platform$OS.type == "windows"

#' Is there already a rule letting this port in?
#'
#' Readable without administrator, unlike most of the firewall. Windows also
#' offers to add one the first time a program listens, so this can be true
#' without anyone having run the command below. A rule scoped to private
#' covers every private network, so it is asked once and not once per router.
rule_exists <- function() {
  if (!win) return(NA)
  out <- tryCatch(suppressWarnings(
    system2("netsh", c("advfirewall", "firewall", "show", "rule",
                       paste0("name=", RULE)),
            stdout = TRUE, stderr = FALSE)), error = function(e) character())
  any(grepl("Rule Name", out, fixed = TRUE))
}

setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                    value = TRUE)[1]))))

# The address helpers are shared with the app, so the terminal and the screen
# can never disagree about where this is.
source(file.path("R", "net.R"))

host <- mdns_name()
ips <- lan_addresses()
net <- network_profile()
have_rule <- rule_exists()

cat("\n  Cal Poly Scout App\n\n")
if (nzchar(host))
  cat(sprintf("  %-18s http://%s:%d\n", "on the tablets", host, port))
if (length(ips)) {
  for (i in seq_along(ips))
    cat(sprintf("  %-18s http://%s:%d\n",
                if (i == 1) "or by address" else "", ips[i], port))
} else {
  cat("  No network address found. Check the laptop is on the router.\n")
}
cat(sprintf("  %-18s http://localhost:%d\n", "on this laptop", port))

public <- !is.null(net) && identical(net$category, "Public")

if (!is.null(net))
  cat(sprintf("\n  network             %s (%s)\n", net$name, net$category))
if (win)
  cat(sprintf("  firewall rule       %s\n",
              if (isTRUE(have_rule)) "in place" else "none by this name"))

if (public || isFALSE(have_rule)) {
  cat("\n  The tablets may not reach this yet.\n")
  if (public)
    cat("  Windows has this network down as Public, so it refuses them\n",
        "  whatever address they use. Campus and venue Wi-Fi also keeps its\n",
        "  clients from seeing each other, and no setting here fixes that.\n",
        sep = "")
  if (isFALSE(have_rule))
    cat("  Windows may ask to allow R when the first tablet connects. Say\n",
        "  yes to private networks and it is done for every router after.\n",
        sep = "")
  cat("\n  Or settle it now, in an administrator PowerShell:\n")
  if (public && !is.null(net))
    cat(sprintf("    Set-NetConnectionProfile -InterfaceAlias \"%s\" -NetworkCategory Private\n",
                net$iface))
  if (isFALSE(have_rule))
    cat(sprintf("    netsh advfirewall firewall add rule name=%s dir=in action=allow protocol=TCP localport=%d profile=private\n",
                RULE, port))
  cat("  The first is per network and the second is once, ever.\n")
} else {
  cat("\n  Ready for the tablets.\n")
}
cat("\n  Ctrl+C here stops it.\n\n")

shiny::runApp(".", port = port, host = "0.0.0.0", launch.browser = FALSE)
