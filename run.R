# ---------------------------------------------------------------------------
# Serve scoutR to the room.
#
# At a match the laptop codes into VolleyStation and the bench reads off iPads
# on the same router, which is the setup VolleyStation's own sharing assumes.
# This does the same thing: bind every interface rather than loopback, then
# print the address to type into Safari.
#
#   Rscript run.R          serve on the LAN, port 7815
#   Rscript run.R 8080     a different port
#
# Running it over a venue's guest Wi-Fi is not the same thing. Guest networks
# usually isolate clients from each other, so the tablets will not reach the
# laptop however right the address looks. The match router is the reliable
# path, and it does not need internet, only to be a network.
# ---------------------------------------------------------------------------

port <- suppressWarnings(as.integer(commandArgs(TRUE)[1]))
if (is.na(port)) port <- 7815L

#' Every address this machine answers on, best guess first
#'
#' Reads the routing table rather than the interface list, because a laptop in
#' a gym typically holds several: the match router, a campus connection it has
#' not dropped, and whatever a dock adds. The one with a default route is the
#' one packets leave by, and the one the tablets are most likely on.
lan_addresses <- function() {
  ip <- character()
  if (.Platform$OS.type == "windows") {
    out <- tryCatch(system2("powershell",
      c("-NoProfile", "-Command",
        "Get-NetIPAddress -AddressFamily IPv4 | Select-Object -ExpandProperty IPAddress"),
      stdout = TRUE, stderr = FALSE), error = function(e) character())
    ip <- trimws(out)
  } else {
    out <- tryCatch(system2("hostname", "-I", stdout = TRUE, stderr = FALSE),
                    error = function(e) character())
    ip <- unlist(strsplit(trimws(out), "\\s+"))
  }
  ip <- ip[grepl("^[0-9]{1,3}([.][0-9]{1,3}){3}$", ip)]
  ip <- ip[!grepl("^(127[.]|169[.]254[.])", ip)]
  # A router hands out a private block. Anything else is likelier to be a
  # campus or VPN address that the tablets cannot see.
  private <- grepl("^(10[.]|192[.]168[.]|172[.](1[6-9]|2[0-9]|3[01])[.])", ip)
  c(ip[private], ip[!private])
}

#' The network this machine is on, and whether Windows trusts it
#'
#' Worth printing because the two ways this fails look identical from the
#' bench, and neither is the app's fault. A campus or venue network isolates
#' its clients, so the tablets cannot reach the laptop at all; and Windows
#' blocks inbound connections on a network it has filed as Public, which is
#' what it files every network it was not told to trust. The match router,
#' marked private, has neither problem.
network_profile <- function() {
  if (.Platform$OS.type != "windows") return(NULL)
  out <- tryCatch(system2("powershell",
    c("-NoProfile", "-Command",
      "Get-NetConnectionProfile | ForEach-Object { $_.Name + '|' + $_.NetworkCategory }"),
    stdout = TRUE, stderr = FALSE), error = function(e) character())
  out <- trimws(out[nzchar(trimws(out))])
  if (!length(out)) return(NULL)
  parts <- strsplit(out[1], "|", fixed = TRUE)[[1]]
  list(name = parts[1], category = if (length(parts) > 1) parts[2] else "")
}

setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                    value = TRUE)[1]))))

ips <- lan_addresses()
cat("\n  Cal Poly Scout App\n")
cat(sprintf("  on this laptop      http://localhost:%d\n", port))
if (length(ips)) {
  for (i in seq_along(ips))
    cat(sprintf("  %-18s http://%s:%d\n",
                if (i == 1) "on the network" else "", ips[i], port))
} else {
  cat("  No network address found. Check the laptop is on the match router.\n")
}
net <- network_profile()
if (!is.null(net)) {
  cat(sprintf("\n  network             %s (%s)\n", net$name, net$category))
  if (identical(net$category, "Public")) {
    cat("\n  Windows has this network down as Public, so it will refuse the\n")
    cat("  tablets whatever address they use. A campus or venue network also\n")
    cat("  tends to keep its clients from seeing each other at all. Put the\n")
    cat("  laptop on the match router for a match.\n")
  }
}
cat("\n  Windows asks to allow R through the firewall the first time.\n")
cat("  Say yes to private networks. Ctrl+C here stops it.\n\n")

shiny::runApp(".", port = port, host = "0.0.0.0", launch.browser = FALSE)
