# ---------------------------------------------------------------------------
# Where this machine can be reached
#
# Shared by run.R, which prints the addresses before the app starts, and by
# the app itself, which shows them with a QR code for the tablets. One
# implementation, so the terminal and the screen can never disagree about
# where the thing is.
# ---------------------------------------------------------------------------

net_ps <- function(cmd) {
  out <- tryCatch(suppressWarnings(
    system2("powershell", c("-NoProfile", "-Command", cmd),
            stdout = TRUE, stderr = FALSE)), error = function(e) character())
  trimws(out[nzchar(trimws(out))])
}

#' The name the tablets can use on any network
#'
#' Windows answers mDNS and an iPad speaks it natively, so <machine>.local
#' finds the laptop without anyone reading an IP off a screen. This is the
#' address worth keeping, because it survives changing routers. Some routers
#' do not pass mDNS, which is what the numeric addresses are for.
mdns_name <- function() {
  h <- tryCatch(unname(Sys.info()[["nodename"]]), error = function(e) "")
  if (!nzchar(h)) return("")
  tolower(paste0(h, ".local"))
}

#' Every address this machine answers on, best guess first
#'
#' All of them rather than one, because a laptop in a gym typically holds
#' several: the match router, a campus connection it has not dropped, and
#' whatever a dock adds. A router hands out a private block, so those come
#' first, but not certainly, which is why they are all kept.
lan_addresses <- function() {
  ip <- if (.Platform$OS.type == "windows") {
    net_ps("Get-NetIPAddress -AddressFamily IPv4 | Select-Object -ExpandProperty IPAddress")
  } else {
    unlist(strsplit(trimws(paste(tryCatch(
      system2("hostname", "-I", stdout = TRUE, stderr = FALSE),
      error = function(e) ""), collapse = " ")), "\\s+"))
  }
  ip <- ip[grepl("^[0-9]{1,3}([.][0-9]{1,3}){3}$", ip)]
  ip <- ip[!grepl("^(127[.]|169[.]254[.])", ip)]
  private <- grepl("^(10[.]|192[.]168[.]|172[.](1[6-9]|2[0-9]|3[01])[.])", ip)
  c(ip[private], ip[!private])
}

#' The addresses to hand someone else, name first
lan_urls <- function(port) {
  hosts <- c(mdns_name(), lan_addresses())
  hosts <- hosts[nzchar(hosts)]
  if (!length(hosts)) return(character())
  sprintf("http://%s:%s", hosts, port)
}

#' The network this machine is on, and whether Windows trusts it
#'
#' Windows blocks inbound connections on any network it has filed as Public,
#' which is every network it was not told to trust, and campus and venue
#' Wi-Fi usually keeps its clients from seeing each other on top of that. A
#' router marked private has neither problem. This is per network, so a new
#' router starts Public and has to be told once.
network_profile <- function() {
  if (.Platform$OS.type != "windows") return(NULL)
  out <- net_ps("Get-NetConnectionProfile | ForEach-Object { $_.Name + '|' + $_.InterfaceAlias + '|' + $_.NetworkCategory }")
  if (!length(out)) return(NULL)
  p <- strsplit(out[1], "|", fixed = TRUE)[[1]]
  list(name = p[1], iface = p[2], category = if (length(p) > 2) p[3] else "")
}
