# ---------------------------------------------------------------------------
# Statbook
#
# A scout report for coded volleyball. Drop one or more DataVolley .dvw files
# in, pick the sets worth counting, and read the box scores.
#
# shiny and bslib; the statistics are plain base R. The readers and the
# counting live in R/, which Shiny sources on startup.
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)

EV <- c("#", "+", "!", "-", "/", "=")

note <- function(...) div(class = "text-body-secondary small mb-2", ...)

#' A card holding a table, with the explanation above it. `fill = FALSE` so a
#' long table makes the page longer instead of growing a scrollbar of its own.
table_card <- function(title, id, ...) {
  card(card_header(title), note(...), tableOutput(id), fill = FALSE)
}

ui <- page_sidebar(
  title = "Statbook",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter")),
  sidebar = sidebar(
    width = 320,
    fileInput("files", "DataVolley files", accept = c(".dvw", ".txt"), multiple = TRUE),
    uiOutput("side_ui"),
    radioButtons("kind", "Sets to count", inline = FALSE,
                 c("Played-out sets only" = "game", "Anything with a score" = "scored",
                   "Everything" = "all"), selected = "all"),
    uiOutput("sets_ui"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Pass grades",
        note("The 0 to 4 scale every pass average and PGP% is built from. ",
             "Change a weight and the whole report follows your convention."),
        lapply(seq_along(EV), function(i)
          numericInput(paste0("w", i), EV[i], GRADE_DEFAULT[[i]], 0, 4, 0.5))),
      accordion_panel(
        "Leave players out",
        note("Practice players skew every team rate. Their touches leave the ",
             "stream entirely, so the rallies keep their shape."),
        uiOutput("excl_ui"))),
    downloadButton("csv", "Box score CSV", class = "btn-sm")),

  uiOutput("scope"),
  navset_card_tab(
    id = "tab",
    nav_panel("Overview",
      uiOutput("kpis"),
      card(card_header("Session shape"), plotOutput("plot_sets", height = "320px"),
           full_screen = TRUE, fill = FALSE),
      table_card("Set by set", "by_set",
                 "Each set in the selection, whether or not it is in the current filter.")),
    nav_panel("Attacking",
      table_card("Attacking", "box_atk",
                 "Best to worst on Eff, which is (kills - errors - blocked) / attempts ",
                 "with blocks counted as errors, matching NCAA convention. Pts is kills ",
                 "plus aces plus winning blocks. Under ten attempts sits below the rule."),
      card(card_header("Efficiency by player"), plotOutput("plot_atk", height = "360px"),
           full_screen = TRUE, fill = FALSE),
      table_card("By set call", "combo",
                 "Descriptions come from the attack combination table inside the file ",
                 "when it has one, otherwise from the house guide. The grouping always ",
                 "comes from the guide. A tick marks a reading somebody checked against ",
                 "video; unconfirmed means the guide supplied it and nobody has."),
      table_card("By group", "combo_group", "The same attacks, gathered by who hit them."),
      table_card("First ball and transition", "phase",
                 "A rally the team received is theirs to side out. The first swing off ",
                 "that reception is the first-ball chance; every swing after it in the ",
                 "same rally is transition."),
      table_card("How rallies ended", "ends",
                 "The last character of a play code, where the scout recorded one.")),
    nav_panel("Serve and pass",
      table_card("Serving", "box_srv",
                 "Toughest server first, on oPGP: the average pass the receiver managed ",
                 "off her serve, on the 0 to 4 scale, so lower is better. PS% is the ",
                 "share of serves that held the receiver to a 2 or worse, aces counted ",
                 "as 0. Under eight serves sits below the rule."),
      table_card("Serve types", "srv_type", "The skill-type character on each serve code."),
      table_card("Passing", "box_rec",
                 "Best to worst on AVG. PGP% is passes graded 3 or 4. Under eight ",
                 "receptions sits below the rule. The weights are editable in the sidebar."),
      card(card_header("The passes each serve forced"),
           note("Across the whole side: how often a serve held the receiver to each grade."),
           plotOutput("plot_forced", height = "280px"), full_screen = TRUE, fill = FALSE),
      table_card("Serve and reception together", "srv_rec",
                 "One row per player who did either, with the side's own line underneath.")),
    nav_panel("Setting",
      table_card("Setter distribution", "setter_calls",
                 "Every attack is credited to the setter who fed it, meaning the last set ",
                 "by that team before the swing. Attacks with no set in front of them ",
                 "belong to nobody and are left out. Only players the file lists as ",
                 "setters appear here, and only the four with the most volume."),
      table_card("By target", "setter_targets",
                 "Counted off the set rows themselves, so this includes sets the attack ",
                 "code never got recorded for. The fuller picture of where she goes."),
      table_card("Setting and blocking", "box_blk", "Assists, stuff blocks, covers and set errors.")),
    nav_panel("Defense",
      table_card("Digging", "box_dig",
                 "Best to worst on Conv%, the share of her digs that led to an attack by ",
                 "her own team in that rally. Succ% is the share that were not errors. ",
                 "Eff is shaped like hitting percentage: (converted - errors - overs) / ",
                 "attempts. Kill% is how often the attack off her dig was put away. ",
                 "A dig that leads to no attack may mean a poor dig or simply a rally ",
                 "that was not coded further, so read Conv% alongside the raw counts."),
      table_card("Rotation", "rot",
                 "Named by the setter's position at the start of the rally, so S1 means ",
                 "the setter is in position 1. Small samples move these numbers a lot; ",
                 "the attempt columns keep that visible.")),
    nav_panel("Player",
      uiOutput("player_ui"),
      uiOutput("player_kpis"),
      table_card("What she does", "player_skills", "One row per skill she performed."),
      table_card("Attacks by call", "player_combo", "Only swings with a call recorded."),
      table_card("Session by session", "player_sets",
                 "One row per set, or per file when several are loaded.")),
    nav_panel("Trends",
      table_card("Across the selection", "trend",
                 "One row per set, or per file when several are loaded. Load more files ",
                 "to see movement."),
      card(card_header("Attack efficiency and pass average"),
           plotOutput("plot_trend", height = "300px"), full_screen = TRUE, fill = FALSE)),
    nav_panel("Notes",
      table_card("Marked to fix", "tofix",
                 "Plays entered against player 00, meaning the scout knew the code was ",
                 "incomplete. These count toward team totals but not toward any ",
                 "individual, so anything still listed here is missing from a player's line."),
      table_card("Flagged for the coach", "flagged",
                 "Fully coded plays tagged on the way past, with the clock time so you ",
                 "can find them on video. Here the tag is the point, not a gap in the data."),
      table_card("Sets in the file", "sets_tbl",
                 "How each set was classified, and why. The call is made on the weight of ",
                 "the evidence: whether score was kept, whether it reached a real ",
                 "scoreline, how long the rallies ran, and how fast the serves came."),
      table_card("Code definitions in use", "defs",
                 "Every number in this report is built from this mapping."),
      table_card("Codes not in the mapping", "unknown",
                 "Counted as neutral, which makes them invisible in the rates above."),
      table_card("Rows that are not scout codes", "malformed",
                 "Skipped entirely, so nothing above counts them."))))

server <- function(input, output, session) {

  grade <- reactive({
    g <- sapply(seq_along(EV), function(i) input[[paste0("w", i)]])
    if (any(sapply(g, is.null)) || any(is.na(unlist(g)))) return(GRADE_DEFAULT)
    setNames(unlist(g), EV)
  })

  # --- the files ----------------------------------------------------------

  loaded <- reactive({
    req(input$files)
    # A file that is not a scout file says so in a line, rather than throwing a
    # stack trace across the page. VolleyMetrics will happily hand you its own
    # JSON export under a .dvw name, and that is the usual way in.
    out <- lapply(seq_len(nrow(input$files)), function(i)
      tryCatch(read_dvw(input$files$datapath[i], sub("\\.[^.]+$", "", input$files$name[i])),
               error = function(e) conditionMessage(e)))
    bad <- vapply(out, is.character, logical(1))
    validate(need(!any(bad), paste(unlist(out[bad]), collapse = "\n")))
    out
  })

  roster <- reactive({
    r <- do.call(rbind, lapply(loaded(), `[[`, "roster"))
    r[!duplicated(paste(r$side, r$number)), ]
  })

  teams <- reactive(loaded()[[1]]$teams)
  file_combos <- reactive(do.call(rbind, lapply(loaded(), `[[`, "combos")))

  #' Every play from every file, with set numbers made unique per file so two
  #' files both carrying a "set 3" do not collide.
  plays <- reactive({
    d <- do.call(rbind, lapply(loaded(), function(x) x$plays))
    multi <- length(loaded()) > 1
    d$set_id <- if (multi) paste(d$file, d$set) else paste("Set", d$set)
    d[!is.na(d$set), ]
  })

  #' What each set is: a played-out set, a scored block, or drill work.
  set_kinds <- reactive({
    d <- plays()
    ids <- unique(d$set_id)
    k <- lapply(ids, function(s) classify_set(d[d$set_id == s, ]))
    names(k) <- ids
    k
  })

  wanted <- reactive({
    k <- set_kinds()
    ids <- names(k)
    switch(input$kind,
           game = ids[sapply(k, `[[`, "kind") == "game"],
           scored = ids[sapply(k, `[[`, "kind") %in% c("game", "scrimmage")],
           ids)
  })

  output$sets_ui <- renderUI({
    ids <- names(set_kinds())
    checkboxGroupInput("sets", "Sets", choices = ids, selected = wanted())
  })

  output$side_ui <- renderUI({
    ch <- setNames(c("*", "a"), teams())
    # Only an intra-squad file is one squad written down twice.
    if (rosters_match(roster())) ch <- c(ch, "Both sides as one squad" = "both")
    radioButtons("view", "Showing", ch)
  })

  output$excl_ui <- renderUI({
    r <- roster()
    selectInput("excl", NULL, multiple = TRUE,
                choices = setNames(sort(unique(r$number)), paste0("#", sort(unique(r$number)))))
  })

  chosen <- reactive({
    d <- plays()
    d <- d[d$set_id %in% input$sets, ]
    validate(need(nrow(d) > 0, "No sets selected."))
    if (length(input$excl))
      d <- d[!(d$kind == "skill" & !is.na(d$number) & d$number %in% as.integer(input$excl)), ]
    d
  })

  # --- the numbers --------------------------------------------------------

  S <- reactive({
    s <- compute(chosen(), roster(), grade())
    s$file_combos <- file_combos()
    s
  })

  #' The same counters one set at a time, for the trend and set-by-set tables.
  per_set <- reactive({
    d <- chosen()
    ids <- unique(d$set_id)
    out <- lapply(ids, function(s) compute(d[d$set_id == s, ], roster(), grade()))
    names(out) <- ids
    out
  })

  view <- reactive(if (is.null(input$view)) "*" else input$view)
  team_line <- reactive(pick(S(), view())$team)

  output$scope <- renderUI({
    s <- S(); T <- team_line(); k <- set_kinds()
    div(class = "pt-2 pb-1 text-body-secondary small",
        sprintf("%s · %s · %d of %d sets · %d rallies · %s",
                paste(unname(teams()), collapse = " vs "),
                if (view() == "both") "both sides as one squad" else unname(teams()[view()]),
                length(input$sets), length(k), s$n_rallies,
                if (s$scored >= 0.5) sprintf("score kept on %.0f%% of rallies", 100 * s$scored)
                else "almost no score kept, so sideout rates are hidden"),
        # Sides are * and a in every file; only the names come from the first.
        if (length(loaded()) > 1)
          div(class = "text-body-secondary small",
              sprintf("%d files pooled. The two side names come from %s; check the other files put the same team on each side.",
                      length(loaded()), loaded()[[1]]$label)))
  })

  output$kpis <- renderUI({
    T <- team_line(); s <- S()
    p <- if (!is.null(s$phase) && view() != "both") s$phase[s$phase$side == view(), ] else NULL
    sets <- max(T$sets, 1)
    box <- function(v, lab, sub) value_box(title = lab, value = v, p(class = "small", sub),
                                           theme = "primary")
    v <- list(
      box(trimws(eff(T$atk_kill, T$atk_err, T$atk_blocked, T$atk_att)), "hitting %",
          sprintf("%d kills, %d errors, %d attempts", T$atk_kill,
                  T$atk_err + T$atk_blocked, T$atk_att)),
      box(avg(T$grade, T$graded), "pass average",
          sprintf("0 to 4 scale · %s positive", pct(T$rec_positive, T$rec_att, 1))))
    if (!is.null(p) && s$scored >= 0.5) v <- c(v, list(
      box(pct(p$sow, p$so), "sideout %", sprintf("%d of %d receptions", p$sow, p$so)),
      box(pct(p$fbso, p$so), "first-ball sideout", "won on the first attack")))
    v <- c(v, list(
      box(T$srv_ace, "aces", sprintf("%d serve errors on %d", T$srv_err, T$srv_att)),
      box(avg(T$blk_stuff, sets), "blocks per set", sprintf("%d total", T$blk_stuff))))
    layout_columns(!!!v, col_widths = 4)
  })

  # --- tables -------------------------------------------------------------

  show <- function(f) renderTable({
    t <- f()
    validate(need(!is.null(t) && nrow(t) > 0, "Nothing recorded for this in the selection."))
    t
  }, striped = TRUE, spacing = "xs", na = "")

  output$box_atk <- show(reactive(box_attacking(S(), view())))
  output$box_srv <- show(reactive(box_serving(S(), view())))
  output$box_rec <- show(reactive(box_passing(S(), view())))
  output$box_blk <- show(reactive(box_blocking(S(), view())))
  output$box_dig <- show(reactive(box_digging(S(), view())))
  output$srv_rec <- show(reactive(tbl_serve_rec(S(), view())))
  output$srv_type <- show(reactive(tbl_serve_types(S(), view())))
  output$combo <- show(reactive(tbl_combo(S(), view(), file_combos())))
  output$combo_group <- show(reactive(tbl_combo_group(S(), view(), file_combos())))
  output$ends <- show(reactive(tbl_ends(S(), view())))
  output$phase <- show(reactive(tbl_phase(S(), view())))
  output$setter_calls <- show(reactive(tbl_setter_calls(S(), setter_list(S()), file_combos())))
  output$setter_targets <- show(reactive(tbl_setter_targets(S(), setter_list(S()))))
  output$defs <- show(reactive(tbl_defs(grade())))
  output$tofix <- show(reactive(tbl_flagged(S(), roster(), "fix")))
  output$flagged <- show(reactive(tbl_flagged(S(), roster(), "coach")))

  output$rot <- renderTable({
    validate(need(view() != "both",
                  "Rotation only means something per side. Pick one side to see it."))
    validate(need(S()$scored >= 0.5, sprintf(
      "Only %.0f%% of rallies in this selection end in a point code, so sideout and point rates would rest on a fraction of the play.",
      100 * S()$scored)))
    tbl_rotation(S(), view())
  }, striped = TRUE, spacing = "xs")

  output$unknown <- show(reactive({
    u <- tbl_unknown(S())
    if (!length(u)) return(NULL)
    do.call(rbind, lapply(names(u), function(k)
      data.frame(What = k, u[[k]], check.names = FALSE)))
  }))

  output$malformed <- show(reactive({
    d <- plays()
    bad <- d[d$kind == "other" & !grepl("^([*a])(P\\d+|z\\d)|^\\*\\*\\dset|^>", d$code), ]
    if (!nrow(bad)) return(NULL)
    t <- sort(table(gsub("[0-9]", "#", bad$code)), decreasing = TRUE)
    data.frame(Shape = names(t), n = as.integer(t),
               Sets = sapply(names(t), function(p)
                 paste(unique(bad$set_id[gsub("[0-9]", "#", bad$code) == p]), collapse = ", ")),
               check.names = FALSE, row.names = NULL)
  }))

  output$sets_tbl <- show(reactive({
    k <- set_kinds()
    data.frame(Set = names(k), Kind = sapply(k, `[[`, "kind"),
               Score = format(sapply(k, `[[`, "score")),
               Rallies = format(sapply(k, `[[`, "rallies")),
               Why = sapply(k, `[[`, "why"),
               Counted = ifelse(names(k) %in% input$sets, "yes", ""),
               check.names = FALSE, row.names = NULL)
  }))

  #' Team numbers one set at a time. Used for both the overview and trends.
  by_set_table <- reactive({
    out <- lapply(names(per_set()), function(id) {
      s <- per_set()[[id]]; T <- pick(s, view())$team
      p <- if (!is.null(s$phase) && view() != "both") s$phase[s$phase$side == view(), ] else NULL
      data.frame(Set = id, Att = T$atk_att, Kills = T$atk_kill,
                 `Hit%` = trimws(eff(T$atk_kill, T$atk_err, T$atk_blocked, T$atk_att)),
                 `Sideout%` = if (!is.null(p) && s$scored >= 0.5) pct(p$sow, p$so) else DASH,
                 `Pass avg` = avg(T$grade, T$graded), Aces = T$srv_ace,
                 `Serve err` = T$srv_err, `Atk err` = T$atk_err + T$atk_blocked,
                 Digs = T$dig_att - T$dig_err, Assists = T$assists, Blocks = T$blk_stuff,
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  })

  output$by_set <- show(by_set_table)
  output$trend <- show(by_set_table)

  # --- one player ---------------------------------------------------------

  output$player_ui <- renderUI({
    P <- pick(S(), view())$players
    P <- P[P$atk_att + P$srv_att + P$rec_att + P$dig_att + P$set_att + P$blk_att > 0, ]
    selectInput("player", "Player", setNames(P$number, P$label), width = "320px")
  })

  pline <- reactive({
    req(input$player)
    L <- player_line(S(), view(), as.integer(input$player))
    validate(need(nrow(L) == 1, "She has no touches in this selection."))
    L
  })

  output$player_kpis <- renderUI({
    L <- pline(); sets <- max(L$sets, 1)
    v <- list()
    add <- function(val, lab, sub) v[[length(v) + 1]] <<-
      value_box(title = lab, value = val, p(class = "small", sub), theme = "secondary")
    if (L$atk_att) add(trimws(eff(L$atk_kill, L$atk_err, L$atk_blocked, L$atk_att)), "hitting %",
                       sprintf("%d kills on %d", L$atk_kill, L$atk_att))
    if (L$rec_att) add(avg(L$grade, L$graded), "pass average",
                       sprintf("%d receptions · %s good or better", L$rec_att,
                               pct(L$g4 + L$g3, L$graded)))
    if (L$srv_att) add(pct(L$srv_ace, L$srv_att, 1), "ace rate",
                       sprintf("%d aces, %d errors on %d", L$srv_ace, L$srv_err, L$srv_att))
    if (L$opp_graded) add(avg(L$opp_grade, L$opp_graded), "pass forced",
                          "average pass off her serve")
    if (L$assists) add(L$assists, "assists", sprintf("%s per set", avg(L$assists, sets)))
    if (L$dig_att) add(pct(L$dig_conv, L$dig_att), "dig conversion",
                       sprintf("%d digs, %d led to an attack", L$dig_att - L$dig_err, L$dig_conv))
    if (L$blk_att) add(L$blk_stuff, "stuff blocks",
                       sprintf("%d touches, %d errors", L$blk_touch, L$blk_err))
    add(L$points, "points", "kills, aces and blocks")
    tagList(
      note(sprintf("%s, #%d%s · %d sets · %d touches", L$label, L$number,
                   if (nzchar(L$position)) paste0(", ", L$position) else "", L$sets,
                   L$atk_att + L$srv_att + L$rec_att + L$dig_att + L$set_att + L$blk_att)),
      layout_columns(!!!v[seq_len(min(6, length(v)))], col_widths = 4))
  })

  output$player_skills <- show(reactive(tbl_player_skills(pline())))
  output$player_combo <- show(reactive(
    tbl_player_combo(S(), view(), as.integer(input$player), file_combos())))

  output$player_sets <- show(reactive({
    n <- as.integer(input$player)
    out <- lapply(names(per_set()), function(id) {
      L <- player_line(per_set()[[id]], view(), n)
      if (!nrow(L)) return(NULL)
      data.frame(Set = id, Att = L$atk_att, K = L$atk_kill,
                 `Hit%` = trimws(eff(L$atk_kill, L$atk_err, L$atk_blocked, L$atk_att)),
                 Rec = L$rec_att, Pass = avg(L$grade, L$graded), Serves = L$srv_att,
                 Aces = L$srv_ace, Ast = L$assists, Digs = L$dig_att - L$dig_err,
                 Blocks = L$blk_stuff, check.names = FALSE, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, out)
    if (is.null(out) || !nrow(out)) return(NULL)
    # Drop the sets she did not appear in at all.
    keep <- apply(out[, -1, drop = FALSE], 1,
                  function(r) any(r != 0 & r != DASH & nzchar(r)))
    out[keep, , drop = FALSE]
  }))

  # --- plots --------------------------------------------------------------

  output$plot_sets <- renderPlot({
    t <- by_set_table()
    e <- as_num(t$`Hit%`); p <- as_num(t$`Pass avg`)
    validate(need(any(!is.na(e)), "No attacks coded in this selection."))
    e[is.na(e)] <- 0
    par(mar = c(5, 4, 1, 4))
    plot(seq_len(nrow(t)), e, type = "b", pch = 19, col = "#2C3E50", xaxt = "n",
         xlab = "", ylab = "attack efficiency", ylim = range(c(0, e), na.rm = TRUE))
    axis(1, seq_len(nrow(t)), t$Set, las = 2, cex.axis = 0.8)
    abline(h = 0, col = "grey70")
    par(new = TRUE)
    plot(seq_len(nrow(t)), p, type = "b", pch = 17, col = "#18BC9C", axes = FALSE,
         xlab = "", ylab = "", ylim = c(0, 4))
    axis(4); mtext("pass average", 4, 3)
    legend("topright", c("attack eff", "pass avg"), col = c("#2C3E50", "#18BC9C"),
           pch = c(19, 17), bty = "n")
  }, bg = "transparent")

  output$plot_atk <- renderPlot({
    P <- pick(S(), view())$players
    P <- P[P$atk_att >= 5, ]
    validate(need(nrow(P) > 0, "Nobody has five attempts in this selection."))
    P <- P[order(eff_num(P$atk_kill, P$atk_err, P$atk_blocked, P$atk_att)), ]
    par(mar = c(4, 13, 1, 2))
    barplot(eff_num(P$atk_kill, P$atk_err, P$atk_blocked, P$atk_att),
            names.arg = paste0(P$label, "  (", P$atk_att, ")"), horiz = TRUE, las = 1,
            xlab = "attack efficiency", col = "#2C3E50", border = NA, cex.names = 0.85)
    abline(v = 0)
  }, bg = "transparent")

  output$plot_forced <- renderPlot({
    T <- team_line()
    v <- c(T$f0, T$f1, T$f2, T$f3, T$f4)
    validate(need(sum(v) > 0, "No serve in this selection was answered by a graded pass."))
    par(mar = c(4, 4, 1, 2))
    b <- barplot(v, names.arg = paste("pass", 0:4), col = "#18BC9C", border = NA,
                 ylab = "serves", xlab = "what the receiver managed")
    text(b, v, v, pos = 3, xpd = NA)
  }, bg = "transparent")

  output$plot_trend <- renderPlot({
    t <- by_set_table()
    e <- as_num(t$`Hit%`)
    validate(need(any(!is.na(e)), "No attacks coded in this selection."))
    e[is.na(e)] <- 0
    par(mar = c(6, 4, 1, 2))
    barplot(e, names.arg = t$Set, las = 2, col = ifelse(e >= 0, "#2C3E50", "#E74C3C"),
            border = NA, ylab = "attack efficiency", cex.names = 0.8)
    abline(h = 0)
  }, bg = "transparent")

  output$csv <- downloadHandler(
    filename = function() paste0("box_score_", Sys.Date(), ".csv"),
    content = function(path) write.csv(csv_box(S(), teams()), path, row.names = FALSE))
}

shinyApp(ui, server)
