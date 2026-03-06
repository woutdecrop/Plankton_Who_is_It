library(shiny)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(readxl)
library(shinycssloaders)
library(digest)

# 1. SETUP
base_dir <- "./Images" 
addResourcePath("plankton_imgs", normalizePath(base_dir, winslash = "/", mustWork = FALSE))

species_df <- if(file.exists(file.path(base_dir, "species_list.xlsx"))) read_excel(file.path(base_dir, "species_list.xlsx")) else data.frame()

# --- HELPER: COLOR CONFIG ---
device_colors <- c(
  "FLOWCAM" = "#e74c3c", "ZOOSCAN" = "#3498db", "UVP6" = "#9b59b6", 
  "PI10" = "#f1c40f", "VPR" = "#1abc9c", "DEFAULT" = "#95a5a6"
)

get_type_color <- function(device) {
  d <- toupper(device)
  match <- names(device_colors)[sapply(names(device_colors), function(x) grepl(x, d))]
  if(length(match) > 0) return(device_colors[[match[1]]])
  return(device_colors[["DEFAULT"]])
}

get_web_path <- function(relative_path) {
  return(file.path("plankton_imgs", relative_path))
}

get_balanced_pool <- function(n_cards = NULL) {
  all_paths <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  device_dirs <- all_paths[basename(all_paths) != basename(base_dir)]
  
  if(length(device_dirs) == 0) return(character(0))
  
  mandatory_picks <- character(0)
  remaining_pool <- character(0)
  
  for (d_path in device_dirs) {
    dev_name <- basename(d_path)
    taxa_dirs <- list.dirs(d_path, full.names = TRUE, recursive = FALSE)
    
    if (length(taxa_dirs) > 0) {
      taxa_dirs <- sample(taxa_dirs)
      found_first <- FALSE
      for (t_idx in seq_along(taxa_dirs)) {
        t_path <- taxa_dirs[t_idx]
        imgs <- list.files(t_path, pattern = "\\.(jpg|png|jpeg)$", ignore.case = TRUE)
        
        if (length(imgs) > 0 && !found_first) {
          mandatory_picks <- c(mandatory_picks, file.path(dev_name, basename(t_path), sample(imgs, 1)))
          found_first <- TRUE
        } else if (length(imgs) > 0) {
          remaining_pool <- c(remaining_pool, file.path(dev_name, basename(t_path), sample(imgs, 1)))
        }
      }
    }
  }
  
  total_pool <- c(mandatory_picks, sample(remaining_pool))
  if (!is.null(n_cards) && is.numeric(n_cards)) return(head(total_pool, n_cards))
  return(total_pool)
}

get_random_name <- function() {
  titles <- c("Scientist", "Doctor", "Researcher", "Explorer", "Diver", "Captain", "Professor")
  names <- c("Sarah", "Johannes", "Bram", "Annika", "Clara", "Arienne", "Wout", "David","Julia")
  return(paste(sample(titles, 1), sample(names, 1)))
}

get_random_color <- function() {
  colors <- c("#2980b9", "#c0392b", "#27ae60", "#8e44ad", "#d35400", "#16a085", "#34495e")
  return(sample(colors, 1))
}

get_random_emoji <- function() {
  emojis <- c("🔬","🧬","🦀","🌊","🦠","🐳","🦑","🐡","⚓")
  return(sample(emojis, 1))
}

get_metadata <- function(path) {
  parts <- strsplit(path, "[/\\\\]")[[1]]
  folder1 <- if (length(parts) >= 1) parts[1] else NA_character_
  folder2 <- if (length(parts) >= 2) parts[2] else NA_character_
  
  meta_parts <- strsplit(folder1, "_")[[1]]
  dev <- if (length(meta_parts) >= 1) meta_parts[1] else "UnknownDevice"
  loc <- if (length(meta_parts) >= 2) meta_parts[2] else "General"
  spec <- if (!is.na(folder2) && nzchar(folder2)) folder2 else "Unknown"
  
  excel_info <- list()
  if (nrow(species_df) > 0) {
    item_col <- names(species_df)[1] 
    if (spec %in% species_df[[item_col]]) {
      match_row <- species_df[species_df[[item_col]] == spec, ][1, , drop = FALSE]
      blacklist <- c("species","device","location","loc","dev","item","sensor","region")
      cols <- names(species_df)[!tolower(names(species_df)) %in% blacklist]
      for (c in cols) {
        val <- as.character(match_row[[c]])
        if (!is.na(val) && nzchar(val)) excel_info[[c]] <- val
      }
    }
  }
  list(device = dev, location = loc, species = spec, excel = excel_info)
}

# 2. SHARED STATE
shared_store <- reactiveValues(
  chat = data.frame(User=character(0), Msg=character(0), Color=character(0), Emoji=character(0), stringsAsFactors=F),
  scoreboard = data.frame(Scientist=character(0), Wins=numeric(0), Streak=numeric(0), Emoji=character(0), stringsAsFactors=F),
  player_secrets = list(),
  game_images = character(0),
  reset_counter = 0,
  game_size = 12
)

# 3. UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background-color: #f4f7f6; font-family: 'Segoe UI', sans-serif; }
    .game-container { display: flex; flex-wrap: wrap; margin: 0 -10px; }
    .card-wrapper { width: 16.66%; padding: 0 10px; box-sizing: border-box; }
    .plankton-card { cursor: pointer; border: 5px solid #fff; padding: 15px; background: white; border-radius: 12px; margin-bottom: 20px; transition: 0.2s; min-height: 350px; display: flex; flex-direction: column; position: relative; }
    .plankton-img { width: 100%; height: 140px; object-fit: contain; background: #fafafa; border-radius: 5px; flex-shrink: 0; }
    .species-name { font-weight: bold; font-size: 14px; margin-top: 10px; min-height: 32px; line-height: 1.2; color: #2c3e50; }
    .hidden-card { opacity: 0.15; filter: grayscale(80%); }
    .guess-active { border: 5px solid #f1c40f !important; box-shadow: 0 0 15px #f1c40f; }
    .zoom-btn { position: absolute; top: 5px; right: 5px; background: rgba(255,255,255,0.8); border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd; color: #333; z-index: 10; font-size: 12px; }
    .filter-btn { cursor: pointer; padding: 4px 10px; border-radius: 20px; margin: 2px; border: 1px solid transparent; display: inline-block; font-size: 10px; font-weight: bold; color: white; transition: 0.2s; }
    .active-filter { border: 2px solid white !important; opacity: 0.5; transform: translateY(2px); }
    .count-badge { background: rgba(0,0,0,0.25); padding: 1px 5px; border-radius: 10px; margin-left: 4px; font-size: 9px; }
    .target-container { background: #2c3e50; color: white; padding: 15px; border-radius: 12px; text-align: center; margin-bottom: 10px; }
    .target-img-box { width: 100%; aspect-ratio: 1.5/1; background: white; border-radius: 8px; display: flex; align-items: center; justify-content: center; overflow: hidden; border: 3px solid #f1c40f; }
    .target-img-box img { max-width: 95%; max-height: 95%; object-fit: contain; }
    #chat_box { height: 220px; overflow-y: auto; background: #fff; padding: 10px; border-radius: 8px; border: 1px solid #ddd; width: 100%; box-sizing: border-box; }
    .leader-badge { position: fixed; bottom: 20px; left: 20px; background: #fff; padding: 12px; border-radius: 12px; border: 2px solid #f1c40f; box-shadow: 0 4px 10px rgba(0,0,0,0.1); z-index: 100; }
    .meta-key { font-weight: bold; color: #7f8c8d; font-size: 11px; text-transform: uppercase; margin-right: 4px; }
    .meta-section { border-top: 1px solid #eee; margin-top: auto; padding-top: 10px; font-size: 12px; color: #555; line-height: 1.4; display: flex; flex-direction: column; gap: 4px; }
    .zoom-img { width: 100%; max-height: 550px; object-fit: contain; background: white; }
    .explanation-box { background:#fdf6e3; padding:12px; border-radius:8px; border-left:5px solid #f1c40f; box-shadow: 0 2px 4px rgba(0,0,0,0.05); color:#34495e; font-size: 11px; height: 100%; }
    .top-guess-btn { position: fixed; top: 15px; right: 20px; z-index: 1000; box-shadow: 0 4px 15px rgba(0,0,0,0.2); border: 2px solid white; }
    .step-badge { background: #f39c12; color: white; padding: 2px 8px; border-radius: 10px; font-weight: bold; margin-right: 5px;}
  "))),
  
  uiOutput("floating_guess_ui"),
  
  fluidRow(
    column(2, 
           h2("🚢 Who Is It?", style="margin-top:10px; font-size: 24px;"),
           actionButton("how_to_play", "How to Play?", icon=icon("question-circle"), class="btn-info btn-xs")
    ),
    column(4, 
           div(style="margin-top:10px; height: 135px;", uiOutput("top_explanation"))
    ),
    column(6, 
           div(style="background:#2c3e50; padding:12px; border-radius:8px; margin-top:10px;", 
               uiOutput("filter_buttons"))
    )
  ),
  
  withSpinner(uiOutput("main_content"), type = 6, color = "#2c3e50"),
  uiOutput("leader_badge"),
  
  div(style="position: fixed; bottom: 20px; right: 20px;",
      dropdownButton(label="Hall of Fame", icon=icon("compact-disc"), status="danger", circle=T, tableOutput("win_table")))
)

# 4. SERVER
server <- function(input, output, session) {
  user_data <- reactiveValues(name=NULL, secret_picked=NULL, eliminated=character(), color="#2980b9", emoji="🔬", active_filters=character(), guessing=FALSE, local_reset=0)
  
  # Help Modal Function
  show_help <- function() {
    showModal(modalDialog(
      title = tags$h3("🚢 Mission Briefing: Plankton Who Is It?", style="color:#2980b9"),
      HTML("
        <div style='font-size: 14px; line-height: 1.6;'>
          <p><span class='step-badge'>1</span> <b>Pick Your Secret:</b> Click a card to hide it from your opponent. They must guess this!</p>
          <p><span class='step-badge'>2</span> <b>Ask Questions:</b> Use the chat. Ask things like <i>'Is it a Copepod?'</i> or <i>'Was it caught in Greenland?'</i>.</p>
          <p><span class='step-badge'>3</span> <b>Filter & Eliminate:</b> Use the colored buttons to cross out hardware or groups. Click a card manually to 'grey it out' if it's not the target.</p>
          <p><span class='step-badge'>4</span> <b>The Final Guess:</b> When you are sure, click the <b>Yellow Trophy Button</b> at the top right, then click the card you suspect!</p>
        </div>"),
      easyClose = TRUE,
      footer = modalButton("Ready for Exploration!")
    ))
  }
  
  observeEvent(input$how_to_play, { show_help() })
  
  observe({
    if(shared_store$reset_counter > user_data$local_reset) {
      user_data$secret_picked <- NULL; user_data$eliminated <- character(); user_data$active_filters <- character(); user_data$guessing <- FALSE
      user_data$local_reset <- shared_store$reset_counter
    }
  })
  
  output$floating_guess_ui <- renderUI({
    if(is.null(user_data$secret_picked)) return(NULL)
    actionButton("toggle_guess", 
                 if(user_data$guessing) "❌ CANCEL GUESS" else "🏆 GUESS OPPONENT TARGET", 
                 class=paste("top-guess-btn", if(user_data$guessing) "btn-danger" else "btn-warning"))
  })
  
  output$top_explanation <- renderUI({
    if(is.null(user_data$secret_picked)) return(NULL)
    div(class="explanation-box",
        HTML("<b style='color:#d35400; font-size:14px;'><i class='fa fa-info-circle'></i> Target Selected!</b><br/> 
                        <b>1. Ask</b> in the chat<br/>
                        <b>2. Filter</b> using category buttons<br/>
                        <b>3. Eliminate</b> by clicking cards<br/>
                        <b>4. Guess</b> using the yellow button top right!")
    )
  })
  
  output$filter_buttons <- renderUI({
    visible_pool <- setdiff(shared_store$game_images, user_data$eliminated)
    meta_list <- lapply(visible_pool, get_metadata)
    
    count_trait <- function(val) {
      if(length(meta_list) == 0) return(0)
      sum(sapply(meta_list, function(m) {
        grepl(tolower(val), tolower(m$device)) || 
          grepl(tolower(val), tolower(m$location)) ||
          any(tolower(as.character(unlist(m$excel))) == tolower(val))
      }))
    }
    
    devs <- names(device_colors)[1:5]
    locs <- c("BPNS", "Mediterranean", "Spitsbergen", "Greenland")
    groups <- c("Zooplankton", "Phytoplankton", "Other")
    stages <- c("Adult", "Larvae", "Juvenile", "Not applicable")
    
    filter_js <- "Shiny.setInputValue('filter_click', '%s', {priority: 'event'}); Shiny.setInputValue('filter_click', '');"
    
    tagList(
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 10px;", "HARDWARE:"),
      lapply(devs, function(d) span(class=paste("filter-btn", if(d %in% user_data$active_filters) "active-filter" else ""), style=paste0("background:", device_colors[[d]]), onclick=sprintf(filter_js, d), d, span(class="count-badge", count_trait(d)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 14px;", "LOCATION:"),
      lapply(locs, function(l) span(class=paste("filter-btn", if(l %in% user_data$active_filters) "active-filter" else ""), style="background:#7f8c8d;", onclick=sprintf(filter_js, l), l, span(class="count-badge", count_trait(l)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 27px;", "GROUP:"),
      lapply(groups, function(g) span(class=paste("filter-btn", if(g %in% user_data$active_filters) "active-filter" else ""), style="background:#2ecc71;", onclick=sprintf(filter_js, g), g, span(class="count-badge", count_trait(g)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 15px;", "LIFESTAGE:"),
      lapply(stages, function(s) span(class=paste("filter-btn", if(s %in% user_data$active_filters) "active-filter" else ""), style="background:#e67e22;", onclick=sprintf(filter_js, s), s, span(class="count-badge", count_trait(s))))
    )
  })
  
  msg_send <- function(m){
    shared_store$chat <<- rbind(shared_store$chat, data.frame(User=user_data$name, Msg=m, Color=user_data$color, Emoji=user_data$emoji))
    updateTextInput(session, "chat_msg", value=""); delay(100, runjs("var d=document.getElementById('chat_box'); if(d) d.scrollTop=d.scrollHeight;"))
  }
  
  observeEvent(input$filter_click, {
    req(input$filter_click != "")
    target <- input$filter_click
    user_data$active_filters <- if(target %in% user_data$active_filters) setdiff(user_data$active_filters, target) else c(user_data$active_filters, target)
    matches <- Filter(function(img) {
      m <- get_metadata(img)
      grepl(tolower(target), tolower(m$device)) || 
        grepl(tolower(target), tolower(m$location)) ||
        any(tolower(as.character(unlist(m$excel))) == tolower(target))
    }, shared_store$game_images)
    user_data$eliminated <- if(target %in% user_data$active_filters) unique(c(user_data$eliminated, matches)) else setdiff(user_data$eliminated, matches)
  })
  
  output$main_content <- renderUI({
    if(is.null(user_data$name)){
      wellPanel(style="max-width:450px; margin: 50px auto; text-align:center;",
                h3("Scientist Registration"),
                textInput("user_name_input", "Choose Username:", value=get_random_name()),
                fluidRow(
                  column(6, colourpicker::colourInput("user_color", "Station Color:", get_random_color())),
                  column(6, selectInput("user_emoji", "Avatar Icon:", choices=c("🔬","🧬","🦀","🌊","🦠","🐳","🦑","🐡","⚓"), selected=get_random_emoji()))
                ),
                sliderInput("num_cards_input", "Deck Size (Number of Cards):", min=10, max=24, value=shared_store$game_size, step=2),
                actionButton("login_btn", "Join Expedition", class="btn-primary", width="100%"))
    } else if(is.null(user_data$secret_picked)){
      div(style="padding: 20px;", 
          div(style="display:flex; justify-content:space-between; align-items:center; background:#d9edf7; padding:10px; border-radius:8px; border:1px solid #bce8f1; margin-bottom:15px;", 
              h4(style="margin:0; color:#31708f;", "🔎 STEP 1: Select your Secret Plankton Target"), 
              actionButton("change_user_btn", "Logout", icon=icon("sign-out-alt"), class="btn-danger btn-xs")),
          uiOutput("selection_grid"))
    } else {
      meta_s <- get_metadata(user_data$secret_picked)
      sidebar_meta <- list(span(tags$b("DEVICE:"), meta_s$device), " | ", span(tags$b("LOC:"), meta_s$location))
      if (length(meta_s$excel) > 0) {
        excel_tags <- lapply(names(meta_s$excel), function(n) {
          tagList(" | ", span(tags$b(toupper(n), ":"), meta_s$excel[[n]]))
        })
        sidebar_meta <- c(sidebar_meta, excel_tags)
      }
      
      fluidRow(
        column(3,
               div(class="target-container", style=paste0("border-bottom: 8px solid ", user_data$color),
                   div(class="target-img-box", img(src=get_web_path(user_data$secret_picked))),
                   h4(paste(user_data$emoji, gsub("[+_-]", " ", meta_s$species)), style="color:#f1c40f; font-weight:bold; margin-top:10px;"), 
                   div(style="font-size: 9px; opacity: 0.8; line-height: 1.4;", sidebar_meta)
               ),
               div(style="margin-bottom:10px; padding:10px; background:#e8f4f8; border-radius:8px; border:1px solid #d4e6f1;",
                   div(tags$b("🟢 Online Scientists:"), style="font-size:12px; margin-bottom:5px; color:#2980b9;"),
                   uiOutput("online_users")
               ),
               div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
                   tags$b("CHAT LOG", style="font-size:10px; color:#7f8c8d;"),
                   actionButton("clear_chat", "Clear", class="btn-link", style="padding:0; font-size:10px; color:#e74c3c; text-decoration:none;")
               ),
               div(id="chat_box", lapply(seq_len(nrow(shared_store$chat)), function(i){
                 row <- shared_store$chat[i,]
                 div(style=paste0("border-left:3px solid ", row$Color, "; padding-left:5px; margin-bottom:5px; font-size:11px;"), 
                     tags$b(paste0(row$Emoji, " ", row$User, ":")), row$Msg)
               })),
               div(style="display:flex; margin-top:5px;", textInput("chat_msg", NULL, placeholder="Ask...", width="100%"), actionButton("send_btn", icon("paper-plane"))),
               div(style="margin-top:5px; text-align:center;",
                   actionButton("q_yes", "YES", class="btn-success btn-xs"), actionButton("q_no", "NO", class="btn-danger btn-xs"),
                   div(style="margin-top:10px; margin-bottom:5px; font-weight:bold; font-size:10px; color:#7f8c8d; text-transform:uppercase;", "Quick Questions:"),
                   actionButton("q_zoo", "Zoo?", class="btn-info btn-xs"), actionButton("q_phyto", "Phyto?", class="btn-info btn-xs")
               ),
               hr(),
               actionButton("change_user_btn", "Change User / Icon", class="btn-block btn-info", style="font-size:11px; margin-bottom:5px;"),
               actionButton("reset_btn", "Force New Round", class="btn-block", style="font-size:10px")
        ),
        column(9, uiOutput("game_grid"))
      )
    }
  })
  
  observeEvent(input$change_user_btn, {
    if(!is.null(user_data$name)) shared_store$player_secrets[[user_data$name]] <- NULL
    user_data$name <- NULL
    user_data$secret_picked <- NULL
    user_data$eliminated <- character()
    user_data$active_filters <- character()
    user_data$guessing <- FALSE
  })
  
  observeEvent(input$clear_chat, {
    shared_store$chat <- data.frame(User=character(0), Msg=character(0), Color=character(0), Emoji=character(0), stringsAsFactors=F)
  })
  
  output$leader_badge <- renderUI({
    df <- shared_store$scoreboard
    if(is.null(user_data$name) || nrow(df) == 0) return(NULL)
    top <- df[which.max(df$Wins),]
    div(class="leader-badge", tags$b("⭐ TOP SCIENTIST"), br(), top$Scientist, br(), tags$span(style="color:#27ae60", paste(top$Wins, "Wins")))
  })
  
  create_card <- function(img_path, is_selection=FALSE){
    meta <- get_metadata(img_path)
    is_hidden <- img_path %in% user_data$eliminated
    meta_html <- list(div(span(class="meta-key", "Device:"), meta$device), div(span(class="meta-key", "Location:"), meta$location))
    if(length(meta$excel) > 0) meta_html <- c(meta_html, lapply(names(meta$excel), function(n) div(span(class="meta-key", paste0(n, ":")), meta$excel[[n]])))
    
    card_js <- "Shiny.setInputValue('game_click', '%s', {priority: 'event'}); Shiny.setInputValue('game_click', '');"
    
    div(class="card-wrapper",
        div(class=paste("plankton-card", if(is_hidden) "hidden-card" else "", if(user_data$guessing) "guess-active" else ""),
            style=paste0("border-color:", get_type_color(meta$device)),
            onclick=sprintf(card_js, img_path),
            actionButton(paste0("zoom_", digest(img_path)), label=icon("search"), class="zoom-btn",
                         onclick=sprintf("event.stopPropagation(); Shiny.setInputValue('zoom_click','%s',{priority:'event'})", img_path)),
            img(src=get_web_path(img_path), class="plankton-img"),
            div(class="species-name", gsub("[+_-]", " ", meta$species)),
            div(class="meta-section", meta_html)))
  }
  
  output$selection_grid <- renderUI({ div(class="game-container", lapply(shared_store$game_images, function(i) create_card(i, T))) })
  output$game_grid <- renderUI({ div(class="game-container", lapply(shared_store$game_images, function(i) create_card(i, F))) })
  output$win_table <- renderTable({ shared_store$scoreboard })
  
  output$online_users <- renderUI({
    users <- names(shared_store$player_secrets)
    if(length(users) == 0) return(tags$i("Wait for others...", style="font-size:10px; color:#7f8c8d;"))
    tagList(lapply(users, function(u) {
      user_row <- shared_store$scoreboard[shared_store$scoreboard$Scientist == u,]
      emoji_icon <- if(nrow(user_row) > 0) user_row$Emoji[1] else "🔬"
      span(style="display:inline-block; margin:2px; background:#34495e; color:white; padding:3px 8px; border-radius:12px; font-size:11px; font-weight:bold;", paste(emoji_icon, u))
    }))
  })
  
  observeEvent(input$zoom_click, {
    meta_z <- get_metadata(input$zoom_click)
    showModal(modalDialog(title = paste("Inspection Window:", meta_z$species), img(src = get_web_path(input$zoom_click), class = "zoom-img"), br(), br(),
                          tags$ul(tags$li(tags$b("Device:"), meta_z$device), tags$li(tags$b("Location:"), meta_z$location), lapply(names(meta_z$excel), function(e) tags$li(tags$b(paste0(e,":")), meta_z$excel[[e]]))),
                          size = "l", easyClose = TRUE))
  })
  
  observeEvent(input$game_click, {
    req(input$game_click != "")
    img_clicked <- input$game_click
    if(is.null(user_data$secret_picked)) {
      user_data$secret_picked <- img_clicked
      shared_store$player_secrets[[user_data$name]] <- img_clicked
      # SECOND STEP: Onboarding for gameplay
      showModal(modalDialog(
        title = "🎯 Target Locked!",
        tags$p("You have chosen your secret card. Now the game begins!"),
        tags$hr(),
        tags$b("How to play Step 2:"),
        tags$ul(
          tags$li("Use the chat to ask the other player questions."),
          tags$li("Eliminate cards that are NOT their target by clicking them."),
          tags$li("When you are ready, click the trophy at the top right to guess!")
        ),
        footer = modalButton("Let's Go!")
      ))
      return()
    }
    if(user_data$guessing) {
      meta_clicked <- get_metadata(img_clicked)
      msg_send(paste("🔬 ANALYZING:", meta_clicked$species))
      opp_name <- names(shared_store$player_secrets)[names(shared_store$player_secrets) != user_data$name][1]
      if(!is.na(opp_name) && img_clicked == shared_store$player_secrets[[opp_name]]) {
        idx <- which(shared_store$scoreboard$Scientist == user_data$name)
        shared_store$scoreboard$Wins[idx] <<- shared_store$scoreboard$Wins[idx] + 1
        msg_send("🏆 TARGET ACQUIRED!")
        showModal(modalDialog(title="VICTORY!", "Target confirmed.", footer=actionButton("reset_confirm", "New Round")))
      } else { showNotification("Analysis failed.", type="error"); user_data$guessing <- FALSE }
    } else {
      user_data$eliminated <- if(img_clicked %in% user_data$eliminated) setdiff(user_data$eliminated, img_clicked) else c(user_data$eliminated, img_clicked)
    }
  })
  
  observeEvent(input$login_btn, { 
    if(input$user_name_input != "") { 
      user_data$name <- input$user_name_input 
      user_data$color <- input$user_color 
      user_data$emoji <- input$user_emoji 
      if(!(user_data$name %in% shared_store$scoreboard$Scientist)) {
        shared_store$scoreboard <<- rbind(shared_store$scoreboard, data.frame(Scientist=user_data$name, Wins=0, Streak=0, Emoji=user_data$emoji)) 
      }
      if(length(shared_store$game_images) == 0) {
        shared_store$game_size <- input$num_cards_input
        shared_store$game_images <- get_balanced_pool(shared_store$game_size)
      }
      # FIRST STEP: Instruction to pick card
      showModal(modalDialog(
        title = "🚢 Welcome Aboard!",
        tags$p("Your first mission is to pick a secret plankton card that your opponent has to find."),
        tags$b("Step 1: Click any card on the next screen to set your target."),
        footer = modalButton("Understood!")
      ))
    } 
  })
  
  observeEvent(input$send_btn, { if(input$chat_msg!="") msg_send(input$chat_msg) })
  observeEvent(input$q_yes, { msg_send("✅ YES") }); observeEvent(input$q_no, { msg_send("❌ NO") })
  observeEvent(input$q_zoo, { msg_send("Is it Zooplankton?") }); observeEvent(input$q_phyto, { msg_send("Is it Phytoplankton?") })
  observeEvent(input$toggle_guess, { user_data$guessing <- !user_data$guessing })
  observeEvent(input$reset_btn, { shared_store$reset_counter <<- shared_store$reset_counter + 1; shared_store$game_images <<- get_balanced_pool(shared_store$game_size) })
  observeEvent(input$reset_confirm, { removeModal(); shared_store$reset_counter <<- shared_store$reset_counter + 1; shared_store$game_images <<- get_balanced_pool(shared_store$game_size) })
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3773))