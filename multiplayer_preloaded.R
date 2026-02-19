library(shiny)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(readxl)
library(shinycssloaders)
library(digest)

# 1. SETUP: Relative Path Handling
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

get_balanced_pool <- function() {
  all_paths <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  main_dirs <- all_paths[basename(all_paths) != basename(base_dir)]
  pool <- unlist(lapply(main_dirs, function(d_path) {
    folder_name <- basename(d_path)
    img_pattern <- "\\.(jpg|png|jpeg)$"
    all_files <- list.files(d_path, pattern = img_pattern, recursive = TRUE, full.names = FALSE, ignore.case = TRUE)
    if(length(all_files) == 0) return(NULL)
    s_size <- min(length(all_files), 4)
    return(file.path(folder_name, sample(all_files, s_size)))
  }))
  return(sample(pool))
}

get_metadata <- function(path) {
  parts <- strsplit(path, "[/\\\\]")[[1]]
  meta_parts <- strsplit(parts[1], "_")[[1]]
  dev <- meta_parts[1]
  loc <- if(length(meta_parts) > 1) meta_parts[2] else "General"
  if(tolower(loc) %in% c("pitsbergen", "spitsbergen")) loc <- "Spitsbergen"
  spec <- if(length(parts) >= 2) parts[length(parts)-1] else "Unknown"
  
  excel_info <- list()
  if(nrow(species_df) > 0 && spec %in% species_df[[1]]) {
    match_row <- species_df[species_df[[1]] == spec, ][1, ]
    blacklist <- c("species", "device", "location", "loc", "dev", "item", "sensor", "region")
    cols <- names(species_df)[!tolower(names(species_df)) %in% blacklist]
    for(c in cols) {
      val <- as.character(match_row[[c]])
      if(!is.na(val) && val != "") excel_info[[c]] <- val
    }
  }
  list(device = dev, location = loc, species = spec, excel = excel_info)
}

# 2. SHARED STATE
shared_store <- reactiveValues(
  chat = data.frame(User=character(0), Msg=character(0), Color=character(0), Emoji=character(0), stringsAsFactors=F),
  scoreboard = data.frame(Scientist=character(0), Wins=numeric(0), Streak=numeric(0), stringsAsFactors=F),
  player_secrets = list(),
  game_images = get_balanced_pool(),
  reset_counter = 0
)

# 3. UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(paste0("
    body { background-color: #f4f7f6; font-family: 'Segoe UI', sans-serif; }
    .game-container { display: flex; flex-wrap: wrap; margin: 0 -10px; }
    .card-wrapper { width: 16.66%; padding: 0 10px; box-sizing: border-box; }
    .plankton-card { cursor: pointer; border: 5px solid #fff; padding: 10px; background: white; border-radius: 12px; margin-bottom: 20px; transition: 0.2s; min-height: 310px; display: flex; flex-direction: column; position: relative; }
    .plankton-img { width: 100%; height: 120px; object-fit: contain; background: #fafafa; border-radius: 5px; flex-shrink: 0; }
    .species-name { font-weight: bold; font-size: 11px; margin-top: 8px; min-height: 28px; line-height: 1.2; color: #2c3e50; }
    .hidden-card { opacity: 0.15; filter: grayscale(80%); }
    .guess-active { border: 5px solid #f1c40f !important; box-shadow: 0 0 15px #f1c40f; }
    
    .zoom-btn { position: absolute; top: 5px; right: 5px; background: rgba(255,255,255,0.8); border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd; color: #333; z-index: 10; font-size: 12px; }
    .zoom-btn:hover { background: #f1c40f; color: white; border-color: #f1c40f; }

    .filter-btn { cursor: pointer; padding: 4px 12px; border-radius: 20px; margin: 3px; border: 1px solid transparent; display: inline-block; font-size: 11px; font-weight: bold; color: white; transition: 0.2s; }
    .active-filter { border: 2px solid white !important; box-shadow: inset 0 3px 5px rgba(0,0,0,0.5) !important; transform: translateY(2px); opacity: 0.5; }
    
    .target-container { background: #2c3e50; color: white; padding: 15px; border-radius: 12px; text-align: center; margin-bottom: 10px; }
    .target-img-box { width: 100%; aspect-ratio: 1.5/1; background: white; border-radius: 8px; display: flex; align-items: center; justify-content: center; overflow: hidden; border: 3px solid #f1c40f; }
    .target-img-box img { max-width: 95%; max-height: 95%; object-fit: contain; }
    
    #chat_box { height: 220px; overflow-y: auto; background: #fff; padding: 10px; border-radius: 8px; border: 1px solid #ddd; width: 100%; box-sizing: border-box; }
    .leader-badge { position: fixed; bottom: 20px; left: 20px; background: #fff; padding: 12px; border-radius: 12px; border: 2px solid #f1c40f; box-shadow: 0 4px 10px rgba(0,0,0,0.1); z-index: 100; }
    .meta-key { font-weight: bold; color: #7f8c8d; font-size: 8px; text-transform: uppercase; margin-right: 4px; }
    .meta-section { border-top: 1px solid #eee; margin-top: auto; padding-top: 5px; font-size: 9px; color: #555; }
    .zoom-img { width: 100%; max-height: 550px; object-fit: contain; background: white; }
  ")))),
  
  fluidRow(
    column(4, h2("🚢 Plankton 'Who Is It?'")),
    column(8, div(style="background:#2c3e50; padding:12px; border-radius:8px; margin-top:10px;",
                  uiOutput("filter_buttons")
    ))
  ),
  
  withSpinner(uiOutput("main_content"), type = 6, color = "#2c3e50", proxy.height = "400px"),
  uiOutput("leader_badge"),
  
  div(style="position: fixed; bottom: 20px; right: 20px;",
      dropdownButton(label="Hall of Fame", icon=icon("compact-disc"), status="danger", circle=T, 
                     tableOutput("win_table")))
)

# 4. SERVER
server <- function(input, output, session) {
  user_data <- reactiveValues(name=NULL, secret_picked=NULL, eliminated=character(), color="#2980b9", emoji="🔬", active_filters=character(), guessing=FALSE, local_reset=0)
  
  observe({
    if(shared_store$reset_counter > user_data$local_reset) {
      user_data$secret_picked <- NULL; user_data$eliminated <- character(); user_data$active_filters <- character(); user_data$guessing <- FALSE
      user_data$local_reset <- shared_store$reset_counter
    }
  })
  
  output$filter_buttons <- renderUI({
    devs <- names(device_colors)[1:5]
    locs <- c("BPNS", "Mediterranean", "Spitsbergen", "Greenland")
    tagList(
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 10px;", "HARDWARE:"),
      lapply(devs, function(d) {
        span(class=paste("filter-btn", if(d %in% user_data$active_filters) "active-filter" else ""), 
             style=paste0("background:", device_colors[[d]]),
             onclick=sprintf("Shiny.setInputValue('filter_click', '%s', {priority:'event'})", d), d)
      }),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 14px;", "LOCATION:"),
      lapply(locs, function(l) {
        span(class=paste("filter-btn", if(l %in% user_data$active_filters) "active-filter" else ""), 
             style="background:#7f8c8d;",
             onclick=sprintf("Shiny.setInputValue('filter_click', '%s', {priority:'event'})", l), l)
      })
    )
  })
  
  msg_send <- function(m){
    shared_store$chat <<- rbind(shared_store$chat, data.frame(User=user_data$name, Msg=m, Color=user_data$color, Emoji=user_data$emoji))
    updateTextInput(session, "chat_msg", value=""); delay(100, runjs("var d=document.getElementById('chat_box'); if(d) d.scrollTop=d.scrollHeight;"))
  }
  
  observeEvent(input$filter_click, {
    target <- input$filter_click
    user_data$active_filters <- if(target %in% user_data$active_filters) setdiff(user_data$active_filters, target) else c(user_data$active_filters, target)
    matches <- Filter(function(img) {
      m <- get_metadata(img); grepl(tolower(target), tolower(m$device)) || grepl(tolower(target), tolower(m$location))
    }, shared_store$game_images)
    user_data$eliminated <- if(target %in% user_data$active_filters) unique(c(user_data$eliminated, matches)) else setdiff(user_data$eliminated, matches)
  })
  
  output$main_content <- renderUI({
    if(is.null(user_data$name)){
      wellPanel(style="max-width:450px; margin: 50px auto; text-align:center;",
                h3("Scientist Registration"),
                textInput("user_name_input", "Full Name:"),
                fluidRow(column(6, colourpicker::colourInput("user_color", "Station Color:", "#2980b9")),
                         column(6, selectInput("user_emoji", "Avatar Icon:", choices=c("🔬","🧬","🦀","🌊","🦠","🐳")))),
                actionButton("login_btn", "Join Station", class="btn-primary", width="100%"))
    } else if(is.null(user_data$secret_picked)){
      div(style="padding: 20px;", h4("Pick your secret target:"), uiOutput("selection_grid"))
    } else {
      meta_s <- get_metadata(user_data$secret_picked)
      fluidRow(
        column(3, class="sidebar-panel",
               div(class="target-container", style=paste0("border-bottom: 8px solid ", user_data$color),
                   div(class="target-img-box", img(src=get_web_path(user_data$secret_picked))),
                   h4(paste(user_data$emoji, meta_s$species), style="color:#f1c40f; font-weight:bold; margin-top:10px;"),
                   div(style="font-size: 9px; opacity: 0.8;",
                       span(tags$b("DEVICE:"), meta_s$device), " | ", span(tags$b("LOC:"), meta_s$location))
               ),
               div(id="chat_box", lapply(1:nrow(shared_store$chat), function(i){
                 row <- shared_store$chat[i,]
                 div(style=paste0("border-left:3px solid ", row$Color, "; padding-left:5px; margin-bottom:5px; font-size:11px;"), 
                     tags$b(paste0(row$Emoji, " ", row$User, ":")), row$Msg)
               })),
               div(style="display:flex; margin-top:5px;", textInput("chat_msg", NULL, placeholder="Ask...", width="100%"), actionButton("send_btn", icon("paper-plane"))),
               div(style="margin-top:5px; text-align:center;",
                   actionButton("q_yes", "YES", class="btn-success btn-xs"), actionButton("q_no", "NO", class="btn-danger btn-xs"),
                   actionButton("q_fc", "FlowCam?", class="btn-info btn-xs"), actionButton("q_zs", "Zooscan?", class="btn-info btn-xs"),
                   actionButton("q_pi", "PI10?", class="btn-info btn-xs"), actionButton("q_gr", "Greenland?", class="btn-info btn-xs")
               ),
               hr(),
               actionButton("toggle_guess", "🏆 GUESS MODE", class=if(user_data$guessing) "btn-danger" else "btn-warning", width="100%"),
               br(), br(), actionButton("reset_btn", "Force New Round", class="btn-block", style="font-size:10px")
        ),
        column(9, uiOutput("game_grid"))
      )
    }
  })
  
  output$leader_badge <- renderUI({
    df <- shared_store$scoreboard
    if(is.null(user_data$name) || nrow(df) == 0) return(NULL)
    top <- df[which.max(df$Wins),]
    div(class="leader-badge", tags$b("⭐ TOP SCIENTIST"), br(), top$Scientist, br(), 
        tags$span(style="color:#27ae60", paste(top$Wins, "Wins")))
  })
  
  create_card <- function(img_path, is_selection=FALSE){
    meta <- get_metadata(img_path)
    is_hidden <- img_path %in% user_data$eliminated
    
    meta_html <- list(
      div(span(class="meta-key", "Device:"), meta$device),
      div(span(class="meta-key", "Location:"), meta$location)
    )
    if(length(meta$excel) > 0) {
      meta_html <- c(meta_html, lapply(names(meta$excel), function(n) div(tags$b(paste0(n,": ")), meta$excel[[n]])))
    }
    
    div(class="card-wrapper",
        div(class=paste("plankton-card", if(is_hidden) "hidden-card" else "", if(user_data$guessing) "guess-active" else ""),
            style=paste0("border-color:", get_type_color(meta$device)),
            onclick=sprintf("Shiny.setInputValue('game_click','%s',{priority:'event'})", img_path),
            
            # ZOOM BUTTON
            actionButton(paste0("zoom_", digest(img_path)), label=icon("search"), class="zoom-btn",
                         onclick=sprintf("event.stopPropagation(); Shiny.setInputValue('zoom_click','%s',{priority:'event'})", img_path)),
            
            img(src=get_web_path(img_path), class="plankton-img"),
            div(class="species-name", meta$species),
            div(class="meta-section", meta_html)))
  }
  
  output$selection_grid <- renderUI({ div(class="game-container", lapply(shared_store$game_images, function(i) create_card(i, T))) })
  output$game_grid <- renderUI({ div(class="game-container", lapply(shared_store$game_images, function(i) create_card(i, F))) })
  output$win_table <- renderTable({ shared_store$scoreboard })
  
  # ZOOM HANDLER
  observeEvent(input$zoom_click, {
    meta_z <- get_metadata(input$zoom_click)
    showModal(modalDialog(
      title = paste("Inspection Window:", meta_z$species),
      img(src = get_web_path(input$zoom_click), class = "zoom-img"),
      br(), br(),
      tags$ul(
        tags$li(tags$b("Device:"), meta_z$device),
        tags$li(tags$b("Location:"), meta_z$location),
        lapply(names(meta_z$excel), function(e) tags$li(tags$b(paste0(e,":")), meta_z$excel[[e]]))
      ),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  # SELECTION & ELIMINATION
  observeEvent(input$game_click, {
    img_clicked <- input$game_click
    
    # Selection Phase
    if(is.null(user_data$secret_picked)) {
      user_data$secret_picked <- img_clicked
      shared_store$player_secrets[[user_data$name]] <- img_clicked
      return()
    }
    
    # Guess Mode
    if(user_data$guessing) {
      meta_clicked <- get_metadata(img_clicked)
      msg_send(paste("🔬 ANALYZING:", meta_clicked$species))
      opp_name <- names(shared_store$player_secrets)[names(shared_store$player_secrets) != user_data$name][1]
      if(!is.na(opp_name) && img_clicked == shared_store$player_secrets[[opp_name]]) {
        idx <- which(shared_store$scoreboard$Scientist == user_data$name)
        shared_store$scoreboard$Wins[idx] <<- shared_store$scoreboard$Wins[idx] + 1
        msg_send("🏆 TARGET ACQUIRED! Identification Successful.")
        showModal(modalDialog(title="VICTORY!", "Target confirmed. Data uploaded.", footer=actionButton("reset_confirm", "New Round")))
      } else { 
        showNotification("Analysis failed. Incorrect specimen.", type="error")
        user_data$guessing <- FALSE 
      }
    } else {
      # Normal Elimination Toggle
      user_data$eliminated <- if(img_clicked %in% user_data$eliminated) setdiff(user_data$eliminated, img_clicked) else c(user_data$eliminated, img_clicked)
    }
  })
  
  # Shared Boilerplate
  observeEvent(input$login_btn, { 
    if(input$user_name_input != "") {
      user_data$name <- input$user_name_input; user_data$color <- input$user_color; user_data$emoji <- input$user_emoji
      if(!(user_data$name %in% shared_store$scoreboard$Scientist)) shared_store$scoreboard <<- rbind(shared_store$scoreboard, data.frame(Scientist=user_data$name, Wins=0, Streak=0))
    }
  })
  observeEvent(input$send_btn, { if(input$chat_msg!="") msg_send(input$chat_msg) })
  observeEvent(input$q_yes, { msg_send("✅ YES") }); observeEvent(input$q_no, { msg_send("❌ NO") })
  observeEvent(input$q_fc, { msg_send("Is it FlowCam?") }); observeEvent(input$q_zs, { msg_send("Is it Zooscan?") })
  observeEvent(input$q_pi, { msg_send("Is it PI10?") }); observeEvent(input$q_gr, { msg_send("Is it from Greenland?") })
  observeEvent(input$toggle_guess, { user_data$guessing <- !user_data$guessing })
  observeEvent(input$reset_btn, { shared_store$reset_counter <<- shared_store$reset_counter + 1; shared_store$game_images <<- get_balanced_pool() })
  observeEvent(input$reset_confirm, { removeModal(); shared_store$reset_counter <<- shared_store$reset_counter + 1; shared_store$game_images <<- get_balanced_pool() })
}

shinyApp(ui, server)