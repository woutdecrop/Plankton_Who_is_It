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

device_colors <- c("FLOWCAM" = "#e74c3c", "ZOOSCAN" = "#3498db", "UVP6" = "#9b59b6", "PI10" = "#f1c40f", "VPR" = "#1abc9c", "DEFAULT" = "#95a5a6")

get_type_color <- function(device) {
  d <- toupper(device)
  match <- names(device_colors)[sapply(names(device_colors), function(x) grepl(x, d))]
  if(length(match) > 0) return(device_colors[[match[1]]])
  return(device_colors[["DEFAULT"]])
}

get_web_path <- function(relative_path) { file.path("plankton_imgs", relative_path) }

get_balanced_pool <- function() {
  all_paths <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  main_dirs <- all_paths[basename(all_paths) != basename(base_dir)]
  pool <- unlist(lapply(main_dirs, function(d_path) {
    folder_name <- basename(d_path)
    img_pattern <- "\\.(jpg|png|jpeg)$"
    all_files <- list.files(d_path, pattern = img_pattern, recursive = TRUE, full.names = FALSE, ignore.case = TRUE)
    if(length(all_files) == 0) return(NULL)
    return(file.path(folder_name, sample(all_files, min(length(all_files), 4))))
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
    cols <- names(species_df)[!tolower(names(species_df)) %in% c("species", "device", "location", "loc", "dev", "item", "sensor", "region")]
    for(c in cols) {
      val <- as.character(match_row[[c]])
      if(!is.na(val) && val != "") excel_info[[c]] <- val
    }
  }
  list(device = dev, location = loc, species = spec, excel = excel_info)
}

# 3. UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background-color: #f4f7f6; font-family: 'Segoe UI', sans-serif; }
    .game-container { display: flex; flex-wrap: wrap; margin: 0 -10px; }
    .card-wrapper { width: 16.66%; padding: 0 10px; box-sizing: border-box; }
    .plankton-card { cursor: pointer; border: 5px solid #fff; padding: 10px; background: white; border-radius: 12px; margin-bottom: 20px; transition: 0.2s; min-height: 310px; display: flex; flex-direction: column; position: relative; }
    .plankton-img { width: 100%; height: 120px; object-fit: contain; background: #fafafa; border-radius: 5px; flex-shrink: 0; }
    .species-name { font-weight: bold; font-size: 11px; margin-top: 8px; min-height: 28px; line-height: 1.2; color: #2c3e50; }
    .hidden-card { opacity: 0.15; filter: grayscale(80%); }
    .guess-active { border: 5px solid #f1c40f !important; box-shadow: 0 0 15px #f1c40f; }
    .zoom-btn { position: absolute; top: 5px; right: 5px; background: rgba(255,255,255,0.8); border-radius: 50%; width: 22px; height: 22px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd; font-size: 10px; z-index: 10; }
    .filter-btn { cursor: pointer; padding: 4px 10px; border-radius: 20px; margin: 2px; font-size: 10px; font-weight: bold; color: white; display: inline-block; }
    .active-filter { border: 2px solid white !important; opacity: 0.4; transform: translateY(1px); }
    .target-container { background: #2c3e50; color: white; padding: 15px; border-radius: 12px; text-align: center; }
    #chat_box { height: 250px; overflow-y: auto; background: #fff; padding: 10px; border-radius: 8px; border: 1px solid #ddd; font-size: 11px; }
    .meta-key { font-weight: bold; color: #7f8c8d; font-size: 8px; text-transform: uppercase; margin-right: 4px; }
    .meta-section { border-top: 1px solid #eee; margin-top: auto; padding-top: 5px; font-size: 9px; }
    .btn-xs-hint { font-size: 9px; padding: 2px 5px; margin: 1px; color: #555; background: #eee; border: 1px solid #ccc; border-radius: 4px; }
  "))),
  
  fluidRow(
    column(4, h2("🔬 Station AI: Mission")),
    column(8, div(style="background:#2c3e50; padding:12px; border-radius:8px; margin-top:10px;", uiOutput("filter_buttons")))
  ),
  withSpinner(uiOutput("main_content"), type = 6, color = "#2c3e50"),
  div(style="position: fixed; bottom: 20px; left: 20px; background:white; padding:10px; border-radius:10px; border:2px solid #f1c40f; box-shadow: 0 4px 10px rgba(0,0,0,0.1);", tags$b("WIN STREAK: "), textOutput("streak_val", inline=T))
)

server <- function(input, output, session) {
  game <- reactiveValues(pool = get_balanced_pool(), ai_target = NULL, chat = data.frame(User=character(0), Msg=character(0), Color=character(0), stringsAsFactors=F), eliminated = character(), filters = character(), guessing = FALSE, streak = 0)
  user <- reactiveValues(name = NULL, color = "#2980b9")
  
  observe({ if(is.null(game$ai_target)) game$ai_target <- sample(game$pool, 1) })
  
  output$streak_val <- renderText({ game$streak })
  
  output$filter_buttons <- renderUI({
    devs <- names(device_colors)[1:5]; locs <- c("BPNS", "Mediterranean", "Spitsbergen", "Greenland")
    tagList(
      span(style="color:white; font-size:10px; margin-right:10px;", "DEVICES:"),
      lapply(devs, function(d) span(class=paste("filter-btn", if(d %in% game$filters) "active-filter" else ""), style=paste0("background:", device_colors[[d]]), onclick=sprintf("Shiny.setInputValue('f_click', '%s')", d), d)),
      tags$br(),
      span(style="color:white; font-size:10px; margin-right:12px;", "LOCATIONS:"),
      lapply(locs, function(l) span(class=paste("filter-btn", if(l %in% game$filters) "active-filter" else ""), style="background:#7f8c8d;", onclick=sprintf("Shiny.setInputValue('f_click', '%s')", l), l))
    )
  })
  
  observeEvent(input$f_click, {
    game$filters <- if(input$f_click %in% game$filters) setdiff(game$filters, input$f_click) else c(game$filters, input$f_click)
    matches <- Filter(function(img) { m <- get_metadata(img); grepl(tolower(input$f_click), tolower(m$device)) || grepl(tolower(input$f_click), tolower(m$location)) }, game$pool)
    game$eliminated <- if(input$f_click %in% game$filters) unique(c(game$eliminated, matches)) else setdiff(game$eliminated, matches)
  })
  
  msg_logic <- function(sender, text, col) {
    game$chat <- rbind(game$chat, data.frame(User=sender, Msg=text, Color=col))
    delay(100, runjs("var d=document.getElementById('chat_box'); d.scrollTop=d.scrollHeight;"))
  }
  
  ai_ask <- function(q_text, type, value) {
    msg_logic("YOU", q_text, user$color)
    target_meta <- get_metadata(game$ai_target)
    answer <- FALSE
    if(type == "dev") answer <- grepl(tolower(value), tolower(target_meta$device))
    if(type == "loc") answer <- grepl(tolower(value), tolower(target_meta$location))
    if(type == "excel") answer <- any(tolower(as.character(unlist(target_meta$excel))) == tolower(value))
    
    if(answer) msg_logic("STATION AI", paste0("✅ YES, the specimen matches: ", value), "#27ae60")
    else msg_logic("STATION AI", paste0("❌ NO, it does not match: ", value), "#c0392b")
  }
  
  output$main_content <- renderUI({
    if(is.null(user$name)) {
      wellPanel(style="max-width:400px; margin: 50px auto; text-align:center;",
                h3("Scientist Login"),
                textInput("user_name", "ID:"), colourpicker::colourInput("u_col", "Theme:", "#2980b9"),
                actionButton("start_game", "Initialize Mission", class="btn-primary", width="100%"))
    } else {
      # Build Dynamic Excel Questions
      target_meta <- get_metadata(game$ai_target)
      excel_questions <- if(length(target_meta$excel) > 0) {
        # Get all unique trait values from the CURRENT pool for realistic questions
        traits <- unlist(lapply(game$pool, function(p) get_metadata(p)$excel))
        unique_traits <- unique(as.character(traits))
        lapply(unique_traits, function(tr) {
          actionButton(paste0("ask_", digest(tr)), tr, class="btn-xs-hint", onclick=sprintf("Shiny.setInputValue('q_excel', '%s')", tr))
        })
      } else { NULL }
      
      fluidRow(
        column(3,
               div(class="target-container", h4("LAB LOGS"), tags$p(style="font-size:10px;","Analyze the encrypted target.")),
               div(id="chat_box", lapply(1:nrow(game$chat), function(i) div(style=paste0("border-left:3px solid ", game$chat$Color[i], "; padding-left:5px; margin-bottom:5px;"), tags$b(game$chat$User[i], ":"), game$chat$Msg[i]))),
               div(style="margin-top:10px; background:#fff; padding:8px; border-radius:8px; border:1px solid #ddd;",
                   tags$b("TRAIT ANALYSIS:"), br(), excel_questions,
                   hr(style="margin:5px;"),
                   tags$b("HARDWARE:"), br(),
                   actionButton("q_fc", "FlowCam?", class="btn-xs"), actionButton("q_zs", "Zooscan?", class="btn-xs"), actionButton("q_pi", "PI10?", class="btn-xs"),
                   br(), tags$b("LOCATIONS:"), br(),
                   actionButton("q_bp", "BPNS?", class="btn-xs"), actionButton("q_md", "Med?", class="btn-xs"), actionButton("q_sp", "Spitsbergen?", class="btn-xs"), actionButton("q_gr", "Greenland?", class="btn-xs")),
               hr(), 
               actionButton("get_hint", "💡 GET HINT (Resets Streak)", class="btn-info btn-block", style="font-size:10px;"),
               actionButton("toggle_guess", "🏆 GUESS MODE", class=if(game$guessing) "btn-danger" else "btn-warning", width="100%", style="margin-top:5px;"),
               br(), br(), actionButton("force_reset", "Abort Mission", class="btn-block btn-xs")
        ),
        column(9, uiOutput("game_grid"))
      )
    }
  })
  
  create_card <- function(img_path) {
    meta <- get_metadata(img_path); is_hid <- img_path %in% game$eliminated
    div(class="card-wrapper", div(class=paste("plankton-card", if(is_hid) "hidden-card" else "", if(game$guessing) "guess-active" else ""),
                                  style=paste0("border-color:", get_type_color(meta$device)),
                                  onclick=sprintf("Shiny.setInputValue('card_click','%s')", img_path),
                                  actionButton(paste0("z", digest(img_path)), icon("search"), class="zoom-btn", onclick=sprintf("event.stopPropagation(); Shiny.setInputValue('z_click','%s')", img_path)),
                                  img(src=get_web_path(img_path), class="plankton-img"), div(class="species-name", meta$species),
                                  div(class="meta-section", 
                                      div(span(class="meta-key","Device:"), meta$device), 
                                      div(span(class="meta-key","Loc:"), meta$location), 
                                      lapply(names(meta$excel), function(n) div(tags$b(n,":"), meta$excel[[n]])))))
  }
  
  output$game_grid <- renderUI({ div(class="game-container", lapply(game$pool, create_card)) })
  
  observeEvent(input$start_game, { user$name <- input$user_name; user$color <- input$u_col; msg_logic("STATION AI", "Mission Active. Target analysis required.", "#2c3e50") })
  
  observeEvent(input$q_excel, ai_ask(paste0("Does it have the trait: ", input$q_excel, "?"), "excel", input$q_excel))
  observeEvent(input$q_fc, ai_ask("Is it FlowCam hardware?", "dev", "FlowCam"))
  observeEvent(input$q_zs, ai_ask("Is it Zooscan hardware?", "dev", "Zooscan"))
  observeEvent(input$q_pi, ai_ask("Is it PI10 hardware?", "dev", "PI10"))
  observeEvent(input$q_bp, ai_ask("Is it from the BPNS region?", "loc", "BPNS"))
  observeEvent(input$q_md, ai_ask("Is it from the Mediterranean?", "loc", "Mediterranean"))
  observeEvent(input$q_sp, ai_ask("Is it from Spitsbergen?", "loc", "Spitsbergen"))
  observeEvent(input$q_gr, ai_ask("Is it from Greenland?", "loc", "Greenland"))
  
  # Hint System
  observeEvent(input$get_hint, {
    remaining_incorrect <- setdiff(game$pool, c(game$ai_target, game$eliminated))
    if(length(remaining_incorrect) > 0) {
      hint_pick <- sample(remaining_incorrect, 1)
      game$eliminated <- c(game$eliminated, hint_pick)
      game$streak <- 0
      msg_logic("STATION AI", "💡 HINT: I've eliminated one outlier. Streak reset.", "#2980b9")
    }
  })
  
  observeEvent(input$z_click, {
    m_z <- get_metadata(input$z_click)
    showModal(modalDialog(title=m_z$species, img(src=get_web_path(input$z_click), style="width:100%"), size="l", easyClose=T))
  })
  
  observeEvent(input$toggle_guess, { game$guessing <- !game$guessing })
  
  observeEvent(input$card_click, {
    if(game$guessing) {
      if(input$card_click == game$ai_target) {
        game$streak <- game$streak + 1
        msg_logic("STATION AI", "🏆 Correct! Analysis complete. Point awarded.", "#27ae60")
        showModal(modalDialog(title="SUCCESS", "Species identified correctly.", footer=actionButton("new_round", "Next Round")))
      } else {
        game$streak <- 0; game$guessing <- FALSE
        msg_logic("STATION AI", "❌ INCORRECT. Sensor error. Streak lost.", "#c0392b")
        showNotification("Wrong Specimen!", type="error")
      }
    } else {
      game$eliminated <- if(input$card_click %in% game$eliminated) setdiff(game$eliminated, input$card_click) else c(game$eliminated, input$card_click)
    }
  })
  
  reset_logic <- function() { game$pool <- get_balanced_pool(); game$ai_target <- sample(game$pool, 1); game$eliminated <- character(); game$chat <- data.frame(User=character(0), Msg=character(0), Color=character(0), stringsAsFactors=F); msg_logic("STATION AI", "System reset. New Target Encrypted.", "#2c3e50") }
  observeEvent(input$new_round, { removeModal(); reset_logic() })
  observeEvent(input$force_reset, { reset_logic() })
}

shinyApp(ui, server)