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

# Load Excel Metadata
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

get_balanced_pool <- function(n_cards = 12) {
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
  full_pool <- sample(pool)
  return(head(full_pool, min(n_cards, length(full_pool))))
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

# 2. STATE
shared_store <- reactiveValues(
  chat = data.frame(User=character(0), Msg=character(0), Color=character(0), stringsAsFactors=F),
  ai_target = NULL,
  game_images = character(0),
  reset_counter = 0,
  streak = 0
)

# 3. UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(" 
    body { background-color: #f4f7f6; font-family: 'Segoe UI', sans-serif; }
    .game-container { display: flex; flex-wrap: wrap; margin: 0 -10px; }
    .card-wrapper { width: 16.66%; padding: 0 10px; box-sizing: border-box; }
    .plankton-card { cursor: pointer; border: 5px solid #fff; padding: 15px; background: white; border-radius: 12px; margin-bottom: 20px; transition: 0.2s; min-height: 400px; display: flex; flex-direction: column; position: relative; }
    .plankton-img { width: 100%; height: 140px; object-fit: contain; background: #fafafa; border-radius: 5px; flex-shrink: 0; }
    .species-name { font-weight: bold; font-size: 14px; margin-top: 10px; min-height: 32px; color: #2c3e50; }
    .hidden-card { opacity: 0.15; filter: grayscale(80%); }
    .guess-active { border: 5px solid #f1c40f !important; box-shadow: 0 0 15px #f1c40f; }
    .zoom-btn { position: absolute; top: 5px; right: 5px; background: rgba(255,255,255,0.8); border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd; z-index: 10; font-size: 12px; }
    .filter-btn { cursor: pointer; padding: 4px 10px; border-radius: 20px; margin: 2px; border: 1px solid transparent; display: inline-block; font-size: 10px; font-weight: bold; color: white; transition: 0.2s; }
    .active-filter { border: 2px solid white !important; opacity: 0.5; transform: translateY(2px); }
    .count-badge { background: rgba(0,0,0,0.25); padding: 1px 5px; border-radius: 10px; margin-left: 4px; font-size: 9px; }
    #chat_box { height: 250px; overflow-y: auto; background: #fff; padding: 10px; border-radius: 8px; border: 1px solid #ddd; width: 100%; margin-bottom:10px; }
    .meta-key { font-weight: bold; color: #7f8c8d; font-size: 11px; text-transform: uppercase; margin-right: 4px; }
    .meta-section { border-top: 1px solid #eee; margin-top: auto; padding-top: 10px; font-size: 11px; color: #555; line-height: 1.4; display: flex; flex-direction: column; gap: 3px; }
    .explanation-box { background:#fdf6e3; padding:12px; border-radius:8px; border-left:5px solid #f1c40f; box-shadow: 0 2px 4px rgba(0,0,0,0.05); color:#34495e; font-size: 11px; height: 135px; }

    /* Mystery Card Style */
    .mystery-card { border: 3px dashed #bdc3c7; background: #ecf0f1; min-height: 180px !important; padding: 10px; margin-bottom: 15px; }
    
    /* Premade Questions Container */
    .trait-group { background: #f9f9f9; padding: 5px; border-radius: 5px; margin-bottom: 8px; border: 1px solid #eee; }
    .trait-title { font-size: 10px; font-weight: bold; color: #7f8c8d; text-transform: uppercase; display: block; margin-bottom: 3px; }
    .btn-ask { margin: 1px; font-size: 9px !important; padding: 2px 5px !important; }
  "))),
  
  fluidRow(
    column(2, 
           h2("🚢 Who Is It?", style="margin-top:10px; font-size: 24px;"),
           actionButton("how_to_play", "How to Play?", icon=icon("question-circle"), class="btn-info btn-xs")
    ),
    column(4, 
           div(style="margin-top:10px;", uiOutput("top_explanation"))
    ),
    column(6, 
           div(style="background:#2c3e50; padding:12px; border-radius:8px; margin-top:10px;", uiOutput("filter_buttons"))
    )
  ),
  
  withSpinner(uiOutput("main_content"), type = 6, color = "#2c3e50"),
  
  div(style="position: fixed; bottom: 20px; left: 20px; background:white; padding:10px; border-radius:10px; border:2px solid #f1c40f; box-shadow: 0 4px 10px rgba(0,0,0,0.1);", 
      tags$b("WIN STREAK: "), textOutput("streak_val", inline=T))
)

# 4. SERVER
server <- function(input, output, session) {
  user_data <- reactiveValues(
    name=NULL, color="#2980b9", eliminated=character(), active_filters=character(), guessing=FALSE, local_reset=0,
    discovered_device = "?", discovered_loc = "?", discovered_traits = list()
  )
  
  observe({
    if(shared_store$reset_counter > user_data$local_reset) {
      user_data$eliminated <- character()
      user_data$active_filters <- character()
      user_data$guessing <- FALSE
      user_data$discovered_device <- "?"
      user_data$discovered_loc <- "?"
      user_data$discovered_traits <- list()
      user_data$local_reset <- shared_store$reset_counter
    }
  })
  
  output$streak_val <- renderText({ shared_store$streak })
  
  # --- MYSTERY CARD (RESTORED TO NEW SMALL STYLE) ---
  output$mystery_suspect <- renderUI({
    req(shared_store$ai_target)
    meta_footer <- list(
      div(span(class="meta-key","Device:"), user_data$discovered_device),
      div(span(class="meta-key","Loc:"), user_data$discovered_loc)
    )
    if(length(user_data$discovered_traits) > 0) {
      meta_footer <- c(meta_footer, lapply(names(user_data$discovered_traits), function(n) {
        div(span(class="meta-key", paste0(n, ":")), user_data$discovered_traits[[n]])
      }))
    }
    div(class="plankton-card mystery-card",
        div(style="text-align:center; padding: 5px 0;", icon("dna", class="fa-2x", style="color:#bdc3c7")),
        div(class="species-name", style="text-align:center; min-height: 20px; font-size: 12px;", "TARGET DOSSIER"),
        div(class="meta-section", meta_footer)
    )
  })
  
  # --- ORIGINAL EXPLANATION BOX ---
  output$top_explanation <- renderUI({
    if(is.null(user_data$name)) return(NULL)
    div(class="explanation-box",
        HTML("<b style='color:#d35400; font-size:13px;'><i class='fa fa-info-circle'></i> Mission Active: Find the Target</b><br/> 
                        <b>1. Use Trait Buttons</b> to ask Station AI questions<br/>
                        <b>2. Filter Cards</b> using the hardware/location tags<br/>
                        <b>3. Eliminate suspects</b> by clicking cards to grey them out<br/>
                        <b>4. Guess Target</b> by clicking the yellow button when you're sure!")
    )
  })
  
  # --- ORIGINAL AUTO FILTERS ---
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
    
    devs <- c("FLOWCAM", "ZOOSCAN", "UVP6", "PI10", "VPR")
    locs <- c("BPNS", "Mediterranean", "Spitsbergen", "Greenland")
    groups <- c("Zooplankton", "Phytoplankton", "Other")
    stages <- c("Adult", "Larvae", "Juvenile")
    
    tagList(
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 10px;", "HARDWARE:"),
      lapply(devs, function(d) span(class=paste("filter-btn", if(d %in% user_data$active_filters) "active-filter" else ""), style=paste0("background:", device_colors[[d]]), onclick=sprintf("Shiny.setInputValue('filter_click', '%s')", d), d, span(class="count-badge", count_trait(d)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 14px;", "LOCATION:"),
      lapply(locs, function(l) span(class=paste("filter-btn", if(l %in% user_data$active_filters) "active-filter" else ""), style="background:#7f8c8d;", onclick=sprintf("Shiny.setInputValue('filter_click', '%s')", l), l, span(class="count-badge", count_trait(l)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 27px;", "GROUP:"),
      lapply(groups, function(g) span(class=paste("filter-btn", if(g %in% user_data$active_filters) "active-filter" else ""), style="background:#2ecc71;", onclick=sprintf("Shiny.setInputValue('filter_click', '%s')", g), g, span(class="count-badge", count_trait(g)))),
      tags$br(),
      span(style="color: #bdc3c7; font-size: 10px; font-weight: bold; margin-right: 15px;", "LIFESTAGE:"),
      lapply(stages, function(s) span(class=paste("filter-btn", if(s %in% user_data$active_filters) "active-filter" else ""), style="background:#e67e22;", onclick=sprintf("Shiny.setInputValue('filter_click', '%s')", s), s, span(class="count-badge", count_trait(s))))
    )
  })
  
  observeEvent(input$filter_click, {
    target <- input$filter_click
    user_data$active_filters <- if(target %in% user_data$active_filters) setdiff(user_data$active_filters, target) else c(user_data$active_filters, target)
    matches <- Filter(function(img) {
      m <- get_metadata(img); grepl(tolower(target), tolower(m$device)) || grepl(tolower(target), tolower(m$location)) || any(tolower(as.character(unlist(m$excel))) == tolower(target))
    }, shared_store$game_images)
    user_data$eliminated <- if(target %in% user_data$active_filters) unique(c(user_data$eliminated, matches)) else setdiff(user_data$eliminated, matches)
  })
  
  # --- CHAT & AI LOGIC ---
  msg_logic <- function(sender, text, col) {
    shared_store$chat <- rbind(shared_store$chat, data.frame(User=sender, Msg=text, Color=col))
    delay(100, runjs("var d=document.getElementById('chat_box'); if(d) d.scrollTop=d.scrollHeight;"))
  }
  
  ai_ask <- function(q_text, type, value) {
    msg_logic("YOU", q_text, user_data$color)
    target_meta <- get_metadata(shared_store$ai_target)
    answer <- FALSE
    if(type == "dev") {
      answer <- grepl(tolower(value), tolower(target_meta$device))
      if(answer) user_data$discovered_device <- value
    } else if(type == "loc") {
      answer <- grepl(tolower(value), tolower(target_meta$location))
      if(answer) user_data$discovered_loc <- value
    } else if(type == "excel") {
      for(col_name in names(target_meta$excel)) {
        if(tolower(target_meta$excel[[col_name]]) == tolower(value)) {
          answer <- TRUE
          user_data$discovered_traits[[col_name]] <- value
        }
      }
    }
    if(answer) msg_logic("STATION AI", paste0("✅ YES: '", value, "'"), "#27ae60")
    else msg_logic("STATION AI", paste0("❌ NO: '", value, "'"), "#c0392b")
  }
  
  # --- MAIN CONTENT ---
  output$main_content <- renderUI({
    if(is.null(user_data$name)) {
      wellPanel(style="max-width:450px; margin: 50px auto; text-align:center;",
                h3("Scientist Login"),
                textInput("u_name", "Choose ID:", "Researcher Alpha"),
                colourpicker::colourInput("u_col", "Theme Color:", "#2980b9"),
                sliderInput("game_size", "Deck Size:", min=12, max=30, value=12, step=6),
                actionButton("start_game", "Begin Mission", class="btn-primary", width="100%"))
    } else {
      # Questions Config
      devs <- c("FLOWCAM", "ZOOSCAN", "UVP6", "PI10", "VPR")
      locs <- c("BPNS", "Mediterranean", "Spitsbergen", "Greenland")
      groups <- c("Zooplankton", "Phytoplankton", "Other")
      stages <- c("Adult", "Larvae", "Juvenile")
      all_meta <- lapply(shared_store$game_images, function(p) get_metadata(p)$excel)
      excel_traits <- unique(as.character(unlist(all_meta)))
      other_traits <- setdiff(excel_traits, c(groups, stages))
      
      fluidRow(
        column(3,
               uiOutput("mystery_suspect"),
               div(id="chat_box", lapply(1:nrow(shared_store$chat), function(i) 
                 div(style=paste0("border-left:3px solid ", shared_store$chat$Color[i], "; padding-left:5px; margin-bottom:5px; font-size:11px;"), 
                     tags$b(shared_store$chat$User[i], ":"), shared_store$chat$Msg[i]))),
               
               div(style="max-height: 400px; overflow-y: auto; background:white; padding:10px; border-radius:8px; border:1px solid #ddd;",
                   tags$b("TRAIT ANALYSIS:"), br(),
                   div(class="trait-group", span(class="trait-title", "Hardware"),
                       lapply(devs, function(x) actionButton(paste0("ask_", x), x, class="btn-ask", onclick=sprintf("Shiny.setInputValue('q_dev','%s')", x)))),
                   div(class="trait-group", span(class="trait-title", "Location"),
                       lapply(locs, function(x) actionButton(paste0("ask_", x), x, class="btn-ask", onclick=sprintf("Shiny.setInputValue('q_loc','%s')", x)))),
                   div(class="trait-group", span(class="trait-title", "Group"),
                       lapply(groups, function(x) actionButton(paste0("ask_", x), x, class="btn-ask", onclick=sprintf("Shiny.setInputValue('q_excel','%s')", x)))),
                   div(class="trait-group", span(class="trait-title", "Lifestage"),
                       lapply(stages, function(x) actionButton(paste0("ask_", x), x, class="btn-ask", onclick=sprintf("Shiny.setInputValue('q_excel','%s')", x)))),
                   if(length(other_traits) > 0) div(class="trait-group", span(class="trait-title", "Comments/Notes"),
                                                    lapply(other_traits, function(x) actionButton(paste0("ask_", digest(x)), x, class="btn-ask", onclick=sprintf("Shiny.setInputValue('q_excel','%s')", x))))
               ),
               hr(),
               actionButton("toggle_guess", if(user_data$guessing) "CANCEL GUESS" else "🏆 GUESS TARGET", 
                            class=if(user_data$guessing) "btn-danger" else "btn-warning", width="100%"),
               br(), br(), actionButton("force_reset", "Abort Mission", class="btn-block btn-xs")
        ),
        column(9, uiOutput("game_grid"))
      )
    }
  })
  
  # --- ORIGINAL CARD RENDER & ZOOM ---
  create_card <- function(img_path) {
    meta <- get_metadata(img_path)
    is_hid <- img_path %in% user_data$eliminated
    meta_footer <- list(div(span(class="meta-key","Device:"), meta$device), div(span(class="meta-key","Loc:"), meta$location))
    if(length(meta$excel) > 0) meta_footer <- c(meta_footer, lapply(names(meta$excel), function(n) div(span(class="meta-key", paste0(n, ":")), meta$excel[[n]])))
    
    div(class="card-wrapper", 
        div(class=paste("plankton-card", if(is_hid) "hidden-card" else "", if(user_data$guessing) "guess-active" else ""),
            style=paste0("border-color:", get_type_color(meta$device)),
            onclick=sprintf("Shiny.setInputValue('card_click','%s')", img_path),
            actionButton(paste0("z", digest(img_path)), icon("search"), class="zoom-btn", 
                         onclick=sprintf("event.stopPropagation(); Shiny.setInputValue('z_click','%s')", img_path)),
            img(src=get_web_path(img_path), class="plankton-img"), 
            div(class="species-name", gsub("[+_]", " ", meta$species)),
            div(class="meta-section", meta_footer)))
  }
  
  output$game_grid <- renderUI({ div(class="game-container", lapply(shared_store$game_images, create_card)) })
  
  # --- ORIGINAL ZOOM HANDLER ---
  observeEvent(input$z_click, {
    m_z <- get_metadata(input$z_click)
    showModal(modalDialog(title=m_z$species, img(src=get_web_path(input$z_click), style="width:100%"), size="l", easyClose=T))
  })
  
  # --- EVENT HANDLERS ---
  observeEvent(input$start_game, {
    user_data$name <- input$u_name; user_data$color <- input$u_col
    shared_store$game_images <- get_balanced_pool(input$game_size)
    shared_store$ai_target <- sample(shared_store$game_images, 1)
    shared_store$chat <- data.frame(User=character(0), Msg=character(0), Color=character(0), stringsAsFactors=F)
    msg_logic("STATION AI", "Mission Active. Use trait buttons to investigate.", "#2c3e50")
  })
  
  observeEvent(input$how_to_play, {
    showModal(modalDialog(title = "How to Play Solo Mission", HTML("<ol><li><b>Login:</b> Choose ID and deck size.</li><li><b>Ask AI:</b> Use 'Trait Analysis' to see if the target has features. Matches appear in the Dossier.</li><li><b>Eliminate:</b> Click cards to grey them out.</li><li><b>Guess:</b> Click yellow button then the card!</li></ol>"), easyClose = TRUE, footer = modalButton("Got it!")))
  })
  
  observeEvent(input$q_dev, ai_ask(paste0("Hardware: ", input$q_dev, "?"), "dev", input$q_dev))
  observeEvent(input$q_loc, ai_ask(paste0("Location: ", input$q_loc, "?"), "loc", input$q_loc))
  observeEvent(input$q_excel, ai_ask(paste0("Trait: ", input$q_excel, "?"), "excel", input$q_excel))
  
  observeEvent(input$toggle_guess, { user_data$guessing <- !user_data$guessing })
  
  observeEvent(input$card_click, {
    if(user_data$guessing) {
      if(input$card_click == shared_store$ai_target) {
        shared_store$streak <- shared_store$streak + 1
        msg_logic("STATION AI", "🏆 Correct! Mission Accomplished.", "#27ae60")
        showModal(modalDialog(title="SUCCESS", "Species identified.", footer=actionButton("new_round", "Next Round")))
      } else {
        shared_store$streak <- 0; user_data$guessing <- FALSE
        msg_logic("STATION AI", "❌ INCORRECT. Sensor error.", "#c0392b")
      }
    } else {
      user_data$eliminated <- if(input$card_click %in% user_data$eliminated) setdiff(user_data$eliminated, input$card_click) else c(user_data$eliminated, input$card_click)
    }
  })
  
  observeEvent(input$new_round, { 
    removeModal(); shared_store$reset_counter <- shared_store$reset_counter + 1
    shared_store$game_images <- get_balanced_pool(length(shared_store$game_images))
    shared_store$ai_target <- sample(shared_store$game_images, 1)
    shared_store$chat <- data.frame(User=character(0), Msg=character(0), Color=character(0), stringsAsFactors=F)
  })
  
  observeEvent(input$force_reset, { session$reload() })
}

shinyApp(ui, server)