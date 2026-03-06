Plankton "Who Is It?" 🦠

An interactive Shiny-based game to identify plankton species from images. Designed for both singleplayer and multiplayer modes, simulating a marine laboratory experience where players act as scientists analyzing plankton specimens.

Features

Singleplayer:
- Mission Mode: Analyze a hidden plankton target chosen from the image library.
- Trait Analysis: Ask questions about species traits, device type, or location.
- Guess Mode: Attempt to identify the target species correctly.
- Hint System: Eliminate one incorrect option at a time (resets streak).
- Win Streak Tracking: Track consecutive correct identifications.
- Dynamic Filtering: Filter images by device type or location.
- Zoom Functionality: Inspect images in detail.
- Built with Shiny, shinyjs, and shinyWidgets.

Multiplayer:
- Live Game State: Players pick secret targets from a shared image pool.
- Collaborative / Competitive Play: Guess other players’ targets while tracking wins and streaks.
- Shared Chat: Communicate with other players in real-time.
- Leaderboard: Displays top scientists based on successful identifications.
- Avatar & Theme Customization: Choose your station color and emoji.
- Reactive Filtering and Zoom: Same as singleplayer, applied to shared pool.

Offline Multiplayer (against AI):
- The same look-and-feel as online multiplayer, but your opponent is the built‑in Station AI.
- AI automatically selects a secret card and answers your questions with logic based on its target.
- You can also watch the AI eliminate suspects and occasionally ask you questions about your own card.
- Scoreboard keeps track of human vs. AI wins and the top scientist banner will include the AI if it leads.

Singleplayer (AI‑only):
- Classic mission mode where you never choose a secret – the AI encrypts one for you.
- Ask trait questions through chat or quick buttons and eliminate suspects manually.
- Guess mode awards streak points and you can request hints that sacrifice streaks.
- This version uses the enhanced layout and features from the multiplayer script.

Installation

1. Clone the repository:
    git clone https://github.com/<your-username>/plankton-who-is-it.git
    cd plankton-who-is-it

2. Install R packages:
    install.packages(c("shiny", "shinyjs", "shinyWidgets", "colourpicker", "readxl", "shinycssloaders", "digest"))

3. Prepare the Images directory:
- Organize plankton images into subfolders: Device_Location/Species/Images
- Include an optional species_list.xlsx file for trait metadata.

Usage

Singleplayer:
Run the game in R:
    library(shiny)
    runApp("singleplayer.R")

- Enter your scientist ID and select a theme color.
- Analyze the AI's encrypted target using chat and trait buttons.
- Activate Guess Mode to identify the specimen and build a streak.

Offline Multiplayer (against AI):
Run the game in R:
    library(shiny)
    runApp("multiplayer_offline.R")

- Play a head‑to‑head match with the Station AI (no network required).
- Both you and the AI pick secret cards and take turns eliminating suspects.
- The AI will automatically ask questions, eliminate cards, and try to guess your target.

Multiplayer (online):
Run the game in R:
    library(shiny)
    runApp("multiplayer_preloaded.R")

- Register with a name, station color, and emoji.
- Pick your secret target from the image pool.
- Use chat and elimination tools to identify other players’ targets.
- Check the Hall of Fame leaderboard for top scientists.

File Structure

Plankton_Who_Is_It/
    Images/                  - Image library for plankton species
        Device_Location/
        species_list.xlsx      - Optional metadata
    singleplayer.R           - Singleplayer game code
    multiplayer_preloaded.R  - Multiplayer game code
    README.txt               - This file
    WhoIsIt.Rproj            - RStudio project
    .Rhistory                - R history
    www/                     - Static web assets (optional)

Customization

- Device Colors: Modify the device_colors vector to customize hardware coloring.
- Excel Metadata: Add columns in species_list.xlsx for traits, devices, or locations to enable AI questions.
- Filters: Update the list of devices or locations in the UI section of the scripts.

License

MIT License

Acknowledgments

- Built with Shiny (https://shiny.rstudio.com/) for interactive web applications in R.
- Uses shinyWidgets, shinyjs, colourpicker, and shinycssloaders for enhanced UI/UX.
