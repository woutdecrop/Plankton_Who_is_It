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
- Analyze the target using chat and trait buttons.
- Activate Guess Mode to select the correct specimen.

Multiplayer:
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
