# app.R

# Define app directory
APP_DIR <- "C:/Users/simon/Dropbox/My_Projects/NFL_4th_Down_App"

# Source scripts
source(file.path(APP_DIR, "src/global.R"))
source(file.path(APP_DIR, "src/utils.R"))
source(file.path(APP_DIR, "src/ui.R"))
source(file.path(APP_DIR, "src/server.R"))

# Specify application settings
options(shiny.host = "0.0.0.0")
options(shiny.port = 6162)
options(shiny.maxRequestSize = 100 * 1024^2)

shinyApp(ui = ui, server = server)