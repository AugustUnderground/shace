library("optparse")
library("arrow")
library("plotly")
library("shiny")
library("shinydashboard")
library("shinyWidgets")

user <- Sys.info()["user"]

opts <- list( make_option( c("-d", "--dir")
                         , type = "character"
                         , default = paste("/tmp", user, "gace", sep = "/")
                         , help = "data log directory"
                         , metavar = "filepath")
            , make_option( c("-H", "--host")
                         , type = "character"
                         , default = "0.0.0.0"
                         , help = "Host address"
                         , metavar = "HOST")
            , make_option( c("-P", "--port")
                         , type = "integer"
                         , default = 6006
                         , help = "Host address port"
                         , metavar = "PORT") )

opt_parser <- OptionParser(option_list = opts);
args <- parse_args(opt_parser)

source("./src/ui.R")
source("./src/server.R")

if (interactive()) {
    shinyApp(ui, server)
} else {
    runApp( list(ui = ui, server = server), host = args$host, port = args$port
          , launch.browser = FALSE )
}
