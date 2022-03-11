## Side Bar Menus
home_menu    <- menuItem( "Home", tabName = "tab_home"
                        , icon = icon("home"))

target_menu  <- menuItem( "Target", tabName = "tab_target"
                        , icon = icon("chart-line"))

op_menu      <- menuItem( "Operating Point", tabName = "tab_op"
                        , icon = icon("chart-line"))

sizing_menu  <- menuItem( "Sizing", tabName = "tab_sizing"
                        , icon = icon("chart-line"))

env_menu   <- menuItem( "Environment", tabName = "tab_env"
                      , icon = icon("globe"))

mdl_menu   <- menuItem( "Model", tabName = "tab_mdl"
                      , icon = icon("user-robot"))

report_menu  <- menuItem( "Report", tabName = "tab_report"
                        , icon = icon("file-pdf"))

license_menu <- menuItem( "License", tabName = "tab_license"
                        , icon = icon("file-contract"))

refresh_button <- actionButton( "button_refresh", label = ""
                              , icon = icon("redo"))

intervall_num <- numericInput( "num_intervall", "Refresh Intervall [s]:" 
                             , 60, min = 1, max = 1000 )

eps_pick <-  uiOutput("pick_eps")

## Main Area Tabs
home_tab <- tabItem( "tab_home"
                   , fluidRow( box( title = "Run and Environment Selection"
                                  , status = "primary", solidHeader = TRUE
                                  , collapsible = FALSE
                                  , selectInput( "select_run"
                                           , label = "Select Run Directory"
                                           , choices = list.files(args$dir) )
                                  , uiOutput("env_selector") ) 
                             , valueBoxOutput("info_runs")
                             , valueBoxOutput("info_envs")
                             )
                   , box( title = "Selected Run and Environment Path"
                        , status = "primary"
                        , solidHeader = FALSE, collapsible = TRUE
                        , verbatimTextOutput("text_file") )
                        )

target_tab <- tabItem( "tab_target", uiOutput("plots_target"))

op_tab <- tabItem("tab_op", uiOutput("plots_op"))

sizing_tab <- tabItem("tab_sizing", uiOutput("plots_sizing"))

env_tab <- tabItem("tab_env", uiOutput("plots_env"))

mdl_tab <- tabItem("tab_mdl", uiOutput("plots_mdl"))

report_tab <- tabItem("tab_rep")

license_tab <- tabItem("tab_license")

## UI
header <- dashboardHeader( title = "SHACE"
                         , dropdownMenuOutput("menu_notifications") )

sidebar <- dashboardSidebar(sidebarMenu( id = "tabs"
                                       , home_menu, target_menu, op_menu
                                       , sizing_menu, env_menu, mdl_menu
                                       , report_menu, license_menu
                                       , eps_pick, intervall_num
                                       , refresh_button ))

body <- dashboardBody(tabItems( home_tab, target_tab, op_tab, sizing_tab
                              , env_tab, mdl_tab, report_tab, license_tab ))

ui <- dashboardPage(skin = "black", header, sidebar, body)
