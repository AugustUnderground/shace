server <- function(input, output, session) {
    notifications <- reactiveValues(data = list())

    output$menu_notifications <- renderMenu({
        dropdownMenu( type = "notifications"
                    , .list = lapply(notifications$data, identity) )
    })

    output$env_selector <- renderUI({
        envs <- list.files(paste(args$dir, input$select_run, sep = "/"))
        selectInput("select_env", label = "Select Environment", choices = envs)
    })

    output$info_runs <- renderValueBox({
        num_runs <- length(list.files(args$dir))
        valueBox( num_runs, "Runs"
                , icon = icon("list"), color = "light-blue")
    })

    output$info_envs <- renderValueBox({
        num_envs <- length(list.files(paste( args$dir
                                           , input$select_run, sep = "/")))
        valueBox( num_envs, "Environments"
                , icon = icon("globe"), color = "light-blue")
    })

    axis_defaults <- list( gridcolor = 'rgb(255,255,255)', showgrid = TRUE
                         , showline = FALSE, showticklabels = TRUE
                         , tickcolor = 'rgb(127,127,127)', ticks = "outside"
                         , zeroline = FALSE )

    env <- reactiveValues( dir = ""
                         , performance = NULL
                         , target = NULL
                         , sizing = NULL )

    observe({
        intervall <<- reactiveVal(input$num_intervall * 1000)
    })

    env_data_reader <- function(file_path) {
        read_feather(file_path, as_data_frame = TRUE)
    }

    update_env_selection <- function() {
        env$dir <- paste( args$dir, input$select_run, input$select_env
                        , sep = "/" )

        env_performance_data <- paste(env$dir, "performance.ft", sep = "/")
        env_environment_data <- paste(env$dir, "environment.ft", sep = "/")
        env_sizing_data      <- paste(env$dir, "sizing.ft",      sep = "/")
        env_target_data      <- paste(env$dir, "target.ft",      sep = "/")

        env$performance <<- reactiveFileReader( intervall(), session
                                              , env_performance_data
                                              , env_data_reader )
        env$environment <<- reactiveFileReader( intervall(), session
                                              , env_environment_data
                                              , env_data_reader )
        env$sizing      <<- reactiveFileReader( intervall(), session
                                              , env_sizing_data
                                              , env_data_reader )
        env$target      <<- reactiveFileReader( intervall(), session
                                              , env_target_data
                                              , env_data_reader )
        status <- ifelse( (!is.null(env$performance))
                       && (!is.null(env$environment))
                       && (!is.null(env$sizing))
                       && (!is.null(env$target))
                        , "success", "danger" )
        note <- env$dir
        notifications$data$env <- notificationItem( text = note
                                                  , icon("database")
                                                  , status = status )
    }

    output$pick_eps <- renderUI({
        if (! is.null(env$performance)) {
            pickerInput( inputId = "pick_episode"
                       , selected = ifelse( !is.null(env$performance)
                                          , tail( env$performance()[["episode"]]
                                                , n = 1)
                                          , NULL )
                       , label = "Select Episodes:"
                       , choices = unique(env$performance()[["episode"]])
                       , options = list( `actions-box` = TRUE
                                       , `selected-text-format` = "count > 3" )
                       , multiple = TRUE )
        }
    })

    observeEvent(input$select_env, update_env_selection())
    observeEvent(input$button_refresh, update_env_selection())

    output$text_file <- renderText(paste0("DEBUG | Selected Dir :", env$dir))

    output$plots_target <- renderUI({
        if ((! is.null(env$target)) && (! is.null(env$performance))) {
            td <- env$target()
            tp <- Filter(function(p) {p != "episode"}, colnames(td))
            plot_list <- lapply(tp, function(p) {
                                        plotlyOutput(paste0(p, "_target"))
                                    })
            do.call(tagList, plot_list)
        }
    })

    observe({
        if ((! is.null(env$target)) && (! is.null(env$performance))) {
            td <- env$target()
            pd <- env$performance()
            for (p in colnames(td)) {
                if (p != "episode") {
                    local({
                        param <- p
                        output[[paste0(param, "_target")]] <- renderPlotly({
                            tgt <- list( type = "line"
                                       , x0 = 0, x1 = 1
                                       , xref = "paper"
                                       , y0 = td[[param]]
                                       , y1 = td[[param]]
                                       , line =  list( shape = "linear"
                                                     , color = "black"
                                                     , dash = "dot" ))
                            fig <- plot_ly()
                            for (eps in input$pick_episode) {
                                x <- pd[pd$episode == eps, ][["step"]]
                                y <- pd[pd$episode == eps, ][[param]]
                                n <- paste("Episode", eps)
                                l <- list(shape = "linear")
                                fig <- fig %>% add_lines( x = x, y = y
                                                        , name = n, line = l )
                                #fig <- fig %>% add_lines( y = y, name = n
                                #  , line = list(shape = "spline"))
                            }
                            fig %>% layout( title = param
                                          , paper_bgcolor = "rgb(255,255,255)"
                                          , plot_bgcolor  = "rgb(229,229,229)"
                                          , shapes = list(tgt)
                                          , xaxis = c( list(title = "Step")
                                                     , axis_defaults )
                                          , yaxis = c( list(title = param)
                                                     , axis_defaults))
                        })
                    })
                }
            }
        }
    })

    output$plots_op <- renderUI({
        if (! is.null(env$performance)) {
            pd <- env$performance()
            pp <- Filter(function(p) {
                            ((p != "episode") && (p != "step")
                          && (grepl(":gmoverid", p) || grepl(":fug", p)))
                    }, colnames(pd))
            plot_list <- lapply(pp, function(p) {
                                        plotlyOutput(gsub(":", "_", p))
                                    })
            do.call(tagList, plot_list)
        }
    })

    observe({
        if (! is.null(env$performance)) {
            pd <- env$performance()
            for (p in colnames(pd)) {
                if ((p != "episode") && (p != "step")
                 && (grepl(":gmoverid", p) || grepl(":fug", p))) {
                    local({
                        param <- p
                        output[[gsub(":", "_", param)]] <- renderPlotly({
                            fig <- plot_ly()
                            for (eps in input$pick_episode) {
                                x <- pd[pd$episode == eps, ][["step"]]
                                y <- pd[pd$episode == eps, ][[param]]
                                n <- paste("Episode", eps)
                                l <- list(shape = "linear")
                                fig <- fig %>% add_lines( x = x, y = y
                                                        , name = n, line = l )
                            }
                            fig %>% layout( title = param
                                          , paper_bgcolor = "rgb(255,255,255)"
                                          , plot_bgcolor  = "rgb(229,229,229)"
                                          , xaxis = c( list(title = "Step")
                                                     , axis_defaults )
                                          , yaxis = c( list( title = param)
                                                     , axis_defaults))
                        })
                    })
                }
            }
        }
    })

    output$plots_sizing <- renderUI({
        if (! is.null(env$sizing)) {
            sd <- env$sizing()
            sp <- Filter( function(p) {(p != "episode") && (p != "step")}
                        , colnames(sd))
            plot_list <- lapply(sp, function(p) {
                                        plotlyOutput(paste0(p, "_sizing"))
                                    })
            do.call(tagList, plot_list)
        }})

    observe({
        if (! is.null(env$sizing)) {
            sd <- env$sizing()
            for (p in colnames(sd)) {
                if (p != "episode") {
                    local({
                        param <- p
                        output[[paste0(param, "_sizing")]] <- renderPlotly({
                            fig <- plot_ly()
                            for (eps in input$pick_episode) {
                                x <- sd[sd$episode == eps, ][["step"]]
                                y <- sd[sd$episode == eps, ][[param]]
                                n <- paste("Episode", eps)
                                l <- list(shape = "linear")
                                fig <- fig %>% add_lines( x = x, y = y
                                                        , name = n
                                                        , line = l )
                            }
                            fig %>% layout( title = param
                                          , paper_bgcolor = "rgb(255,255,255)"
                                          , plot_bgcolor  = "rgb(229,229,229)"
                                          , xaxis = c( list(title = "Step")
                                                     , axis_defaults )
                                          , yaxis = c( list( title = param)
                                                     , axis_defaults))
                        })
                    })
                }
            }
        }
    })

#pd <- read_feather("/tmp/uhlmanny/gace/20220228-131001-pool/env_11/performance.ft", as_data_frame = TRUE)

    output$plots_env <- renderUI({
        if (! is.null(env$environment)) {
            ed <- env$environment()
            ep <- c("reward_episode", "reward_mean", "reward_total")
            plot_list <- lapply(ep, function(p) {
                                        plotlyOutput(p)
                                    })
            do.call(tagList, plot_list)
        }})

    observe({
        if (! is.null(env$environment)) {
            ed <- env$environment()

            output$reward_episode <- renderPlotly({
                fig <- plot_ly()
                for (eps in input$pick_episode) {
                    x <- ed[ed$episode == eps, ][["step"]]
                    r <- ed[ed$episode == eps, ][["reward"]]
                    n <- paste("Episode", eps)
                    l <- list(shape = "linear")
                    fig <- fig %>% add_lines(x = x, y = r, name = n, line = l)
                }
                fig %>% layout( title = "Step Reward"
                              , paper_bgcolor = "rgb(255,255,255)"
                              , plot_bgcolor  = "rgb(229,229,229)"
                              , xaxis = c(list(title = "Step"), axis_defaults)
                              , yaxis = c(list( title = "Reward"), axis_defaults) )
            })
            output$reward_total <- renderPlotly({
                ag <- aggregate(ed$reward, list(ed$episode), FUN=sum)
                e <- ag$Group.1
                r <- ag$x
                fig <- plot_ly( x = e, y = r, name = "Total Reward / Episode"
                             , line = list(shape = "linear"))
                fig %>% layout( title = "Total Reward per Episode"
                              , paper_bgcolor = "rgb(255,255,255)"
                              , plot_bgcolor  = "rgb(229,229,229)"
                              , xaxis = c(list(title = "Step"), axis_defaults)
                              , yaxis = c(list( title = "Reward"), axis_defaults) )
            })
            output$reward_mean <- renderPlotly({
                ag <- aggregate(ed$reward, list(ed$episode), FUN=mean)
                e <- ag$Group.1
                r <- ag$x
                fig <- plot_ly( x = e, y = r, name = "Average Reward / Episode"
                             , line = list(shape = "linear"))
                fig %>% layout( title = "Average Reward per Episode"
                              , paper_bgcolor = "rgb(255,255,255)"
                              , plot_bgcolor  = "rgb(229,229,229)"
                              , xaxis = c(list(title = "Step"), axis_defaults)
                              , yaxis = c(list( title = "Reward"), axis_defaults) )
            })
        }
    })
}
