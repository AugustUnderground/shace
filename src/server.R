server <- function(input, output, session) {
    notifications <- reactiveValues(data = list())

    output$menu_notifications <- renderMenu({
        dropdownMenu( type = "notifications"
                    , .list = lapply(notifications$data, identity) )
    })

    output$env_selector <- renderUI({
        envs <- Filter( function(f) {grepl("env_", f)} 
                      , list.files(paste( args$dir
                                        , input$select_run
                                        , sep = "/")))
        selectInput("select_env", label = "Select Environment", choices = envs)
    })

    output$info_runs <- renderValueBox({
        num_runs <- length(list.files(args$dir))
        valueBox( num_runs, "Runs"
                , icon = icon("list"), color = "light-blue")
    })

    output$info_envs <- renderValueBox({
        num_envs <- length( Filter( function(f) {grepl("env_", f)} 
                                  , list.files(paste( args$dir
                                           , input$select_run, sep = "/"))))
        valueBox( num_envs, "Environments"
                , icon = icon("globe"), color = "light-blue")
    })

    axis_defaults <- list( gridcolor = 'rgb(255,255,255)', showgrid = TRUE
                         , showline = FALSE, showticklabels = TRUE
                         , tickcolor = 'rgb(127,127,127)', ticks = "outside"
                         , zeroline = FALSE )

    palette <- brewer.pal(n = 8, name = "Dark2")
    #palette <- rep(brewer.pal(n = 8, name = "Dark2"), each = 2)

    env <- reactiveValues( dir = ""
                         , performance = NULL
                         , target = NULL
                         , sizing = NULL 
                         , loss = NULL
                         , reward = NULL )

    observe({
        intervall <<- reactiveVal(input$num_intervall * 1000)
    })

    env_data_reader <- function(file_path) {
        read.csv(file_path, header = TRUE, sep = ",")
    }

    mdl_data_reader <- function(file_path) {
        read.csv(file_path, header = TRUE, sep = ",")
    }

    update_env_selection <- function() {
        env$dir <- paste( args$dir, input$select_run, input$select_env
                        , sep = "/" )
        mdl_dir <- paste(args$dir, input$select_run, "model", sep = "/")

        env_performance_data <- paste(env$dir, "performance.csv", sep = "/")
        env_environment_data <- paste(env$dir, "environment.csv", sep = "/")
        env_sizing_data      <- paste(env$dir, "sizing.csv",      sep = "/")
        env_target_data      <- paste(env$dir, "target.csv",      sep = "/")
        mdl_loss_data        <- paste(mdl_dir, "loss.csv",        sep = "/")
        mdl_reward_data      <- paste(mdl_dir, "reward.csv",      sep = "/")

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

        if (file.exists(mdl_loss_data) && file.exists(mdl_reward_data)) {
            env$loss        <<- reactiveFileReader( intervall(), session
                                                  , mdl_loss_data
                                                  , mdl_data_reader )
            env$reward      <<- reactiveFileReader( intervall(), session
                                                  , mdl_reward_data
                                                  , mdl_data_reader )
        } else {
            env$loss   <<- NULL
            env$reward <<- NULL
        }

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
                            fig <- plot_ly()

                            for (eps in input$pick_episode) {
                                e <- strtoi(eps)
                                tc <- palette[((e %% 8) + 1)]
                                x <- pd[pd$episode == eps, ][["step"]]
                                y <- pd[pd$episode == eps, ][[param]]
                                l <- loess(y~x, span = 0.5)
                                n <- paste("Episode", eps)
                                fig <- fig %>% add_trace( x = x, y = y
                                                        , color = tc
                                                        , name = n
                                                        , colors = c()
                                                        , type = "scatter"
                                                        , mode = "markers" )
                                fig <- fig %>% add_trace( x = x, y = predict(l)
                                                        , color = tc
                                                        , name = n
                                                        , colors = c()
                                                        , type = "scatter"
                                                        , mode = "line" )
                                ty <- rep( td[td$episode == eps, ][[param]]
                                         , length(x) )

                                fig <- fig %>% add_trace( x = x, y = ty
                                                        , type = "scatter"
                                                        , color = tc
                                                        , line = list( width = 2
                                                                     , dash = "dash" )
                                                        , name = paste0(n, " Target")
                                                        , colors = c()
                                                        , mode = "lines" )
                            }
                            fig %>% layout( title = param
                                          , paper_bgcolor = "rgb(255,255,255)"
                                          , plot_bgcolor  = "rgb(229,229,229)"
                                          , xaxis = c( list(title = "Step")
                                                     , axis_defaults )
                                          , yaxis = c( list(title = param)
                                                     , axis_defaults ))
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

    output$plots_mdl <- renderUI({
        if ((! is.null(env$loss)) && (! is.null(env$reward))) {
            ld <- env$loss()
            rd <- env$reward()

            mp <- c( lapply(unique(ld$Model), function(l) {paste0("loss_",l)})
                   , "reward_per_env", "total" )

            plot_list <- lapply(mp, function(p) { plotlyOutput(p) })

            do.call(tagList, plot_list)
        }})

    observe({
        if ((! is.null(env$loss)) && (! is.null(env$reward))) {
            ld <- env$loss()
            rd <- env$reward()

            #lp <- lapply(unique(ld$Model), function(l) {paste0("loss_", l)})
            lp <- unique(ld$Model)

            for (p in lp) {
                local({
                    param <- p
                    t <- paste(param, "Loss")
                    output[[paste0("loss_", param)]] <- renderPlotly({
                        fig <- plot_ly()
                        x <- ld[(ld$Model == param), ][["Iteration"]]
                        y <- ld[(ld$Model == param), ][["Loss"]]
                        n <- param
                        l <- list(shape = "linear")
                        fig <- fig %>% add_lines( x = x, y = y
                                                , name = n
                                                , line = l )

                        fig %>% layout( title = param
                                      , paper_bgcolor = "rgb(255,255,255)"
                                      , plot_bgcolor  = "rgb(229,229,229)"
                                      , xaxis = c( list(title = "Iteration")
                                                 , axis_defaults )
                                      , yaxis = c( list(title = t)
                                                 , axis_defaults))
                    })
                })
            }

            output[["reward_per_env"]] <- renderPlotly({
                fig <- plot_ly()
                x   <- unique(rd$Iteration)
                avg <- aggregate(rd$Reward, list(rd$Iteration), FUN = mean)$x
                hi  <- aggregate(rd$Reward, list(rd$Iteration), FUN = max)$x
                lo  <- aggregate(rd$Reward, list(rd$Iteration), FUN = min)$x
                la <- loess(avg~x, span = 0.3)
                lh <- loess(hi~x, span = 0.3)
                ll <- loess(lo~x, span = 0.3)

                fig <- fig %>% add_trace( x = x, y = predict(lh)
                                        , type = "scatter"
                                        , mode = "lines"
                                        , line = list(color = "transparent")
                                        , showlegend = FALSE
                                        , name = "High" )
                fig <- fig %>% add_trace( x = x, y = predict(ll)
                                        , type = "scatter"
                                        , mode = "lines"
                                        , fill = "tonexty"
                                        , fillcolor='rgba(55,125,184,0.2)'
                                        , alpha = 0.2
                                        , opacity = 0.2
                                        , line = list(color = "transparent")
                                        , showlegend = FALSE
                                        , name = "Low" )
                fig <- fig %>% add_trace( x = x, y = predict(la) # avg
                                        , type = "scatter"
                                        , mode = "lines"
                                        , line = list(color = "rgba(55,125,184,1.0)")
                                        , showlegend = FALSE
                                        , name = "Average" )
                fig <- fig %>% add_trace( x = rd$Iteration, y = rd$Reward
                                        , type = "scatter"
                                        , mode = "markers"
                                        , marker = list( color = "rgba(55,125,184,0.2)"
                                                        , size = 2.0 )
                                        , showlegend = FALSE
                                        , name = "Actual" )
                fig %>% layout( title = "Average Reward per Iteration"
                              , paper_bgcolor = "rgb(255,255,255)"
                              , plot_bgcolor  = "rgb(229,229,229)"
                              , xaxis = c( list(title = "Iteration")
                                         , axis_defaults )
                              , yaxis = c( list(title = t)
                                         , axis_defaults))
            })
        }
    })
}
