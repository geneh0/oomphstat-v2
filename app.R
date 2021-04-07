#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(argonDash)
library(argonR)
library(stringr)
library(reactlog)
library(tidyverse)
library(scales)

reactlog_enable()

# -----------------------------------------------------------------------
# Predefine main elements

# fonts
fonts <- "https://fonts.googleapis.com/css2?family=Fira+Code:wght@300&family=Fira+Sans+Condensed:wght@400&family=Fira+Sans:wght@300;900&display=swap"

# sidebar
argonSidebar <- argonDashSidebar(
    vertical = TRUE,
    skin = "light",
    background = "white",
    size = "md",
    side = "left",
    id = "my_sidebar",
    brand_logo = "ph-logo.jpg",
    argonSidebarHeader(title = "Navigation"), br(),
    argonSidebarMenu(
        argonSidebarItem(
            tabName = "01_norm",
            icon = icon("fas fa-bell", lib = 'font-awesome'),
            "The Normal Distribution"),
        argonSidebarItem(
            tabName = "02_t",
            icon = icon("fas fa-tshirt", lib = 'font-awesome'),
            "The t Distribution"),
        # argonSidebarItem(
        # tabName = "03_chi2",
        # icon = icon("fas fa-user-graduate", lib = 'font-awesome'),
        # "The chi-squared Distribution")
        ),
    br())

# argonNav <- argonDashNavbar(
#     argonDropNav(
#         title = "Education Sector COVID-19 Case Maps",
#         orientation = "right",
#     )
# )


# header
argonHeader <- argonDashHeader(
    argonH1("OOMPH Stat", display = 3) %>% argonTextColor('secondary'),
    gradient = TRUE,
    color = "primary",
    separator = TRUE,
    separator_color = "secondary")

argonFooter <- argonDashFooter(
    copyrights = "\uA9 Berkeley Online MPH, 2021",
    src = "http://onlinemph.berkeley.edu",
    argonFooterMenu(
        argonFooterItem("Original OOMPH Stat", src = "https://xandersph.shinyapps.io/OOMPHstat/"),
        argonFooterItem("Originally based on SurfStat", src = "https://www.math.mcgill.ca/keith/surfstat/"),
        argonFooterItem("Source Code", src = "https://github.com/geneh0/oomphstat-v2")
    )
) # end argonDashFooter

# -----------------------------------------------------------------------


# Define UI logic
ui <- argonDashPage(
    title = "OOMPH Stat",
    author = "Gene Ho",
    description = NULL,
    sidebar = argonSidebar,
    # navbar = argonNav,
    header = argonHeader,
    body = argonDashBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = fonts),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        argonTabItems(
            # * this is the structure for adding content to the ui:
            #   + tab item
            #   + row
            #   + card
            #   + row
            # argonTabItem(
            #   argonRow(
            #     argonCard(
            #       argonRow(
            #         plotOutput()
            #       )
            #     )
            #   )
            # ),
            # Normal Distribution ----
            argonTabItem(
                tabName = '01_norm',
                argonH1('Normal Distribution',
                        display = 2),
                argonRow(
                    argonCard(title = argonH1('Inputs',
                                              display = 4),
                              width = 4,
                              # normal tail type buttons
                              argonRow(
                                  radioButtons(
                                      "norm_tail", "Type",
                                      choices = c(
                                          "Left-Tailed" = "left",
                                          "Right-Tailed" = "right",
                                          "Central Area" = "middle",
                                          "Two-Tailed" = "two"
                                      ),
                                      selected = "left"
                                  )
                              ),
                              selectInput(
                                  'normCurve_att',
                                  label = 'Select Curve Input Attribute',
                                  choices = c(
                                      'z score' = 'z_value_select',
                                      'area under curve' = 'auc_select'
                                  ),
                                  width = '100%'
                              ),
                              uiOutput('normAtt',
                                       width = '100%'),
                              uiOutput('normAutoCalc',
                                       width = '100%')
                              # end argonRow
                    ),  # end argonCard

                    argonCard(
                        title = argonH1('Plot',
                                        display = 4),
                        width = 8,
                        plotOutput('normPlot')
                    )
                ) # end argonRow
            ), # end argonTabItem
            # t Distribution ----
            argonTabItem(
                tabName = '02_t',
                argonH1("t Distribution",
                        display = 2),
                argonRow(
                    argonCard(title = argonH1('Inputs',
                                              display = 4),
                              width = 4,
                              # t tail type buttons
                              argonRow(
                                  radioButtons(
                                      "t_tail", "Type",
                                      choices = c(
                                          "Left-Tailed" = "left",
                                          "Right-Tailed" = "right",
                                          "Central Area" = "middle",
                                          "Two-Tailed" = "two"
                                      ),
                                      selected = "left"
                                  )
                              ),
                              argonRow(
                                  numericInput(
                                      "tdf",
                                      "Enter Degrees of Freedom",
                                      value = 2,
                                      min = 2,
                                      max = Inf,
                                      step = 1
                                  )
                              ),
                              selectInput(
                                  'tCurve_att',
                                  label = 'Select Curve Input Attribute',
                                  choices = c(
                                      't score' = 't_value_select',
                                      'area under curve' = 'auc_select'
                                  ),
                                  width = '100%'
                              ),
                              uiOutput('tAtt',
                                       width = '100%'),
                              uiOutput('tAutoCalc',
                                       width = '100%')
                              # end argonRow
                    ),  # end argonCard

                    argonCard(
                        title = argonH1('Plot',
                                        display = 4),
                        width = 8,
                        plotOutput('tPlot')
                    )
                )
            ), # end argonTabItem
            # chi2 Distribution ----
            argonTabItem(
                tabName = '03_chi2',
                argonH1("t Distribution",
                        display = 2),
                argonRow(
                    argonCard(title = argonH1('Inputs',
                                              display = 4),
                              width = 4,
                              # t tail type buttons
                              argonRow(
                                  radioButtons(
                                      "chi2_tail", "Type",
                                      choices = c(
                                          "Left-Tailed" = "left",
                                          "Right-Tailed" = "right"
                                      ),
                                      selected = "left"
                                  )
                              ),
                              argonRow(
                                  numericInput(
                                      "chi2df",
                                      "Enter Degrees of Freedom",
                                      value = 2,
                                      min = 2,
                                      max = Inf,
                                      step = 1
                                  )
                              ),
                              selectInput(
                                  'chi2Curve_att',
                                  label = 'Select Curve Input Attribute',
                                  choices = c(
                                      'chi2 score' = 'chi2_value_select',
                                      'area under curve' = 'chi2_auc_select'
                                  ),
                                  width = '100%'
                              ),
                              uiOutput('chi2Att',
                                       width = '100%'),
                              uiOutput('chi2AutoCalc',
                                       width = '100%')
                              # end argonRow
                    ),  # end argonCard

                    argonCard(
                        title = argonH1('Plot',
                                        display = 4),
                        width = 8,
                        plotOutput('chi2Plot')
                    )
                )
            ) # end argonTabItem
        ) # end argonTabItems
    ), # end argonDashBody
    footer = argonFooter
) # end argonDashPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    df <- data.frame(x = c(-10, 10))

    # normal distribution ----

    tail_type <- reactive({input$norm_tail})

    output$normAtt <- renderUI({
        if(input$normCurve_att == 'z_value_select'){
            print('stop')
            numericInput(
                'z_value',
                label = NULL,
                value = 0,
                min = -5,
                max = 5,
                width = '100%')
        } else if(input$normCurve_att == 'auc_select'){
            numericInput(
                'norm_auc',
                label = NULL,
                value = 0,
                min = 0,
                max = 1,
                width = '100%')
        }
    })


    calc_z_score <- function(auc){
        case_when(tail_type() == 'left' ~ qnorm(auc),
                  tail_type() == 'right' ~ qnorm(auc,
                                                 lower.tail = FALSE),
                  tail_type() == 'middle' ~ qnorm(0.50 + auc/2),
                  tail_type() == 'two' ~ qnorm(auc/2)
        ) %>%
            round(7)
    }

    calc_z_auc <- function(z_val){
        case_when(tail_type() == 'left' ~ pnorm(z_score()),
                  tail_type() == 'right' ~ pnorm(z_score(),
                                                 lower.tail = FALSE),
                  tail_type() == 'middle' ~ pnorm(abs(z_score())) -
                      pnorm(-abs(z_score())),
                  tail_type() == 'two' ~ 2 * pnorm(-abs(z_score())),
                  TRUE ~ 0) %>%
            round(7)
    }


    z_score <- reactive({
        if(input$normCurve_att == 'z_value_select'){

            print(input$z_value)
            input$z_value %>%
                round(7)
        }
        else if(input$normCurve_att == 'auc_select'){
            calc_z_score(norm_auc_react())
        }
    })


    norm_auc_react <- reactive({
        if(input$normCurve_att == 'z_value_select'){

            print(z_score())
            calc_z_auc(z_score())


        }
        else if(input$normCurve_att == 'auc_select'){
            input$norm_auc %>%
                round(7)
        }
    })



    output$normAutoCalc <- renderUI({
        tail_type <- input$norm_tail

        if(input$normCurve_att == 'z_value_select'){
            auc <- norm_auc_react()

            HTML(paste0(argonH1("Area Under Curve (AUC):",
                                display = 4),
                        h4(round(auc, 5))))
        } else if(input$normCurve_att == 'auc_select'){
            if(between(norm_auc_react(), 0, 1)){
                z_value <- z_score()
            }
            if(is.finite(z_value)){

                if(tail_type() %in% c('middle', 'two') & z_score() != 0){
                    z_value <- paste('+/-', abs(z_value))
                }

                HTML(paste0(argonH1("z score:",
                                    display = 4),
                            h4(z_value)))


            } else {
                "Please enter a value between 0 and 1. "
            }
        }
    })

    output$normPlot <- renderPlot({
        tail_type <- input$norm_tail


        lower_lim <- case_when(tail_type %in% c('left', 'two') ~ -10,
                               tail_type == 'right' ~ as.double(z_score()),
                               tail_type == 'middle' ~ -as.double(z_score()),
                               TRUE ~ 0)
        upper_lim <- case_when(tail_type %in% c('right', 'two') ~ 10,
                               tail_type == 'left' ~ as.double(z_score()),
                               tail_type == 'middle' ~ as.double(z_score()),
                               TRUE ~ 0)

        label <- HTML(paste0('z value: ', case_when(tail_type() %in% c('middle', 'two') & z_score() != 0 ~
                                                        paste('+/-', abs(z_score())),
                                                    TRUE ~ as.character(z_score())), '\n',
                             'auc: ', norm_auc_react()))


        ggplot(data = df, aes(x = x)) +
            stat_function(fun = dnorm,
                          geom = 'area',
                          xlim = c(lower_lim, upper_lim),
                          fill = "#FDB515") +
            stat_function(fun = dnorm,
                          geom = 'line',
                          size = 1.5,
                          xlim = c(-10, 10),
                          color = '#3B7EA1') +
            xlim(-4, 4) +
            annotate('text',
                     label = label,
                     x = Inf, y = Inf,
                     hjust = 1,
                     vjust = 2,
                     color = 'red',
                     size = 6) +
            labs(x = 'z score') +
            theme_minimal() +
            if(tail_type == 'two'){
                stat_function(fun = dnorm,
                              geom = 'area',
                              fill = 'white',
                              xlim = c(-abs(z_score()), abs(z_score())))
            } else NULL
    })

    # t distribution ----

    t_tail_type <- reactive({input$t_tail})
    t_df <- reactive({input$tdf})


    output$tAtt <- renderUI({
        if(input$tCurve_att == 't_value_select'){
            numericInput(
                't_value',
                label = NULL,
                value = 0,
                min = -5,
                max = 5,
                width = '100%')
        } else if(input$tCurve_att == 'auc_select'){
            numericInput(
                't_auc',
                label = NULL,
                value = 0,
                min = 0,
                max = 1,
                width = '100%')
        }
    })

    calc_t_score <- function(auc, df){
        case_when(t_tail_type() == 'left' ~ qt(auc, df = df),
                  t_tail_type() == 'right' ~ qt(auc, df = df,
                                              lower.tail = FALSE),
                  # for t-val of middle, want half of prob to be above 50%
                  # then add 50%
                  t_tail_type() == 'middle' ~ qt(auc/2 + .5, df = df),
                  t_tail_type() == 'two' ~ qt(auc/2, df = df)
        ) %>%
            round(7)
    }

    calc_t_auc <- function(t_star, df){
        case_when(t_tail_type() == 'left' ~ pt(t_star, df = df),
                  t_tail_type() == 'right' ~ pt(t_star, df = df,
                                              lower.tail = FALSE),
                  t_tail_type() == 'middle' ~ pt(abs(t_star), df = df) -
                      pt(-abs(t_star), df = df),
                  t_tail_type() == 'two' ~ 2 * pt(t_star, df = df),
                  TRUE ~ 0) %>%
            round(7)
    }


    t_score <- reactive({
        if(input$tCurve_att == 't_value_select'){
            input$t_value %>%
                round(7)
        }
        else if(input$tCurve_att == 'auc_select'){
            calc_t_score(t_auc_react(), t_df())
        }
    })




    t_auc_react <- reactive({
        if(input$tCurve_att == 't_value_select'){
            calc_t_auc(t_score(), t_df())
        }
        else if(input$tCurve_att == 'auc_select'){
            input$t_auc %>%
                round(7)
        }
    })


    output$tAutoCalc <- renderUI({

        if(input$tCurve_att == 't_value_select'){
            auc <- t_auc_react()

            HTML(paste0(argonH1("Area Under Curve (AUC):",
                                display = 4),
                        h4(round(auc, 5))))
        } else if(input$tCurve_att == 'auc_select'){
            if(between(t_auc_react(), 0, 1)){

                t_value <- t_score() %>%
                    round(7)

            }
            if(is.finite(t_value)){

                if(t_tail_type() %in% c('middle', 'two') & t_score() != 0){
                    t_value <- paste('+/-', abs(t_value))
                }

                HTML(paste0(argonH1("t score:",
                                    display = 4),
                            h4(t_value)))
            } else {
                "Please enter a value between 0 and 1. "
            }
        }
    })

    output$tPlot <- renderPlot({


        lower_lim <- case_when(t_tail_type() %in% c('left', 'two') ~ -10,
                               t_tail_type() == 'right' ~ as.double(t_score()),
                               t_tail_type() == 'middle' ~ -as.double(t_score()),
                               TRUE ~ 0)
        upper_lim <- case_when(t_tail_type() %in% c('right', 'two') ~ 10,
                               t_tail_type() == 'left' ~ as.double(t_score()),
                               t_tail_type() == 'middle' ~ as.double(t_score()),
                               TRUE ~ 0)

        label <- HTML(paste0('t value: ', case_when(t_tail_type() %in% c('middle', 'two') & t_score() != 0 ~
                                                        paste('+/-', abs(t_score())),
                                                    TRUE ~ as.character(t_score())), '\n',
                             'df: ', t_df(), '\n',
                             'auc: ', t_auc_react()))


        ggplot(data = df, aes(x = x)) +
            stat_function(fun = ~ dt(.x, df = t_df()),
                          geom = 'area',
                          xlim = c(lower_lim, upper_lim),
                          fill = "#FDB515") +
            stat_function(fun = ~ dt(.x, df = t_df()),
                          geom = 'line',
                          size = 1.5,
                          xlim = c(-10, 10),
                          color = '#3B7EA1') +
            xlim(-4, 4) +
            annotate('text',
                     label = label,
                     x = Inf, y = Inf,
                     hjust = 1,
                     vjust = 2,
                     color = 'red',
                     size = 6) +
            labs(x = 't score') +
            theme_minimal() +
            if(t_tail_type() == 'two'){
                stat_function(fun = ~ dt(.x, df = t_df()),
                              geom = 'area',
                              fill = 'white',
                              xlim = c(-abs(t_score()), abs(t_score())))
            } else NULL
    })

    # chi2 distribution ----

    chi2_tail_type <- reactive({input$chi2_tail})
    chi2_df <- reactive({input$chi2df})
    chi2_dta <- data.frame(x = c(0, 100))


    output$chi2Att <- renderUI({
        if(input$chi2Curve_att == 'chi2_value_select'){
            numericInput(
                'chi2_value',
                label = NULL,
                value = 0,
                min = 0,
                max = Inf,
                width = '100%')
        } else if(input$chi2Curve_att == 'chi2_auc_select'){
            numericInput(
                'chi2_auc',
                label = NULL,
                value = 0,
                min = 0,
                max = 1,
                width = '100%')
        }
    })

    calc_chi2_score <- function(auc, df){
        case_when(chi2_tail_type() == 'left' ~ qchisq(auc, df = df),
                  chi2_tail_type() == 'right' ~ qchisq(auc, df = df,
                                                lower.tail = FALSE),
                  # chi2_tail_type() == 'exact' ~ dchisq()
                  TRUE ~ 0
        ) %>%
            round(7)
    }

    calc_chi2_auc <- function(chi2_star, df){
        case_when(chi2_tail_type() == 'left' ~ pchisq(chi2_star, df = df),
                  chi2_tail_type() == 'right' ~ pchisq(chi2_star, df = df,
                                                       lower.tail = FALSE),
                  TRUE ~ 0) %>%
            round(7)
    }


    chi2_score <- reactive({
        if(input$chi2Curve_att == 'chi2_value_select'){
            input$chi2_value %>%
                round(7)
        }
        else if(input$chi2Curve_att == 'chi2_auc_select'){
            calc_chi2_score(chi2_auc_react(), t_df())
        }
    })



    chi2_auc_react <- reactive({
        if(input$chi2Curve_att == 'chi2_value_select'){

            calc_chi2_auc(chi2_score(), chi2_df())
        }
        else if(input$chi2Curve_att == 'chi2_auc_select'){

            input$chi2_auc %>%
                round(7)
        }
    })


    output$chi2AutoCalc <- renderUI({

        if(input$chi2Curve_att == 'chi2_value_select'){

            auc <- chi2_auc_react()

            HTML(paste0(argonH1("Area Under Curve (AUC):",
                                display = 4),
                        h4(round(auc, 5))))
        } else if(input$chi2Curve_att == 'chi2_auc_select'){
            if(between(chi2_auc_react(), 0, 1)){

                chi2_value <- chi2_score() %>%
                    round(7)


            }
            if(is.finite(chi2_value)){

                if(chi2_tail_type() %in% c('middle', 'two') & chi2_score() != 0){
                    chi2_value <- paste('+/-', abs(chi2_value))
                }

                HTML(paste0(argonH1(withMathJax("$\\chi^2$ score:"),
                                    display = 4),
                            h4(chi2_value)))
            } else {
                "Please enter a value between 0 and 1. "
            }
        }
    })

    output$chi2Plot <- renderPlot({


        lower_lim <- case_when(chi2_tail_type() %in% c('left', 'right') ~ 0,
                               TRUE ~ 0)
        upper_lim <- case_when(chi2_tail_type() %in% c('right', 'left') ~ max(50, chi2_df() * 2),
                               TRUE ~ 0)

        lower_lim_area <- case_when(chi2_tail_type() == 'left' ~ as.double(0),
                                    chi2_tail_type() ==  'right' ~ as.double(chi2_score()),
                                    TRUE ~ 0)
        upper_lim_area <- case_when(chi2_tail_type() == 'left' ~ as.double(chi2_score()),
                                    chi2_tail_type() ==  'right' ~ upper_lim,
                                    TRUE ~ 0)

        label <- HTML(paste0('chi2 value: ', chi2_score(), '\n',
                             'df: ', chi2_df(), '\n',
                             'auc: ', chi2_auc_react()))

        print(c(lower_lim, upper_lim))

        print(c(lower_lim_area, upper_lim_area))


        ggplot(data = chi2_dta, aes(x = x)) +
            stat_function(fun = ~ dchisq(.x, df = chi2_df()),
                          geom = 'area',
                          xlim = c(lower_lim_area, upper_lim_area),
                          fill = "#FDB515") +
            stat_function(fun = ~ dchisq(.x, df = chi2_df()),
                          geom = 'line',
                          size = 1.5,
                          xlim = c(lower_lim, upper_lim),
                          color = '#3B7EA1') +
            xlim(0, upper_lim) +
            annotate('text',
                     label = label,
                     x = Inf, y = Inf,
                     hjust = 1,
                     vjust = 2,
                     color = 'red',
                     size = 6) +
            labs(x = 't score') +
            theme_minimal()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
