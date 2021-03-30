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


    z_score <- reactive({
        if(input$normCurve_att == 'z_value_select'){
            input$z_value %>%
                round(4)
        }
        else if(input$normCurve_att == 'auc_select'){
            case_when(tail_type() == 'left' ~ qnorm(norm_auc_react()),
                      tail_type() == 'right' ~ qnorm(norm_auc_react(),
                                                     lower.tail = FALSE),
                      tail_type() == 'middle' ~ qnorm(norm_auc_react() + ((1 - norm_auc_react())/2)),
                      tail_type() == 'two' ~ qnorm(norm_auc_react()/2,
                                                   lower.tail = FALSE)
            ) %>%
                round(4)
        }
    })


    norm_auc_react <- reactive({
        if(input$normCurve_att == 'z_value_select'){
            case_when(tail_type() == 'left' ~ pnorm(z_score()),
                      tail_type() == 'right' ~ pnorm(z_score(),
                                                     lower.tail = FALSE),
                      tail_type() == 'middle' ~ pnorm(z_score()) -
                          pnorm(-(z_score())),
                      tail_type() == 'two' ~ 2 * pnorm(-abs(z_score())),
                      TRUE ~ 0) %>%
                round(4)
        }
        else if(input$normCurve_att == 'auc_select'){
            input$norm_auc %>%
                round(4)
        }
    })



    output$normAutoCalc <- renderUI({
        tail_type <- input$norm_tail

        if(input$normCurve_att == 'z_value_select'){
            auc <- case_when(tail_type == 'left' ~ pnorm(z_score()),
                             tail_type == 'right' ~ pnorm(z_score(),
                                                          lower.tail = FALSE),
                             tail_type == 'middle' ~ pnorm(z_score()) -
                                 pnorm(-(z_score())),
                             tail_type == 'two' ~ 2 * pnorm(-abs(z_score())),
                             TRUE ~ 0)

            HTML(paste0(argonH1("Area Under Curve (AUC):",
                                display = 4),
                        h4(round(auc, 3))))
        } else if(input$normCurve_att == 'auc_select'){
            if(between(norm_auc_react(), 0, 1)){
                z_value <- case_when(tail_type() == 'left' ~ qnorm(norm_auc_react()),
                                     tail_type() == 'right' ~ qnorm(norm_auc_react(),
                                                                    lower.tail = FALSE),
                                     tail_type() == 'middle' ~ qnorm(norm_auc_react() + ((1 - norm_auc_react())/2)),
                                     tail_type() == 'two' ~ qnorm(norm_auc_react()/2,
                                                                  lower.tail = FALSE))

            }
            if(is.finite(z_value)){
                HTML(paste0(argonH1("z score:",
                                    display = 4),
                            h4(round(z_value, 3))))
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

        label <- HTML(paste0('z value: ', z_score(), '\n',
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
                  t_tail_type() == 'middle' ~ qt(auc/2 + .5, df = df) - qt(auc/2, df = df),
                  t_tail_type() == 'two' ~ qt(auc/2, df = df)
        ) %>%
            round(4)
    }

    calc_t_auc <- function(t_star, df){
        case_when(t_tail_type() == 'left' ~ pt(t_star, df = df),
                  t_tail_type() == 'right' ~ pt(t_star, df = df,
                                              lower.tail = FALSE),
                  t_tail_type() == 'middle' ~ pt(t_star, df = df) -
                      pt(-(t_star), df = df),
                  t_tail_type() == 'two' ~ pt(t_star, df = df) + pt(-t_star, df = df),
                  TRUE ~ 0) %>%
            round(4)
    }


    t_score <- reactive({
        if(input$tCurve_att == 't_value_select'){
            input$t_value %>%
                round(4)
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
                round(4)
        }
    })


    output$tAutoCalc <- renderUI({

        if(input$tCurve_att == 't_value_select'){
            auc <- t_auc_react()

            HTML(paste0(argonH1("Area Under Curve (AUC):",
                                display = 4),
                        h4(round(auc, 3))))
        } else if(input$tCurve_att == 'auc_select'){
            if(between(t_auc_react(), 0, 1)){

                t_value <- t_score() %>%
                    round(3)

            }
            if(is.finite(t_value)){

                if(t_tail_type() %in% c('middle', 'two')){
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

        label <- HTML(paste0('t value: ', case_when(t_tail_type() %in% c('middle', 'two') ~
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
}

# Run the application
shinyApp(ui = ui, server = server)
