#    ---------------------------------------------------------------------    #
#                                                                             #
#   App:    "OOMPH Stat" Statistics Visualizer                                #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Author: Xander Posner, MPH '20 | Epidemiology & Biostatistics             #
#           Instructional Designer, Online/On-Campus MPH Program (OOMPH)      #
#           UC Berkeley School of Public Health                               #
#           xander@berkeley.edu                                               #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Course: PBHLTH W142                                                       #
#           Statistics and Probability for Biology and Public Health          #
#           Professor Maureen Lahiff, Ph.D., lahiff@berkeley.edu              #
#           Online/On-Campus Master of Public Health Program (OOMPH)          #
#           School of Public Health                                           #
#           University of California, Berkeley                                #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Find out more about building applications with Shiny here:                #
#                                                                             #
#   https://shiny.rstudio.com/                                                #
#                                                                             #
#   ----------------------------------------------------------------------    #


# libraries required
library(shiny)
library(shinydashboard)
library(stats)
library(readxl)
library(rsconnect)
library(tidyverse)


################################# Norm Tab UI #################################


# 1. norm yellow Inputs box
norm_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 4,
    verticalLayout(
        # Tail type radio buttons box
        radioButtons(
            "norm_tail",
            "Type",
            choices = c(
                "Left-Tailed" = "left",
                "Right-Tailed" = "right",
                "Central Area" = "middle",
                "Two-Tailed" = "two"
            ),
            selected = "left",
            inline = FALSE
        ),
        # Z-score numeric input widget
        numericInput(
            "z",
            "Enter Z-Score",
            value = 0,
            min = -Inf,
            max = Inf,
            step = 0.5
        ),
        # Arrow icons radio buttons
        radioButtons(
            "norm_arrow",
            NULL,
            choiceNames = list(icon("arrow-up"),
                               icon("arrow-down")),
            choiceValues = list("up",
                                "down"),
            inline = TRUE,
            selected = "down"
        ),
        # Area numeric inputs widget
        numericInput(
            "norm_area",
            "Area Under the Curve",
            value = 0,
            min = 0,
            max = 1,
            step = 0.01
        ),
        radioButtons(
            "action",
            NULL,
            choices = c("Submit" = "go",
                        "Clear" = "reset"),
            selected = "reset",
            inline = TRUE
        ),
        box(
            title = "Notes",
            status = "danger",
            solidHeader = FALSE,
            h4(
                em("Make sure Z-Score and Area are not blank before clicking Submit"),
                style = "text-align:center"
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
        )
    )
)

# 2. norm blue Plot box
norm_plot_box <- box(
    plotOutput("normPlot"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 8
)

# 3. norm first row
norm_row1 <- fluidRow(norm_input_box, norm_plot_box)

# 4. Normal Distribution tab
norm_tab <- tabItem(tabName = "norm",
                    h2("The Normal Distribution"),
                    norm_row1)


################################### t Tab UI ##################################


# 1. t yellow Inputs box
t_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 4,
    verticalLayout(
        # Tail type radio buttons box
        radioButtons(
            "t_tail",
            "Type",
            choices = c(
                "Left-Tailed" = "left",
                "Right-Tailed" = "right",
                "Central Area" = "middle",
                "Two-Tailed" = "two"
            ),
            selected = "left"
        ),
        numericInput(
            "df",
            "Enter Degrees of Freedom",
            value = 2,
            min = 2,
            max = Inf,
            step = 1
        ),
        # T-statistic numeric input widget
        numericInput(
            "t",
            "Enter T-Statistic",
            value = 0,
            min = -Inf,
            max = Inf,
            step = 0.5
        ),
        # Arrow icons radio buttons
        radioButtons(
            "t_arrow",
            NULL,
            choiceNames = list(icon("arrow-up"),
                               icon("arrow-down")),
            choiceValues = list("up",
                                "down"),
            inline = TRUE,
            selected = "down"
        ),
        # Area numeric inputs widget
        numericInput(
            "t_area",
            "Area Under the Curve",
            value = 0,
            min = 0,
            max = 1,
            step = 0.01
        ),
        radioButtons(
            "t_action",
            NULL,
            choices = c("Submit" = "go",
                        "Clear" = "reset"),
            selected = "reset",
            inline = TRUE
        ),
        box(
            title = "Notes",
            status = "danger",
            solidHeader = FALSE,
            h4(
                em(
                    "Make sure t-statistic and Area are not blank before clicking Submit"
                ),
                style = "text-align:center"
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
        )
    )
)

# 2. blue t Plot box
t_plot_box <- box(
    plotOutput("tPlot"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 8
)

# 3. t first row
t_row1 <- fluidRow(t_input_box, t_plot_box)

# 4. t Distribution tab
t_tab <- tabItem(tabName = "ttab",
                 h2(uiOutput("t_header")),
                 t_row1)


################################### Chi Tab UI ##################################


# 1. Chi yellow Inputs box
chi_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 4,
    verticalLayout(
        # Tail type radio buttons box
        radioButtons(
            "chi_tail",
            "Type",
            choices = c("Right-Tailed" = "right"),
            selected = "right"
        ),
        numericInput(
            "chidf",
            "Enter Degrees of Freedom",
            value = 2,
            min = 2,
            max = Inf,
            step = 1
        ),
        # Chi-squared-statistic numeric input widget
        numericInput(
            "chi",
            "Enter Chi-Squared Statistic",
            value = 1,
            min = 0,
            max = Inf,
            step = 1
        ),
        # Arrow icons radio buttons
        radioButtons(
            "chi_arrow",
            NULL,
            choiceNames = list(icon("arrow-up"),
                               icon("arrow-down")),
            choiceValues = list("up",
                                "down"),
            inline = TRUE,
            selected = "down"
        ),
        # Area numeric inputs widget
        numericInput(
            "chi_area",
            "Area Under the Curve",
            value = 0,
            min = 0,
            max = 1,
            step = 0.01
        ),
        radioButtons(
            "chi_action",
            NULL,
            choices = c("Submit" = "go",
                        "Clear" = "reset"),
            selected = "reset",
            inline = TRUE
        ),
        box(
            title = "Notes",
            status = "danger",
            solidHeader = FALSE,
            h4(
                em(
                    "Make sure Chi-Squared Statistic and Area are not blank before clicking Submit"
                ),
                style = "text-align:center"
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
        )
    )
)

# 2. blue chi Plot box
chi_plot_box <- box(
    plotOutput("chiPlot"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 8
)

# 3. chi first row
chi_row1 <- fluidRow(chi_input_box, chi_plot_box)

# 4. chi Distribution tab
chi_tab <- tabItem(tabName = "chitab",
                   h2("The Chi-Squared Distribution"),
                   chi_row1)


################################ Source Tab UI ################################

# Source tab contact info box
source_contact_box <- box(
    title = "Author",
    p(uiOutput("contact")),
    collapsible = TRUE,
    collapsed = FALSE,
    solidHeader = TRUE,
    status = "primary",
    width = 6
)

# Source tab R Source Code box
source_code_box <- box(
    title = "Code",
    p(uiOutput("code")),
    collapsible = TRUE,
    collapsed = FALSE,
    solidHeader = TRUE,
    status = "success",
    width = 6
)

source_course_box <- box(
    title = "Course",
    p(uiOutput("course")),
    collapsible = TRUE,
    collapsed = FALSE,
    solidHeader = TRUE,
    status = "warning",
    width = 6
)

source_row1 <-
    fluidRow(source_course_box, source_contact_box, source_code_box)

source_tab <- tabItem(tabName = "source",
                      h2("App Info"),
                      source_row1)


################################# CLT Tab UI #################################


# 1. CLT yellow Inputs box
clt_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 4,
    # Pop. parameter radio buttons box
    radioButtons(
        inputId = "par",
        label = "Population Parameter",
        choices = c("Mean" = "mean",
                    "Proportion" = "prop"),
        inline = TRUE
    ),
    radioButtons(
        "dist",
        "Distribution of Source Population",
        choices = c(
            "Uniform" = "unif",
            "Roughly Symmetric" = "norm",
            "Slightly Skewed" = "sskew",
            "Highly Skewed" = "skew",
            "Binomial" = "binom"
        ),
        selected = "unif"
    ),
    selectInput(
        "var",
        "Variable",
        choices = c(
            "Age" = "age",
            "Height" = "height",
            "SAT Scores" = "SAT",
            "MediCal Costs: Hip & Knee Replacements" = "hip",
            "Annual Income" = "LA",
            "Current Smoker" = "smoke"
        ),
        selected = "age"
    ),
    # Source pop. size slider input
    sliderInput(
        "pop_size",
        "Population Size",
        value = 10000,
        step = 10000,
        min = 10000,
        max = 100000
    ),
    # Sample size slider input
    sliderInput(
        'sampleSize',
        'Sample Size',
        value = 10,
        step = 10,
        min = 10,
        max = 500
    ),
    # Number of samples slider input
    sliderInput(
        "iterate",
        "Sampling Iteration",
        value = 10,
        step = 10,
        min = 10,
        max = 1000
    )
)

# 2. CLT blue Plot box
clt_plot_box <- box(
    plotOutput("plot_pop"),
    plotOutput("plot_smpl_mean"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 8
)

# 3. norm first row
clt_row1 <- fluidRow(clt_input_box, clt_plot_box)

# 4. Normal Distribution tab
clt_tab <- tabItem(tabName = "clt",
                   h2("Central Limit Theorem"),
                   clt_row1)

################################ Dashboard UI #################################

header <- dashboardHeader(title = "OOMPH Stat",
                          titleWidth = "300px")

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(
        "The Normal Distribution",
        tabName = "norm",
        icon = icon("bell")
    ),
    menuItem(
        span("The ", em("t") , "-distribution"),
        tabName = "ttab",
        icon = icon("tshirt")
    ),
    menuItem(
        "The Chi-Squared Distribution",
        tabName = "chitab",
        icon = icon("times")
    ),
    menuItem(
        "The Central Limit Theorem",
        tabName = "clt",
        icon = icon("align-center")
    ),
    menuItem(
        "App Info",
        tabName = "source",
        icon = icon("laptop-code")
    )
))

# 16x16 pixel favicon (icon next to site name in web browser tab)
favicon <- titlePanel(title = tags$head(
    tags$link(rel = "shortcut icon",
              href = "https://raw.githubusercontent.com/posnerab/OOMPHstat/master/www/favicon.ico",
              type = "www/favicon.ico")
))

body <- dashboardBody(favicon,
                      tabItems(norm_tab, t_tab, chi_tab, clt_tab, source_tab))

ui <- dashboardPage(header, sidebar, body)




################################ Server Logic #################################


server <- function(input, output, session) {
    xvalues <- data.frame(x = c(-3, 3))
    chixvalues <- data.frame(chix = c(0, 100))
    val_box <- data.frame(x = c(0, 4))


    observe({
        #### Input Access Objects ####
        z <- input$z
        nt <- input$norm_tail
        na <- input$norm_arrow
        nu <- input$norm_area
        act <- input$action
        x <- input$dist
        y <- input$par

        t <- input$t
        df <- ifelse(input$df > 1, input$df, 2)
        tt <- input$t_tail
        ta <- input$t_arrow
        tu <- input$t_area
        tact <- input$t_action

        chi <- input$chi
        chidf <- input$chidf
        chia <- input$chi_arrow
        chiu <- input$chi_area
        cact <- input$chi_action



        #### NORMAL DISTRIBUTION SERVER LOGIC ####

        #### dnorm_tail ####
        # ggplot statistical function for shading area under Normal curve
        dnorm_tail <- reactive({
            if (nt == "left") {
                function(x) {
                    norm_left <- dnorm(x)
                    norm_left[x <= -3 | x >= z] <- NA
                    return(norm_left)
                }
            }
            else if (nt == "right") {
                function(x) {
                    norm_right <- dnorm(x)
                    norm_right[x <= z | x >= 3] <- NA
                    return(norm_right)
                }
            }
            else if (nt == "middle") {
                function(x) {
                    norm_mid <- dnorm(x)
                    norm_mid[x <= -z | x >= z] <- NA
                    return(norm_mid)
                }
            }
            else if (nt == "two") {
                function(x) {
                    norm_two <- dnorm(x)
                    norm_two[x >= -z & x <= z] <- NA
                    return(norm_two)
                }
            }
        })

        #### norm_area_fun ####
        # function to compute area under normal curve from z-score
        norm_area_fun <- reactive({
            req(na)
            req(act)
            req(z)
            if (nt == "left") {
                round(pnorm(q = z,
                            lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "right") {
                round(pnorm(q = z,
                            lower.tail = FALSE),
                      digits = 5)
            }
            else if (nt == "middle") {
                round(pnorm(q = z,
                            lower.tail = TRUE) -
                          pnorm(q = -z,
                                lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "two") {
                round(
                    pnorm(q = z,
                          lower.tail = FALSE) +
                        pnorm(q = -z,
                              lower.tail = TRUE),
                    digits = 5
                )
            }
        })

        #### z_fun ####
        # function to compute t-statistic from area under the curve
        z_fun <- reactive({
            req(nu)
            if (nt == "left") {
                round(qnorm(p = nu,
                            lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "right") {
                round(qnorm(p = nu,
                            lower.tail = FALSE),
                      digits = 5)
            }
            else if (nt == "middle") {
                round(qnorm(
                    p = (nu + ((
                        1 - nu
                    ) / 2)),
                    mean = 0,
                    sd = 1,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (nt == "two") {
                round(qnorm(
                    p = nu / 2,
                    mean = 0,
                    sd = 1,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
        })

        #### norm_area_value ####
        norm_area_value <- reactive({
            if (act == "reset" & na == "down") {
                c(0)
            }
            else if (act == "reset" & na == "up") {
                c(nu)
            }
            else if (act == "go" & na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (act == "go" & na == "up") {
                c(nu)
            }
        })
        #### norm_area_label ####
        norm_area_label <- reactive({
            req(z)
            req(nu)
            if (na == "down") {
                c("Area Under the Curve")
            }
            else if (na == "up") {
                c("Enter Area Under the Curve (0 to 1)")
            }
        })
        #### z_value ####
        z_value <- reactive({
            if (na == "up") {
                if (act == "reset") {
                    c(0)
                }
                else {
                    # if action button is go
                    c(round(z_fun(), 5))
                }
            }
            else {
                # if arrow is down
                if (act == "reset") {
                    if (nt == "left" | nt == "right") {
                        c(z)
                    }
                    else if (nt == "middle" | nt == "two") {
                        if (z > 0) {
                            c(z)
                        }
                        else if (z < 0) {
                            c(-z)
                        }
                    }
                }
                else {
                    # if action button is go
                    if (nt == "left" | nt == "right") {
                        c(z)
                    }
                    else if (nt == "middle" | nt == "two") {
                        if (z > 0) {
                            c(z)
                        }
                        else if (z < 0) {
                            c(-z)
                        }
                    }
                }
            }
        })
        #### z_label ####
        z_label <- reactive({
            req(z)
            req(nu)
            if (na == "up") {
                c("Z-Score")
            }
            else if (na == "down") {
                if (nt == "left" | nt == "right") {
                    c("Enter Z-Score")
                }
                else if (nt == "middle" | nt == "two") {
                    c("Enter Z-Score (Positive)")
                }
            }
        })
        #### nu_min ####
        nu_min <- reactive({
            req(z)
            if (na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (na == "up") {
                c(0)
            }
        })
        #### nu_max ####
        nu_max <- reactive({
            req(z)
            if (na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (na == "up") {
                c(1)
            }
        })
        #### z_min ####
        z_min <- reactive({
            req(na)
            req(act)
            req(nu)
            if (na == "up") {
                if (act == "reset") {
                    c(0)
                }
                else {
                    c(round(z_fun(), 5))
                }
            }
            else if (na == "down") {
                if (nt == "left" | nt == "right") {
                    c(-Inf)
                }
                else if (nt == "middle" | nt == "two") {
                    c(0)
                }
            }
        })
        #### z_max ####
        z_max <- reactive({
            req(na)
            req(act)
            req(nu)
            if (na == "up") {
                if (act == "go") {
                    c(round(z_fun(), 5))
                }
                else {
                    c(0)
                }
            }
            else if (na == "down") {
                c(Inf)
            }
        })
        #### norm_plot_area_label ####
        norm_plot_area_label <- reactive({
            req(z)
            req(nu)
            req(act)
            if ((round(norm_area_fun(), 6) * 100) < 0.01) {
                paste0("Area: ", c(
                    formatC(
                        norm_area_fun(),
                        format = "e",
                        digits = 5
                    )
                ))
            }
            else {
                paste0("Area: ", round(norm_area_fun(), 5) * 100, "%")
            }
        })


        updateNumericInput(
            session,
            "z",
            label = z_label(),
            value = z_value(),
            min = z_min(),
            max = z_max()
        )

        updateNumericInput(
            session,
            "norm_area",
            label = norm_area_label(),
            value = norm_area_value(),
            min = nu_min(),
            max = nu_max()
        )

        #### NORM PLOT ####
        if (act == "go") {
            output$normPlot <- renderPlot({
                ggplot(xvalues, aes(x = x)) +
                    stat_function(fun = dnorm, size = .9) +
                    stat_function(
                        fun = dnorm_tail(),
                        geom = "area",
                        fill = "red",
                        alpha = 0.3
                    ) +
                    labs(x = "Z-Score (z)",
                         y = "",
                         title = "Standard Normal Distribution") +
                    geom_text(
                        x = 2.1,
                        y = 0.3,
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = norm_plot_area_label()
                    ) +
                    geom_text(
                        x = 2.1,
                        y = 0.25,
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = paste0("z: ",
                                       formatC(
                                           round(z, 5),
                                           format = "f",
                                           digits = 5
                                       ))
                    ) +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 14
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    scale_x_continuous(limits = c(-3, 3),
                                       breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
                    scale_y_continuous(breaks = NULL)
            })
        }
        else if (act == "reset") {
            output$normPlot <- renderPlot({
                ggplot(xvalues, aes(x = x)) +
                    stat_function(fun = dnorm, size = .9) +
                    labs(
                        x = "Z-Score (z)",
                        y = "",
                        title = bquote("Standard Normal Distribution")
                    ) +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 14
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    scale_x_continuous(limits = c(-3, 3),
                                       breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
                    scale_y_continuous(breaks = NULL)
            })
        }


        #### T-DISTRIBUTION SERVER LOGIC ####
        #### t_header ####
        output$t_header <- renderUI({
            h2("The ", em("t"), "-distribution")
        })


        #### dt_density ####
        # ggplot statistical function for shading area under Normal curve

        dt_density <- reactive({
            req(t)
            req(df)
            req(tu)
            function(x) {
                t_den <- dt(x, df = df)
                return(t_den)
            }
        })
        #### dt_tail ####
        dt_tail <- reactive({
            req(t)
            req(df)
            req(tu)
            if (tt == "left") {
                function(x) {
                    t_left <- dt(x, df = df)
                    t_left[x < -4 | x > t] <- NA
                    return(t_left)
                }
            }
            else if (tt == "right") {
                function(x) {
                    t_right <- dt(x, df = df)
                    t_right[x < t | x > 4] <- NA
                    return(t_right)
                }
            }
            else if (tt == "middle") {
                function(x) {
                    t_mid <- dt(x, df = df)
                    t_mid[x < -t | x > t] <- NA
                    return(t_mid)
                }
            }
            else if (tt == "two") {
                function(x) {
                    t_two <- dt(x, df = df)
                    t_two[x > -t & x < t] <- NA
                    return(t_two)
                }
            }
        })

        #### t_area_fun ####
        # function to compute area under t curve from t-statistic
        t_area_fun <- reactive({
            req(df)
            req(t)
            req(tu)
            if (tt == "left") {
                round(pt(
                    q = t,
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "right") {
                round(pt(
                    q = t,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
            else if (tt == "middle") {
                round(
                    pt(
                        q = t,
                        df = df,
                        lower.tail = TRUE
                    ) -
                        pt(
                            q = -t,
                            df = df,
                            lower.tail = TRUE
                        ),
                    digits = 5
                )
            }
            else if (tt == "two") {
                round(
                    pt(
                        q = t,
                        df = df,
                        lower.tail = FALSE
                    ) +
                        pt(
                            q = -t,
                            df = df,
                            lower.tail = TRUE
                        ),
                    digits = 5
                )
            }
        })

        #### t_fun ####
        # function to compute t-statistic from area under the curve
        t_fun <- reactive({
            req
            req(df)
            req(tu)
            if (tt == "left") {
                round(qt(
                    p = tu,
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "right") {
                round(qt(
                    p = tu,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
            else if (tt == "middle") {
                round(qt(
                    p = (tu + ((
                        1 - tu
                    ) / 2)),
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "two") {
                round(qt(
                    p = tu / 2,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
        })

        #### t_area_value ####
        t_area_value <- reactive({
            if (ta == "down") {
                if (tact == "reset") {
                    c(0)
                }
                else {
                    c(round(t_area_fun(), 5))
                }
            }
            else if (ta == "up") {
                c(tu)
            }
        })
        #### t_area_label ####
        t_area_label <- reactive({
            req(t)
            req(tu)
            req(df)
            if (ta == "down") {
                c("Area Under the Curve")
            }
            else if (ta == "up") {
                c("Enter Area Under the Curve (0 to 1)")
            }
        })

        # t_value ####
        t_value <- reactive({
            if (ta == "up") {
                if (tact == "reset") {
                    c(0)
                }
                else {
                    # if action button is go
                    c(round(t_fun(), 5))
                }
            }
            else {
                # if arrow is down
                if (tact == "reset") {
                    if (tt == "left" | tt == "right") {
                        c(t)
                    }
                    else if (tt == "middle" | tt == "two") {
                        if (t >= 0) {
                            c(t)
                        }
                        else if (t < 0) {
                            c(-t)
                        }
                    }
                }
            }
        })
        #### t_label ####
        t_label <- reactive({
            req(t)
            req(df)
            req(tu)
            if (ta == "up") {
                c("t-statistic")
            }
            else if (ta == "down") {
                if (tt == "left" | tt == "right") {
                    c("Enter t-statistic")
                }
                else if (tt == "middle" | tt == "two") {
                    c("Enter t-statistic (Positive)")
                }
            }
        })
        #### tu_min ####
        tu_min <- reactive({
            req(t)
            req(df)
            req(tu)
            if (ta == "down") {
                c(round(t_area_fun(), 5))
            }
            else if (ta == "up") {
                c(0)
            }
        })
        #### tu_max ####
        tu_max <- reactive({
            req(t)
            req(df)
            req(tact)
            if (ta == "down") {
                if (tact == "reset") {
                    c(0)
                }
                else {
                    c(round(t_area_fun(), 5))
                }
            }
            else if (ta == "up") {
                c(1)
            }
        })
        #### t_min ####
        t_min <- reactive({
            req(tact)
            req(tu)
            req(df)
            if (ta == "up") {
                if (act == "reset") {
                    c(0)
                }
                else {
                    # if action is go
                    c(round(t_fun(), 5))
                }
            }
            else {
                if (tt == "left" | tt == "right") {
                    c(-Inf)
                }
                else if (tt == "middle" | tt == "two") {
                    c(0)
                }
            }
        })
        #### t_max ####
        t_max <- reactive({
            req(tact)
            req(tu)
            req(df)
            if (ta == "up") {
                if (tact == "reset") {
                    c(0)
                }
                else {
                    c(round(t_fun(), 5))
                }
            }
            else {
                c(Inf)
            }
        })
        #### t_plot_area_label ####
        t_plot_area_label <- reactive({
            req(t)
            req(tu)
            req(df)
            if (ta == "up") {
                paste0("Area: ", round(t_area_fun(), 6) * 100, "%")
            }
            else {
                if ((round(t_area_fun(), 6) * 100) < 0.01) {
                    paste0("Area: ", c(
                        formatC(
                            t_area_fun(),
                            format = "e",
                            digits = 5
                        )
                    ))
                }
                else {
                    paste0("Area: ",
                           round(t_area_fun(), 5) * 100, "%")
                }
            }
        })
        #### t_plot_title ####
        t_plot_title <- reactive({
            req(df)
            req(t)
            req(tu)
            if (df == 1) {
                bquote(italic("t") ~
                           "-distribution with" ~
                           .(df) ~
                           "degree of freedom")
            }
            else {
                bquote(italic("t") ~
                           "-distribution with" ~
                           .(df) ~
                           "degrees of freedom")
            }
        })
        #### df_value ####
        df_value <- reactive({
            req(df)
            req(t)
            req(tu)
            if (df == 1) {
                c(2)
            }
            else {
                c(df)
            }
        })

        updateNumericInput(
            session,
            "t",
            label = t_label(),
            value = t_value(),
            min = t_min(),
            max = t_max()
        )

        updateNumericInput(
            session,
            "t_area",
            label = t_area_label(),
            value = t_area_value(),
            min = tu_min(),
            max = tu_max()
        )

        updateNumericInput(session,
                           "df",
                           value = df_value())

        #### T PLOT ####
        if (tact == "go") {
            output$tPlot <- renderPlot({
                ggplot(xvalues, aes(x = x)) +
                    stat_function(fun = dt_density(),
                                  size = .9) +
                    stat_function(
                        fun = dt_tail(),
                        geom = "area",
                        fill = "orange",
                        alpha = 0.5
                    ) +
                    xlab(expression(italic("t") ~ "-statistic (t)")) +
                    labs(
                        y = "",
                        title = t_plot_title()
                    ) +
                    geom_text(
                        x = 2.1,
                        y = 0.3,
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = t_plot_area_label()
                    ) +
                    geom_text(
                        x = 2.1,
                        y = 0.25,
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = paste0("t: ",
                                       formatC(
                                           round(t, 4),
                                           format = "f",
                                           digits = 4
                                       ))
                    ) +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 12
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    scale_x_continuous(
                        limits = c(-4, 4),
                        breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
                    ) +
                    scale_y_continuous(breaks = NULL)
            })
        }
        else if (tact == "reset") {
            output$tPlot <- renderPlot({
                ggplot(xvalues, aes(x = x)) +
                    stat_function(fun = dt_density(),
                                  size = .9) +
                    xlab(expression(italic("t") ~ "-statistic (t)")) +
                    labs(
                        y = "",
                        title = t_plot_title()
                    ) +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 12
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    scale_x_continuous(
                        limits = c(-4, 4),
                        breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
                    ) +
                    scale_y_continuous(breaks = NULL)
            })
        }


        #### CHI SERVER LOGIC ####


        #### dchisq_density ####
        # ggplot statistical function for shading area under Chi curve

        dchisq_density <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            function(x, df) {
                chi_den <- dchisq(x, df = chidf)
                return(chi_den)
            }
        })
        dchisq_tail <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            function(x, df) {
                chisq_right <- dchisq(x, df = chidf)
                chisq_right[x < chi] <- NA
                chisq_right[x < 0] <- NA
                chisq_right[x > 100] <- NA
                return(chisq_right)
            }
        })

        #### chi_area_fun ####
        # function to compute area under t curve from t-statistic
        chi_area_fun <- reactive({
            req(chidf)
            req(chi)
            req(chiu)
            round(pchisq(
                q = chi,
                df = chidf,
                lower.tail = FALSE
            ),
            digits = 10)
        })

        #### chi_fun ####
        # function to compute chisq-statistic from area under the curve
        chi_fun <- reactive({
            req(chidf)
            req(chiu)
            req(chi)
            round(qchisq(
                p = chiu,
                df = chidf,
                lower.tail = FALSE
            ),
            digits = 10)
        })

        #### chi_area_value ####
        chi_area_value <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            if (chia == "down") {
                if (cact == "reset") {
                    c(0)
                }
                else {
                    c(round(chi_area_fun(), 10))
                }
            }
            else if (chia == "up") {
                c(chiu)
            }
        })

        chi_area_label <- reactive({
            req(chi)
            req(chiu)
            req(chidf)
            if (chia == "down") {
                c("Area Under the Curve")
            }
            else if (chia == "up") {
                c("Enter Area Under the Curve (0 to 1)")
            }
        })
        #### chi_value ####
        chi_value <- reactive({
            req(chiu)
            req(chidf)
            req(chi)
            if (chia == "up") {
                if (cact == "reset") {
                    c(0)
                }
                else {
                    c(round(chi_fun(), 10))
                }
            }
            else if (chia == "down") {
                c(chi)
            }
        })

        chi_label <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            if (chia == "up") {
                c("Chi-Squared Statistic")
            }
            else if (chia == "down") {
                c("Enter Chi-Squared Statistic")
            }
        })

        chiu_min <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            if (chia == "down") {
                c(round(chi_area_fun(), 10))
            }
            else if (chia == "up") {
                c(0)
            }
        })

        chiu_max <- reactive({
            req(chi)
            req(chidf)
            req(chiu)
            if (chia == "down") {
                c(round(chi_area_fun(), 10))
            }
            else if (chia == "up") {
                c(1)
            }
        })

        chi_min <- reactive({
            req(chiu)
            req(chidf)
            req(chi)
            if (chia == "up") {
                if (cact == "reset") {
                    c(0)
                }
                else {
                    c(round(chi_fun(), 10))
                }
            }
            else if (chia == "down") {
                c(0)
            }
        })

        chi_max <- reactive({
            req(chi)
            req(chiu)
            req(chidf)
            if (chia == "up") {
                if (cact == "reset") {
                    c(0)
                }
                else {
                    c(round(chi_fun(), 10))
                }
            }
            else if (chia == "down") {
                c(Inf)
            }
        })

        chi_plot_area_label <- reactive({
            req(chi)
            req(chiu)
            req(chidf)
            if (chia == "up") {
                paste0("Area: ", (round(chi_area_fun(), 6) * 100), "%")
            }
            else {
                if ((round(chi_area_fun(), 6) * 100) < 0.01) {
                    paste0("Area: ", (c(
                        formatC(
                            chi_area_fun(),
                            format = "e",
                            digits = 5
                        )
                    )))
                }
                else {
                    paste0("Area: ", (round(chi_area_fun(
                    ), 6) * 100), "%")
                }
            }
        })

        chi_plot_title <- reactive({
            req(chidf)
            req(chi)
            req(chiu)
            bquote("Chi-Squared Distribution with" ~
                       .(chidf) ~
                       "Degrees of Freedom")
        })

        updateNumericInput(
            session,
            "chi",
            label = chi_label(),
            value = chi_value(),
            min = chi_min(),
            max = chi_max()
        )

        updateNumericInput(
            session,
            "chi_area",
            label = chi_area_label(),
            value = chi_area_value(),
            min = chiu_min(),
            max = chiu_max()
        )
        #### CHI PLOT ####
        if (cact == "go") {
            req(chi)
            req(chiu)
            req(chidf)
            output$chiPlot <- renderPlot({
                ggplot(chixvalues, aes(x = chix)) +
                    stat_function(fun = dchisq_density(),
                                  size = .9) +
                    stat_function(
                        fun = dchisq_tail(),
                        geom = "area",
                        fill = "green",
                        alpha = 0.5
                    ) +
                    labs(x = expression(
                        paste("Chi-Squared Statistic ", "(", chi ^ 2, ")")
                    ),
                    y = "",
                    title = chi_plot_title()) +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 12
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    geom_text(
                        x = ifelse(chi + 25 < 90,
                                   chi + 25,
                                   chi - 25),
                        y = ifelse(chidf < 25, .35 / chidf, .35 / 25),
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = chi_plot_area_label()
                    ) +
                    geom_text(
                        x = ifelse(chi + 25 < 90,
                                   chi + 25,
                                   chi - 25),
                        y = ifelse(chidf < 25, .25 / chidf, .25 / 25),
                        size = 6,
                        fontface = "bold",
                        colour = "brown",
                        label = bquote(bold(paste(
                            chi ^ 2 ~ ": " ~
                                .(formatC(
                                    round(chi, 5),
                                    format = "f",
                                    digits = 5
                                ))
                        )))
                    ) +
                    scale_x_continuous(
                        limits = c(0, 100),
                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
                    ) +
                    scale_y_continuous(breaks = NULL)
            })
        }
        else {
            req(chi)
            req(chiu)
            req(chidf)
            output$chiPlot <- renderPlot({
                ggplot(chixvalues, aes(x = chix)) +
                    stat_function(fun = dchisq_density(),
                                  size = .9) +
                    labs(x = expression(
                        paste("Chi-Squared Statistic ", "(", chi ^ 2, ")")
                    ),
                    y = "",
                    title = "Chi-Squared Distribution") +
                    theme(
                        plot.title = element_text(
                            # face = "bold",
                            size = 18,
                            hjust = 0.5),
                        axis.title.x = element_text(
                            # face = "bold",
                            colour = "brown",
                            size = 16),
                        axis.title.y = element_text(
                            face = "bold",
                            colour = "brown",
                            size = 12
                        ),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank()
                    ) +
                    scale_x_continuous(
                        limits = c(0, 100),
                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
                    ) +
                    scale_y_continuous(breaks = NULL)
            })
        }


        #### CLT SERVER LOGIC ####


        dist_choices <- reactive({
            if (y == "prop") {
                c("Binomial" = "binom")
            }
            else {
                c(
                    "Uniform" = "unif",
                    "Roughly Symmetric" = "norm",
                    "Slightly Skewed" = "sskew",
                    "Highly Skewed" = "skew"
                )
            }
        })

        dist_selected <- reactive({
            if (y == "prop") {
                c("Binomial" = "binom")
            }
            else if (y == "mean") {
                if (x == "unif") {
                    c("Uniform" = "unif")
                }
                else if (x == "norm") {
                    c("Roughly Symmetric" = "norm")
                }
                else if (x == "sskew") {
                    c("Slightly Skewed" = "sskew")
                }
                else if (x == "skew") {
                    c("Highly Skewed" = "skew")
                }
            }
        })


        updateRadioButtons(
            session = session,
            inputId = "dist",
            choices = dist_choices(),
            selected = dist_selected()
        )


        var_choices <- reactive({
            if (x == "binom") {
                c("Current Smoker" = "smoke")
            }
            else if (x == "unif") {
                c("Age" = "age")
            }
            else if (x == "norm") {
                c("Height" = "height",
                  "SAT Scores" = "SAT")
            }
            else if (x == "sskew") {
                c("MediCal Costs: Hip & Knee Replacements" = "hip")
            }
            else {
                c("Annual Income" = "LA")
            }
        })

        var_selected <- reactive({
            if (y == "prop") {
                c("Current Smoker" = "smoke")
            }
            else if (x == "unif") {
                c("Age" = "age")
            }
            else if (x == "norm") {
                c("Height" = "height")
            }
            else if (x == "sskew") {
                c("MediCal Costs: Hip & Knee Replacements" = "hip")
            }
            else {
                c("Annual Income" = "LA")
            }
        })

        updateSelectInput(
            session = session,
            inputId = "var",
            choices = var_choices(),
            selected = var_selected()
        )

        population <- reactive({
            hip <- read_excel("data/medicare.xlsx",
                              col_names = TRUE) %>%
                filter(
                    State == "CA",
                    Procedure == "469 - MAJOR HIP AND KNEE JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREM"
                )

            data <- read.csv("data/data.csv",
                             header = TRUE)

            smoking <- read.csv("data/smoking.csv",
                                header = TRUE)

            if (input$var == "height") {
                rnorm(
                    n = input$pop_size,
                    mean = mean(data$Height),
                    sd = sd(data$Height)
                )
            }
            else if (input$var == "SAT") {
                sample(
                    x = data$SAT,
                    size = input$pop_size,
                    replace = TRUE
                )

            }
            else if (input$var == "age") {
                sample(
                    x = data$Age,
                    size = input$pop_size,
                    replace = TRUE
                )
            }
            else if (input$var == "income") {
                sample(
                    x = data$Income,
                    size = input$pop_size,
                    replace = TRUE
                )
            }
            else if (input$var == "hip") {
                sample(
                    x = hip$Charges,
                    size = input$pop_size,
                    replace = TRUE
                )
            }
            else if (input$var == "LA") {
                sample(
                    x = data$Income,
                    size = input$pop_size,
                    replace = TRUE
                )
            }
            else {
                sample(
                    x = smoking$smokes,
                    size = input$pop_size,
                    replace = TRUE
                )
            }
        }) # population reactive

        smpl_mean <- reactive({
            for (i in 1:input$iterate) {
                if (i == 1) {
                    smpl_mean <- c(mean(sample(
                        population(),
                        input$sampleSize, T
                    )))
                }
                else {
                    smpl_mean <-
                        c(smpl_mean, mean(sample(
                            population(),
                            input$sampleSize, T
                        )))
                }
            }
            smpl_mean
        }) # smpl_mean reactive

        ax_choices <- reactive({
            if (input$var == "age") {
                c("Age (Years)")
            }
            else if (input$var == "height") {
                c("Height (Inches)")
            }
            else if (input$var == "SAT") {
                c("SAT Scores (400-1600)")
            }
            else if (input$var == "hip") {
                c("Average Charges Per Hospital, CA ($)")
            }
            else if (input$var == "smoke") {
                c("Proportion Current Smokers (1 = Yes, 0 = No)")
            }
            else {
                c("Annual Income ($)")
            }
        })

        titles <- reactive({
            if (input$var == "smoke") {
                c("Sample Proportion Histogram and Density Plot")
            }
            else {
                c("Sample Mean Histogram and Density Plot")
            }
        })

        output$plot_pop <- renderPlot({
            plot(
                density(population()),
                axes = FALSE,
                xlab = "",
                ylab = "",
                main = ""
            )
            par(new = TRUE)
            hist(population(),
                 main = "Population Histogram and Density Plot",
                 xlab = ax_choices())
            abline(v = mean(population()),
                   col = "blue",
                   lwd = 2)
        })

        output$plot_smpl_mean <- renderPlot({
            plot(
                density(smpl_mean()),
                axes = FALSE,
                xlab = "",
                ylab = "",
                main = ""
            )
            par(new = TRUE)
            hist(smpl_mean(),
                 main = titles(),
                 xlab = ax_choices())
            abline(v = mean(smpl_mean()),
                   col = "blue",
                   lwd = 2)
        })

        #### uiOutput objects ####
        email <- a("xander@berkeley.edu",
                   href = "mailto:xander@berkeley.edu")

        lahiff <- a("Maureen Lahiff",
                    href = "mailto:lahiff@berkeley.edu")

        oomph <- a("Online/On-Campus MPH Program (OOMPH)",
                   href = "https://onlinemph.berkeley.edu/",
                   target = "_blank")

        sph <- a("School of Public Health",
                 href = "https://publichealth.berkeley.edu/",
                 target = "_blank")

        github <- a("Github.",
                    href = "https://github.com/posnerab/OOMPHstat",
                    target = "_blank")

        shiny <- a("here.",
                   href = "https://docs.rstudio.com/shinyapps.io/index.html",
                   target = "_blank")

        output$contact <- renderUI({
            tagList(
                p("Xander Posner, ", email),
                p("MPH '20 | Epidemiology & Biostatistics"),
                p(sph),
                p("University of California, Berkeley")
            )
        })

        output$code <- renderUI({
            tagList(
                p(code(
                    "This Shiny app was made in RStudio"
                )),
                p("Check out the source code on ", github),
                p("Learn more about building Shiny apps ", shiny)
            )
        })

        output$course <- renderUI({
            tagList(
                p(strong("PBHLTH W142")),
                p(
                    "Intro to Probability and Statistics in Biology and Public Health"
                ),
                p("Professor Maureen Lahiff, Ph.D."),
                p(oomph),
                p(sph),
                p("University of California, Berkeley"),
                br(""),
                p(strong("Copyright © 2020 "), lahiff, " / UC Berkeley")
            )
        })

    }) #### closes out top level observe function ####
}
# Run the application
shinyApp(ui = ui, server = server)
