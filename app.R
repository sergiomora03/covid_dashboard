#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(deSolve)
library(coronavirus)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(scales)

options(digits = 2,
        scipen = 999999)

ui <- fluidPage(
    tags$head(tags$link(rel = "shortcut icon", href = "https://www.uniandes.edu.co/sites/default/files/favicon.ico")),
    theme = shinytheme("slate"),
    navbarPage(title = "Dashboard COVID-19",
               tabPanel(title = "SIR model", value =  "sir_model",
                        fluidPage(
                            sidebarLayout(
                            sidebarPanel(width = 3,
                                         img(src = 'unnamed.png', align = "right", height = "100%", width = "100%"),
                                         br(),
                                         br(),
                                         #h5("COVID-19"),
                                         br(),
                                         br(),
                                         br(),
                                         h5("Select the country/region of your preference to download the report with SIR modeling. "),
                                         h5("The dashboard has the ability to run this modeling for the country/region of your choice."),
                                         selectInput(inputId = "country", label = "Country/region", choices = unique(coronavirus$country), selected = "Colombia"),
                                         #br(),
                                         checkboxInput(inputId = "code", label = "Download with code", value = T),
                                         #actionButton(inputId = "run_SIR", label = "Run SIR!", icon = icon("viruses")),
                                         br(),
                                         br(),
                                         HTML("&nbsp"),
                                         downloadButton("report", "Generate report"),
                                         br(),
                                         #progressBar(id = "download", value = 0, striped = TRUE, display_pct = TRUE),
                                         br(),
                                         br(),
                                         hr(),         
                                         tags$div("Authors:"), 
                                         br(),
                                         tags$div("Sergio Mora, 2020"), 
                                         tags$a(href="mailto:s.morap@uniandes.edu.co", "s.morap@uniandes.edu.co"),
                                         # br(), br(),
                                         # tags$div("Colaboradores:"), 
                                         tags$div("Alex Hernandez, 2020"), 
                                         tags$a(href="mailto:ja.hernandezp@uniandes.edu.co", "ja.hernandezp@uniandes.edu.co"),
                                         tags$div("Alex Camargo, 2020"), 
                                         tags$a(href="mailto:a.camargo733@uniandes.edu.co", "a.camargo733@uniandes.edu.co"),
                                         br(), br()
                                         ),
                                mainPanel(
                                    HTML("<center>"),
                                    fluidRow(
                                        column(width = 12,
                                               valueBoxOutput("ask1"),
                                               valueBoxOutput("ask2"),
                                               valueBoxOutput("ask3")
                                               ),
                                        column(width = 12,
                                               valueBoxOutput("ask4"),
                                               valueBoxOutput("ask5"),
                                               valueBoxOutput("ask6")
                                               )
                                    ),
                                    tabsetPanel(
                                        tabPanel(title = "Fitted vs. Obs.",value = "tabPanel1",
                                                 fluidRow(
                                                     column(width = 6,
                                                            plotOutput(outputId = "plot1")
                                                     ),
                                                     column(width = 6,
                                                            plotOutput(outputId = "plot2")
                                                            )
                                                     )
                                                 ),
                                        tabPanel(title = "SIR model",value = "tabPanel2",
                                                 fluidRow(
                                                     column(width = 6,
                                                            plotOutput(outputId = "plot3")
                                                            ),
                                                     column(width = 6,
                                                            plotOutput(outputId = "plot4")
                                                            )
                                                     )
                                                 )
                                        ),
                                    HTML("</center>")
                                    )
                                )
                            ) 
                        ),
               tabPanel(title = shiny::tagList(icon("chart-area"), "Simulation outbreak"), value =  "simulation",
                        includeHTML("www/index.html")
                        ),
               tabPanel(
                   title = shiny::tagList(icon("chart-bar"),"Simulation social distance"),
                   sidebarLayout(
                       sidebarPanel(
                           sliderInput("r0", 
                                       div(HTML("Value for R<sub>0</sub>")), 
                                       min   = 1.5, 
                                       max   = 3.0, 
                                       value = 2.25, 
                                       step  = 0.25),
                           checkboxInput("y_axis_fixed",
                                         div(HTML("Fix y axis (helpful to compare different R<sub>0</sub>)")),
                                         value = FALSE),
                           br(),
                           sliderInput("x_max", 
                                       "Max days for the model", 
                                       min   = 100, 
                                       max   = 365, 
                                       value = 150,
                                       step  =  25),
                           hr(),
                           br(),
                           sliderInput("sdp",
                                       "First 'social distance period' (sdp 1)",
                                       min   =   0,
                                       max   = 150,
                                       value = c(30, 60), 
                                       step  =   5), 
                           br(), br(),
                           sliderInput("red_one",
                                       "Reduction during first period (sdp 1)",
                                       min   = 0.2,
                                       max   = 1.0,
                                       value = 0.6, 
                                       step  = 0.05), 
                           sliderInput("red_two",
                                       "Reduction after first period (sdp 2)",
                                       min   = 0.2,
                                       max   = 1.0,
                                       value = 0.8, 
                                       step  = 0.05), 
                           br()
                       ),
                       mainPanel(
                           # p("Note: This tool is not intended to create a prediction."),
                           plotOutput("chart", height = "500px"), 
                           br(),
                           hr(),
                           h4("Initial code, mathematical model and idea:"),
                           tags$a(href="https://staff.math.su.se/hoehle/blog/2020/03/16/flatteningthecurve.html", 
                                  "Michael Höhle, 'Flatten the COVID-19 curve'"), 
                           p(),
                           br()
                           )
                       )
                   ),
               tabPanel(title = shiny::tagList(icon("map-marked-alt"),"Map Covid-19"), value = "map",
                        tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                                    src = "Covid_map.html")
                        ),
               tabPanel(title = shiny::tagList(icon("book"),"Notebook"), value = "notebook",
                        tags$script(src = "https://gist.github.com/Alexher1032/a99b739cd1723f5713e725b58f060462.js")
                        ),
               tabPanel(title = shiny::tagList(icon("file-powerpoint"),"Covid-19"), value = "presentation",
                        tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                                    src = "Covid.html")
                        )
               )
)


server <- function(input, output, session) {
    
    start_app <- reactiveValues(pop_up = FALSE)
    
    show_pop_up <- reactive({start_app$pop_up = TRUE})
    
    observeEvent(start_app$pop_up,{
        showModal(
            modalDialog(
                title = "Important message",
                "This is an important message!",
                easyClose = TRUE,
                footer = NULL
            )
        ) 
    })
    
    
    source("www/SIR_simulation.R")
    updateProgressBar(session = session, id = "download", value = 64)
    output$report <- downloadHandler(
        filename = "Covid-19.pdf",
        content = function(file) {
            tempReport <- file.path("www/Covid-19.Rmd")
            file.copy("Covid-19.Rmd", tempReport, overwrite = T)
            params <- list(country_selected = input$country,
                           printcode = input$code)
            rmarkdown::render(tempReport, output_file = file,
                              output_format = "pdf_document",
                              params = params,
                              envir = new.env(parent = globalenv()))
        }
    )
    updateProgressBar(session = session, id = "download", value = 100)

# data ----------------------------------------------------------------------------------------------------------------------

    data(coronavirus)
    population_data <- readxl::read_xls("www/API_SP.POP.TOTL_DS2_en_excel_v2_988396.xls")
    


# Methods -------------------------------------------------------------------------------------------------------------------

    population_country <- function(country) {
        return(population_data %>% filter(`Country Name` == country) %>% gather("year", "population", 5:64) %>% filter(!is.na(population)) %>% mutate(max_year = max(year)) %>% filter(year == max_year) %>% select(population) %>% {.[[1]]})
    }
    SIR <- function(time, state, parameters) {
        par <- as.list(c(state, parameters))
        with(par, {
            dS <- -beta * I * S / N
            dI <- beta * I * S / N - gamma * I
            dR <- gamma * I
            list(c(dS, dI, dR))
        })
    }
    RSS <- function(parameters) {
        names(parameters) <- c("beta", "gamma")
        out <- ode(y = init, times = Day, func = SIR, parms = parameters)
        fit <- out[, 3]
        sum((Infected - fit)^2)
    }
    



# Standar -------------------------------------------------------------------------------------------------------------------
    output$selected_country <- renderText(paste("you select: ", "'",input$country,"'"))
    # SIR model -----------------------------------------------------------------------------------------------------------------
    
    df <- coronavirus %>%
        filter(country == "Colombia") %>%
        group_by(date, type) %>%
        summarise(total = sum(cases, na.rm = TRUE)) %>%
        pivot_wider(
            names_from = type,
            values_from = total
        ) %>%
        arrange(date) %>%
        ungroup() %>%
        mutate(active = confirmed - death - recovered) %>%
        mutate(
            confirmed_cum = cumsum(confirmed),
            death_cum = cumsum(death),
            recovered_cum = cumsum(recovered),
            active_cum = cumsum(active)
        )
    sir_start_date <- df %>% filter(active_cum >= 1) %>% select(date) %>% summarise(date = min(date)) %>% {.[[1]]}
    sir_end_date <- df %>% filter(active_cum >= 1) %>% select(date) %>% summarise(date = max(date)) %>% {.[[1]]}
    
    Infected <- subset(df, date >= ymd(sir_start_date) & date <= ymd(sir_end_date))$active_cum
    Day <- 1:(length(Infected))
    
    N <- population_country("Colombia")
    init <- c(
        S = N - Infected[1],
        I = Infected[1],
        R = 0
    )
    
    Opt <- optim(c(0.5, 0.5),
                 RSS,
                 method = "L-BFGS-B",
                 lower = c(0, 0),
                 upper = c(1, 1)
    )
    
    Opt$message
    
    Opt_par <- setNames(Opt$par, c("beta", "gamma"))
    Opt_par
    
        # Predict -------------------------------------------------------------------------------------------------------------------
    
    t <- 1:as.integer(ymd(sir_end_date) + 1 - ymd(sir_start_date))
    fitted_cumulative_incidence <- data.frame(ode(
        y = init, times = t,
        func = SIR, parms = Opt_par
    ))
    fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
        mutate(
            Date = ymd(sir_start_date) + days(t - 1),
            Country = "Colombia",
            cumulative_incident_cases = Infected
        )
    
        # Reproduction number  ------------------------------------------------------------------------------------------------------
    
    Opt_par
    R0 <- as.numeric(Opt_par[1] / Opt_par[2])
    R0
    
        # Using our model to analyze the outbreak if there was no intervention ------------------------------------------------------
    
    # time in days for predictions
    t <- 1:400
    
    # get the fitted values from our SIR model
    fitted_cumulative_incidence <- data.frame(ode(
        y = init, times = t,
        func = SIR, parms = Opt_par
    ))
    
    # add a Date column and join the observed incidence data
    fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
        mutate(
            Date = ymd(sir_start_date) + days(t - 1),
            Country = "Colombia",
            cumulative_incident_cases = c(Infected, rep(NA, length(t) - length(Infected)))
        )
    
    
    
        # More summary statistics ---------------------------------------------------------------------------------------------------
    
    fit <- fitted_cumulative_incidence
    
    # peak of pandemic
    peak_pandemic <- fit[fit$I == max(fit$I), c("Date", "I")]
    
    # severe cases
    max_infected <- max(fit$I)
    severe_cases <- max_infected * 0.2
    
    # cases with need for intensive care
    intensive_care <- max_infected * 0.06
    
    # deaths with supposed 4.5% fatality rate
    deaths <- max_infected * 0.045
    
    # date finish infeced
    date_finish_infeced <- fitted_cumulative_incidence %>% filter(I > 1) %>% summarise(Date = max(Date))
    
    
    # Plots ---------------------------------------------------------------------------------------------------------------------
    
    output$plot1 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            #geom_bar(aes(y = cumulative_incident_cases), colour = "darkblue", fill = "darkblue", stat = "identity") +
            geom_point(aes(y = cumulative_incident_cases), colour = "darkblue") +
            geom_line(aes(y = I), colour = "darkred", size = 1.2) +
            labs(
                y = "Acumulación de incidencias",
                x = "t",
                title = paste0("SARS-CoV-2 ajustado vs incidencia acumulada observada, ", "Colombia"),
                subtitle = "(Rojo = ajustado desde modelo SIR, Azul = observado)"
            ) +
            theme_classic()
    })
    output$plot2 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            #geom_bar(aes(y = cumulative_incident_cases), colour = "darkblue", fill = "darkblue", stat = "identity") +
            geom_line(aes(y = cumulative_incident_cases), colour = "darkblue", size = 1.2) +
            geom_line(aes(y = I), colour = "darkred", size = 1.2) +
            labs(
                y = "Acumulación de incidencias",
                x = "t",
                title = paste0("SARS-CoV-2 ajustado vs incidencia acumulada observada, Colombia"),
                subtitle = "(Rojo = ajustado desde modelo SIR, Azul = observado)"
            ) +
            theme_classic() +
            scale_y_log10(labels = scales::comma) 
        #scale_x_date(labels = date_format("%d-%b"), date_breaks = "24 days")
    })
    output$plot3 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            geom_line(aes(y = I), colour = "red", size = 1) +
            geom_line(aes(y = S), colour = "black", size = 1) +
            geom_line(aes(y = R), colour = "green", size = 1) +
            geom_bar(aes(y = cumulative_incident_cases),
                     colour = "blue", fill = "blue",
                     stat = "identity"
            ) +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(labels = date_format("%d-%b"), date_breaks = "24 days") +
            labs(y = "Personas", 
                 title = "SARS-CoV-2 ajustado vs incidencia acumulada observada",
                 subtitle = paste0("Modelo SIR aplicado a ", "Colombia"),
                 x = "t") +
            scale_colour_manual(name = "", values = c(
                red = "red", black = "black",
                green = "green", blue = "blue"
            ), labels = c(
                "Susceptible",
                "Recuperdo", "Observado", "Infeccioso"
            )) +
            theme_classic()
    })
    output$plot4 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            geom_bar(aes(y = cumulative_incident_cases),
                     colour = "darkblue", fill = "darkblue",
                     stat = "identity") +
            geom_line(aes(y = I, colour = "red"), size = 1) +
            geom_line(aes(y = S, colour = "black"), size = 1) +
            geom_line(aes(y = R, colour = "green"), size = 1) +
            scale_y_log10(labels = scales::comma) +
            scale_x_date(labels = date_format("%d-%b"), date_breaks = "4 week") +
            labs(
                y = "Personas",
                x = "t",
                title = "SARS-CoV-2 ajustado vs incidencia acumulada observada",
                subtitle = paste0("Modelo SIR aplicado a ", "Colombia")
            ) +
            scale_colour_manual(
                name = "",
                values = c(red = "red", black = "black", green = "green", blue = "blue"),
                labels = c("Susceptible", "Observado", "Recuperado", "Infeccioso")
            ) +
            theme_classic() 
    })
    
    
    # Box -----------------------------------------------------------------------------------------------------------------------
    
    output$ask1 <- renderValueBox({
        valueBox(
            paste0("R0 = ", round(R0, digits = 2)), "Reproduction Number", icon = icon("virus"), color = "yellow"
        )
    })        
    output$ask2 <- renderValueBox({
        valueBox(
            paste0(as.Date(peak_pandemic[[1]], format = "%Y-%m-%d"), "\n",
                   "Infected:",prettyNum(peak_pandemic[2], big.mark = ","), "\n"), "Peak of pandemic", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask3 <- renderValueBox({
        valueBox(
            prettyNum(round(severe_cases), big.mark = ","), "Severe cases", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask4 <- renderValueBox({
        valueBox(
            prettyNum(round(intensive_care), big.mark = ","), "Intensive Care", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask5 <- renderValueBox({
        valueBox(
            prettyNum(round(deaths), big.mark = ","), "Deaths with 4.5% fatality rate", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask6 <- renderValueBox({
        valueBox(
            as.Date(date_finish_infeced[[1]], format = "%Y-%m-%d"), "date Finish Pandemic", icon = icon("viruses"), color = "yellow"
        )
    })    
    

    

# Bottum action -------------------------------------------------------------------------------------------------------------

        observeEvent(input$run_SIR, {
        output$selected_country <- renderText(paste("you select: ", "'",input$country,"'"))
    # SIR model -----------------------------------------------------------------------------------------------------------------
        updateProgressBar(session = session, id = "pb3", value = 6)
    df <- coronavirus %>%
        filter(country == input$country) %>%
        group_by(date, type) %>%
        summarise(total = sum(cases, na.rm = TRUE)) %>%
        pivot_wider(
            names_from = type,
            values_from = total
        ) %>%
        arrange(date) %>%
        ungroup() %>%
        mutate(active = confirmed - death - recovered) %>%
        mutate(
            confirmed_cum = cumsum(confirmed),
            death_cum = cumsum(death),
            recovered_cum = cumsum(recovered),
            active_cum = cumsum(active)
        )
    print(df)
    cat("\n")
    sir_start_date <- df %>% filter(active_cum >= 1) %>% select(date) %>% summarise(date = min(date)) %>% {.[[1]]}
    cat("sir_start_date",sir_start_date, "\n")
    sir_end_date <- df %>% filter(active_cum >= 1) %>% select(date) %>% summarise(date = max(date)) %>% {.[[1]]}
    cat("sir_end_date",sir_end_date,"\n")
    
    Infected <- subset(df, date >= ymd(sir_start_date) & date <= ymd(sir_end_date))$active_cum
    print(Infected)
    Day <- 1:(length(Infected))
    print(Day)
    
    N <- population_country(input$country)
    cat("N", N, "\n")
    init <- c(
        S = N - Infected[1],
        I = Infected[1],
        R = 0
    )
    print(init)
    
    Opt <- optim(c(0.5, 0.5),
                 RSS,
                 method = "L-BFGS-B",
                 lower = c(0, 0),
                 upper = c(1, 1)
    )
    
    cat("Opt$message",Opt$message, "\n")
    
    Opt_par <- setNames(Opt$par, c("beta", "gamma"))
    cat("Optimal parameters:",Opt_par, "\n")
    
        # Predict -------------------------------------------------------------------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 18)
    t <- 1:as.integer(ymd(sir_end_date) + 1 - ymd(sir_start_date))
    fitted_cumulative_incidence <- data.frame(ode(
        y = init, times = t,
        func = SIR, parms = Opt_par
    ))
    print(fitted_cumulative_incidence)
    fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
        mutate(
            Date = ymd(sir_start_date) + days(t - 1),
            Country = input$country,
            cumulative_incident_cases = Infected
        )
    print(fitted_cumulative_incidence)
        # Reproduction number  ------------------------------------------------------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 32)
    cat("Optimal parameters =", Opt_par, "\n")
    R0 <- as.numeric(Opt_par[1] / Opt_par[2])
    cat("Reproduction number",R0, "\n")
    
        # Using our model to analyze the outbreak if there was no intervention ------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 46)
    # time in days for predictions
    t <- 1:400
    
    # get the fitted values from our SIR model
    fitted_cumulative_incidence <- data.frame(ode(
        y = init, times = t,
        func = SIR, parms = Opt_par
    ))
    
    # add a Date column and join the observed incidence data
    fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
        mutate(
            Date = ymd(sir_start_date) + days(t - 1),
            Country = input$country,
            cumulative_incident_cases = c(Infected, rep(NA, length(t) - length(Infected)))
        )
    
    
    
        # More summary statistics ---------------------------------------------------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 58)
    fit <- fitted_cumulative_incidence
    
    # peak of pandemic
    peak_pandemic <- fit[fit$I == max(fit$I), c("Date", "I")]
    cat("peak of pandemic", peak_pandemic[[1]], "cases:", peak_pandemic[[2]], "\n")
    
    # severe cases
    max_infected <- max(fit$I)
    severe_cases <- max_infected * 0.2
    cat("severe cases", severe_cases, "\n")
    
    # cases with need for intensive care
    intensive_care <- max_infected * 0.06
    cat("cases with need for intensive care", intensive_care, "\n")
    
    # deaths with supposed 4.5% fatality rate
    deaths <- max_infected * 0.045
    cat("deaths with supposed 4.5% fatality rate", deaths, "\n")
    
    # date finish infeced
    date_finish_infeced <- fitted_cumulative_incidence %>% filter(I > 1) %>% summarise(Date = max(Date))
    cat("date finish infeced", date_finish_infeced[[1]], "\n")

    # Plots ---------------------------------------------------------------------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 64)
    output$plot1 <- renderPlot({
        fitted_cumulative_incidence %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = I), colour = "red") +
            geom_point(aes(y = cumulative_incident_cases), colour = "blue") +
            labs(
                y = "Cumulative incidence",
                title = paste("COVID-19 fitted vs observed cumulative incidence,", input$country),
                subtitle = "(Red = fitted from SIR model, blue = observed)"
            ) +
            theme_minimal()
        #plotly::ggplotly(p1)
    })
    output$plot2 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            geom_line(aes(y = I), colour = "red") +
            geom_point(aes(y = cumulative_incident_cases), colour = "blue") +
            labs(
                y = "Cumulative incidence",
                title = paste("COVID-19 fitted vs observed cumulative incidence,", input$country),
                subtitle = "(Red = fitted from SIR model, blue = observed)"
            ) +
            theme_minimal() +
            scale_y_log10(labels = scales::comma)
        #plotly::ggplotly(p2)
    })
    output$plot3 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            geom_line(aes(y = I), colour = "red") +
            geom_line(aes(y = S), colour = "black") +
            geom_line(aes(y = R), colour = "green") +
            geom_point(aes(y = cumulative_incident_cases),
                       colour = "blue"
            ) +
            scale_y_continuous(labels = scales::comma) +
            labs(y = "Persons", title = paste("COVID-19 fitted vs observed cumulative incidence,",input$country)) +
            scale_colour_manual(name = "", values = c(
                red = "red", black = "black",
                green = "green", blue = "blue"
            ), labels = c(
                "Susceptible",
                "Recovered", "Observed", "Infectious"
            )) +
            theme_minimal()
        #plotly::ggplotly(p3)
    })
    output$plot4 <- renderPlot({
        fitted_cumulative_incidence %>% filter(I > 1) %>% 
            ggplot(aes(x = Date)) +
            geom_line(aes(y = I, colour = "red")) +
            geom_line(aes(y = S, colour = "black")) +
            geom_line(aes(y = R, colour = "green")) +
            geom_point(aes(y = cumulative_incident_cases, colour = "blue")) +
            scale_y_log10(labels = scales::comma) +
            labs(
                y = "Persons",
                title = paste("COVID-19 fitted vs observed cumulative incidence,", input$country),
                subtitle = "Creador: Sergio A.M. Website: https://rpubs.com/sergiomora123"
            ) +
            scale_colour_manual(
                name = "",
                values = c(red = "red", black = "black", green = "green", blue = "blue"),
                labels = c("Susceptible", "Observed", "Recovered", "Infectious")
            ) +
            theme_minimal()
        #plotly::ggplotly(p4)
    })
    

    # Box -----------------------------------------------------------------------------------------------------------------------
    updateProgressBar(session = session, id = "pb3", value = 80)
    output$ask1 <- renderValueBox({
        valueBox(
            paste0("R0 = ", round(R0, digits = 2)), "Reproduction Number", icon = icon("viruses"), color = "yellow"
        )
    })        
    output$ask2 <- renderValueBox({
        valueBox(
            paste0(as.Date(peak_pandemic[[1]], format = "%Y-%m-%d"), "\n",
                   "Infected:",prettyNum(peak_pandemic[2], big.mark = ","), "\n"), "Peak of pandemic", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask3 <- renderValueBox({
        valueBox(
            prettyNum(round(severe_cases), big.mark = ","), "Severe cases", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask4 <- renderValueBox({
        valueBox(
            prettyNum(round(intensive_care), big.mark = ","), "Intensive Care", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask5 <- renderValueBox({
        valueBox(
                 prettyNum(round(deaths), big.mark = ","), "Deaths with 4.5% fatality rate", icon = icon("viruses"), color = "yellow"
        )
    })    
    output$ask6 <- renderValueBox({
        valueBox(
            as.Date(date_finish_infeced[[1]], format = "%Y-%m-%d"), "date Finish Pandemic", icon = icon("viruses"), color = "yellow"
        )
    })    
    updateProgressBar(session = session, id = "pb3", value = 100)
    sendSweetAlert(
        session = session,
        title = "Covid!",
        text = "SIR model end!!",
        type = "success"
    )
    })
    res <- reactive({
        run(sdp = c(input$sdp), 
            red = c(input$red_one, input$red_two), 
            r0  = input$r0, 
            max_time = input$x_max)
    })
    
    
    output$chart <- renderPlot({
        plot_result(res(), input$sdp, input$x_max, input$y_axis_fixed )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
