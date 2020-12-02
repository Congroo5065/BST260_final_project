library(dplyr)
library(tidyverse)
library(shiny)
p <- readRDS("TestData_peopleword(1).RDS")
s <- readRDS("TestData_info (1).RDS")
a <- unlist(s$cumulative_worldwide_gross)
c <- str_replace_all(a,",","")
c <- as.numeric(c)
b <- s %>% mutate(worldwide = c) %>% filter(!is.na(rating_all) & !is.na(worldwide))

ui <- fluidPage(
    titlePanel("Moive Adventure"),
    tabsetPanel(
        tabPanel("Time",
                 fluidRow(
                     column(4,
                            sliderInput(inputId = "year", label = "Choose a year",
                                        min = 2000, max = 2010, value = 2000, step = 1,
                                        sep = "", ticks = FALSE)),
                     column(2,
                            selectInput("x",
                                        label = "X axis",
                                        c("Rating All","Award Win")))
                     ),
                 fluidRow(
                     column(width = 12,
                            plotOutput("scatterplot",
                                       click = "sp_click"))),
                 fluidRow(
                     column(width = 12,
                            h4("points near click"),
                            verbatimTextOutput("click_info")
                     )
                 )
            )
)
)

server <- function(input, output) {
    click <- reactive(b[, c("year", "title", "rating_all","worldwide","genres")] %>% filter(year == input$year))
    output$scatterplot = renderPlot({
        if (input$x == "Rating All"){
            b %>% filter(year == input$year) %>%
                ggplot(aes(rating_all,worldwide)) +
                geom_point() +
                xlab("Rating All") +
                ylab("Cumulative Worldwide Gross (US dollar)") +
                scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
                scale_y_log10()
        } else{
            b %>% filter(year == input$year) %>%
                ggplot(aes(award_win,worldwide)) +
                geom_point() +
                xlab("Award Win") +
                ylab("Cumulative Worldwide Gross (US dollar)") +
                scale_y_log10()
        }
    })
    output$click_info <- renderPrint({
        nearPoints(click(),input$sp_click, addDist = TRUE)
    })
}

shinyApp(ui = ui, server = server)
