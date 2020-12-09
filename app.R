library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
p <- readRDS("Data_people_00to20.RDS")
s <- readRDS("Data_info_00to20 orginal.RDS")
all_movies <- inner_join(p, s, by = "imdb_ID")


ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("Moive Adventure"),
                tabsetPanel(
                  tabPanel("Data Exploration",
                           fluidRow(
                             column(3,
                                    sliderInput(inputId = "year", label = "Year Released",
                                                min = 2000, max = 2020, value = c(2000,2020),
                                                sep = "", step = 1)),
                             column(3,
                                    sliderInput("boxoffice", "Dollars at Box Office (millions)",
                                                0, 2800, c(0, 2800), step = 10)),
                             column(3,
                                    selectInput("x",
                                                label = "X axis",
                                                c("Rating All","Rating Female", "Rating Male",
                                                  "Award Win", "Award Nominee"),
                                                selected = "Rating All")),
                             column(3,
                                    selectInput("y",
                                                label = "y axis",
                                                c("Budget","Open-week gross in USA",
                                                  "Gross in USA",
                                                  "Cumulative Worldwide Gross"),
                                                selected = "Cumulative Worldwide Gross"))),
                           fluidRow(
                             column(3,
                                    selectInput("country",
                                                label = "Choose a Country",
                                                c("All","USA","UK","Canada","France",
                                                  "Germany", "Australia","India",
                                                  "Italy","China","Spain","Ireland",
                                                  "Japan"))),
                             column(3,
                                    selectInput("genre", "Genre (a movie can have multiple genres)",
                                                c("All", "action", "adventure", "animation", "biography", "comedy",
                                                  "crime", "drama", "family", "fantasy", "history",
                                                  "horror", "music", "musical", "mystery", "romance", "sci-fi",
                                                  "sport", "thriller", "war", "western"))),
                             column(3,
                                    selectInput("language",
                                                label = "Choose a Language",
                                                c("All","English","Spanish","French","German",
                                                  "Italian")))
                           ),
                           fluidRow(
                             column(width = 12,
                                    plotOutput("scatterplot",
                                               click = "sp_click",brush = brushOpts(
                                                 id = "plot1_brush"
                                               )))),
                           fluidRow(
                             column(width = 6,
                                    h4("Points near click"),
                                    verbatimTextOutput("click_info")
                             ),
                             column(width = 6,
                                    h4("Brushed points"),
                                    verbatimTextOutput("brush_info")
                             )
                           )
                  )
                )
)

server <- function(input, output) {
  movies <- reactive({
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minboxoffice <- input$boxoffice[1]
    maxboxoffice <- input$boxoffice[2]
    L <- paste0("lang_", input$language)
    G <- paste0("genre_", input$genre)
    C <- paste0("country_", input$country)
    
    
    m <- all_movies %>%
      filter(
        year >= minyear,
        year <= maxyear,
        cumulative_worldwide_gross >= minboxoffice,
        cumulative_worldwide_gross <= maxboxoffice
      )
    
    if (input$genre != "All") {
      m <- m %>% filter(.data[[G]] == 1)
    }
    if (input$language != "All"){
      m <- m %>% filter(.data[[L]] == 1)
    }
    if (input$country != "All"){
      m <- m %>% filter(.data[[C]] == 1)
    }
    m <- data.frame(m)
  })
  
  click1 <- reactive(movies()[, c("year", "title", "rating_all","rating_female",
                                  "rating_male","genres",
                                  "director", "writer","cast",
                                  "award_win","award_nom", "winner_oscar",
                                  "budget", "opening_weekend_usa", "gross_usa",
                                  "cumulative_worldwide_gross")])
  
  click2 <- reactive(movies()[, c("year", "title", "rating_all","rating_female",
                                  "rating_male","genres",
                                  "award_win","award_nom",
                                  "budget", "opening_weekend_usa", "gross_usa",
                                  "cumulative_worldwide_gross")])
  
  output$scatterplot = renderPlot({
    if (input$x == "Rating All"){
      if (input$y == "Budget"){
        movies() %>%
          ggplot(aes(rating_all,budget)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
      else if (input$y == "Open-week gross in USA"){
        movies() %>%
          ggplot(aes(rating_all,opening_weekend_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Gross in USA"){
        movies() %>%
          ggplot(aes(rating_all,gross_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Cumulative Worldwide Gross"){
        movies() %>%
          ggplot(aes(rating_all,cumulative_worldwide_gross)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
    } else if (input$x == "Rating Female"){
      if (input$y == "Budget"){
        movies() %>%
          ggplot(aes(rating_female,budget)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
      else if (input$y == "Open-week gross in USA"){
        movies() %>%
          ggplot(aes(rating_female,opening_weekend_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Gross in USA"){
        movies() %>%
          ggplot(aes(rating_female,gross_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Cumulative Worldwide Gross"){
        movies() %>%
          ggplot(aes(rating_female,cumulative_worldwide_gross)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
    } else if (input$x == "Rating Male"){
      if (input$y == "Budget"){
        movies() %>%
          ggplot(aes(rating_male,budget)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
      else if (input$y == "Open-week gross in USA"){
        movies() %>%
          ggplot(aes(rating_male,opening_weekend_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Gross in USA"){
        movies() %>%
          ggplot(aes(rating_male,gross_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Cumulative Worldwide Gross"){
        movies() %>%
          ggplot(aes(rating_male,cumulative_worldwide_gross)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
    } else if (input$x == "Award Win"){
      if (input$y == "Budget"){
        movies() %>%
          ggplot(aes(award_win,budget)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
      else if (input$y == "Open-week gross in USA"){
        movies() %>%
          ggplot(aes(award_win,opening_weekend_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Gross in USA"){
        movies() %>%
          ggplot(aes(award_win,gross_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Cumulative Worldwide Gross"){
        movies() %>%
          ggplot(aes(award_win,cumulative_worldwide_gross)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
    } else {
      if (input$y == "Budget"){
        movies() %>%
          ggplot(aes(award_nom,budget)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
      else if (input$y == "Open-week gross in USA"){
        movies() %>%
          ggplot(aes(award_nom,opening_weekend_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Gross in USA"){
        movies() %>%
          ggplot(aes(award_nom,gross_usa)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      } else if (input$y == "Cumulative Worldwide Gross"){
        movies() %>%
          ggplot(aes(award_nom,cumulative_worldwide_gross)) +
          geom_point() +
          xlab(paste0(input$x)) +
          ylab(paste0(input$y, " (in Million US dollar)")) +
          ggtitle(paste0(input$x, " vs ", input$y))
      }
    } 
  })
  output$click_info <- renderPrint({
    nearPoints(click1(),input$sp_click, addDist = TRUE)
  })
  output$brush_info <- renderPrint({
    brushedPoints(click2(), input$plot1_brush)
  })
}

shinyApp(ui = ui, server = server)
