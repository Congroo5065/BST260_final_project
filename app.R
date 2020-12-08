library(dplyr)
library(tidyverse)
library(shiny)
p <- readRDS("Data_people_00to20.RDS")
s <- readRDS("Data_info_00to20 orginal.RDS")
all_movies <- inner_join(p, s, by = "imdb_ID")


ui <- fluidPage(
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
                                  c("Rating All","Award Win"))),
               column(3,
                      textInput("director", "Director name contains (e.g., Miyazaki)"))
             ),
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
                                    "Italian"))),
               column(3,
                      textInput("cast", "Cast names contains (e.g. Tom Hanks)"))
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
    
    D <- paste0(input$director)
    Cast <- paste0(input$cast)
    
    if (!is.null(input$cast) && input$cast != ""){
      list_ID1 <- list()
      movies_na <- all_movies %>% na.omit(cast)
      for (n in c(1:length(movies_na$imdb_ID))){
        if (Cast == unlist(movies_na$cast[n])){
          list_ID1 <- append(list_ID1, movies_na$imdb_ID[n])
        } 
      }
      ID1 <- as.character(list_ID1)
      m <- m %>% filter(imdb_ID %in% ID1)
    }
    if (!is.null(input$director) && input$director != ""){
      list_ID2 <- list()
      for (n in c(1:length(all_movies$imdb_ID))){
        if (D == unlist(all_movies$director[n])){
          list_ID2 <- append(list_ID2, all_movies$imdb_ID[n])
        } 
      }
      ID2 <- as.character(list_ID2)
      m <- m %>% filter(imdb_ID %in% ID2)
    }
    
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
  
  click <- reactive(movies()[, c("year", "title", "rating_all","cumulative_worldwide_gross","genres",
                                 "director", "writer","cast", "award_win", "winner_oscar")])
  output$scatterplot = renderPlot({
    if (input$x == "Rating All"){
      movies() %>%
        ggplot(aes(rating_all,cumulative_worldwide_gross)) +
        geom_point() +
        xlab("Rating All") +
        ylab("Cumulative Worldwide Gross (US dollar)") +
        scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10))
    } else{
      movies() %>%
        ggplot(aes(award_win,cumulative_worldwide_gross)) +
        geom_point() +
        xlab("Award Win") +
        ylab("Cumulative Worldwide Gross (US dollar)")
    }
  })
  output$click_info <- renderPrint({
    nearPoints(click(),input$sp_click, addDist = TRUE)
  })
  output$brush_info <- renderPrint({
    brushedPoints(click(), input$plot1_brush)
  })
}

shinyApp(ui = ui, server = server)
