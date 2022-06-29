####
## project: spatially decoupled parasite
## file: outreach_game_explore
## author: chris wojan
## function: use shiny to test out outreach game mechanics
####

## Load required libraries
library(shiny)
library(tidyverse)
library(ggplot2)

plot_revealed_patch <- function(patch){
  geom <- geom_tile(aes(x = patch$x, y = patch$y), alpha = patch$rich, 
                    color = "black", fill = "darkolivegreen4", size = 1)
  return(geom)
}


## Define ui
ui <- fluidPage(
  titlePanel("Forests and Foragers"),
  sidebarLayout(
    sidebarPanel(
      helpText("Move:"),
      fluidRow(
        actionButton(inputId = "up", label = "Up")
      ),
      fluidRow(
        actionButton(inputId = "left", label = "Left"),
        actionButton(inputId = "right", label = "Right")
      ),
      fluidRow(
        actionButton(inputId = "down", label = "Down") 
      )
    ),
    mainPanel(
      plotOutput("map")
    )
  )
)

## Define server
server <- function(input, output) {
  map_data <- reactiveValues(
    curr_coords = c(4,4),
    patches = list(tibble(x = 4, 
                          y = 4, 
                          rich = 1, 
                          coords = str_c(x, "_", y)))
  )
  observeEvent(input$up, {
    if(map_data$curr_coords[2] < 7){
      map_data$curr_coords[2] <- map_data$curr_coords[2] + 1
      coord_label <- str_c(map_data$curr_coords[1], "_", 
                           map_data$curr_coords[2])
      if(!coord_label %in% unlist(map_data$patches)){
        map_data$patches <- append(map_data$patches, 
                                   list(tibble(x = map_data$curr_coords[1],
                                               y = map_data$curr_coords[2],
                                               rich = runif(1),
                                               coords = coord_label)))
      }
    }
  })
  observeEvent(input$left, {
    if(map_data$curr_coords[1] > 1){
      map_data$curr_coords[1] <- map_data$curr_coords[1] - 1
      coord_label <- str_c(map_data$curr_coords[1], "_", 
                           map_data$curr_coords[2])
      if(!coord_label %in% unlist(map_data$patches)){
        map_data$patches <- append(map_data$patches, 
                                   list(tibble(x = map_data$curr_coords[1],
                                               y = map_data$curr_coords[2],
                                               rich = runif(1),
                                               coords = coord_label)))
      }
    }
  })
  observeEvent(input$right, {
    if(map_data$curr_coords[1] < 7){
      map_data$curr_coords[1] <- map_data$curr_coords[1] + 1
      coord_label <- str_c(map_data$curr_coords[1], "_", 
                           map_data$curr_coords[2])
      if(!coord_label %in% unlist(map_data$patches)){
        map_data$patches <- append(map_data$patches, 
                                   list(tibble(x = map_data$curr_coords[1],
                                               y = map_data$curr_coords[2],
                                               rich = runif(1),
                                               coords = coord_label)))
      }
    }
  })
  observeEvent(input$down, {
    if(map_data$curr_coords[2] > 1){
      map_data$curr_coords[2] <- map_data$curr_coords[2] - 1
      coord_label <- str_c(map_data$curr_coords[1], "_", 
                           map_data$curr_coords[2])
      if(!coord_label %in% unlist(map_data$patches)){
        map_data$patches <- append(map_data$patches, 
                                   list(tibble(x = map_data$curr_coords[1],
                                               y = map_data$curr_coords[2],
                                               rich = runif(1),
                                               coords = coord_label)))
      }
    }
  })
  landscape <- reactive({
    landscape <- ggplot() +
      map(map_data$patches, plot_revealed_patch) +
      geom_point(aes(x = map_data$curr_coords[1], y = map_data$curr_coords[2]), 
                 color = "tan4", size = 5) +
      coord_fixed(xlim = c(0,8), ylim = c(0,8)) +
      theme_void() +
      theme(legend.position = "none")
    return(landscape)
  })
  output$map <- renderPlot({
    landscape()
  })
}

## Run app
shinyApp(ui = ui, server = server)