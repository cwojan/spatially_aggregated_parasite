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

move_mouse <- function(map_data, mouse_data, coord, boundary, direction){
  if(abs(map_data$curr_coords[coord] + direction) < boundary){
    map_data$curr_coords[coord] <- map_data$curr_coords[coord] + direction
    coord_label <- str_c(map_data$curr_coords[1], "_", 
                         map_data$curr_coords[2])
    new_tick_max <- max(0.8, mouse_data$ticks) + (rbinom(1,4,0.2))/5
    if(new_tick_max >= 1){
      tick_seq <- seq(1, new_tick_max, by = 0.2)
    } else {
      tick_seq <- 0
    }
    mouse_data$ticks <- tick_seq
    if(!coord_label %in% unlist(map_data$patches)){
      map_data$patches <- append(map_data$patches, 
                                 list(tibble(x = map_data$curr_coords[1],
                                             y = map_data$curr_coords[2],
                                             rich = runif(1),
                                             coords = coord_label)))
    }
  }
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
      plotOutput("map"),
      plotOutput("status")
    )
  )
)

## Define server
server <- function(input, output) {
  map_data <- reactiveValues(
    curr_coords = c(0,0),
    patches = list(tibble(x = 0, 
                          y = 0, 
                          rich = 1, 
                          coords = str_c(x, "_", y)))
  )
  mouse_data <- reactiveValues(
    energy = c(1, 1.5),
    ticks = 0
  )
  
  observeEvent(input$up, {
    move_mouse(map_data = map_data, mouse_data = mouse_data,
               coord = 2, boundary = 4, direction = 1)
  })
  observeEvent(input$left, {
    move_mouse(map_data = map_data, mouse_data = mouse_data,
               coord = 1, boundary = 4, direction = -1)
  })
  observeEvent(input$right, {
    move_mouse(map_data = map_data, mouse_data = mouse_data,
               coord = 1, boundary = 4, direction = 1)
  })
  observeEvent(input$down, {
    move_mouse(map_data = map_data, mouse_data = mouse_data,
               coord = 2, boundary = 4, direction = -1)
  })
  landscape <- reactive({
    landscape <- ggplot() +
      map(map_data$patches, plot_revealed_patch) +
      geom_point(aes(x = map_data$curr_coords[1], y = map_data$curr_coords[2]), 
                 color = "tan4", size = 5) +
      coord_fixed(xlim = c(-4,4), ylim = c(-4,4)) +
      theme_void() +
      theme(legend.position = "none")
    return(landscape)
  })
  mouse <- reactive({
    ggplot() +
      geom_point(aes(x = mouse_data$energy, y = "Energy"), 
                 size = 10, color = "red") +
      geom_point(aes(x = c(0, mouse_data$ticks), y = "Ticks Fed"),
                 size = 2) +
      coord_fixed(xlim = c(1,8)) +
      scale_y_discrete(limits = c("Ticks Fed", "Energy")) +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.x = element_blank())
  })
  output$map <- renderPlot({
    landscape()
  })
  output$status <- renderPlot({
    mouse()
  })
}

## Run app
shinyApp(ui = ui, server = server)