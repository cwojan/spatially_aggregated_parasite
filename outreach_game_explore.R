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

generate_patch <- function(type){
  patch <- tibble(type = type, rich = sample(1:6, 1)) %>%
    mutate(
      type = factor(type, levels = c("grass","savanna","forest")),
      ticks = as.numeric(type) * 2
    )
}

generate_deck <- function(size = 48){
  type_count <- floor(size / 3)
  forest_count <- type_count + (size %% 3)
  types <- c(rep(c("grass", "savanna"), each = type_count),
             rep("forest", forest_count)) 
  deck <- map(types, generate_patch)
}

plot_revealed_patch <- function(patch){
  geom <- geom_tile(aes(x = patch$x, y = patch$y, fill = patch$type), 
                    color = "black", alpha = 0.7, size = 1)
  return(geom)
}

plot_revealed_food <- function(patch){
  if(patch$rich == 0){
    food_x <- 1
    food_size = 0
  }else{
    food_x <- seq(from = patch$x - 0.35, to = (patch$x - 0.35) + (patch$rich - 1) * 0.15, 
                  by = 0.15)
    food_size = 3
  }
  geom <- geom_point(aes(x = food_x, y = patch$y + 0.25), color = "red", size = food_size)
}

move_mouse <- function(map_data, mouse_data, coord, boundary, direction){
  if(abs(map_data$curr_coords[coord] + direction) < boundary){
    map_data$curr_coords[coord] <- map_data$curr_coords[coord] + direction
    coord_label <- str_c(map_data$curr_coords[1], "_", 
                         map_data$curr_coords[2])
    if(!coord_label %in% unlist(map_data$revealed)){
      new_patch_id <- sample(length(map_data$unrevealed), 1)
      new_patch <- map_data$unrevealed[[new_patch_id]]
      new_patch_list <- list(tibble(x = map_data$curr_coords[1],
                                         y = map_data$curr_coords[2],
                                         rich = new_patch$rich,
                                         type = new_patch$type,
                                         ticks = new_patch$ticks,
                                         coords = coord_label))
      names(new_patch_list) <- coord_label
      map_data$revealed <- append(map_data$revealed, new_patch_list)
      map_data$unrevealed[[new_patch_id]] <- NULL
    }
    revealed_data <- bind_rows(map_data$revealed)
    mouse_data$patch_ticks <- revealed_data %>%
      filter(coords == coord_label) %>%
      pull(ticks)
    mouse_data$tick_roll <- sample(1:6, 1)
    mouse_data$new_ticks <- ifelse(mouse_data$tick_roll > mouse_data$patch_ticks,
                                   0, mouse_data$tick_roll)
    mouse_data$ticks <- mouse_data$ticks + mouse_data$new_ticks
    mouse_data$turns <- mouse_data$turns + 1
  }
}


## Define ui
ui <- fluidPage(
  titlePanel("Forests and Foragers"),
  sidebarLayout(
    sidebarPanel(
      h3("Actions"),
      h4("Move:"),
      fluidRow(
        actionButton(inputId = "up", label = "Up")
      ),
      fluidRow(
        actionButton(inputId = "left", label = "Left"),
        actionButton(inputId = "right", label = "Right")
      ),
      fluidRow(
        actionButton(inputId = "down", label = "Down") 
      ),
      h4("Do:"),
      fluidRow(
        actionButton(inputId = "forage", label = "Forage")
      )
    ),
    mainPanel(
      h3("Landscape"),
      plotOutput("map"),
      column(6,
             h4("Latest Status Update"),
             tableOutput("update")),
      column(6, 
             h4("Current Status"),
             tableOutput("status"))
    )
  )
)

## Define server
server <- function(input, output) {
  map_data <- reactiveValues(
    curr_coords = c(0,0),
    revealed = list("0_0" = tibble(x = 0, 
                                   y = 0,
                                   rich = 1, 
                                   type = factor("forest", 
                                                 levels = c("grass", "savanna", "forest")),
                                   ticks = 0.6,
                                   coords = str_c(x, "_", y))),
    unrevealed = generate_deck()
  )
  mouse_data <- reactiveValues(
    energy = 2,
    ticks = 0,
    tick_roll = NA,
    new_ticks = NA,
    patch_ticks = NA,
    forage_roll = NA,
    forage = NA,
    patch_rich = NA,
    turns = 0
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
  
  observeEvent(input$forage, {
    coord_label <- str_c(map_data$curr_coords[1], "_", 
                         map_data$curr_coords[2])
    mouse_data$patch_rich <- map_data$revealed[[coord_label]]$rich
    mouse_data$forage_roll <- sample(1:6, 1)
    mouse_data$forage <- ifelse(mouse_data$forage_roll > mouse_data$patch_rich, 
                          0, 
                          mouse_data$forage_roll)
    map_data$revealed[[coord_label]][1,"rich"] <- mouse_data$patch_rich - mouse_data$forage
    mouse_data$energy <- mouse_data$energy + mouse_data$forage
    mouse_data$turns <- mouse_data$turns + 1
  })
  
  landscape <- reactive({
    landscape <- ggplot() +
      map(unname(map_data$revealed), plot_revealed_patch) +
      map(unname(map_data$revealed), plot_revealed_food) +
      geom_point(aes(x = map_data$curr_coords[1], y = map_data$curr_coords[2] - 0.25), 
                 color = "black", fill = "tan4", size = 5, shape = 24) +
      coord_fixed(xlim = c(-4,4), ylim = c(-4,4), expand = FALSE) +
      scale_fill_manual(values = c("chartreuse", "goldenrod", "darkgreen"),
                        labels = c("Grass - Low Ticks",
                                   "Savanna - Medium Ticks",
                                   "Forest - High Ticks"),
                        drop = FALSE, name = "Patch Type") +
      theme_void() +
      theme(panel.border = element_rect(color = "black", size = 1, fill = NA))
    return(landscape)
  })
  mouse <- reactive({
    tibble(Attribute = c("Total Energy", "Total Ticks Fed", "Total Turns Taken"),
               Value = c(mouse_data$energy, mouse_data$ticks, mouse_data$turns))
  })
  latest <- reactive({
    tibble(Action = c("Move", "Forage"),
               Roll = c(mouse_data$tick_roll, mouse_data$forage_roll),
               Patch = c(mouse_data$patch_ticks, mouse_data$patch_rich),
               Result = c(str_c(as.character(mouse_data$new_ticks), " new ticks"),
                          str_c(as.character(mouse_data$forage), " energy gained")))
  })

  output$map <- renderPlot({
    landscape()
  })
  output$update <- renderTable({
    latest()
  })
  output$status <- renderTable({
    mouse()
  })
}

## Run app
shinyApp(ui = ui, server = server)