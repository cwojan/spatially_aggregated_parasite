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
  patch <- tibble(type = type) %>%
    mutate(
      type = factor(type, levels = c("grass","savanna","forest")),
      ticks = as.numeric(type) * 2,
      min_rich = as.numeric(type),
      max_rich = as.numeric(type) + 3,
      rich = sample(min_rich:max_rich, 1),
      remain = rich
    )
}

generate_deck <- function(size = 25){
  type_count <- floor(size / 3)
  forest_count <- type_count + (size %% 3)
  types <- c(rep(c("grass", "savanna"), each = type_count),
             rep("forest", forest_count))
  available_coords <- str_c(rep(-2:2, 5), rep(-2:2, each = 5), 
                            sep = "_")
  deck <- map_df(types, generate_patch) %>%
    mutate(
      coords = sample(available_coords, size = size)
    ) %>%
    separate(coords, into = c("x","y"), sep = "_", remove = FALSE, convert = TRUE)
}

plot_food_spaces <- function(patch){
  food_x <- seq(from = patch$x - 0.35, to = (patch$x - 0.35) + (patch$rich - 1) * 0.15, 
                by = 0.15)
  geom <- geom_point(aes(x = food_x, y = patch$y + 0.25), 
                     color = "black", shape = 1, size = 3.5)
}

plot_revealed_food <- function(patch){
  if(patch$remain == 0){
    food_x <- 1
    food_size = 0
  }else{
    food_x <- seq(from = patch$x - 0.35, to = (patch$x - 0.35) + (patch$remain - 1) * 0.15, 
                  by = 0.15)
    food_size = 3
  }
  geom <- geom_point(aes(x = food_x, y = patch$y + 0.25), color = "red", size = food_size)
}

move_mouse <- function(map_data, revealed, mouse_data, coord, boundary, direction){
  if(mouse_data$turns > 0){
    if(abs(mouse_data$curr_coords[coord] + direction) < boundary){
      mouse_data$curr_coords[coord] <- mouse_data$curr_coords[coord] + direction
      coord_label <- str_c(mouse_data$curr_coords[1], "_", 
                           mouse_data$curr_coords[2])
      new_patch <- filter(map_data$patches, coords == coord_label)
      if(!coord_label %in% unlist(revealed$revealed)){
        new_patch_list <- list(new_patch)
        names(new_patch_list) <- coord_label
        revealed$revealed <- append(revealed$revealed, new_patch_list)
      }
      mouse_data$patch_ticks <- new_patch$ticks
      mouse_data$tick_roll <- sample(1:6, 1)
      mouse_data$new_ticks <- ifelse(mouse_data$tick_roll > mouse_data$patch_ticks,
                                     0, mouse_data$tick_roll)
      mouse_data$ticks <- mouse_data$ticks + mouse_data$new_ticks
      mouse_data$turns <- mouse_data$turns - 1
    }
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
      ),
      h3("Start Over"),
      fluidRow(
        actionButton(inputId = "reset", label = "Reset"),
        numericInput(inputId = "game_turns", label = "# of Turns for New Game",
                     value = 20, min = 5, max = 50)
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
  map_data <- reactiveValues(patches = generate_deck())
  mouse_data <- reactiveValues(
    curr_coords = c(0,0),
    energy = 2,
    ticks = 0,
    tick_roll = NA,
    new_ticks = NA,
    patch_ticks = NA,
    forage_roll = NA,
    forage = NA,
    patch_remain = NA,
    turns = isolate(input$game_turns)
  )
  revealed <- reactiveValues(
    revealed = list("0_0" = isolate(map_data$patches) %>%
      filter(coords == "0_0"))
  )
  
  observeEvent(input$up, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 2, boundary = 4, direction = 1)
  })
  observeEvent(input$left, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 1, boundary = 4, direction = -1)
  })
  observeEvent(input$right, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 1, boundary = 4, direction = 1)
  })
  observeEvent(input$down, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 2, boundary = 4, direction = -1)
  })
  
  observeEvent(input$forage, {
    if(mouse_data$turns > 0){
      coord_label <- str_c(mouse_data$curr_coords[1], "_", 
                           mouse_data$curr_coords[2])
      mouse_data$patch_remain <- revealed$revealed[[coord_label]]$remain
      mouse_data$forage_roll <- sample(1:6, 1)
      mouse_data$forage <- ifelse(mouse_data$forage_roll > mouse_data$patch_remain, 
                                  0, 
                                  mouse_data$forage_roll)
      revealed$revealed[[coord_label]]$remain <- 
        mouse_data$patch_remain - mouse_data$forage
      mouse_data$energy <- mouse_data$energy + mouse_data$forage
      mouse_data$turns <- mouse_data$turns - 1
    }
  })
  
  observeEvent(input$reset, {
    map_data$patches <- generate_deck()
    
    mouse_data$curr_coords <- c(0,0)
    mouse_data$energy <- 2
    mouse_data$ticks <- 0
    mouse_data$tick_roll <- NA
    mouse_data$new_ticks <- NA
    mouse_data$patch_ticks <- NA
    mouse_data$forage_roll <- NA
    mouse_data$forage <- NA
    mouse_data$patch_rich <- NA
    mouse_data$turns <- input$game_turns
    
    revealed$revealed <- list("0_0" = isolate(map_data$patches) %>%
                                filter(coords == "0_0"))
    
    removeNotification(id = "game_over")
  })
  
  observeEvent(mouse_data$turns, {
    if(mouse_data$turns == 0){
      results <- str_c("Game Over!\nScore: ",
                       mouse_data$energy - (mouse_data$ticks)/2)
      showNotification(ui = results,
                       type = "message",
                       duration = NULL,
                       id = "game_over")
    }
  })
  
  landscape <- reactive({
    landscape <- ggplot() +
      geom_tile(data = map_data$patches, 
                aes(x = x, y = y, fill = type),
                color = "black", alpha = 0.7) +
      map(revealed$revealed, plot_food_spaces) +
      map(revealed$revealed, plot_revealed_food) +
      geom_point(aes(x = mouse_data$curr_coords[1], y = mouse_data$curr_coords[2] - 0.25), 
                 color = "black", fill = "tan4", size = 5, shape = 24) +
      coord_fixed(xlim = c(-3,3), ylim = c(-3,3), expand = FALSE) +
      scale_fill_manual(values = c("chartreuse", "goldenrod", "darkgreen"),
                        labels = c("Grass \n- Low Ticks & Resources",
                                   "Savanna \n- Medium Ticks & Resources",
                                   "Forest \n- High Ticks & Resources"),
                        drop = FALSE, name = "Patch Type") +
      theme_void() +
      theme(panel.border = element_rect(color = "black", size = 1, fill = NA))
    return(landscape)
  })
  mouse <- reactive({
    tibble(Attribute = c("Total Energy", "Total Ticks Fed", "Total Turns Remaining"),
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