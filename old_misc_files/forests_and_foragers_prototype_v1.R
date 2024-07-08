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
      mticks = max(as.numeric(type) - 2, 0),
      mt_remain = mticks,
      fticks = as.numeric(type) - 1,
      ft_remain = fticks,
      min_rich = as.numeric(type) + 1,
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

plot_ft_spaces <- function(patch){
  if(patch$fticks > 0){
    ft_x <- seq(from = patch$x - 0.3, to = (patch$x - 0.3) + (patch$fticks - 1) * 0.15, 
                by = 0.15)
    ft_alpha <- 1
  } else {
    ft_x <- 1
    ft_alpha <- 0
  }
  geom <- geom_point(aes(x = ft_x, y = patch$y + 0.35), 
                     color = "black", shape = 5, size = 3.5, alpha = ft_alpha)
}

plot_ft_remain <- function(patch){
  if(patch$ft_remain == 0){
    ft_x <- 1
    ft_size <- 0
  }else{
    ft_x <- seq(from = patch$x - 0.3, to = (patch$x - 0.3) + (patch$ft_remain - 1) * 0.15, 
                  by = 0.15)
    ft_size = 4
  }
  geom <- geom_point(aes(x = ft_x, y = patch$y + 0.35), color = "black", shape = 18, size = ft_size)
}

plot_food_spaces <- function(patch){
  food_x <- seq(from = patch$x - 0.35, to = (patch$x - 0.35) + (patch$rich - 1) * 0.15, 
                by = 0.15)
  geom <- geom_point(aes(x = food_x, y = patch$y + 0.2), 
                     color = "black", shape = 1, size = 3.5)
}

plot_revealed_food <- function(patch){
  if(patch$remain == 0){
    food_x <- 1
    food_size <- 0
  }else{
    food_x <- seq(from = patch$x - 0.35, to = (patch$x - 0.35) + (patch$remain - 1) * 0.15, 
                  by = 0.15)
    food_size = 3
  }
  geom <- geom_point(aes(x = food_x, y = patch$y + 0.2), color = "red", size = food_size)
}

move_mouse <- function(map_data, revealed, mouse_data, coord, boundary, direction, fox_data){
  if(mouse_data$turns > 0 & mouse_data$energy > 0){
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
      mouse_data$patch_mticks <- new_patch$mticks
      map_data$patches[map_data$patches$coords == coord_label,"mt_remain"] <- 0
      mouse_data$ticks <- mouse_data$ticks + mouse_data$patch_mticks
      mouse_data$turns <- mouse_data$turns - 1
      fox_action(fox_data = fox_data, boundary = boundary, mouse_data = mouse_data)
    }
  }
}

fox_action <- function(fox_data, boundary, mouse_data){
  old_coords <- fox_data$curr_coords
  fox_data$curr_coords <- fox_data$curr_coords +
    sample(x = list(c(1,0), c(0,1), c(-1,0), c(0,-1)), 1)[[1]]
  if(abs(fox_data$curr_coords[1]) == boundary || abs(fox_data$curr_coords[2]) == boundary){
    fox_data$curr_coords <- old_coords
  }
}

pred_check <- function(fox_data, mouse_data){
  if(sum(fox_data$curr_coords == mouse_data$curr_coords) == 2){
    mouse_data$energy <- 0
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
    patch_mticks = NA,
    patch_ft_remain = NA,
    forage_roll = NA,
    forage = NA,
    patch_remain = NA,
    turns = isolate(input$game_turns)
  )
  
  fox_data <- reactiveValues(curr_coords = c(
    sample(c(1,-1), 1),
    sample(c(1,-1), 1)
  ))
  
  revealed <- reactiveValues(
    revealed = list("0_0" = isolate(map_data$patches) %>%
      filter(coords == "0_0"))
  )
  
  observeEvent(input$up, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 2, boundary = 3, direction = 1, fox_data = fox_data)
    pred_check(mouse_data = mouse_data, fox_data = fox_data)
  })
  observeEvent(input$left, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 1, boundary = 3, direction = -1, fox_data = fox_data)
    pred_check(mouse_data = mouse_data, fox_data = fox_data)
  })
  observeEvent(input$right, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 1, boundary = 3, direction = 1, fox_data = fox_data)
    pred_check(mouse_data = mouse_data, fox_data = fox_data)
  })
  observeEvent(input$down, {
    move_mouse(map_data = map_data, revealed = revealed, mouse_data = mouse_data,
               coord = 2, boundary = 3, direction = -1, fox_data = fox_data)
    pred_check(mouse_data = mouse_data, fox_data = fox_data)
  })
  
  observeEvent(input$forage, {
    if(mouse_data$turns > 0 & mouse_data$energy > 0){
      coord_label <- str_c(mouse_data$curr_coords[1], "_", 
                           mouse_data$curr_coords[2])
      mouse_data$patch_remain <- revealed$revealed[[coord_label]]$remain
      mouse_data$patch_ft_remain <- revealed$revealed[[coord_label]]$ft_remain
      mouse_data$forage_roll <- sample(1:6, 1)
      mouse_data$forage <- ifelse(mouse_data$forage_roll > mouse_data$patch_remain, 
                                  0, 
                                  mouse_data$forage_roll)
      revealed$revealed[[coord_label]]$remain <- 
        mouse_data$patch_remain - mouse_data$forage
      if(mouse_data$patch_ft_remain > 0){
        mouse_data$ticks  <- mouse_data$ticks + 1
        revealed$revealed[[coord_label]]$ft_remain <- mouse_data$patch_ft_remain - 1
      }
      mouse_data$energy <- mouse_data$energy + mouse_data$forage
      mouse_data$turns <- mouse_data$turns - 1
      fox_action(fox_data = fox_data, boundary = 3)
      pred_check(mouse_data = mouse_data, fox_data = fox_data)
    }
  })
  
  observeEvent(input$reset, {
    map_data$patches <- generate_deck()
    
    mouse_data$curr_coords <- c(0,0)
    mouse_data$energy <- 4
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
      results <- str_c("Winter is here!\nWeight Score: ",
                       mouse_data$energy - (mouse_data$ticks)/2)
      showNotification(ui = results,
                       type = "message",
                       duration = NULL,
                       id = "game_over")
    }
  })
  
  observeEvent(mouse_data$energy, {
    if(mouse_data$energy == 0){
      results <- str_c("Eaten by a fox!\nScore: ",
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
      geom_point(data = map_data$patches,
                 aes(x = x - 0.35, y = y - 0.3, alpha = if_else(mticks == 1, "y", "n")),
                 color = "black", shape = 5, size = 3.5) +
      geom_point(data = map_data$patches,
                 aes(x = x - 0.35, y = y - 0.3, alpha = if_else(mt_remain == 1, "y", "n")),
                 color = "black", shape = 18, size = 4) +
      map(revealed$revealed, plot_food_spaces) +
      map(revealed$revealed, plot_revealed_food) +
      map(revealed$revealed, plot_ft_spaces) +
      map(revealed$revealed, plot_ft_remain) +
      geom_point(aes(x = mouse_data$curr_coords[1], y = mouse_data$curr_coords[2] - 0.25), 
                 color = "black", fill = "tan4", size = 5, shape = 24) +
      geom_point(aes(x = fox_data$curr_coords[1], y = fox_data$curr_coords[2]), 
                 color = "black", fill = "darkorange2", size = 7, shape = 25) +
      coord_fixed(xlim = c(-3,3), ylim = c(-3,3), expand = FALSE) +
      scale_fill_manual(values = c("chartreuse", "goldenrod", "darkgreen"),
                        labels = c("Grass \n- Low Resources, Low Ticks",
                                   "Savanna \n- Medium Resources, Medium Ticks",
                                   "Forest \n- High Resources, High Ticks"),
                        drop = FALSE, name = "Patch Type") +
      scale_alpha_manual(values = c(0, 1), guide = "none") +
      theme_void() +
      theme(panel.border = element_rect(color = "black", size = 1, fill = NA))
    return(landscape)
  })
  mouse <- reactive({
    tibble(Attribute = c("Total Energy", "Total Ticks Fed", "Total Turns Remaining"),
               Value = c(mouse_data$energy, mouse_data$ticks, mouse_data$turns))
  })
  latest <- reactive({
    tibble(Action = c("Forage"),
               Roll = c(mouse_data$forage_roll),
               Patch = c(mouse_data$patch_remain),
               Result = c(str_c(as.character(mouse_data$forage), " energy gained")))
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