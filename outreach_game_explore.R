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

patches <- tibble(id = 1:49,
                  food = c(rbinom(15, 6, 0.75), rbinom(15, 6, 0.5), rbinom(19, 6, 0.25)), 
                  ticks = c(rbinom(15, 5, 0.75), rbinom(15, 5, 0.5), rbinom(19, 5, 0.25)))

curr_coords <- c(4,4)

revealed <- list()
revealed[[1]] <- tibble(x = curr_coords[1], y = curr_coords[2], text = "Start", rich = 1)
revealed[[2]] <- tibble(x = 5, y = 4, text = "Food: 6\nTicks: 4", rich = 1)
revealed[[3]] <- tibble(x = 5, y = 5, text = "Food: 3\nTicks: 1", rich = 0.5)

plot_revealed_patch <- function(patch){
  geom <- geom_tile(aes(x = patch$x, y = patch$y), alpha = patch$rich, color = "black", fill = "darkolivegreen4", size = 1)
  return(geom)
}

plot_revealed_text <- function(patch){
  geom <- geom_text(aes(x = patch$x, y = patch$y + 0.2, label = patch$text))
  return(geom)
}

landscape <- ggplot() +
  geom_tile(aes(x = curr_coords[1], y = curr_coords[2]), color = "black", fill = "darkolivegreen4", size = 1) +
  geom_text(aes(x = curr_coords[1], y = curr_coords[2] + 0.2), label = "Start") +
  geom_point(aes(x = curr_coords[1], y = curr_coords[2] - 0.3), color = "tan4", size = 5) +
  coord_fixed(xlim = c(0,8), ylim = c(0,8)) +
  theme_void() +
  theme(legend.position = "none")

landscape

landscape +
  map(revealed, plot_revealed_patch) +
  map(revealed, plot_revealed_text)

landscape +
  geom_tile(aes(x = 4, y = 4), color = "black", fill = "darkolivegreen4", size = 1) +
  geom_text(aes(x = 4, y = 4.25), label = "Start") +
  geom_tile(aes(x = 5, y = 4), color = "black", fill = "darkolivegreen4", size = 1) +
  geom_text(aes(x = 5, y = 4.25), label = "Food: ") +
  geom_point(aes(x = 5, y = 4), color = "tan4", size = 5) 

## Define ui
ui <- fluidPage(
  
)

## Define server
server <- function(input, output) {
  
}

## Run app
shinyApp(ui = ui, server = server)