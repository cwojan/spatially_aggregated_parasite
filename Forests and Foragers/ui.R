## Load required libraries
library(shiny)
library(tidyverse)
library(ggplot2)

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