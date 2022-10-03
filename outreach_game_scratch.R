library(shiny)
library(tidyverse)
library(ggplot2)


generate_patch <- function(type){
  patch <- tibble(type = type) %>%
    mutate(
      type = factor(type, levels = c("grass","savanna","forest")),
      mticks = max(as.numeric(type) - 2, 0),
      fticks = as.numeric(type),
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

map_data <- list(patches = generate_deck(size = 25))

map_dat <- generate_deck()

ggplot() +
  geom_tile(data = map_dat, 
            aes(x = x, y = y, fill = type),
            color = "black", alpha = 0.7)


########

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

generate_deck <- function(size = 49){
  type_count <- floor(size / 3)
  forest_count <- type_count + (size %% 3)
  types <- c(rep(c("grass", "savanna"), each = type_count),
             rep("forest", forest_count))
  available_coords <- str_c(rep(-3:3, 7), rep(-3:3, each = 7), 
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


df <- generate_deck()

map_data <- list(
  curr_coords = c(0,0),
  patches = generate_deck()
)
map_data$revealed <- list("0_0" = map_data$patches %>%
  filter(coords == str_c(map_data$curr_coords[1], "_", map_data$curr_coords[2])))

map_data$revealed <- append(map_data$revealed, 
                            list(filter(map_data$patches, coords == "0_1")))

pmap_dfr(select(map_data$revealed, remain, x, y), plot_revealed_food)

plot_revealed_food(select(map_data$revealed, remain, x, y))

map(map_data$revealed, plot_revealed_food)

ggplot() +
  geom_tile(data = map_data$patches, 
            aes(x = x, y = y, fill = type),
            color = "black", alpha = 0.7) +
  map(map_data$revealed, plot_revealed_food) +
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

latest <- reactive({
  data.frame(Action = c("Move", "Forage"),
             Roll = c(mouse_data$tick_roll, mouse_data$forage_roll),
             Patch = c(mouse_data$patch_ticks, mouse_data$patch_rich),
             Result = c(str_c(as.character(mouse_data$new_ticks), " new ticks"),
                        str_c(as.character(mouse_data$forage), " energy gained")))
})



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
  return(deck)
}

coord_label <- "0_1"

map_data <- list(
  curr_coords = c(0,0),
  revealed = list("0_0" = tibble(x = 0, 
                                 y = 0,
                                 rich = 1, 
                                 type = factor("forest", levels = c("grass", "savanna", "forest")),
                                 ticks = 0.6,
                                 coords = str_c(x, "_", y))),
  unrevealed = generate_deck()
)


map_data$revealed[[coord_label]] <- tibble(x = 0, 
                                         y = 0,
                                         rich = 1, 
                                         type = factor("forest", levels = c("grass", "savanna", "forest")),
                                         ticks = 0.6,
                                         coords = str_c(x, "_", y))

map_data$revealed[[coord_label]]$rich

2 > map_data$revealed[[coord_label]][1,"rich"]

unname(map_data$revealed)

coord_label <- "0_0"

map_data$revealed[map_data$revealed$coords == coord_label, "rich"] 

map_data["revealed"]

deck <- generate_deck()

testlist <- list(name = tibble(x = 1, y = 2), second = tibble(x = 4, y = 3))

testlist$name$x

deck_Dat <- bind_rows(deck)

patch_id <- sample(length(deck), 1)

deck[[patch_id]]
deck[[patch_id]] <- NULL

energy <- seq(1,1.5,0.5)
ticks <- 0

new_tick_max <- max(0.8, ticks) + (rbinom(1,4,0.2))/5
tick_seq <- if_else(new_tick_max >= 1, 
                   seq(1, new_tick_max, by = 0.2),
                   0)
seq(1, new_tick_max, by = 0.2)

tick_seq
ticks <- tick_seq

(rbinom(1,4,0.2))/5

ifelse(new_tick_max >= 1, 
       seq(1, new_tick_max, 0.2),
       0)

new_ticks <- max(0.9, ticks) + (rbinom(1,4,0.2))/5
ticks <- c(ticks, new_ticks)

max(0.9, ticks)
ggplot() +
  geom_point(aes(x = energy, y = "Energy"), size = 10, color = "red") +
  geom_point(aes(x = c(0,ticks), y = "Ticks Fed"), size = 2) +
  coord_fixed(xlim = c(1,9)) +
  scale_y_discrete(limits = c("Ticks Fed", "Energy")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())





patches <- tibble(id = 1:49,
                  food = c(rbinom(15, 6, 0.75), rbinom(15, 6, 0.5), rbinom(19, 6, 0.25)), 
                  ticks = c(rbinom(15, 5, 0.75), rbinom(15, 5, 0.5), rbinom(19, 5, 0.25)))

curr_coords <- c(4,4)

revealed <- list()
revealed[[1]] <- tibble(x = curr_coords[1], y = curr_coords[2], text = "Start", rich = 1)
revealed[[2]] <- tibble(x = 5, y = 4, text = "Food: 6\nTicks: 4", rich = 1)
revealed[[3]] <- tibble(x = 5, y = 5, text = "Food: 3\nTicks: 1", rich = 0.5)

revealed <- list(tibble(x = curr_coords[1], y = curr_coords[2], text = "Start", rich = 1))
revealed <- append(revealed, list(tibble(x = 5, y = 4, text = "Food: 6\nTicks: 4", rich = 1)))



bind_rows(revealed)

revealed <- list(tibble(x = curr_coords[1], y = curr_coords[2], rich = 1))
revealed <- append(revealed, list(tibble(x = 5, y = 4, rich = 1, coords = str_c(5,"_",4))))

str_c(5,"_",4) %in% unlist(revealed)

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