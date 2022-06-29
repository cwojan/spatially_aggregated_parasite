generate_patch <- function(type){
  patch <- tibble(type = type, rich = runif(1))
}

generate_deck <- function(size = 48){
  type_count <- floor(size / 3)
  forest_count <- type_count + (size %% 3)
  types <- c(rep(c("grass", "savanna"), each = type_count),
             rep("forest", forest_count)) 
  deck <- map(types, generate_patch)
  return(deck)
}

deck <- generate_deck()

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