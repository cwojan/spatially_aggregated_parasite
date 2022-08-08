####
## project: spatially_decoupled_parasite
## file: host_move_wip.R
## function: scratch code brainstorming how to simulate host movement
## author: chris wojan
####

library(purrr)
library(raster)
library(tidyverse)

### modified percolation function

landscape_percolator <- function(size, potential_prop, cluster){
  potential <- floor((size^2) * potential_prop)
  coords <- tibble(x = rep(1:size, size), 
                   y = rep(1:size, each = size),
                   id = str_c(x, y, sep = "_"),
                   weight = rep(1, size^2),
                   value = rep(0, size^2))
  
  
  
  possibles <- coords$id
  for(p in seq_len(potential)){
    
    point <- sample(possibles, size = 1, prob = coords[coords$id %in% possibles,]$weight)
    coords[coords$id %in% point, "value"] <- 1
    
    ## Update weights
    ones <- coords[coords$value == 1,]$id
    zeroes <- coords[coords$value == 0,]$id
    for(i in zeroes){
      coords_i <- coords[coords$id %in% i, c("x", "y")]
      dists_i <- NULL
      for(j in ones){
        coords_j <- coords[coords$id %in% j, c("x", "y")]
        dist_ij <- sqrt((coords_j$x - coords_i$x)^2 + (coords_j$y - coords_i$y)^2)
        dists_i <- c(dists_i, dist_ij)
      }
      inv_dist <- 1/min(dists_i)
      coords[coords$id %in% i, "weight"] <- (1 + inv_dist)^cluster
    }
    
    possibles <- coords %>%
      filter(value == 0) %>%
      #slice_max(weight, n = floor((length(coords$weight) * cluster))) %>%
      pull(id)
    
  }
  
  landscape <- list(coords = coords, 
                    matrix = matrix(coords$value, nrow = size, ncol = size))
  
  return(landscape)
}

## place hosts
size <- 12

test_ls <- landscape_percolator(size, 0.25, 0)

test_matrix <- test_ls$matrix

test_coords <- test_ls$coords

n_hosts <- 24

host_origins <- slice_sample(test_coords, n = n_hosts, replace = TRUE)

hosts <- host_origins %>%
  select(x, y, id) %>%
  mutate(host_id = seq_len(nrow(host_origins)),
         parasites = 0,
         time = 0)

## move hosts

moves <- tibble(move = c("stay", "up", "left", "right", "down"),
                dx = c(0, 0, -1, 1, 0),
                dy = c(0, 1, 0, 0, -1))

torus_helper <- tibble(raw = c(0, seq_len(size), size + 1),
                       wrap = c(size, seq_len(size),1))


for(i in hosts$host_id){
  host_i <- filter(hosts, host_id %in% i)
  move_i <- filter(moves, move %in% sample(moves$move, size = 1))
  host_i <- host_i %>%
    mutate(
      x = x + move_i$dx,
      y = y + move_i$dy,
      x = torus_helper %>% filter(raw %in% x) %>% pull(wrap),
      y = torus_helper %>% filter(raw %in% y) %>% pull(wrap),
      id = str_c(x, y, sep = "_"),
      time = time + 1
    )
  hosts <- bind_rows(hosts, host_i)
}

move_host <- function(hid, host_data, moves, torus_helper){
  host_i <- filter(host_data, host_id %in% hid)
  move_i <- filter(moves, move %in% sample(moves$move, size = 1))
  host_i <- host_i %>%
    mutate(
      x = x + move_i$dx,
      y = y + move_i$dy,
      x = torus_helper %>% filter(raw %in% x) %>% pull(wrap),
      y = torus_helper %>% filter(raw %in% y) %>% pull(wrap),
      id = str_c(x, y, sep = "_"),
      time = time + 1
    )
  return(host_i)
}


move_test <- map_df(seq_len(n_hosts), move_host, host_data = hosts, moves = moves, torus_helper = torus_helper)
