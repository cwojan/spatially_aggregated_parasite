####
## project: spatially_decoupled_parasite
## file: clustering_sim_wip.R
## function: exploring relationship of landscape moran's i and parasite burden stats
## author: chris wojan
####

library(purrr)
library(raster)
library(tidyverse)
library(ape)

## Landscape generator function
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
  
  ls_dists <- as.matrix(dist(cbind(coords$x, coords$y)))
  
  ls_dists_inv <- 1/ls_dists
  diag(ls_dists_inv) <- 0
  
  moran_result <- Moran.I(coords$value, ls_dists_inv)
  
  ls_stats <-tibble(cluster = cluster, 
                    moran = moran_result$observed, 
                    p = moran_result$p.value)
  
  landscape <- list(coords = coords, 
                    matrix = matrix(coords$value, nrow = size, ncol = size),
                    ls_stats = ls_stats)
  
  return(landscape)
}

## Host movement simulator function
move_sim <- function(landscape, n_hosts, timesteps){
  coords <- landscape$coords
  
  size <- max(coords$x)
  
  host_origins <- slice_sample(coords, n = n_hosts, replace = TRUE)
  
  hosts <- host_origins %>%
    select(x, y, id) %>%
    mutate(host_id = seq_len(nrow(host_origins)),
           parasites = 0,
           time = 0)
  
  moves <- tibble(move = c("stay", "up", "left", "right", "down"),
                  dx = c(0, 0, -1, 1, 0),
                  dy = c(0, 1, 0, 0, -1))
  
  torus_helper <- tibble(raw = c(0, seq_len(size), size + 1),
                         wrap = c(size, seq_len(size),1))
  
  for(t in (seq_len(timesteps) - 1)) {
    hosts_t <- filter(hosts, time %in% t)
    
    for(i in seq_len(n_hosts)){
      host_i <- filter(hosts_t, host_id %in% i)
      move_i <- filter(moves, move %in% sample(moves$move, size = 1))
      parasite_locs <- filter(coords, value %in% 1) %>% pull(id)
      host_i <- host_i %>%
        mutate(
          x = x + move_i$dx,
          y = y + move_i$dy,
          x = torus_helper %>% filter(raw %in% x) %>% pull(wrap),
          y = torus_helper %>% filter(raw %in% y) %>% pull(wrap),
          id = str_c(x, y, sep = "_"),
          time = time + 1
        )
      if(host_i$id %in% parasite_locs){
        host_i <- host_i %>%
          mutate(parasites = parasites + rbinom(1, 1, 0.5))
      }
      hosts <- bind_rows(hosts, host_i)
    }
  }
  
  end_hosts <- filter(hosts, time %in% timesteps)
  end_parasites <- pull(end_hosts, parasites)
  
  sim_data <- list(hosts = hosts,
                   stats = cbind(landscape$ls_stats,
                                 tibble(
                                   mean = mean(end_parasites),
                                   variance = var(end_parasites),
                                   dispersion = variance / mean
                   )))
  
  return(sim_data)
}

## Generate a bunch of landscapes

cluster_values <- 0:10

landscape_list <- map(cluster_values, 
                      ~landscape_percolator(size = 12, potential_prop = 0.25, cluster = .x))

## simulate movement on those landscapes

test_ls <- landscape_list[[1]]

test_move <- move_sim(landscape = test_ls, n_hosts = 24, timesteps = 10)

cbind(test_ls$ls_stats, test_move$stats)

sim_list <- map(landscape_list, ~move_sim(landscape = .x, n_hosts = 24, timesteps = 100))

sim_stats <- map_df(sim_list, function(x){return(x$stats)})


