##
#' file: parasite_sim_functions
#' description: a set of function to simulate accumulation of environmental 
#' parasite burdens
#' author: chris wojan
#' date created: 2022/02/15
##

##
#' update 20250314-16
#' added alternate version of the movement and sim functions for more replication
#' also adjusted package lodaing
##

##
#' update 20230219
#' host movement and parasite sim functions fully vectorized
#' diagonal movement no longer possible
#' can account for individual variation in movement tendency
#' updated functions sourced from "sdp_individual_move_variation_nb.rmd"
#' Note: had to add ungroup to move function 20230220 
##

##
#' update 20230222
#' modified ls function to be more efficient with props > 0.5
##

##
#' update 20230227
#' moved dist matrix initialization outside of for loop
##

##
#' update 20230327
#' revised parasite gain to happen based on previous timestep
#' added ability to specify if parasite gain can happen after moving, staying, or both
##

##
#' update 20230328
#' sim function now saves p_gain type
##

##
#' update 20230412
#' added recovery into simulation functions
#' added gc()
##

## load libraries to use
library(dplyr) # general data manipulation
library(stringr) # for string manipulation
library(purrr) # for mapping functions
library(tidyr) # for data manipulation
library(tibble) # for tibbles instead of dataframes
library(som.nn) # for calculating distance matrix on torus (som.nn::dist.torus)
library(ape) # for calculating Moran's I (ape::Moran.I)

#####
# Basic Fast Landscape Percolator Function 
##

#' Function inputs:
#' size, as one dimension (e.g. 12 -> 12 x 12 grid)
#' prop, proportion of landscape filled
#' cluster, degree to which percolation is spatially correlated
#' Requires:
#' tidyverse
#' som.nn
#' ape

fast_ls_perc <- function(size, prop, cluster){
  ## Calculate number of cells to fill based on size and prop
  if(prop > 0.5){
    potential <- floor((size^2) * (1-prop))
    fill <- 0
  } else {
    potential <- floor((size^2) * prop)
    fill <- 1
  }
  ## Create empty data representing the landscape
  coords <- tibble(x = rep(1:size, size), 
                   y = rep(1:size, each = size),
                   id = str_c(x, y, sep = "_"),
                   weight = rep(1, size^2), # Weight starts equal for all cells
                   value = rep(1-fill, size^2)) # Cell type starts out as 0 for all
  
  ## Make an inverse distance matrix of all points
  inv_dists <- 1/as.matrix(dist(cbind(coords$x, coords$y)))
  ## Remove Inf values
  diag(inv_dists) <- 0
  
  ## Initialize the possible percolation locations
  possibles <- coords$id
  ## Percolate as many cells as need to be filled
  for(p in seq_len(potential)){
    
    ## Pick a random cell (weighted by the "weight" column) to set as landscape value "1"
    point <- sample(possibles, size = 1, prob = coords[coords$id %in% possibles,]$weight)
    coords[coords$id %in% point, "value"] <- fill
    
    ## Record current ones (filled cells)
    ones <- coords$value == fill
    
    ## Select only the columns of points with 1 as their value
    inv_dists_filt <- as.matrix(inv_dists[,ones])
    
    ## Assign the weights as each point's nearest distance to a 1 
    coords$weight <- map_dbl(1:nrow(inv_dists_filt), 
                             function(x){
                               (1 + max(inv_dists_filt[x,]))^cluster
                             })
    
    ## Refresh the possible cells for percolation as only the remaining zeroes
    possibles <- coords %>%
      filter(value == 1-fill) %>%
      #slice_max(weight, n = floor((length(coords$weight) * cluster))) %>%
      pull(id)
    
  }
  
  ## Create a distance matrix of each point assuming a torus shape
  ls_dists <- as.matrix(som.nn::dist.torus(cbind(coords$x, coords$y)))
  
  ## Invert the distance matrix
  ls_dists_inv <- 1/ls_dists
  diag(ls_dists_inv) <- 0
  
  ## Calculate Moran's I spatial autocorrelation from the inverse distance matrix
  moran_result <- Moran.I(coords$value, ls_dists_inv)
  
  ## Save the statistical output from the Moran test
  ls_stats <- tibble(size = size,
                     prop = prop,
                     cluster = cluster, 
                     moran = moran_result$observed, 
                     p = moran_result$p.value)
  
  ## Save a list of the coordinates data frame, a landscape matrix, and the Moran stats
  landscape <- list(coords = coords, 
                    matrix = matrix(coords$value, nrow = size, ncol = size),
                    ls_stats = ls_stats)
  
  ## Return that landscape list
  return(landscape)
}


#####
# Host Movement Function
##

#' Function inputs:
#' hosts, data frame of host info (x, y, id, inactivity)
#' coords, coords of the landscape
#' n_moves, number of timesteps
#' n_reps, number of replicates
#' p_gain, when parasites are acquired: "move", "stay", or "both"
#' infec_prob, probability of gaining a parasite when occupying a parasite cell
#' recov_prob, probability of losing a parasite any given timestep
#' Requires:
#' tidyverse

move_host_fast <- function(hosts, coords, n_moves, n_reps, p_gain = "both",
                           infec_prob = 0.5, recov_prob = 0){
  
  ## save number of hosts
  n_hosts <- nrow(hosts) / n_reps
  
  ## grab landscape size for torus wrap
  max_x <- max(coords$x)
  max_y <- max(coords$y)
  
  ## set up moves to sample from (stay, move left, right, up, or down)
  moveset <- tibble(
    move_id = 1:5,
    xmove = c(0, 0, 0, -1, 1),
    ymove = c(0, -1, 1, 0, 0)
  )
  
  ## move hosts
  hosts_moving <- hosts[rep(1:nrow(hosts), each = n_moves + 1),] %>%
    mutate(time = rep(0:n_moves, n_hosts * n_reps),
           active = inactivity * runif(n()) >= inactivity^2,
           move_id = sample(2:5, size = n(), replace = TRUE)^active) %>% ## create moves
    left_join(moveset, by = "move_id") %>%
    mutate(xmove = if_else(time == 0, x, as.integer(xmove)),
           ymove = if_else(time == 0, y, as.integer(ymove)),
           last = pmax(abs(xmove), abs(ymove)),
           last = if_else(last > 1, as.integer(NA), last)) %>% ## save last move
    group_by(rep_id, host_id) %>%
    mutate(pre_x = cumsum(xmove),
           pre_y = cumsum(ymove),
           oob_x = (pre_x - 1) %/% max_x, ## calc out of bounds
           oob_y = (pre_y - 1) %/% max_y,
           x = pre_x - (max_x * oob_x), ## wrap around torus
           y = pre_y - (max_y * oob_y),
           id = str_c(x, y, sep = "_")
    ) %>%
    left_join(select(coords, id, value), by = "id") ## join with parasite data
  
  ## add parasites based on scenario
  if (p_gain == "both") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * rbinom(n = n(), size = 1, prob = infec_prob)))
  } else if (p_gain == "move") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * last * rbinom(n = n(), size = 1, prob = infec_prob)))
  } else if (p_gain == "stay") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * as.numeric(!last) * rbinom(n = n(), size = 1, prob = infec_prob)))
  }
  
  ## calc cumulative infection and parasite durations
  infected <- infected %>%
    mutate(cum_burden = cumsum(parasite_gain),
           durations = rnbinom(n = n(), size = 1, prob = recov_prob),
           lose_points = time + durations)
  
  ## calculate when parasites are lost
  losses <- infected %>%
    filter(parasite_gain == 1) %>%
    group_by(rep_id, host_id, lose_points) %>%
    summarise(losses = n()) %>%
    rename(time = lose_points)
  
  ## calculate current burdens
  recovered <- infected %>%
    left_join(losses, by = c("rep_id", "host_id", "time")) %>%
    mutate(losses = replace_na(losses, 0),
           cum_losses = cumsum(losses),
           cur_burden = cum_burden - cum_losses)
  
  gc()
  return(ungroup(recovered))
}

## version that only keeps the last timestep
move_host2 <- function(hosts, coords, n_moves, n_reps, p_gain = "both",
                           infec_prob = 0.5, recov_prob = 0){
  
  ## save number of hosts
  n_hosts <- nrow(hosts) / n_reps
  
  ## grab landscape size for torus wrap
  max_x <- max(coords$x)
  max_y <- max(coords$y)
  
  ## set up moves to sample from (stay, move left, right, up, or down)
  moveset <- tibble(
    move_id = 1:5,
    xmove = c(0, 0, 0, -1, 1),
    ymove = c(0, -1, 1, 0, 0)
  )
  
  ## move hosts
  hosts_moving <- hosts[rep(1:nrow(hosts), each = n_moves + 1),] %>%
    mutate(time = rep(0:n_moves, n_hosts * n_reps),
           active = inactivity * runif(n()) >= inactivity^2,
           move_id = sample(2:5, size = n(), replace = TRUE)^active) %>% ## create moves
    left_join(moveset, by = "move_id") %>%
    mutate(xmove = if_else(time == 0, x, as.integer(xmove)),
           ymove = if_else(time == 0, y, as.integer(ymove)),
           last = pmax(abs(xmove), abs(ymove)),
           last = if_else(last > 1, as.integer(NA), last)) %>% ## save last move
    group_by(rep_id, host_id) %>%
    mutate(pre_x = cumsum(xmove),
           pre_y = cumsum(ymove),
           oob_x = (pre_x - 1) %/% max_x, ## calc out of bounds
           oob_y = (pre_y - 1) %/% max_y,
           x = pre_x - (max_x * oob_x), ## wrap around torus
           y = pre_y - (max_y * oob_y),
           id = str_c(x, y, sep = "_")
    ) %>%
    left_join(select(coords, id, value), by = "id") ## join with parasite data
  
  ## add parasites based on scenario
  if (p_gain == "both") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * rbinom(n = n(), size = 1, prob = infec_prob)))
  } else if (p_gain == "move") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * last * rbinom(n = n(), size = 1, prob = infec_prob)))
  } else if (p_gain == "stay") {
    infected <- hosts_moving %>%
      mutate(parasite_gain = if_else(time == 0, 0,
                                     value * as.numeric(!last) * rbinom(n = n(), size = 1, prob = infec_prob)))
  }
  
  ## calc cumulative infection and parasite durations
  infected <- infected %>%
    mutate(cum_burden = cumsum(parasite_gain),
           durations = rnbinom(n = n(), size = 1, prob = recov_prob),
           lose_points = time + durations)
  
  ## calculate when parasites are lost
  losses <- infected %>%
    filter(parasite_gain == 1) %>%
    group_by(rep_id, host_id, lose_points) %>%
    summarise(losses = n()) %>%
    rename(time = lose_points)
  
  ## calculate current burdens
  recovered <- infected %>%
    left_join(losses, by = c("rep_id", "host_id", "time")) %>%
    mutate(losses = replace_na(losses, 0),
           cum_losses = cumsum(losses),
           cur_burden = cum_burden - cum_losses) %>%
    filter(time == n_moves)
    
  gc()
  return(ungroup(recovered))
}

#####
# Environmental Parasite Burden Simulator Function
##

#' Function inputs:
#' landscape, a list including the coordinates and Moran output
#' n_hosts, how many hosts
#' n_moves, how many timesteps
#' n_reps, how many replicates per landscape
#' movetypes, 2 column tibble specifying movement tendencies and pop. proportions
#' p_gain, when parasites are acquired: "move", "stay", or "both"
#' infec_prob, probability of gaining a parasite when occupying a parasite cell
#' recov_prob, probability of losing a parasite any given timestep
#' Requires:
#' tidyverse

parasite_sim <- function(landscape, n_hosts, n_moves, n_reps = 1, 
                         movetypes = tibble(type = 0.2, prop = 1),
                         p_gain = "both", infec_prob = 0.5, recov_prob = 0){
  
  ## Set random starting positions for each host
  host_origins <- slice_sample(landscape$coords, n = n_hosts * n_reps, replace = TRUE)
  
  ## Create host data frame
  hosts <- host_origins %>%
    select(x, y, id) %>%
    mutate(host_id = seq_len(nrow(host_origins)),
           time = 0,
           rep_id = rep(seq_len(n_reps), each = n_hosts),
           inactivity = rep(rep(movetypes$type, movetypes$prop * n_hosts), n_reps))
  
  ## Simulate movement and parasite gain
  hosts_moved <- move_host_fast(hosts = hosts, coords = landscape$coords, 
                                n_moves = n_moves, n_reps = n_reps, p_gain = p_gain,
                                infec_prob = infec_prob, recov_prob = recov_prob)
  
  ## Package and return host and landscape info
  out <- bind_cols(hosts_moved, landscape$ls_stats) %>%
    mutate(scenario = p_gain,
           infec_prob = infec_prob,
           recov_prob = recov_prob)
  gc()
  return(out)
}

## version that only keeps the last timestep
parasite_sim2 <- function(landscape, n_hosts, n_moves, n_reps = 1, 
                         movetypes = tibble(type = 0.2, prop = 1),
                         p_gain = "both", infec_prob = 0.5, recov_prob = 0){
  
  ## Set random starting positions for each host
  host_origins <- slice_sample(landscape$coords, n = n_hosts * n_reps, replace = TRUE)
  
  ## Create host data frame
  hosts <- host_origins %>%
    select(x, y, id) %>%
    mutate(host_id = seq_len(nrow(host_origins)),
           time = 0,
           rep_id = rep(seq_len(n_reps), each = n_hosts),
           inactivity = rep(rep(movetypes$type, movetypes$prop * n_hosts), n_reps))
  
  ## Simulate movement and parasite gain
  hosts_moved <- move_host2(hosts = hosts, coords = landscape$coords, 
                                n_moves = n_moves, n_reps = n_reps, p_gain = p_gain,
                                infec_prob = infec_prob, recov_prob = recov_prob)
  
  ## Package and return host and landscape info
  out <- bind_cols(hosts_moved, landscape$ls_stats) %>%
    mutate(scenario = p_gain,
           infec_prob = infec_prob,
           recov_prob = recov_prob)
  gc()
  return(out)
}
