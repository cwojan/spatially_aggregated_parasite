####
## project: spatially_decoupled_parasite
## file: landscape_percolator_functions.R
## function: functions to generate landscapes via spatially correlated percolation
## author: chris wojan
####


## Load Libraries
library(tidyverse)
library(raster)

##
# Basic Landscape Percolator Function 
##

#' Function inputs:
#' size, as one dimension (e.g. 12 -> 12 x 12 grid)
#' prop, proportion of landscape filled
#' cluster, degree to which percolation is spatially correlated

landscape_percolator <- function(size, prop, cluster){
  ## Calculate number of cells to fill based on size and prop
  potential <- floor((size^2) * prop)
  ## Create empty data representing the landscape
  coords <- tibble(x = rep(1:size, size), 
                   y = rep(1:size, each = size),
                   id = str_c(x, y, sep = "_"),
                   weight = rep(1, size^2), # Weight starts equal for all cells
                   value = rep(0, size^2)) # Cell type starts out as 0 for all
  
  ## Initialize the possible percolation locations
  possibles <- coords$id
  ## Percolate as many cells as need to be filled
  for(p in seq_len(potential)){
    
    ## Pick a random cell (weighted by the "weight" column) to set as landscape value "1"
    point <- sample(possibles, size = 1, prob = coords[coords$id %in% possibles,]$weight)
    coords[coords$id %in% point, "value"] <- 1
    
    ## Create vectors of all cells with value 1 and 0
    ones <- coords[coords$value == 1,]$id
    zeroes <- coords[coords$value == 0,]$id
    ## Loop through each cell with value 0
    for(i in zeroes){
      ## Save its cordinates as a vector of length 2
      coords_i <- coords[coords$id %in% i, c("x", "y")]
      ## Initialize its distances to cells with value 1
      dists_i <- NULL
      ## Loop through each cell with value 1
      for(j in ones){
        ## Save the jth 1 cell's coordinates as a vector of length 2
        coords_j <- coords[coords$id %in% j, c("x", "y")]
        ## Calculate the euclidean distance between the ith 0 cell and the jth 1 cell
        dist_ij <- sqrt((coords_j$x - coords_i$x)^2 + (coords_j$y - coords_i$y)^2)
        ## Add the ij distance to the list of distances for the ith 0 cell
        dists_i <- c(dists_i, dist_ij)
      }
      ## Create an object that represents the inverse minimum distance between the ith 0 cell and a 1 cell
      inv_dist <- 1/min(dists_i)
      ## Update the weight for the ith 0 cell, accounting for the cluster factor as an exponent
      coords[coords$id %in% i, "weight"] <- (1 + inv_dist)^cluster
    }
    
    ## Refill the possible cells for percolation as only the remaining zeroes
    possibles <- coords %>%
      filter(value == 0) %>%
      #slice_max(weight, n = floor((length(coords$weight) * cluster))) %>%
      pull(id)
    
  }
  
  ## Save the landscape as the percolated data as a data frame AND as an actual landscape matrix
  landscape <- list(coords = coords, 
                    matrix = matrix(coords$value, nrow = size, ncol = size))
  
  ## Return that landscape list
  return(landscape)
}



##
# Modified Landscape Percolator Function 
##

#' Function inputs:
#' width, width of grid
#' length, length of grid
#' prop, proportion of landscape filled
#' c_factor, degree to which percolation is spatially correlated
#' c_num, number of clusters

mod_ls_percolator <- function(width, length, prop, c_factor, c_num){
  dims <- width * length
  ## Calculate number of cells per cluster to fill based on size and prop
  potential <- floor((dims * prop) / c_num)
  ## Create empty data representing the landscape
  coords <- tibble(x = rep(1:width, length), 
                   y = rep(1:length, each = width),
                   id = str_c(x, y, sep = "_"),
                   weight = rep(1, dims), # Weight starts equal for all cells
                   value = rep(0, dims), # Cell type starts out as 0 for all
                   cluster = rep(0, dims)) # Cluster group starts as 0
  
  ## Initialize the possible percolation locations
  possibles <- coords$id
  
  ##
  for(n in seq_len(c_num)){
    
    ## Set weights for cluster n 
    coords$weight <- 1
    
    ## Percolate as many cells as need to be filled
    for(p in seq_len(potential)){
      
      ## Pick a random cell (weighted by the "weight" column) to set as landscape value "1"
      point <- sample(possibles, size = 1, prob = coords[coords$id %in% possibles,]$weight)
      coords[coords$id %in% point, "value"] <- 1
      coords[coords$id %in% point, "cluster"] <- n
      
      ## Create vectors of all cells with value 1 in current cluster
      ones <- coords[coords$value == 1 & coords$cluster == n,]$id
      zeroes <- coords[coords$value == 0,]$id
      ## Loop through each cell with value 0
      for(i in zeroes){
        ## Save its cordinates as a vector of length 2
        coords_i <- coords[coords$id %in% i, c("x", "y")]
        ## Initialize its distances to cells with value 1
        dists_i <- NULL
        ## Loop through each cell with value 1
        for(j in ones){
          ## Save the jth 1 cell's coordinates as a vector of length 2
          coords_j <- coords[coords$id %in% j, c("x", "y")]
          ## Calculate the euclidean distance between the ith 0 cell and the jth 1 cell
          dist_ij <- sqrt((coords_j$x - coords_i$x)^2 + (coords_j$y - coords_i$y)^2)
          ## Add the ij distance to the list of distances for the ith 0 cell
          dists_i <- c(dists_i, dist_ij)
        }
        ## Create an object that represents the inverse minimum distance between the ith 0 cell and a 1 cell
        inv_dist <- 1/min(dists_i)
        ## Update the weight for the ith 0 cell, accounting for the cluster factor as an exponent
        coords[coords$id %in% i, "weight"] <- (1 + inv_dist)^c_factor
      }
      
      ## Refill the possible cells for percolation as only the remaining zeroes
      possibles <- coords %>%
        filter(value == 0) %>%
        #slice_max(weight, n = floor((length(coords$weight) * cluster))) %>%
        pull(id)
      
    }
  }
  
  ## Save the landscape as the percolated data as a data frame AND as an actual landscape matrix
  landscape <- list(coords = coords, 
                    matrix = matrix(coords$value, nrow = width, ncol = length))
  
  ## Return that landscape list
  return(landscape)
}

##
# Test functions!
##

## Basic
plot(raster(landscape_percolator(12,0.25,0)$matrix))

## Modified
plot(raster(mod_ls_percolator(10,15,0.2,100,6)$matrix))



                  
                  
          
