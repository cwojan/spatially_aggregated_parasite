####
## project: spatially_decoupled_parasite
## file: landscape_gen_wip.R
## function: scratch code brainstorming how to generate parasite landscapes
## author: chris wojan
####



library(purrr)
library(landscapeR)
library(raster)
library(tidyverse)
library(ape)

### modified percolation idea

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

walk(seq(0,10,1), function(x){
  plot(raster(landscape_percolator(size = 12, potential_prop = 0.25, cluster = x)))})


plot(raster(landscape_percolator(size = 12, potential_prop = 0.25, cluster = 7)))
sum(landscape_percolator(12, 0.5, 1))

## test moran's I

landscape <- landscape_percolator(size = 12, potential_prop = 0.25, cluster = 4)

plot(raster(landscape$matrix))

ls_coords <- landscape$coords

ls_dists <- as.matrix(dist(cbind(ls_coords$x, ls_coords$y)))

ls_dists_inv <- 1/ls_dists
diag(ls_dists_inv) <- 0

Moran.I(ls_coords$value, ls_dists_inv)

moran_tester <- function(landscape){
  ls_coords <- landscape$coords
  
  ls_dists <- as.matrix(dist(cbind(ls_coords$x, ls_coords$y)))
  
  ls_dists_inv <- 1/ls_dists
  diag(ls_dists_inv) <- 0
  
  result <- Moran.I(ls_coords$value, ls_dists_inv)
  return(result)
}

analyze_clustering <- function(size, potential_prop, cluster){
  landscape <- landscape_percolator(size = size, 
                                    potential_prop = potential_prop, 
                                    cluster = cluster)
  moran_result <- moran_tester(landscape)
  
  result <- tibble(cluster = cluster, 
                   moran = moran_result$observed, 
                   p = moran_result$p.value)
  
  return(result)
}

test_ac <- analyze_clustering(size = 12, potential_prop = 0.25, cluster = 3)

test_values <- rep(0:10, each = 10)

test_map_ac <- map_df(.x = test_values, .f = function(x){
  analyze_clustering(size = 12, potential_prop = 0.25, cluster = x)})

ggplot(data = test_map_ac) +
  geom_point(aes(x = cluster, y = moran)) +
  geom_smooth(aes(x = cluster, y = moran), method = "lm") +
  theme_bw()

moran_cluster_lm <- lm(moran ~ cluster, data = test_map_ac)

summary(moran_cluster_lm)

plot(sort(test_map_ac$moran))

## set grid size
size <- 12
potential_prop <- 0.25
cluster <- 1

## Create landscape info tibble

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

sum(coords$value)

landscape <- matrix(coords$value, nrow = size, ncol = size)

plot(raster(landscape))

possibles <- coords$id
point <- sample(possibles, size = 1, prob = coords[coords$id %in% possibles,]$weight)
coords[coords$id %in% point, "value"] <- 1

## Update weights
ones <- coords[coords$value == 1,]$id
zeroes <- coords[coords$value == 0,]$id
for(i in zeroes){
  coords_i <- coords[coords$id %in% i, c("x", "y")]
  dists_i <- 0
  for(j in ones){
    coords_j <- coords[coords$id %in% j, c("x", "y")]
    dist_ij <- sqrt((coords_j$x - coords_i$x)^2 + (coords_j$y - coords_i$y)^2)
    dists_i <- dists_i + dist_ij
  }
  inv_dist <- 1/dists_i
  coords[coords$id %in% i, "weight"] <- (1/(size^2)) + inv_dist
}

possibles <- coords %>%
  filter(value == 0) %>%
  slice_max(weight, n = floor((length(coords$weight) * cluster))) %>%
  pull(id)


cluster <- 0.1
order(coords$weight)[1:floor((length(coords$weight) * cluster))]


### while loop clustering

generate_landscape <- function(size = 24, c_factor = 1, potential_prop = 0.25){
  potential <- (size^2) * potential_prop
  landscape <- matrix(0, nrow = size, ncol = size)
  coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), 
                   nrow = size^2, ncol = 2)
  
  c_size <- size - (c_factor - 1)
  start_coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                         nrow = c_size^2, ncol = 2)
  
  rownames(coords) <- str_c("cell", coords[,1], coords[,2], sep = "_")
  rownames(start_coords) <- str_c("cell", start_coords[,1], start_coords[,2], sep = "_")
  
  counter <- 0
  
  while(sum(landscape) < potential){
    start <- start_coords[sample(nrow(start_coords), size = 1),]
    block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                      rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                    nrow = c_factor^2, ncol = 2)
    rownames(block) <- str_c("cell", block[,1], block[,2], sep = "_")
    if(all(rownames(block) %in% rownames(coords))){
      alt_block <- matrix(block[order(block[,1]),], ncol = 2)
      rownames(alt_block) <- str_c("cell", alt_block[,1], alt_block[,2], sep = "_")
      block <- list(block, alt_block)[[sample(2,1)]]
      iterator <- list(1:nrow(block), nrow(block):1)[[sample(2,1)]]
      for(i in iterator){
        landscape[matrix(block[i,], ncol = 2)] <- 1
        if(sum(landscape) >= potential){
          break
        }
      }
      start_coords <- start_coords[!rownames(start_coords) %in% rownames(block),]
      coords <- coords[!rownames(coords) %in% rownames(block),]
    } else {
      counter <- counter + 1
    }
    if(counter > 100){
      coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), 
                       nrow = size^2, ncol = 2)
      start_coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                             nrow = c_size^2, ncol = 2)
      rownames(coords) <- str_c("cell", coords[,1], coords[,2], sep = "_")
      rownames(start_coords) <- str_c("cell", start_coords[,1], start_coords[,2], sep = "_")
      counter <- 0
      landscape[] <- 0
    }
  }
  
  return(landscape)
}

plot(raster(generate_landscape(24, 1, 0.25)))

walk(c(1,2,3,4,6,12), function(x){
  plot(raster(generate_landscape(c_factor = x, potential_prop = 0.25)))})

walk(c(1,2,3,4,6,12), function(x){
  plot(raster(generate_landscape(c_factor = x, potential_prop = 0.5)))})


144 / 2

generate_landscape(24, 1, 0.5)

generate_landscape(24, 12, 0.5)



c_factor <- 4
size <- 24
potential <- (size^2) * 0.25
landscape <- matrix(0, nrow = size, ncol = size)
c_size <- size - (c_factor - 1)

coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), 
                 nrow = size^2, ncol = 2)
start_coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                       nrow = c_size^2, ncol = 2)

rownames(coords) <- str_c("cell", coords[,1], coords[,2], sep = "_")
rownames(start_coords) <- str_c("cell", start_coords[,1], start_coords[,2], sep = "_")

counter <- 0

while(sum(landscape) < potential){
  start <- start_coords[sample(nrow(start_coords), size = 1),]
  block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                    rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                  nrow = c_factor^2, ncol = 2)
  rownames(block) <- str_c("cell", block[,1], block[,2], sep = "_")
  if(all(rownames(block) %in% rownames(coords))){
    alt_block <- matrix(block[order(block[,1]),], ncol = 2)
    rownames(alt_block) <- str_c("cell", alt_block[,1], alt_block[,2], sep = "_")
    block <- list(block, alt_block)[[sample(2,1)]]
    iterator <- list(1:nrow(block), nrow(block):1)[[sample(2,1)]]
    for(i in iterator){
      landscape[matrix(block[i,], ncol = 2)] <- 1
      if(sum(landscape) >= potential){
        break
      }
    }
    start_coords <- start_coords[!rownames(start_coords) %in% rownames(block),]
    coords <- coords[!rownames(coords) %in% rownames(block),]
  } else {
    counter <- counter + 1
  }
  if(counter > 100){
    coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), 
                     nrow = size^2, ncol = 2)
    start_coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                           nrow = c_size^2, ncol = 2)
    rownames(coords) <- str_c("cell", coords[,1], coords[,2], sep = "_")
    rownames(start_coords) <- str_c("cell", start_coords[,1], start_coords[,2], sep = "_")
    counter <- 0
    landscape[] <- 0
  }
}
plot(raster(landscape))

sum(landscape)

block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                  rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                nrow = c_factor^2, ncol = 2)

alt_block <- matrix(block[order(block[,1]),], ncol = 2)

start_coords[sample(nrow(start_coords), size = 1),]

start_coords[!rownames(start_coords) %in% rownames(block),]





any(c(1,2,3) %in% c(3,4,5))

m1 <- matrix(c(2,3,2,2,4,4), ncol = 2)
m2 <- matrix(c(2,4,2,1,5,5), ncol = 2)

paste0(m1[,1], m1[,2])
paste0(m2[,1], m2[,2])

any(paste0(m1[,1], m1[,2]) %in% paste0(m2[,1], m2[,2]))




size <- 24
c_factor <- 6
prop <- 0.25

landscape <- matrix(0, nrow = size, ncol = size)

c_size <- size - (c_factor - 1)
coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                 nrow = c_size^2, ncol = 2)

potential <- (size^2) * prop

while(sum(landscape) < potential){
  start <- coords[sample(c_size^2, size = 1),]
  block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                    rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                  nrow = c_factor^2, ncol = 2)
  alt_block <- block[order(block[,1]),]
  block <- list(block, alt_block)[[sample(2,1)]]
  iterator <- list(1:nrow(block), nrow(block):1)[[sample(2,1)]]
  for(i in iterator){
    landscape[matrix(block[i,], ncol = 2)] <- 1
    if(sum(landscape) > potential){
      break
    }
  }
}


alt_block <- block[order(block[,1]),]
plot(raster(landscape))


generate_landscape <- function(size, c_factor, prop){
  landscape <- matrix(0, nrow = size, ncol = size)
  
  c_size <- size - (c_factor - 1)
  coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                   nrow = c_size^2, ncol = 2)
  
  potential <- (size^2) * prop
  
  while(sum(landscape) < potential){
    start <- coords[sample(c_size^2, size = 1),]
    block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                      rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                    nrow = c_factor^2, ncol = 2)
    alt_block <- block[order(block[,1]),]
    block <- list(block, alt_block)[[sample(2,1)]]
    iterator <- list(1:nrow(block), nrow(block):1)[[sample(2,1)]]
    for(i in iterator){
      landscape[matrix(block[i,], ncol = 2)] <- 1
      if(sum(landscape) == potential){
        break
      }
    }
  }
  
  return(landscape)
}

plot(raster(generate_landscape(24, 7, 0.25)))


seq_along(36)

rnorm(1)

generate_landscape <- function(size, c_factor, prop){
  landscape <- matrix(0, nrow = size, ncol = size)
  
  c_size <- size - (c_factor - 1)
  coords <- matrix(c(rep(1:c_size, c_size), rep(1:c_size, each = c_size)), 
                   nrow = c_size^2, ncol = 2)
  
  potential <- (size^2) * prop
  
  while(sum(landscape) < potential){
    start <- coords[sample(c_size^2, size = 1),]
    block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                      rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                    nrow = c_factor^2, ncol = 2)
    iterator <- sample(nrow(block), size = c_factor^2)
    for(i in iterator){
      landscape[matrix(block[i,], ncol = 2)] <- 1
      if(sum(landscape) > potential){
        break
      }
    }
  }
  
  return(landscape)
}

### Recursion idea...
while(sum(landscape) < potential){
  start <- coords[sample(nrow(coords), size = 1),]
  block <- matrix(c(rep(start[1]:(start[1] + (c_factor - 1)), c_factor),
                    rep(start[2]:(start[2] + (c_factor - 1)), each = c_factor)),
                  nrow = c_factor^2, ncol = 2)
  fill <- sum(landscape)
  while(fill + nrow(block) > potential){
    sub_size <- c_factor
    c_factor <- c_factor - 1
    sub_landscape <- generate_landscape(size = sub_size, 
                                        c_factor = c_factor, 
                                        potential = potential - fill)
    block
  }
  for(i in 1:nrow(block)){
    landscape[matrix(block[i,], ncol = 2)] <- 1
    if(sum(landscape) == potential){
      break
    }
  }
}
####



#### free clustering
c_factor <- 6
size <- 12
landscape <- matrix(0, nrow = size, ncol = size)
coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), nrow = size^2, ncol = 2)
rows <- sample(size, size = c_factor)
cols <- sample(size, size = c_factor)
avail <- which(!(coords[,1] %in% rows | coords[,2] %in% cols))
parasite_coords <- coords[sample(avail, size = (size^2)/4),] 
landscape[parasite_coords] <- 1
landscape

plot(raster(landscape))
#####


#### single clustering
c_factor <- 4
size <- 12
landscape <- matrix(0, nrow = size, ncol = size)
coords <- matrix(c(rep(1:size, size), rep(1:size, each = size)), nrow = size^2, ncol = 2)
rows <- (size - c_factor + 1):(size + 1)
cols <- (size - c_factor + 1):(size + 1)
avail <- which(!(coords[,1] %in% rows | coords[,2] %in% cols))
parasite_coords <- coords[sample(avail, size = (size^2)/4),] 
landscape[parasite_coords] <- 1
landscape

plot(raster(landscape))
#####


### landscapeR

landscape <- matrix(0, nrow = size, ncol = size)
land_rast <- raster(landscape, xmn = 0, xmx = 12, ymn = 0, ymx = 12)
patches <- makeClass(land_rast, npatch = 5, size = 20)
plot(patches)
m_patches <- raster::as.matrix(patches)
which(m_patches == 1)
m_patches
sum(m_patches)

avail <- which(m_patches == 1)
parasite_coords <- coords[sample(avail, size = (size^2)/4),] 
landscape[parasite_coords] <- 1
landscape

plot(raster(landscape, xmn = 0, xmx = 12, ymn = 0, ymx = 12))

plot(raster(landscape))

raster::as.matrix(raster(landscape))

####

### 

clusters <- rbinom(n = 1, size = 10, prob = 0.5)
cluster_ids <- sample(clusters, size = (size^2)/4, replace = TRUE)
cluster_centers <- coords[sample(100, size = clusters),]

place_parasite <- function(cluster_id, cluster_centers, cluster_size){
  
}

clustered_fill <- function(cluster_centers, parasites){
  
}

###





### Loop through subsets of a matrix, fill parasites in those subsets



### This code from:
## https://stackoverflow.com/questions/59734686/select-non-overlapping-submatrices-by-index-in-a-data-frame
## (neglected to cite on commit f715c12116a378a73ef2e873936da81acd888eec)
x <- matrix(1:144, 12, 12)

nbyn <- 3
groups <- (slice.index(x, 1) - 1) %/% nbyn * nrow(x) / nbyn + (slice.index(x, 2) + nbyn - 1) %/% nbyn
rando_groups <- sample(unique(c(groups)), 1)

which(groups %in% rando_groups)  #Gives you the indices
x[groups %in% rando_groups]      #Gives you the values
groups[groups %in% rando_groups] #Gives you the group
groups %in% rando_groups         #Gives you a matrix of the selected regions

### end stack overflow code

landscape_1d <- rep(0, 100)

clusters <- rbinom(n = 1, size = 10, prob = 0.5)
cluster_sizes <- rbinom(n = clusters, size = 30, prob = 0.5)
avail <- 1:100
for(i in cluster_sizes){
  start <- sample(avail, size = 1)
  avail <- avail[-(start:(start + i))]
}

parasite_locs <- sample(x = avail, size = 30)

landscape_1d[parasite_locs] <- 1

landscape_2d <- matrix(landscape_1d, nrow = 10, ncol = 10)
landscape_2d


landscape <- matrix(0, nrow = 12, ncol = 12)
rbind(rep(1:6, each = 2), rep(1:6, each = 2))

12 / 2 

dead_zones <- sample(100, size = 10)

open_spots <- (1:100)[-dead_zones]

sample((1:100)[-dead_zones], size = 40)

landscape_generator <- function(parasites, dead_zones = 0){
  landscape <- matrix(0, nrow = 10, ncol = 10)
  coords <- matrix(c(rep(1:10, 10), rep(1:10, each = 10)), nrow = 100, ncol = 2)
  dead_zones <- sample(100, size = 10)
  open_spots <- (1:100)[-dead_zones]
  parasite_coords <- coords[sample(open_spots, size = parasites),] 
  landscape[parasite_coords] <- 1
  return(landscape)
}

landscape_generator(parasites = 40)


clusters <- rbinom(n = 1, size = 10, prob = 0.5)
cluster_ids <- sample(clusters, size = nrow(parasite_coords), replace = TRUE)
cluster_centers <- coords[sample(100, size = clusters),]

dim <- 10

landscape <- matrix(0, nrow = dim, ncol = dim)
coords <- matrix(c(rep(1:dim, dim), rep(1:dim, each = dim)), nrow = dim^2, ncol = 2)

n_barren <- rbinom(n = 1, size = 5, prob = 0.5)
barren_size <- 2
barren_coords <- matrix(c(rep(1:(dim - barren_size), dim - barren_size), 
                          rep(1:(dim - barren_size), each = dim - barren_size)), 
                        nrow = (dim - barren_size)^2, ncol = 2)
barren_corners <- barren_coords[sample(nrow(barren_coords), size = n_barren),]

barren_coords <- matrix(coords[coords < 9], ncol = 2)


landscape <- matrix(0, nrow = dim, ncol = dim)
coords <- matrix(c(rep(1:dim, dim), rep(1:dim, each = dim)), nrow = dim^2, ncol = 2)
for(i in 1:2){
  barren_coords <- matrix(coords[coords < 7], ncol = 2)
  i_corner <- barren_coords[sample(nrow(barren_coords), size = 1),]
  i_zone <- matrix(c(
    rep(i_corner[1]:(i_corner[1]+4), 3),
    rep(i_corner[2]:(i_corner[2]+4), each = 3)
  ),
  nrow = 9)
  coords <- coords[!(coords[,1] %in% i_zone[,1] & coords[,2] %in% i_zone[,2]),]
}

parasite_coords <- coords[sample(nrow(coords), size = 40),] 
landscape[parasite_coords] <- 1
landscape


landscape <- matrix(1:100, nrow = dim, ncol = dim)
i_corner <- barren_coords[sample(nrow(barren_coords), size = 1),]
i_zone <- matrix(c(
  rep(i_corner[1]:(i_corner[1]+2), 3),
  rep(i_corner[2]:(i_corner[2]+2), each = 3)
  ),
  nrow = 9)
coords[!(coords[,1] %in% i_zone[,1] & coords[,2] %in% i_zone[,2]),]


coords



parasite_coords <- coords[sample(dim^2, size = 40),] 
landscape[parasite_coords] <- 1
landscape

n_barren <- rbinom(n = 1, size = 5, prob = 0.5)
barren_size <- 2








