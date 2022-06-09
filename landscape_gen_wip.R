library(purrr)
library(landscapeR)
library(raster)

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
x <- matrix(1:144, 12, 12)

nbyn <- 3
groups <- (slice.index(x, 1) - 1) %/% nbyn * nrow(x) / nbyn + (slice.index(x, 2) + nbyn - 1) %/% nbyn
rando_groups <- sample(unique(c(groups)), 1)

which(groups %in% rando_groups)  #Gives you the indices
x[groups %in% rando_groups]      #Gives you the values
groups[groups %in% rando_groups] #Gives you the group
groups %in% rando_groups         #Gives you a matrix of the selected regions

###

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








