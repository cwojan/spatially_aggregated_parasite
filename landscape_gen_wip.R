library(purrr)

landscape_1d <- rep(0, 100)

parasite_locs <- sample(x = 100, size = 40)

landscape_1d[parasite_locs] <- 1

landscape_2d <- matrix(landscape_1d, nrow = 10, ncol = 10)


landscape <- matrix(0, nrow = 10, ncol = 10)
coords <- matrix(c(rep(1:10, 10), rep(1:10, each = 10)), nrow = 100, ncol = 2)
parasite_coords <- coords[sample(100, size = 40),] 
landscape[parasite_coords] <- 1
landscape

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

for(i in 1:n_barren){
  i_corner <- barren_coords[sample(nrow(barren_coords), size = 1),]
  i_zone <- matrix(c(
    rep(i_corner[1]:(i_corner[1]+2), 3),
    rep(i_corner[2]:(i_corner[2]+2), each = 3)
  ),
  nrow = 9)
  coords <- coords[!(coords[,1] %in% i_zone[,1] & coords[,2] %in% i_zone[,2]),]
}


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








