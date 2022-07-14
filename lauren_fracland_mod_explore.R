####
## project: spatially decoupled parasite
## file: lauren_fracland_mod_explore
## original author: Shannon Pittman, James Forester, modified by Lauren White
## function: generate parasite suitability landscapes
####

library(raster)
library(tidyverse)

#############################################################################################
#############################################################################################

#' Create neutral landscape maps
#' 
#' Use standard methods to generate fractal maps. Binary and continuous surfaces may be produced.
#' 
#' @param k integer. The extent of the map (2^k+1)^2 pixels
#' @param h numeric. Level of aggregation in the map.
#' @param p numeric (0,1). The proportion of map in habitat=1
#' @param binary logical. If TRUE, a 0/1 categorical landscape is produced.
#' @author Shannon Pittman, James Forester, modified by Lauren White
#' @export
#' @example examples/neutral.landscape_example.R
fracland_mod <- function(k, h, p, binary = TRUE) {
  ## Function for creating neutral landscapes Shannon Pittman University of Minnesota May, 2013 k = the extent of the map (2^k+1)^2 pixels h =
  ## how clumped the map should be (ranging from ?? to ??) -- weird behavior at higher values p = proportion of map in habitat 1 binary =
  ## plotflag == if TRUE will plot a filled contour version of the matrix
  
  ## function call: testmap=land(6,1,.5,FALSE,TRUE)
  A <- 2^k + 1  # Scalar-determines length of landscape matrix
  
  #Right now, as written (1-p) represents the amount of habitat listed as "1"
  B <- matrix(0, A, A)  # Creates landscape matrix
  
  B[1, 1] <- 0
  B[1, A] <- 0
  B[A, 1] <- 0
  B[A, A] <- 0
  
  
  iter <- 1
  for (iter in 1:k) {
    scalef <- (0.5 + (1 - h)/2)^(iter)
    
    d <- 2^(k - iter)
    
    # ALL SQUARE STEPS#
    for (i in seq(d + 1, A - d, 2 * d)) {
      for (j in seq(d + 1, A - d, 2 * d)) {
        B[i, j] <- mean(c(B[i - d, j - d], B[i - d, j + d], B[i + d, j - d], B[i + d, j + d])) + scalef * rnorm(n = 1)
      }
    }
    
    # OUTSIDE DIAMOND STEP#
    for (j in seq(d + 1, A - d, 2 * d)) {
      B[1, j] <- mean(c(B[1, j - d], B[1, j + d], B[1 + d, j])) + scalef * rnorm(n = 1)
      B[A, j] <- mean(c(B[A, j - d], B[A, j + d], B[A - d, j])) + scalef * rnorm(n = 1)
    }
    
    for (i in seq(d + 1, A - d, 2 * d)) {
      B[i, 1] <- mean(c(B[i - d, 1], B[i + d, 1], B[i, 1 + d])) + scalef * rnorm(n = 1)
      B[i, A] <- mean(c(B[i - d, A], B[i + d, A], B[i, A - d])) + scalef * rnorm(n = 1)
    }
    
    # INSIDE DIAMOND STEP#
    if (2 * d + 1 <= A - 2 * d) {
      for (i in seq(d + 1, A - d, 2 * d)) {
        for (j in seq(2 * d + 1, A - 2 * d, 2 * d)) {
          B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
        }
      }
      
      for (i in seq(2 * d + 1, A - 2 * d, 2 * d)) {
        for (j in seq(d + 1, A - d, 2 * d)) {
          B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
        }
      }
    }
    
    iter <- iter + 1
  }
  
  if (binary == T) {
    R <- sort(B)
    PosR <- (1 - p) * length(R)  #larger values become habitat, designated as 1
    pval <- R[PosR]
    T1 <- which(B > pval)
    T2 <- which(B <= pval)
    B[T1] <- 1  #habitat is 1
    B[T2] <- 0
  } 
  return(B)
}

##################################################

plot(raster(fracland_mod(k = 4, h = 1, p = 0.3)))

######

k <- 4
h <- 0.5
p <- 0.3
binary = TRUE

## function call: testmap=land(6,1,.5,FALSE,TRUE)
A <- 2^k + 1  # Scalar-determines length of landscape matrix

#Right now, as written (1-p) represents the amount of habitat listed as "1"
B <- matrix(0, A, A)  # Creates landscape matrix

B[1, 1] <- 0
B[1, A] <- 0
B[A, 1] <- 0
B[A, A] <- 0


iter <- 1
for (iter in 1:k) {
  scalef <- (0.5 + (1 - h)/2)^(iter)
  
  d <- 2^(k - iter)
  
  # ALL SQUARE STEPS#
  for (i in seq(d + 1, A - d, 2 * d)) {
    for (j in seq(d + 1, A - d, 2 * d)) {
      B[i, j] <- mean(c(B[i - d, j - d], B[i - d, j + d], B[i + d, j - d], B[i + d, j + d])) + scalef * rnorm(n = 1)
    }
  }
  
  # OUTSIDE DIAMOND STEP#
  for (j in seq(d + 1, A - d, 2 * d)) {
    B[1, j] <- mean(c(B[1, j - d], B[1, j + d], B[1 + d, j])) + scalef * rnorm(n = 1)
    B[A, j] <- mean(c(B[A, j - d], B[A, j + d], B[A - d, j])) + scalef * rnorm(n = 1)
  }
  
  for (i in seq(d + 1, A - d, 2 * d)) {
    B[i, 1] <- mean(c(B[i - d, 1], B[i + d, 1], B[i, 1 + d])) + scalef * rnorm(n = 1)
    B[i, A] <- mean(c(B[i - d, A], B[i + d, A], B[i, A - d])) + scalef * rnorm(n = 1)
  }
  
  # INSIDE DIAMOND STEP#
  if (2 * d + 1 <= A - 2 * d) {
    for (i in seq(d + 1, A - d, 2 * d)) {
      for (j in seq(2 * d + 1, A - 2 * d, 2 * d)) {
        B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
      }
    }
    
    for (i in seq(2 * d + 1, A - 2 * d, 2 * d)) {
      for (j in seq(d + 1, A - d, 2 * d)) {
        B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
      }
    }
  }
  
  iter <- iter + 1
}

if (binary == T) {
  R <- sort(B)
  PosR <- (1 - p) * length(R)  #larger values become habitat, designated as 1
  pval <- R[PosR]
  T1 <- which(B > pval)
  T2 <- which(B <= pval)
  B[T1] <- 1  #habitat is 1
  B[T2] <- 0
} 

plot(raster(B))


####

h <- 0.9
B <- matrix(0, A, A) 
iter <- 1

scalef <- (h)^(iter)

scale2 <- (1 - scalef) * 0.05

d <- 2^(k - iter)

seq(d + 1, A - d, 2 * d)

# ALL SQUARE STEPS#
for (i in seq(d + 1, A - d, 2 * d)) {
  for (j in seq(d + 1, A - d, 2 * d)) {
    B[i, j] <- mean(c(B[i - d, j - d], B[i - d, j + d], B[i + d, j - d], B[i + d, j + d])) * scalef + rnorm(n = 1) * scale2
  }
}

# OUTSIDE DIAMOND STEP#
for (j in seq(d + 1, A - d, 2 * d)) {
  B[1, j] <- mean(c(B[1, j - d], B[1, j + d], B[1 + d, j])) * scalef + rnorm(n = 1) * scale2
  B[A, j] <- mean(c(B[A, j - d], B[A, j + d], B[A - d, j])) * scalef + rnorm(n = 1) * scale2
}

for (i in seq(d + 1, A - d, 2 * d)) {
  B[i, 1] <- mean(c(B[i - d, 1], B[i + d, 1], B[i, 1 + d])) * scalef + rnorm(n = 1) * scale2
  B[i, A] <- mean(c(B[i - d, A], B[i + d, A], B[i, A - d])) * scalef + rnorm(n = 1) * scale2
}

# INSIDE DIAMOND STEP#
if (2 * d + 1 <= A - 2 * d) {
  for (i in seq(d + 1, A - d, 2 * d)) {
    for (j in seq(2 * d + 1, A - 2 * d, 2 * d)) {
      B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) * scalef + rnorm(n = 1) * scale2
    }
  }
  
  for (i in seq(2 * d + 1, A - 2 * d, 2 * d)) {
    for (j in seq(d + 1, A - d, 2 * d)) {
      B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) * scalef + rnorm(n = 1) * scale2
    }
  }
}

iter <- iter + 1

plot(raster(B))


if (binary == T) {
  R <- sort(B)
  PosR <- (1 - p) * length(R)  #larger values become habitat, designated as 1
  pval <- R[PosR]
  T1 <- which(B > pval)
  T2 <- which(B <= pval)
  B[T1] <- 1  #habitat is 1
  B[T2] <- 0
} 

plot(raster(B))


mean(c(0.5, 0.4, 0.6)) * 0.75 + rnorm(1) * 0.25

data <- tibble(mean_scale = seq(0, 1, 0.1),
               rand_scale = 1 - mean_scale,
               mean = 0.5,
               rand = rnorm(11) * 0.05,
               new = mean * mean_scale + rand * rand_scale,
               diff = abs(new - mean))

ggplot(data = data, aes(x = mean_scale, y = diff)) +
  geom_point()



data <- tibble(h = seq(-2, 2, 0.1),
               scalef = 1-h)
ggplot(aes(x = h, y = scalef), data = data) +
  geom_point()
