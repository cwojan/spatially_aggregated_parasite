####
## project: parasite_burden_modeling
## file: pb_exploratory_sims.R
## function: explore simulating environmental parasite transmission in different ways
## author: chris wojan
####

## Load libraries

## 
# Detachment Rate ABM
##

detach_rate_abm <- function(timesteps = 1000, hosts = 100,
                            move = 2, metroid = 0.3, detach = 0.2) {
  ## Initialize host burdens
  burdens <- rep(0, hosts)
  ## Loop for t time steps
  for (t in 1:timesteps) {
    ## Loop through i hosts
    for (i in 1:hosts) {
      ## If host i has metroids attached, some detach
      if (burdens[i] > 0) {
        burdens[i] <- burdens[i] - rbinom(1, burdens[i], detach)
      }
      ## Host i gains more metroids
      burdens[i] <- burdens[i] + rbinom(1, move, metroid)
    }
  }
  return(burdens)
}

##
# Attachment Length ABM
##

attach_length_abm <- function(timesteps = 1000, hosts = 100,
                              move = 2, metroid = 0.3,
                              attach_length = 5, attach_sd = 1) {
  ## Initialize empty list to track metroid attachment periods
  host_metroids <- vector(mode = "list", length = hosts)
  
  ## Loop for t time steps
  for (t in 1:timesteps) {
    ## Loop through i hosts
    for (i in 1:hosts) {
      ## If host i has metroids...
      if (length(host_metroids[[i]]) > 0) {
        ## Subtract one timestep from each metroid's attachment length
        host_metroids[[i]] <- host_metroids[[i]] - 1
        ## Remove all metroids with 0 timesteps remaining on their attachmetn length
        host_metroids[[i]] <- host_metroids[[i]][!host_metroids[[i]] <= 0]
      }
      ## Calculate new metroids to attach
      new_metroids <- rbinom(1, move, metroid)
      ## Host i gains metroids, each with a randomly generated attachment length
      host_metroids[[i]] <- c(host_metroids[[i]], round(rnorm(new_metroids, mean = attach_length, sd = attach_sd)))
    }
  }
  ## Return the number of metroids for each host
  return(lengths(host_metroids))
}

##
# Compare ABMS
##

compare_abms <- function(runs = 10, timesteps = 1000, hosts = 100,
                         move = 2, metroid = 0.3,
                         detach = 0.2, attach_sd = 1) {
  ## Create list of detachment rate abm outputs
  dburdens <- replicate(runs, detach_rate_abm(timesteps = timesteps, hosts = hosts,
                                              move = move, metroid = metroid, detach = detach), simplify = FALSE)
  ## Create list of attachment length abm outputs
  aburdens <- replicate(runs, attach_length_abm(timesteps = timesteps, hosts = hosts,
                                                move = move, metroid = metroid, 
                                                attach_length = 1/detach, attach_sd = attach_sd), simplify = FALSE)
  ## Initialize empty vector for p values from Kolmogorov-Smirnov test
  ks_ps <- NULL
  ## Compare all pairs of burden distributions from each abm with the Kolmogorov-Smirnov test
  for (x in 1:length(dburdens)) {
    for (y in 1:length(aburdens)) {
      ks_ps <- c(ks_ps, ks.test(dburdens[[x]], aburdens[[y]])$p)
    }
  }
  return(sum(ks_ps < 0.05)/length(ks_ps))
}

##
# Explore parameter space
##

## Explore SD
attach_sds <- 1:10 * 2

sd_comparisons <- sapply(attach_sds, FUN = function(x) compare_abms(attach_sd = x, detach = 0.1))

plot(sd_comparisons ~ attach_sds)

## Explore transmission rate

metroids <- 1:9/10

metroid_comparisons <- sapply(metroids, FUN = function(x) compare_abms(metroid = x))

plot(metroid_comparisons ~ metroids)

## Explore detach rate

detachs <- 1:9/10

detach_comparisons <- sapply(detachs, FUN = function(x) compare_abms(detach = x))

plot(detach_comparisons ~ detachs)

## Explore movement rate

moves <- 2:10 * 2

move_comp <- sapply(moves, FUN = function(x) compare_abms(move = x))

plot(move_comp ~ moves)


##
# Scratch code
##

hist(round(rnorm(10000, mean = 5, sd = 0.5)))

abm_comparison <- compare_abms(runs = 10)

sum(abm_comparison < 0.05)/length(abm_comparison)

runs <- 5
timesteps <- 1000
hosts <- 100
move <- 2
metroid <- 0.3
detach <- 0.2
dburdens <- replicate(runs, detach_rate_abm(timesteps = timesteps, hosts = hosts,
                   move = move, metroid = metroid, detach = detach), simplify = FALSE)
aburdens <- replicate(runs, attach_length_abm(timesteps = timesteps, hosts = hosts,
                   move = move, metroid = metroid, 
                   attach_length = attach_length, attach_sd = attach_sd), simplify = FALSE)
ks_ps <- NULL
for (x in 1:length(dburdens)) {
  for (y in 1:length(aburdens)) {
    ks_ps <- c(ks_ps, ks.test(dburdens[[x]], aburdens[[y]])$p)
  }
}

1/0.9

hist(attach_length_abm(attach_sd = 10))
hist(detach_rate_abm())

a <- attach_length_abm(attach_sd = 10, attach_length = 10)
b <- detach_rate_abm(detach = 0.1)

ks.test(a,b)

## Initialize empty list to track metroid attachment periods
host_metroids <- vector(mode = "list", length = hosts)

attach_length = 5
attach_sd = 10
## Loop for t time steps
for (t in 1:timesteps) {
  ## Loop through i hosts
  for (i in 1:hosts) {
    ## If host i has metroids...
    if (length(host_metroids[[i]]) > 0) {
      ## Subtract one timestep from each metroid's attachment length
      host_metroids[[i]] <- host_metroids[[i]] - 1
      ## Remove all metroids with 0 timesteps remiaing on their attachmetn length
      host_metroids[[i]] <- host_metroids[[i]][!host_metroids[[i]] <= 0]
    }
    ## Calculate new metroids to attach
    new_metroids <- rbinom(1, move, metroid)
    ## Host i gains metroids, each with a randomly generated attachment length
    host_metroids[[i]] <- c(host_metroids[[i]], round(rnorm(new_metroids, mean = attach_length, sd = attach_sd)))
  }
}


