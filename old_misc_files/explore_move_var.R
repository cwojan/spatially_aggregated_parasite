###
## Explore Individual Variation in Movement Probability
###

## load code
library(tidyverse)
library(furrr)
library(ape)
library(som.nn)
source("parasite_sim_functions.R")

## load ls data
ls_list <- read_rds("sim_output/ls_list_20240719_091018.rds")

## filter ls data to only include landscapes with prop values of 0.3, 0.5, and 0.7 and cluster values of 0, 5, 10 and 15

## create a key to the list of landscapes, so each landscape has an ID
ls_key <- map_df(ls_list, function(x){x$ls_stats}, .id = "element_id")

## grab 9 landscapes across different parasite densities and clustering factors
ls_sub_ids <- ls_key %>%
  filter(prop %in% c(0.3, 0.5, 0.7), cluster %in% c(0, 5, 10, 15)) %>%
  mutate(element_id = as.numeric(element_id)) %>%
  pull(element_id)

## subset list for those landscapes
ls_subset <- ls_list[ls_sub_ids]
## replicate for different movement scenarios
ls_rep <- rep(ls_subset, each = 3)

## create a list of tibbles for different movement scenarios
move_list <- list(tibble(type = c(0.1, 0.7), prop = c(0.5, 0.5)), 
                  tibble(type = c(0.5, 0.7), prop = c(0.5, 0.5)), 
                  tibble(type = c(0.3, 0.5), prop = c(0.5, 0.5)))
## replicate move_list for simulation
move_list <- rep(move_list, length(ls_subset))

## run simulations for those movement probabilities
# set multisession plan
plan(multisession)
# run function in parallel
move_sims <- future_map2(
  .x = ls_rep,
  .y = move_list,
  ~parasite_sim(landscape = .x, n_hosts = 96, n_moves = 100,
                n_reps = 10, movetypes = .y, recov_prob = 0.15),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)
plan(sequential)

## Filter to only the data from the end of the simulations
move_sim_data <- map_df(
  move_sims,
  function(x){
    filter(x, time == 100)
  }
)

## add helpful labels to sim data
move_sim_data <- move_sim_data %>%
  mutate(sim_id = str_c(prop, cluster, rep_id, sep = "_")) %>%
  mutate(sex = rep(rep(c("Male","Female"), each = 48), 360),
         move_scenario = rep(
           rep(c("Male: 0.9, Female: 0.3", "Male: 0.5, Female: 0.3", "Male: 0.7, Female: 0.5"), each = 960),
                             12))

## summarize mean and variance of current burdens by sex and simulation
move_sim_summary <- move_sim_data %>%
  group_by(rep_id, prop, cluster, moran, move_scenario, sex) %>%
  summarise(mean_cur_burden = mean(cur_burden),
            var_cur_burden = var(cur_burden),
            disp_cur_burden = var_cur_burden / mean_cur_burden)

## plot the mean current burden by moran, prop, move_scenario and sex
ggplot(data = move_sim_summary, aes(x = moran, y = mean_cur_burden, color = as.factor(prop)), alpha = sex) +
  geom_point(aes(shape = sex)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = sex)) +
  facet_wrap(vars(move_scenario)) +
  scale_color_viridis_d(name = "Parasite Density \n(Proportion Cells Occupied)") +
  scale_shape_discrete(name = "Sex") +
  scale_linetype_discrete(name = "Sex") +
  scale_alpha_manual(name = "Sex", values = c(0.5,1)) +
  labs(y = "Mean Ending Parasite Burden") +
  scale_x_continuous(name = "Parasite Spatial Aggregation (Moran's I)",
                     breaks = c(0, 0.06, 0.12, 0.18),
                     sec.axis = sec_axis(~ . , name = "Movement Scenario", 
                                         breaks = NULL, labels = NULL)) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data = move_sim_summary, aes(x = moran, y = var_cur_burden, color = as.factor(prop)), alpha = sex) +
  geom_jitter(aes(shape = sex), width = 0.005, height = 0) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = sex)) +
  facet_wrap(vars(move_scenario)) +
  scale_color_viridis_d(name = "Parasite Density \n(Proportion Cells Occupied)") +
  scale_shape_discrete(name = "Sex") +
  scale_linetype_discrete(name = "Sex") +
  scale_alpha_manual(name = "Sex", values = c(0.5,1)) +
  labs(y = "Variance of Ending Parasite Burden") +
  scale_x_continuous(name = "Parasite Spatial Aggregation (Moran's I)",
                     breaks = c(0, 0.06, 0.12, 0.18),
                     sec.axis = sec_axis(~ . , name = "Movement Scenario", 
                                         breaks = NULL, labels = NULL)) +
  theme_bw() +
  theme(legend.position = "bottom")

lm_test <- glm(var_cur_burden ~ prop + sex*moran, data = move_sim_summary)
summary(lm_test)
