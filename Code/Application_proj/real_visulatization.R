library(largeList)
library(tidyverse)

# read in results from local file
results <- readRDS(file = "~/F/data_local/Projekt_AMEIUS_Daten/20kRun/stretch_sample_test.RDS")
curves <- readList(file = "~/F/data_local/Projekt_AMEIUS_Daten/20kRun/test_data.llo")
ids <- readRDS(file = "~/F/data_local/Projekt_AMEIUS_Daten/20kRun/test_ids.RDS")

# transform curves into tibble for plotting
plot_tibble <- tibble(
  ids = unlist(map(
    .x = 1:length(curves),
    .f = function(i) rep(x = ids[i], times = length(curves[[i]]$args))
  )),
  args = unlist(map(
    .x = curves,
    .f = function(c) c$args
  )),
  args_zeroed = unlist(map(
    .x = curves,
    .f = function(c) c$args - c$args[1]
  )),
  vals = unlist(map(
    .x = curves,
    .f = function(c) c$vals
  )),
  n_samples = unlist(map(
    .x = 1:length(curves),
    .f = function(i) rep(x = results$num_samples[i], times = length(curves[[i]]$args))
  )),
  n_outliers = unlist(map(
    .x = 1:length(curves),
    .f = function(i) rep(x = results$num_outliers[i], times = length(curves[[i]]$args))
  )),
  certainties = unlist(map(
    .x = 1:length(curves),
    .f = function(i) rep(x = results$certainties[i], times = length(curves[[i]]$args))
  ))
) %>% 
  distinct(ids, args_zeroed, .keep_all = TRUE)

# create plot
real_plot <- ggplot(data = plot_tibble) +
  geom_line(aes(x = args_zeroed, y = vals, col = certainties, group = ids), alpha = 0.2) +
  scale_color_gradient(low = "#0062ff", high = "#ff0000") +
  theme_light() +
  theme(legend.position="bottom")

ggsave(filename = 'real_data_plot.png', plot = real_plot, path = '../../material/')