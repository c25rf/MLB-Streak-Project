library(tidyverse)
library(furrr)
plan(multisession(workers = 12))
source('fx.R')

### Simulate Random .300 hitter
sim_random_hitter <- function(n, p) {
  return(rbinom(n, 1, p))
}

### Baseline .300 hitter but their true p_j is 50% and 50% average of last 25
sim_streaky_hitter <- function(n, p) {
  ### seed 1st 25
  seq <- rep(NA, n + 25) 
  seq[1:25] <- rbinom(25, 1, p)
  for(i in 25 + (1:n)) {
    seq[i] <- rbinom(n = 1, size = 1, p = 0.5 * (p + mean(seq[(i-25):(i-1)])))
  }
  return(seq[-c(1:25)])
}

### Baseline .300 hitter but they have a 0.25-0.35 average in chunks of 25
sim_consistent_hitter <- function(n, p) {
  seq <- rep(NA, n) 
  chunk_size <- 25
  n_chunks <- n/chunk_size
  lower <- ifelse(p > 0.1, -0.05, -0.01)
  upper <- ifelse(p > 0.1, 0.05, 0.01)
    
  for(i in 1:n_chunks) {
    p_chunk <- p + runif(1, lower, upper)
    n_success <- round(p_chunk * chunk_size)
    seq[(1 + chunk_size * (i-1)):(chunk_size * i)] <-  sample(rep(c(0, 1), c(chunk_size - n_success, n_success)))
  }
  
  return(seq)
}


perm_test <- function(binary_sequence, statistic, n_sims) {
  ### (1) Compute observed statistic
  if(statistic == 'streak_score') {
    observed_stat <- streak_squared(binary_sequence) 
  } else if(statistic == 'spacing_score') {
    observed_stat <- spacing_gaps_squared(binary_sequence)
  }
  
  ### (2)
  sim_statistics <- rep(NA, n_sims)
  for(i in 1:n_sims) {
    ### Shuffle sequence
    sim_sequence <- sample(binary_sequence)
    
    ### Compute and save statistic on simulated streak
    if(statistic == 'streak_score') {
      sim_statistics[i] <- streak_squared(sim_sequence) 
    } else if(statistic == 'spacing_score') {
      sim_statistics[i] <- spacing_gaps_squared(sim_sequence)
    }
  }
  
  ### percentile
  p_val <- mean(sim_statistics <= observed_stat)
  return(p_val)
  
}



### Conduct Study to show that streak_score metric works as intended
set.seed(13)
n_hitters <- 1000
n_abs <- 500
p_avg <- 0.3

random_hitters <- 
  map(1:n_hitters, ~sim_random_hitter(n = n_abs, p = p_avg))
streaky_hitters <- 
  map(1:n_hitters, ~sim_streaky_hitter(n = n_abs, p = p_avg))
consistent_hitters <- 
  map(1:n_hitters, ~sim_consistent_hitter(n = n_abs, p = p_avg))
  

percentiles_random <- 
  future_map_dbl(random_hitters, ~perm_test(binary_sequence = .x,
                                            statistic = 'streak_score',
                                            n_sims = 1000),
                 .options = furrr_options(seed = 12321))
percentiles_streaky <- 
  future_map_dbl(streaky_hitters, ~perm_test(binary_sequence = .x,
                                            statistic = 'streak_score',
                                            n_sims = 1000),
                 .options = furrr_options(seed = 12321))
percentiles_consistent <- 
  future_map_dbl(consistent_hitters, ~perm_test(binary_sequence = .x,
                                             statistic = 'streak_score',
                                             n_sims = 1000),
                 .options = furrr_options(seed = 12321))


### Conduct Study to show that streak_score metric works as intended
set.seed(13)
n_hitters <- 1000
n_abs <- 500
p_avg <- 0.1


random_hitters_rare <- 
  map(1:n_hitters, ~sim_random_hitter(n = n_abs, p = p_avg))
streaky_hitters_rare <- 
  map(1:n_hitters, ~sim_streaky_hitter(n = n_abs, p = p_avg))
consistent_hitters_rare <- 
  map(1:n_hitters, ~sim_consistent_hitter(n = n_abs, p = p_avg))


percentiles_random_rare <- 
  future_map_dbl(random_hitters, ~perm_test(binary_sequence = .x,
                                            statistic = 'spacing_score',
                                            n_sims = 1000),
                 .options = furrr_options(seed = 12321))
percentiles_streaky_rare <- 
  future_map_dbl(streaky_hitters, ~perm_test(binary_sequence = .x,
                                             statistic = 'spacing_score',
                                             n_sims = 1000),
                 .options = furrr_options(seed = 12321))
percentiles_consistent_rare <- 
  future_map_dbl(consistent_hitters, ~perm_test(binary_sequence = .x,
                                                statistic = 'spacing_score',
                                                n_sims = 1000),
                 .options = furrr_options(seed = 12321))



tibble('pct' = c(percentiles_random, percentiles_streaky, percentiles_consistent),
       'hitter_type' = rep(c('Random Hitters', 'Streaky Hitters', 'Consistent Hitters'), each = n_hitters),
       'setting' = 'Common Outcome (Streak Score)') %>% 
  bind_rows(tibble('pct' = c(percentiles_random_rare, percentiles_streaky_rare, percentiles_consistent_rare),
                   'hitter_type' = rep(c('Random Hitters', 'Streaky Hitters', 'Consistent Hitters'), each = n_hitters),
                   'setting' = 'Rare Outcome (Spacing Score)')) %>% 
  ggplot(aes(x = pct)) + 
  facet_grid(setting~hitter_type, scales = 'free_y') + 
  geom_histogram(aes(fill = hitter_type), col = 'black',) + 
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Internal Streakiness Percentile',
       y = '# of Simulated Hitters',
       title = 'Distribution of Internal Consistency from\nStreak/Spacing Score Metrics',
       subtitle = '1000 Simulated Hitters of Each Type') +
  theme(legend.position = 'none')

ggsave('Figures/sim_study_1.png', height = 9/1.2, width = 16/1.2)





