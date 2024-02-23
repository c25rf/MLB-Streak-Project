library(tidyverse)
library(furrr)
plan(multisession(workers = 12))
source('Code/Version 2/fx.R')

### Example histograms for a selected player 
###

df_player <- 
  get_statcast_data(mlbam_id = 605141,
                    start = '2023-03-30', 
                    end = Sys.Date(), 
                    player_type = 'batter')

### 4 binary sequence
binary_seq_xbh <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'XBH')
binary_seq_onbase <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'out_vs_onbase')
binary_seq_contact <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'swing_contact')
binary_seq_strikeout <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'strikeout')

### 4 permutation tests
perm_test_strikeout <-
  perm_test(binary_sequence = binary_seq_strikeout$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_strikeout$group,
            n_sims = 10000)

perm_test_onbase <-
  perm_test(binary_sequence = binary_seq_onbase$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_onbase$group,
            n_sims = 10000)

perm_test_contact <-
  perm_test(binary_sequence = binary_seq_contact$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_contact$group,
            n_sims = 10000)

perm_test_xbh <-
  perm_test(binary_sequence = binary_seq_xbh$binary_sequence,
            group = binary_seq_xbh$group,
            statistic = 'spacing_score',
            n_sims = 10000)

### Make the plot
### (1) First convert the lists into a data frame we can use to make a plot
df_all <- 
  bind_rows(
    as_tibble(perm_test_xbh) %>% mutate('outcome' = 'Extra Base Hit'),
    as_tibble(perm_test_onbase) %>% mutate('outcome' = 'Out vs. On Base'),
    as_tibble(perm_test_strikeout) %>% mutate('outcome' = 'Strikeout'),
    as_tibble(perm_test_contact) %>% mutate('outcome' = 'Swing Contact')
  )

### (2) Make Plot
ggplot(df_all, aes(x = sim_statistics)) + 
  facet_wrap(~outcome, scales = 'free_x') + 
  geom_histogram(fill = 'dodgerblue', col = 'white') + 
  geom_vline(data = distinct(df_all, outcome, observed_stat),
             aes(xintercept = observed_stat), lty = 2, col = 'red', lwd = 1.2) + 
  labs(x = 'Streakiness Score',
       y = 'Frequency',
       title = 'Histogram of Streakiness Score in 10,000 Permutations',
       subtitle = 'Mookie Betts',
       caption = 'Streakiness score of observed results in red')

### Save Plot
ggsave('Figures/example_histograms_mookie.png', height = 9, width = 16)



### Olson
df_player <- 
  get_statcast_data(mlbam_id = 621566, 
                    start = '2023-03-30', 
                    end = Sys.Date(), 
                    player_type = 'batter')

### 4 binary sequence
binary_seq_xbh <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'XBH')
binary_seq_onbase <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'out_vs_onbase')
binary_seq_contact <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'swing_contact')
binary_seq_strikeout <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'strikeout')

### 4 permutation tests
perm_test_strikeout <-
  perm_test(binary_sequence = binary_seq_strikeout$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_strikeout$group,
            n_sims = 10000)

perm_test_onbase <-
  perm_test(binary_sequence = binary_seq_onbase$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_onbase$group,
            n_sims = 10000)

perm_test_contact <-
  perm_test(binary_sequence = binary_seq_contact$binary_sequence,
            statistic = 'streak_score',
            group = binary_seq_contact$group,
            n_sims = 10000)

perm_test_xbh <-
  perm_test(binary_sequence = binary_seq_xbh$binary_sequence,
            group = binary_seq_xbh$group,
            statistic = 'spacing_score',
            n_sims = 10000)

### Make the plot
### (1) First convert the lists into a data frame we can use to make a plot
df_all <- 
  bind_rows(
    as_tibble(perm_test_xbh) %>% mutate('outcome' = 'Extra Base Hit'),
    as_tibble(perm_test_onbase) %>% mutate('outcome' = 'Out vs. On Base'),
    as_tibble(perm_test_strikeout) %>% mutate('outcome' = 'Strikeout'),
    as_tibble(perm_test_contact) %>% mutate('outcome' = 'Swing Contact')
  )

### (2) Make Plot
ggplot(df_all, aes(x = sim_statistics)) + 
  facet_wrap(~outcome, scales = 'free_x') + 
  geom_histogram(fill = 'blue', col = 'white') + 
  geom_vline(data = distinct(df_all, outcome, observed_stat),
             aes(xintercept = observed_stat), lty = 2, col = 'red', lwd = 1.2) + 
  labs(x = 'Streakiness Score',
       y = 'Frequency',
       title = 'Histogram of Streakiness Score in 10,000 Permutations',
       subtitle = 'Matt Olson',
       caption = 'Streakiness score of observed results in red')

### Save Plot
ggsave('Figures/example_histograms_olson.png', height = 9, width = 16)

### Histogram of Acuna 
df_player <- 
  get_statcast_data(mlbam_id = 660670,
                    start = '2023-03-30', 
                    end = Sys.Date(), 
                    player_type = 'batter')

### 4 binary sequence
binary_seq_xbh <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'XBH')
binary_seq_onbase <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'out_vs_onbase')
binary_seq_contact <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'swing_contact')
binary_seq_strikeout <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'strikeout')
df_acuna <- 
  bind_rows(
    as_tibble(binary_seq_xbh$binary_sequence) %>% mutate('outcome' = 'Extra Base Hit'),
    as_tibble(binary_seq_onbase$binary_sequence)  %>% mutate('outcome' = 'Out vs. On Base'),
    as_tibble(binary_seq_strikeout$binary_sequence)  %>% mutate('outcome' = 'Strikeout'),
    as_tibble(binary_seq_contact$binary_sequence)  %>% mutate('outcome' = 'Swing Contact')
  ) %>% 
  mutate('player' = 'Ronald Acuña')


### Mookie
df_player <- 
  get_statcast_data(mlbam_id = 605141,
                    start = '2023-03-30', 
                    end = Sys.Date(), 
                    player_type = 'batter')
binary_seq_xbh <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'XBH')
binary_seq_onbase <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'out_vs_onbase')
binary_seq_contact <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'swing_contact')
binary_seq_strikeout <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'strikeout')
df_mookie <- 
  bind_rows(
    as_tibble(binary_seq_xbh$binary_sequence) %>% mutate('outcome' = 'Extra Base Hit'),
    as_tibble(binary_seq_onbase$binary_sequence)  %>% mutate('outcome' = 'Out vs. On Base'),
    as_tibble(binary_seq_strikeout$binary_sequence)  %>% mutate('outcome' = 'Strikeout'),
    as_tibble(binary_seq_contact$binary_sequence)  %>% mutate('outcome' = 'Swing Contact')
  ) %>% 
  mutate('player' = 'Mookie Betts')

### Freeman
df_player <- 
  get_statcast_data(mlbam_id = 518692,
                    start = '2023-03-30', 
                    end = Sys.Date(), 
                    player_type = 'batter')
binary_seq_xbh <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'XBH')
binary_seq_onbase <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'out_vs_onbase')
binary_seq_contact <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'swing_contact')
binary_seq_strikeout <- 
  process_stacast_data(df_statcast = df_player,
                       player_type = 'batter',
                       outcome = 'strikeout')
df_freeman <- 
  bind_rows(
    as_tibble(binary_seq_xbh$binary_sequence) %>% mutate('outcome' = 'Extra Base Hit'),
    as_tibble(binary_seq_onbase$binary_sequence)  %>% mutate('outcome' = 'Out vs. On Base'),
    as_tibble(binary_seq_strikeout$binary_sequence)  %>% mutate('outcome' = 'Strikeout'),
    as_tibble(binary_seq_contact$binary_sequence)  %>% mutate('outcome' = 'Swing Contact')
  ) %>% 
  mutate('player' = 'Freddie Freeman')

df_plot <- 
  bind_rows(df_acuna, df_mookie, df_freeman) %>% 
  group_by(player, outcome) %>% 
  mutate('rm' = zoo::rollmean(value, k = 25, align = 'left', na.pad = T),
         'obs' = 1:n())

ggplot(df_plot %>% filter(player == 'Ronald Acuña'), aes(x = obs, y = rm)) + 
  facet_wrap(~outcome, scale = 'free_x') +
  geom_line(color = 'navy') + 
  labs(x = 'Observation Index (k)',
       y = 'Rolling Mean: RM(25,k)',
       title = '25 Observation Rolling Means for 2023 MLB Season',
       subtitle = 'Ronald Acuña')
ggsave('Figures/example_rolling_mean.png', height = 9, width = 16)
