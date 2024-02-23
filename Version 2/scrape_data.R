library(baseballr)
library(tidyverse)
library(glue)
library(furrr)
plan(multisession(workers = 12))

### Load in some functions we've written elsewhere
source('Code/Version 2/fx.R')

### Get Bref Stats 2023
bat_stats_2023 <- bref_daily_batter('2023-03-30', '2023-10-01')
pitch_stats_2023 <- bref_daily_pitcher('2023-03-30', '2023-10-01')
write_csv(bat_stats_2023, 'Data/batting_stats_2023.csv')
write_csv(pitch_stats_2023, 'Data/pitching_stats_2023.csv')

### IDs of players for batting analysis
df_bat <- 
  bat_stats_2023 %>% 
  filter(PA >= 500) %>% 
  select(bbref_id) %>% 
  mutate('mlbam_id' = NA, 
         'name' = NA,
         'xbh' = NA,
         'swing_contact' = NA,
         'strikeout' = NA,
         'onbase' = NA)


### IDs of players for Pitching analysis
df_pitch <- 
  pitch_stats_2023 %>% 
  filter(IP >= 100)  %>% 
  select(bbref_id) %>% 
  mutate('mlbam_id' = NA, 
         'name' = NA,
         'xbh' = NA,
         'swing_contact' = NA,
         'strikeout' = NA,
         'onbase' = NA)


n <- nrow(df_bat)
for(i in 1:n) {
  print(paste('Scraping Data for Player', i, 'of', n, df_bat$bbref_id[i]))

  ### Look up player from ID (to get their Statcast ID)
  df_id <- playername_lookup(df_bat$bbref_id[i])

  ### Save their name and ID
  df_bat$mlbam_id[i] <- df_id$key_mlbam
  df_bat$name[i] <- paste(df_id$name_first, df_id$name_last)

  ### Scrape their statcast data
  n_try <- 0
  while(n_try < 5) {
    df_statcast <- 
      tryCatch(get_statcast_data(mlbam_id = df_bat$mlbam_id[i],
                                 start = '2023-03-30', 
                                 end = '2023-10-01',
                                 player_type = 'batter') %>% 
                 arrange(game_date, game_pk, at_bat_number, pitch_number))
    
    if(all(class(df_statcast) != 'try-error')) {
      n_try <- 5 
    } else {
      n_try <- n_try + 1
      Sys.sleep(10)
    }
  }
  

  ### Process Binary Sequence Data
  binary_seq_xbh <-
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'XBH',
                         player_type = 'batter')

  binary_seq_onbase <-
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'out_vs_onbase',
                         player_type = 'batter')

  binary_seq_contact <-
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'swing_contact',
                         player_type = 'batter')

  binary_seq_strikeout <-
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'strikeout',
                         player_type = 'batter')

  ### Permutation Tests
  perm_test_strikeout <-
    perm_test(binary_sequence = binary_seq_strikeout$binary_sequence,
              statistic = 'streak_score',
              group = binary_seq_strikeout$group,
              n_sims = 1000)

  perm_test_onbase <-
    perm_test(binary_sequence = binary_seq_onbase$binary_sequence,
              statistic = 'streak_score',
              group = binary_seq_onbase$group,
              n_sims = 1000)

  perm_test_contact <-
    perm_test(binary_sequence = binary_seq_contact$binary_sequence,
              statistic = 'streak_score',
              group = binary_seq_contact$group,
              n_sims = 1000)

  perm_test_xbh <-
    perm_test(binary_sequence = binary_seq_xbh$binary_sequence,
              group = binary_seq_xbh$group,
              statistic = 'spacing_score',
              n_sims = 1000)

  ### Save Results
  df_bat$xbh[i] <- perm_test_xbh$consistency
  df_bat$onbase[i] <- perm_test_onbase$consistency
  df_bat$strikeout[i] <- perm_test_strikeout$consistency
  df_bat$swing_contact[i] <- perm_test_contact$consistency

}
write_csv(df_bat, 'Data/batter_results.csv')

### Pitching Results
n <- nrow(df_pitch)
for(i in 1:n) {
  print(paste('Scraping Data for Player', i, 'of', n, df_pitch$bbref_id[i]))
  
  ### Look up player from ID (to get their Statcast ID)
  df_id <- playername_lookup(df_pitch$bbref_id[i])
  
  ### Save their name and ID
  df_pitch$mlbam_id[i] <- df_id$key_mlbam
  df_pitch$name[i] <- paste(df_id$name_first, df_id$name_last)
  
  ### Scrape their statcast data
  n_try <- 0
  while(n_try < 5) {
    df_statcast <- 
      tryCatch(get_statcast_data(mlbam_id = df_pitch$mlbam_id[i],
                                 start = '2023-03-30', 
                                 end = '2023-10-01',
                                 player_type = 'pitcher') %>% 
                 arrange(game_date, game_pk, at_bat_number, pitch_number))
    
    if(all(class(df_statcast) != 'try-error')) {
      n_try <- 5 
    } else {
      n_try <- n_try + 1
      Sys.sleep(10)
    }
  }
  
  ### Process Binary Sequence Data
  binary_seq_xbh <- 
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'XBH',
                         player_type = 'pitcher')
  
  binary_seq_onbase <- 
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'out_vs_onbase',
                         player_type = 'pitcher')
  binary_seq_contact <- 
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'swing_contact',
                         player_type = 'pitcher')
  binary_seq_strikeout <- 
    process_stacast_data(df_statcast = df_statcast,
                         outcome = 'strikeout',
                         player_type = 'pitcher')
  
  ### Permutation Tests
  perm_test_strikeout <- 
    perm_test(binary_sequence = binary_seq_strikeout$binary_sequence,
              statistic = 'streak_score',
              group = binary_seq_strikeout$group,
              n_sims = 1000)
  
  perm_test_onbase <- 
    perm_test(binary_sequence = binary_seq_onbase$binary_sequence, 
              statistic = 'streak_score',
              group = binary_seq_onbase$group,
              n_sims = 1000)
  
  perm_test_contact <- 
    perm_test(binary_sequence = binary_seq_contact$binary_sequence, 
              statistic = 'streak_score',
              group = binary_seq_contact$group,
              n_sims = 1000)
  
  perm_test_xbh <- 
    perm_test(binary_sequence = binary_seq_xbh$binary_sequence, 
              group = binary_seq_xbh$group,
              statistic = 'spacing_score',
              n_sims = 1000)
  
  ### Save Results
  df_pitch$xbh[i] <- perm_test_xbh$consistency
  df_pitch$onbase[i] <- perm_test_onbase$consistency
  df_pitch$strikeout[i] <- perm_test_strikeout$consistency
  df_pitch$swing_contact[i] <- perm_test_contact$consistency
  
}
write_csv(df_pitch, 'Data/pitch_results.csv')