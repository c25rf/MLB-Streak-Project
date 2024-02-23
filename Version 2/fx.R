library(tidyverse)
library(baseballr)

### Functions to Get and Process Statcast Data ###
get_statcast_data <- function(mlbam_id, start, end, player_type) {
  # ### (1) Get Player ID
  # ids <- 
  #   playerid_lookup(first_name = first,
  #                   last_name = last)
  # 
  # ### Sometimes multiple people have the same name so we will default to 
  # ### the row not missing any info (and the last one if there is still > 1)
  # ids <- 
  #   ids %>% 
  #   filter(given_name != 'Luis Felipe') %>% 
  #   arrange(birth_year) %>% 
  #   filter(!is.na(mlb_played_first),
  #          !is.na(mlbam_id),
  #          !is.na(retrosheet_id),
  #          !is.na(bbref_id),
  #          !is.na(fangraphs_id)) %>% 
  #   slice(nrow(.))
  # 
  ### (2) Get the statcast data
  if(player_type == 'batter') {
    df_statcast <- 
      statcast_search_batters(start_date = start,
                              end_date = end,
                              batterid = mlbam_id)
    
  } else {
    df_statcast <- 
      statcast_search_pitchers(start_date = start,
                               end_date = end,
                               pitcherid = mlbam_id)
    
  }
  
  return(df_statcast)
}


### ggplot theme
theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 30),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  plot.caption = element_text(size = 12),
                  strip.text = element_text(size = 12),
                  axis.title = element_text(size = 24),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 16),
                  legend.position = "bottom"))


### Simulate Random Hitter
process_stacast_data <- function(df_statcast, outcome, player_type) {
  ### Outcome is out (0) vs. on base event (1)
  if(outcome == 'out_vs_onbase') {
    
    df_statcast <- 
      df_statcast %>% 
      filter(!is.na(events)) %>% 
      filter(events != '')
    
    binary_sequence <- 
      as.numeric(df_statcast$events %in% c('single', 'double', 'triple', 'home_run', 'walk'))
    
    ### Group to protect in randomization
    if(player_type == 'batter') {
      group <- 1:length(binary_sequence)
    } else {
      group <- paste0(df_statcast$game_pk, '-', df_statcast$inning) 
    }
    
    
    
  } else if(outcome == 'swing_contact') {
    ### Swings
    df_statcast <- 
      df_statcast %>% 
      filter(description %in% c('foul', 'swinging_strike', 'swinging_strike_blocked', 'hit_into_play'))
    
    binary_sequence <- 
      as.numeric( !(df_statcast$description %in% c('swinging_strike', 'swinging_strike_blocked')) )
    
    ### Get Sequences of ABs (for Swings)
    group <- paste0(df_statcast$game_pk, '-', df_statcast$at_bat_number)
    
  } else if(outcome == 'XBH') {
    df_statcast <- 
      df_statcast %>% 
      filter(!is.na(events)) %>% 
      filter(events != '')
    
    binary_sequence <- 
      as.numeric(df_statcast$events %in% c('double', 'triple', 'home_run'))
    
    ### Group to protect in randomization
    if(player_type == 'batter') {
      group <- 1:length(binary_sequence)
    } else {
      group <- paste0(df_statcast$game_pk, '-', df_statcast$inning) 
    }
    
  } else if(outcome == 'strikeout') {
    ### (1) if AB = strikeout and (0) if any other result
    df_statcast <- 
      df_statcast %>% 
      filter(!is.na(events)) %>% 
      filter(events != '')
    
    binary_sequence <- 
      as.numeric(df_statcast$events %in% c('strikeout', 'strikeout_double_play'))
    
    ### Group to protect in randomization
    if(player_type == 'batter') {
      group <- 1:length(binary_sequence)
    } else {
      group <- paste0(df_statcast$game_pk, '-', df_statcast$inning) 
    }
    
  }
  
  ### Add other outcomes you're interested in
  
  return(list('binary_sequence' = binary_sequence,
              'group' = group))
}

#### Perm Test Fx
streak_squared <- function(binary_sequence) {
  ### Fill in rest 
  streaks <- rle(binary_sequence)
  streak_score <- sum( (streaks$lengths)^2 )
  return(streak_score)
}

spacing_gaps_squared <- function(binary_sequence) {
  
  streak_gaps <- c()
  streak_length <- 0
  for(i in 1:length(binary_sequence)) {
    if(binary_sequence[i] == 1) {
      ### Streak Length gets saved
      streak_gaps <- c(streak_gaps, streak_length)
      
      ### Reset length counter back to 0
      streak_length <- 0
    } else {
      streak_length <- streak_length + 1  
    }
  }
  
  spacings <- streak_gaps - lag(streak_gaps, default = 0)
  spacings <- spacings[-1]
  spacing_score <- sum( (spacings)^2 )
  
  return(spacing_score) 
}


### Perm Test function which preserves ordering w/in grouping (AB or Game)
perm_test <- function(binary_sequence, group, statistic, n_sims) {
  if(any(is.na(group))) {
    group <- 1:length(binary_sequence)  
  }
  
  ### (1) Compute observed statistic
  if(statistic == 'streak_score') {
    observed_stat <- streak_squared(binary_sequence) 
  } else if(statistic == 'spacing_score') {
    observed_stat <- spacing_gaps_squared(binary_sequence)
  }
  
  df_ordering <- 
    tibble('group' = group, 
           'sequence' = binary_sequence) %>% 
    group_by(group) %>% 
    mutate('ordering' = 1:n()) %>% 
    ungroup()
  
  ### Shuffle sequence, protecting the group ordering
  sim_sequence <- 
    future_map(1:n_sims, ~{
      tibble('group' = sample(unique(group))) %>% 
        left_join(df_ordering, by = 'group') %>% 
        pull(sequence)
    }, 
    .options = furrr_options(seed = 12031))
  
  ### (2)
  sim_statistics <- rep(NA, n_sims)
  for(i in 1:n_sims) {
    ### Compute and save statistic on simulated streak
    if(statistic == 'streak_score') {
      sim_statistics[i] <- streak_squared(sim_sequence[[i]]) 
    } else if(statistic == 'spacing_score') {
      sim_statistics[i] <- spacing_gaps_squared(sim_sequence[[i]])
    }
  }
  
  pkg <- 
    list('observed_stat' = observed_stat,
         'sim_statistics' = sim_statistics,
         'consistency' = mean(sim_statistics <=  observed_stat))
  
  return(pkg)
  
}
