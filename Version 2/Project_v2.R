library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(ggimage)
source('Code/Version 2/fx.R')

batter_results <- read_csv('Data/batter_results.csv')
batter_stats <- read_csv('Data/batting_stats_2023.csv')
pitcher_results <- read_csv('Data/pitch_results.csv')
pitcher_stats <- read_csv('Data/pitching_stats_2023.csv')

batter_total <- 
  batter_results %>% 
  left_join(batter_stats)

pitcher_total <- 
  pitcher_results %>% 
  left_join(pitcher_stats)

### Correlation plots
x_bat <- 
  batter_total %>% 
  mutate('XBH' = X2B + X3B + HR) %>% 
  select('ISP XBH' = xbh,
         'ISP Swing Contact' = swing_contact,
         'ISP Strikeout' = strikeout,
         'ISP Onbase' = onbase, 
         H, HR, XBH, BA, SO, RBI, OPS, SLG, OBP) %>% 
  cor()

p_bat <- 
  ggcorrplot(x_bat[1:4,], 
             ggtheme = theme_bw(),
             p.mat = ifelse(x_bat[1:4,] == 1, 1, 0),
             insig = 'blank',
             title = 'Hitting Outcomes',
             lab = T) + 
  scale_fill_gradient2(limits = c(-0.4, 0.4),
                       low = "#6D9EC1", 
                       mid = "white", 
                       high = "#E46726") + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, hjust = 0.5)) + 
  labs(fill = 'Correlation')




x_pitch <- 
  pitcher_total %>% 
  mutate('XBH' = X2B + X3B + HR,
         'HR9' = HR/IP * 9) %>% 
  select('ISP XBH' = xbh,
         'ISP Swing Contact' = swing_contact,
         'ISP Strikeout' = strikeout,
         'ISP Onbase' = onbase, 
         HR9, XBH, ERA, WHIP, SO, SO9, SO.W, GB.FB) %>% 
  cor()

p_pitch <- 
  ggcorrplot(x_pitch[1:4,], 
             ggtheme = theme_bw(),
             p.mat = ifelse(x_pitch[1:4,] == 1, 1, 0),
             insig = 'blank',
             title = 'Pitching Outcomes',
             lab = T) + 
  scale_fill_gradient2(limits = c(-0.4, 0.4),
                       low = "#6D9EC1", 
                       mid = "white", 
                       high = "#E46726") + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, hjust = 0.5)) + 
  labs(fill = 'Correlation')

p_bat + p_pitch +
  plot_layout(guides = "collect") +
  plot_annotation(subtitle = 'Correlation Between Internal Streak Percentile\nand Observed Statistics') &
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(size = 32, hjust = 0.5)
  )
ggsave('Figures/correlation.png', height = 16, width = 10)


### Histograms
batter_results %>% 
  select('XBH' = xbh,
         'Swing Contact' = swing_contact,
         'Strikeout' = strikeout,
         'Onbase' = onbase) %>% 
  pivot_longer(cols = everything(),
               names_to = 'metric',
               values_to = 'isp') %>% 
  ggplot(aes(x = isp)) + 
  facet_wrap(~metric) + 
  geom_histogram(binwidth = 0.1, aes(fill = metric), col = 'black') + 
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Internal Streakines Percentile',
       y = '# of Hitters',
       title = 'Distribution of ISP During 2023 MLB Season',
       subtitle = '136 Qualified Hitters') + 
  theme(legend.position = 'none')
ggsave('Figures/isp_histogram_bat.png', height = 9, width = 16)

pitcher_results %>% 
  select('XBH' = xbh,
         'Swing Contact' = swing_contact,
         'Strikeout' = strikeout,
         'Onbase' = onbase) %>% 
  pivot_longer(cols = everything(),
               names_to = 'metric',
               values_to = 'isp') %>% 
  ggplot(aes(x = isp)) + 
  facet_wrap(~metric) + 
  geom_histogram(binwidth = 0.1, aes(fill = metric), col = 'black') + 
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Internal Streakines Percentile',
       y = '# of Pitchers',
       title = 'Distribution of ISP During 2023 MLB Season',
       subtitle = '127 Qualified Pitchers') + 
  theme(legend.position = 'none')
ggsave('Figures/isp_histogram_pitch.png', height = 9, width = 16)


### Top 20 players
df_plot_batters <- 
  batter_total %>% 
  arrange(-OPS) %>% 
  head(20) %>% 
  pivot_longer(cols = c('onbase', 'swing_contact', 'xbh', 'strikeout'),
               names_to = 'outcome',
               values_to = 'percentile') %>% 
  mutate('player' = name)

### Batting Plot
ggplot(df_plot_batters, aes(x = percentile, y = player)) + 
  geom_point(aes(color = outcome), size = 5) + 
  scale_color_discrete(labels = c('Out vs. Onbase', 'Strikeouts', 'Swing Contact', 'Extra Base Hit')) +
  scale_x_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Internal Streakiness Percentile',
       y = 'Player',
       color = 'Outcome',
       title = 'Streakiness Comparison',
       subtitle = 'Top 20 Batters by OPS')
ggsave('Figures/batting_comparison.png', height = 9, width = 16)

### Top 20 players
df_plot_pitchers <- 
  pitcher_total %>% 
  filter(IP > 162) %>% 
  arrange(ERA) %>% 
  head(20) %>% 
  pivot_longer(cols = c('onbase', 'swing_contact', 'xbh', 'strikeout'),
               names_to = 'outcome',
               values_to = 'percentile') %>% 
  mutate('player' = name)

### Pitching Plot
ggplot(df_plot_pitchers, aes(x = percentile, y = player)) + 
  geom_point(aes(color = outcome), size = 5) + 
  scale_color_discrete(labels = c('Out vs. Onbase', 'Strikeouts', 'Swing Contact', 'Extra Base Hit')) +
  scale_x_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Internal Streakiness Percentile',
       y = 'Player',
       color = 'Outcome',
       title = 'Streakiness Comparison',
       subtitle = 'Top 20 Starting Pitchers by ERA\nMin. 162 IP')
ggsave('Figures/pitching_comparison.png', height = 9, width = 16)


### Shohei
df_sho <- 
  bind_rows(
    batter_results %>% 
      filter(name == 'Shohei Ohtani') %>% 
      select('XBH' = xbh,
             'Swing Contact' = swing_contact,
             'Strikeout' = strikeout,
             'Onbase' = onbase) %>% 
      pivot_longer(cols = everything(),
                   names_to = 'metric',
                   values_to = 'isp') %>% 
      mutate('direction' = 'Batter'),
    
    pitcher_results %>% 
      filter(name == 'Shohei Ohtani') %>% 
      select('XBH' = xbh,
             'Swing Contact' = swing_contact,
             'Strikeout' = strikeout,
             'Onbase' = onbase) %>% 
      pivot_longer(cols = everything(),
                   names_to = 'metric',
                   values_to = 'isp') %>% 
      mutate('direction' = 'Pitcher')
  )

ggplot(df_sho, aes(x = metric, y = isp)) + 
  geom_col(aes(fill = direction), position = 'dodge') +
  geom_image(image = 'https://b.fssta.com/uploads/application/mlb/headshots/9355.vresize.350.350.medium.43.png',
             x = 4, y = 0.9, size = 0.3) +
  labs(x = 'Outcome', 
       y = 'Internal Streakiness Percentile',
       title = 'Internal Streakiness Percentiles for Shohei Ohtani',
       fill = '') + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent)
ggsave('Figures/shohei.png', height = 9, width = 16)

