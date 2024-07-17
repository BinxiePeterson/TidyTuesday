# Tidy Tuesday: 2024-07-16
# Written by Bianca Peterson (Ph.D)
# Senior Data Scientist @ Fathom Data
# www.fathomdata.dev
# info@fathomdata.dev

# Import Libraries --------------------------------------------------------

library(tidyverse)
library(pointblank)
library(gganimate)
library(ggthemes)
library(scales)

# Import Data -------------------------------------------------------------

# Data: https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-07-16

tuesdata <- tidytuesdayR::tt_load('2024-07-16')

# Data Transformation -----------------------------------------------------

# Custom function to convert character variables to factors.
transform_data <- function(data){
  data_clean <- data %>%
    mutate_if(is.character, as.factor)

  return(data_clean)
}

# Map over each element in the tuesdata list and apply transform_data.
data_transformed <- lapply(tuesdata, transform_data)

# Extract individual dataframes.
ewf_appearances <- data_transformed$ewf_appearances
ewf_matches <- data_transformed$ewf_matches
ewf_standings <- data_transformed$ewf_standings

# Exploratory Data Analysis -----------------------------------------------

pointblank::scan_data(ewf_appearances)
pointblank::scan_data(ewf_matches)
pointblank::scan_data(ewf_standings)

# Data Wrangling ----------------------------------------------------------

# Let's focus on the ewf_appearances data.

# Summarise total games, with result, for each team.
total_games_summary <- ewf_appearances %>%
  count(team_name,
        result) %>%
  group_by(team_name) %>%
  mutate(total_games = sum(n)) %>%
  ungroup() %>%
  mutate(result_perc = n/total_games*100)

# Reorder result_short levels.
total_games_summary$result <- factor(
  total_games_summary$result,
  levels = c("Win", "Draw", "Loss")
)

# Data Visualisation ------------------------------------------------------

# Barplot with animation.
plot <- ggplot(total_games_summary,
               aes(x = result,
                   y = result_perc,
                   fill = result)) +
  geom_bar(stat='identity') +
  scale_fill_colorblind() +
  scale_y_continuous(limits = c(0,100),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1)) +
  theme_bw() +
  # Add dynamic title based on home_team_name.
  labs(title = 'Team: {closest_state}',
       x = NULL,
       y = 'Total number of games (%)',
       fill = NULL) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        plot.title = element_text(colour = "black")) +
  # gganimate transitioning.
  transition_states(
    team_name
  ) +
  ease_aes('linear')

animate(plot, nframes = 800)

# Save animation as GIF.
anim_save("20240716/animated-barplot-transition.gif")
