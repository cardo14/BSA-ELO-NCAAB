# Building a successful ELO model
# load packages
library(tidyverse)
library(gt)
library(elo)
library(MLmetrics)
library(hoopR)
library(scales)
library(viridis)

# Load the 2023 data
progressr::with_progress({
  box_score_2023 <-  hoopR::load_mbb_team_box()
})

# Get regular season data
reg_season_2023 <- box_score_2023 %>%
  filter(game_date <= "2023-03-12")

# Create simplified dataframe of stats
simplified_2023 <- reg_season_2023 %>%
  group_by(game_id) %>%
  summarise(
    team1_score = first(team_score),
    team2_score = last(team_score),
    team1_id = first(team_id),
    team2_id = last(team_id),
    team1_name = first(team_display_name),
    team2_name = last(team_display_name),
    game_date = first(game_date),
  )

# Add result and margin columns, arrange based on game_date
simplified_2023 <- simplified_2023 %>%
  mutate(team1_result = case_when(team1_score > team2_score ~ 1,
                                  team1_score < team2_score ~ 0),
         margin = abs(team1_score - team2_score)) %>%
  arrange(game_date)

#run the model
elo_model <- elo.run(data = simplified_2023,
                     formula = team1_result ~ team1_name + team2_name +
                     + k(20 + 10*margin))


# Look at the results
final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 30)

elo_results <- elo_model %>%
  as.data.frame()

tail(elo_results, n = 80)

# Making a boxplot of the results
elos_df <- data.frame(final_elos)
ggplot(elos_df) +
  geom_boxplot(aes(x = final_elos), outlier.color = 'blue',
               outlier.size = 3, color = 'darkorchid1',
               width = 0.5) +
  coord_flip() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Final Elos",
       title = "Distribution of ELO Scores from 2023 Season")


# Loading 2022 data
progressr::with_progress({
  box_score_2022 <-  hoopR::load_mbb_schedule(seasons = 2022)
})

# Fixing the date
box_score_2022$date <- as.Date(box_score_2022$date)

# Get the necessary variables for the model
simplified_2022 <- box_score_2022 %>%
  filter(date < "2022-03-14") %>%
  arrange(date) %>%
  select(home_score, away_score, home_uid, away_uid, 
         home_short_display_name, 
         away_short_display_name,
         date, home_winner) %>%
  mutate(margin = abs(as.numeric(home_score) - as.numeric(away_score)))

elo_model <- elo.run(data = simplified_2022, 
                     formula = home_winner ~ home_short_display_name +
                       away_short_display_name + k(20 + 10* margin))

final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 30)


elo_results <- elo_model %>%
  as.data.frame()

tail(elo_results, n = 80)

# Loading 2021 data
progressr::with_progress({
  box_score_2021 <-  hoopR::load_mbb_schedule(seasons = 2021)
})

box_score_2021$date <- as.Date(box_score_2021$date)

# Get the necessary variables for the model
simplified_2021 <- box_score_2021 %>%
  filter(date < as.Date('2021-03-15')) %>%
  arrange(date) %>%
  select(home_score, away_score, home_uid, away_uid, 
         home_short_display_name, 
         away_short_display_name,
         date, home_winner) %>%
  mutate(margin = abs(as.numeric(home_score) - as.numeric(away_score)))

elo_model <- elo.run(data = simplified_2021, 
                     formula = home_winner ~ home_short_display_name +
                       away_short_display_name + k(20 + 10* margin))

final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 30)


# Load the 2020 data
progressr::with_progress({
  box_score_2020 <-  hoopR::load_mbb_schedule(seasons = 2020)
})

# Get the necessary variables for the model
simplified_2020 <- box_score_2020 %>%
  select(home_score, away_score, home_uid, away_uid, 
         home_short_display_name, 
         away_short_display_name,
         date, home_winner) %>%
  mutate(margin = abs(as.numeric(home_score) - as.numeric(away_score)))

elo_model <- elo.run(data = simplified_2020, 
                       formula = home_winner ~ home_short_display_name +
                         away_short_display_name + k(20 + 10* margin))

final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 50)


