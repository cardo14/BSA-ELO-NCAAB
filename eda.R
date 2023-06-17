library(tidyverse)
library(gt)
library(elo)
library(MLmetrics)
library(hoopR)
library(scales)
library(viridis)

cbb_13_19 <- read.csv("cbb.csv")
summary(cbb_13_19)

mm_efficiency <- cbb_13_19 %>% filter(!is.na(SEED))  %>% select(ADJOE, ADJDE)
median(mm_efficiency$ADJOE)
median(mm_efficiency$ADJDE)

cbb20 <- read.csv("cbb20.csv")


progressr::with_progress({
  mbb_box_score <-  hoopR::load_mbb_team_box()
})

cbb20 %>%
select(RK, TEAM, CONF, G, W, ADJOE, ADJDE, EFG_O, EFG_D, BARTHAG)    %>%
gt() %>%
data_color(columns = BARTHAG, scales::col_numeric(cm.colors(100), c(0, 1), 
                                                  reverse = T)) %>%
  tab_header(
    title = "Who Would've Won the 2020 NCAA Title?",
    subtitle = "Top 10 Teams by Chance of Winning Against Average Team")

power_6<- cbb_13_19 %>%
  filter(CONF == "ACC" | CONF == "B10" | CONF == "B12" | 
           CONF == "P12" | CONF =="BE" | CONF =="SEC") %>%
  filter(!is.na(SEED))
ggplot(power_6) +
  geom_point(aes(x = ADJOE, y = ADJDE, col = CONF)) +
  labs(title = "Power 6 That Made the Tournament By Conference", 
       x = "Adjusted Offensive Efficiency",
       y = "Adjusted Defensive Efficiency")

# Make a dataframe with just the team names and scores
simplified_2023 <- mbb_box_score %>%
  group_by(game_id) %>%
  summarise(
    team1_score = first(team_score),
    team2_score = last(team_score),
    team1_id = first(team_id),
    team2_id = last(team_id),
    team1_name = first(team_display_name),
    team2_name = last(team_display_name)
  )
# Add result and margin columns
simplified_2023 <- simplified_2023 %>%
  mutate(team1_result = case_when(team1_score > team2_score ~ 1,
                                  team1_score < team2_score ~ 0),
         margin = abs(team1_score - team2_score))
#run the mode
elo_model <- elo.run(data = simplified_2023,
                     formula = team1_result ~ team1_name + team2_name
                    + k(30 + 30*margin))

elo_results <- elo_model %>%
  as.data.frame()

tail(elo_results)

# Even out of order, it does a pretty good job!
final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 10)