#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(gridExtra)
library(plyr)
library(dplyr)

nbaPlayers <- as.data.frame(assign_nba_players())

# WRITE AS A FUNCTION
lakers_shots <- teams_shots(teams = "Los Angeles Lakers", seasons = c(2020))
view(lakers_shots)
lakersPlot <- lakers_shots %>%
  ggplot(aes(x = locationX, y = locationY, color = isShotMade)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("gold2", "slateblue3")) +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "Shot attempts by Los Angeles Lakers in 2019-20 NBA season",
       caption = "Data accessed via nbastatR")


blazers_shots <- teams_shots(teams = "Portland Trail Blazers", seasons = c(2020))

blazersPlot <- blazers_shots %>%
  ggplot(aes(x = locationX, y = locationY, color = isShotMade)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("black", "red3")) +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "Shot attempts by Portland Trail Blazers in 2019-20 NBA season",
       caption = "Data accessed via nbastatR")
grid.arrange(lakersPlot, blazersPlot, ncol = 2)


# Make a lot of summary statistics similar to this
sum(blazers_shots$isShotMade)/nrow(blazers_shots)

# Cool dunk statistics
lakers_shots1 <- lakers_shots %>% dplyr::filter(grepl('Dunk', typeAction)) %>% dplyr::group_by(namePlayer) %>% 
  dplyr::summarize(makes = sum(typeEvent == "Made Shot"), misses = sum(typeEvent == "Missed Shot"), percent = (makes / (makes + misses)) * 100)

lebronData <- players_careers(players = c("LeBron James")) #Incredible level of data here
