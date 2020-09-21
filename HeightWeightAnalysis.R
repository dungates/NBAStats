library(teamcolors)
library(nbastatR)
library(gridExtra)
library(plyr)
library(tidyverse)
library(ggrepel)
library(knitr)
library(caTools)
library(gghighlight)
library(ggpmisc)
library(hexbin)
library(magick)
library(cowplot)
library(wrapr)
library(gt)
library(ggimage)
# Getting 2020 playoffs data
nbastatR::teams_players_stats(seasons = c(2020), types = c("player"), tables = "general", measures = "Base", season_types = "Playoffs",
                              assign_to_environment = TRUE)
nbaPlayoffs <- dataGeneralPlayers
# Filtering for just those still in playoffs
nbaPlayoffsCurrent <- nbaPlayoffs %>% dplyr::filter(slugTeam %in% c("DEN","LAL","BOS","MIA"))

playoffsPlayerData <- player_profiles(players = c(nbaPlayoffs$namePlayer[1:216]), player_ids = c(nbaPlayoffs$idPlayer[1:216])) #This takes a while
# saveRDS(playoffsPlayerData, file = "/Users/dunk/NBAStats/2020PlayoffsPlayerData.rds")
# playoffsPlayerData <- readRDS(file = "/Users/dunk/NBAStats/2020PlayoffsPlayerData.rds") # Should be run later with full nba data
playoffsPlayerDataCurrent <- player_profiles(players = c(nbaPlayoffsCurrent$namePlayer), player_ids = c(nbaPlayoffsCurrent$idPlayer))
# Function for inches to feet height
footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep=""))
}
# Adding a foot column to data frame
playoffsPlayerData <- playoffsPlayerData %>% mutate(cm = heightInches*2.54, feet = footinch_formatter(heightInches))


# Plotting the distribution of player height and weight
# Height
ggplot(playoffsPlayerData, aes(x = heightInches)) + geom_histogram(alpha = 0.7, fill = "#011627") + 
  geom_vline(xintercept = 70, color = "red", linetype = "dashed") + 
  geom_text(aes(x = 70, label = "\nAverage Human Height", y = 15), color = "red", text = element_text(size = 11), angle = 90) +
  labs(x = "Height", y = "Count", title = "Distribution of Height", subtitle = "NBA 2019-2020 Playoffs",
       caption = glue::glue("data via nbastatR")) + scale_x_continuous(labels = footinch_formatter) +
  theme_bw() + theme()
# Weight
ggplot(playoffsPlayerData, aes(x = weightLBS)) + geom_histogram(alpha = 0.7, fill = "#011627") + 
  geom_vline(xintercept = 178, color = "blue", linetype = "dashed") + 
  annotate(geom = "text", x = 178, label = "\nAverage American Weight", y = 15, color = "blue", text = element_text(size = 11), angle = 90) +
  labs(x = "Height", y = "Count", title = "Distribution of Weight", subtitle = "NBA 2019-2020 Playoffs",
       caption = glue::glue("data via nbastatR")) +
  theme_bw() + theme()

# Plotting players with labels if 7 feet or greater or 6 feet and under for just Lakers vs Nuggets
playoffsPlayerDataCurrent %>% filter(slugTeam == "LAL" | slugTeam == "DEN") %>% ggplot(aes(heightInches, weightLBS)) + geom_image(aes(image = urlPlayerHeadshot), size = 0.15) + xlim(70, 90) +
  labs(y = "Weight (Pounds)", x = "Height (Feet)") + scale_x_continuous(labels = footinch_formatter) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + 
  geom_text(aes(label = ifelse(heightInches >= 84 | heightInches <= 72, as.character(namePlayer),'')), hjust = 1.2, vjust = 0, color = "blue")
# Plotting players with labels if 7 feet or greater or 6 feet and under for just Boston vs Heat
playoffsPlayerDataCurrent %>% filter(slugTeam == "MIA" | slugTeam == "BOS") %>% ggplot(aes(heightInches, weightLBS)) + geom_image(aes(image = urlPlayerHeadshot), size = 0.15) + xlim(70, 90) +
  labs(y = "Weight (Pounds)", x = "Height (Feet)") + scale_x_continuous(labels = footinch_formatter) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + 
  geom_text(aes(label = ifelse(heightInches >= 84 | heightInches <= 72, as.character(namePlayer),'')), hjust = 1.2, vjust = 0, color = "blue")
# Plotting everyone in the playoffs
playoffsPlayerData %>% 
  filter(urlPlayerHeadshot != "https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/1628778.png") %>%
  ggplot(aes(x = heightInches, y = weightLBS)) + geom_image(aes(image = urlPlayerHeadshot), size = 0.15) + xlim(70, 90) +
  labs(y = "Weight (Pounds)", x = "Height (Feet)") + scale_x_continuous(labels = footinch_formatter) + 
  ggtitle("The Straight Line of NBA Athleticism") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
# Plotting for data purposes
# ggplot(playoffsPlayerData, aes(x = weightLBS, y = heightInches*2.54)) + geom_point() + geom_smooth(method = "lm", se = F) +
#   labs(x = "Weight (Pounds)",y = "Height (Centimeters)") +
#   stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = y ~ x, parse = T) + theme_bw()
# Pretty interactive plot
hchart(playoffsPlayerData, "scatter", hcaes(x = "weightLBS", y = "cm", group = "numberRound", name = "namePlayer",
                                            Weight = "weightLBS", Height = "cm", Round = "numberRound")) %>%
  hc_tooltip(pointFormat = "<b>{point.name}</b><br />Weight: {point.Weight}<br />Height: {point.Height}") %>%
  hc_title(text = "Relationship Between Player Height and Weight in Playoffs") %>%
  hc_subtitle(text = "NBA 2019-2020 Playoffs") %>%
  hc_credits(enabled = TRUE,
             text = "via nbastatR",
             href = "https://github.com/abresler/nbastatR",
             style = list(
               fontSize = "10px",
               color = "#4a4a4a"
             )
  ) %>%
  hc_add_theme(hc_theme_bloom())

# Making a correlogram
correlogramData <- playoffsPlayerData %>% mutate(roundNumber = factor(numberRound, levels = c(1,2))) %>%
  select(cm, weightLBS, pts, ast, treb, ratioPIE, roundNumber) 
colnames(correlogramData) <- c("Height", "Weight", "Points", "Assists","Rebounds","ratioPIE","Round Number")
correlogramData <- dplyr::tbl_df(correlogramData)
correlogramData %>% 
  ggcorr(method = "everything", label = T, hjust = 0.1)
# Another correlogram
library(lares)
# Top 10 correlations of all available variables
corr_cross(correlogramData, max_pvalue = 0.05, top = 10)
# One variable correlated with all others
corr_var(correlogramData, Height, top = 5)




# Creating new metrics

nbaPlayoffs$net_rating <- 100((nbaPlayoffs$pts/))