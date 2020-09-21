#devtools::install_github("abresler/nbastatR")
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

# Base stats import
nbastatR::teams_players_stats(seasons = c(2010:2020), types = c("player"), tables = "general", measures = "Base", assign_to_environment = TRUE)
nbaDecade <- dataGeneralPlayers

#Last years stats import
nbastatR::teams_players_stats(seasons = c(2020), types = c("player"), tables = "general", measures = "Base", assign_to_environment = TRUE)
nba2020 <- dataGeneralPlayers

# Playoff data
nbastatR::teams_players_stats(seasons = c(2020), types = c("player"), tables = "general", measures = "Base", season_types = "Playoffs",
                              assign_to_environment = TRUE)
nbaPlayoffs <- dataGeneralPlayers

playerData <- player_profiles(players = c(nbaPlayoffs$namePlayer[1:216]), player_ids = c(nbaPlayoffs$idPlayer[1:216])) #This takes a while

# Getting 2020 player photos
bref_players_stats(seasons = 2020, tables = c("advanced", "totals"), widen = TRUE, assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
adv_player_stats <- bref_advanced %>%
  filter(minutes >= 500) %>%
  mutate(bref_url = glue::glue("https://www.basketball-reference.com/players/{stringr::str_sub(idPlayer, 1, 1)}/{idPlayer}.html"),
         bref_link = glue::glue('<a href="{bref_url}">{namePlayer}</a>'))
playerPhotos <- adv_player_stats %>% select(namePlayer, idPlayerNBA, bref_url, urlPlayerHeadshot)

#Joining player data and photos
playerStatsPhotos <- playerData %>% left_join(playerPhotos, by = c("idPlayer"))

# Plotting the players
footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep=""))
}
playerData$feet <- footinch_formatter(playerData$heightInches)
ggplot(data = playerData, aes(heightInches, weightLBS)) + geom_image(aes(image = urlPlayerHeadshot), size = 0.15) +
  labs(y = "Weight (Pounds)", x = "Height (Feet)") + scale_x_continuous(labels = footinch_formatter) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()

# Function that writes shot data for each team with color and logo
teamColors <- teamcolors %>% filter(league == "nba")
shotsGraph <- function(team, season, seasonType) {
  nbaIcon <- teamColors %>% filter(team == name) %>% pull(logo)
  y <- read.gif(nbaIcon)
  mat = y$col[y$image+1]
  dim(mat) = dim(y$image)
  twoColors <- teamColors %>% filter(team == name) %>% select(primary, secondary)
  teamsShots <- teams_shots(teams = team, seasons = season, season_types = seasonType)
  p1 <- teamsShots %>% ggplot(aes(x = locationX, y = locationY, color = isShotMade)) +
    geom_point(alpha = 0.5) + scale_color_manual(values = c(twoColors$primary, twoColors$secondary), labels = c("Miss","Make")) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) + 
    labs(title = paste("Shot attempts by", team, "in", season, seasonType),
         caption = "Data accessed via nbastatR", color = "Make/Miss") + 
    annotate(geom = "text", x = 250, y = 300, 
             label = paste0(round((sum(teamsShots$isShotMade)/nrow(teamsShots))*100,digits = 2),"%\nShooting"), 
             color = twoColors$secondary) + 
    geom_text(aes(label = ifelse(locationY > 400, as.character(namePlayer),'')), hjust = 0.1, vjust = -0.5, size = 3) +
    coord_cartesian(clip = "off") + 
    annotation_custom(rasterGrob(mat), xmin = -300, xmax = -200, ymin = -250, ymax = -100)
  return(p1)
}
# Options are team, season, and season type (Playoffs, Pre Season, Regular Season, All Star)
shotsGraph(team = c("Atlanta Hawks"), season = 2020, seasonType = "Regular Season")



# Making new statistics
nbastatR::teams_players_stats(seasons = 2020, types = c("player"), 
                              tables = "general", measures = "Misc", assign_to_environment = TRUE)
dataGeneralPlayers<- 
teams_players_stats(measu)

players_passes <- dataPassesPlayerBase %>% 
  filter(typePass=='made') %>% 
  group_by(namePlayer) %>% 
  summarize(passes=sum(passes),
            fgm=sum(fgm), 
            fga=sum(fga),
            fg2m=sum(fg2m),
            fg2a=sum(fg2a),
            fg3m=sum(fg3m),
            fg3a=sum(fg3a),
            ast=sum(ast))

left_join(passing_perGame, passing_Totals,by="PLAYER_ID") -> t
right_join(t,players_passes,by=c("PLAYER_NAME.x"="namePlayer")) -> tx

names(tx) <- wrapr::qc(Player_ID,Player_Name,Team_ID,Team_Abbrev,GP,
                       W,L,MPG,PassesMade_PG,PassesReceived_PG,APG,
                       FTAst_PG,SecAst_PG,PotentAst_PG,AstPointsCreated_PG,
                       AdjAst_PG,AstToPassPct_PG,AstToPassPctAdj_PG,
                       name,teamid,abbr,gp,w,l,MP,TotalPassesMade,
                       TotalPassesReceived,TotalAst,
                       TotalFTAst,TotalSecAst,TotalPotentAst,
                       TotalAstPointsCreated,TotalAdjAst,TotalAstToPassPct,
                       TotalAdjAstToPassPct,passes,EventualFGM,EventualFGA,
                       EventualFG2M,EventualFG2A,
                       EventualFG3M,EventualFG3A,ast)

tx %>% select(Player_Name,Team_Abbrev,
              GP,MPG,MP,PassesMade_PG,
              TotalPassesMade,APG,
              TotalAst,FTAst_PG,
              TotalFTAst,SecAst_PG,
              TotalSecAst,PotentAst_PG,
              TotalPotentAst,AstPointsCreated_PG,
              TotalAstPointsCreated,
              EventualFGM,EventualFGA,
              EventualFG2M,EventualFG2A,
              EventualFG3M,EventualFG3A) -> base
base %>% mutate(PassesMade_PG=TotalPassesMade/GP,
                APG=TotalAst/GP,
                TotalFTAst=as.numeric(TotalFTAst),
                FTAst_PG=TotalFTAst/GP,
                MPG=MP/GP,
                SecAst_PG=TotalSecAst/GP,
                PotentAst_PG=TotalPotentAst/GP,
                AstPointsCreated_PG=TotalAstPointsCreated/GP) -> base

#adding turnovers
tov <- read.csv("Downloads/Bad Passes w names.csv",stringsAsFactors=F)
tov %>% select(1,3)-> tov
names(tov) <- c("Player_Name","TOV")
turnovers <- tov
turnovers$TOV <- as.numeric(turnovers$TOV)

base %>% mutate(ProductivePasses=TotalAst+.44*TotalFTAst+TotalSecAst+TotalPotentAst,
                ProductivePasses_per_game=ProductivePasses/GP,
                ProductivePassRate=ProductivePasses/TotalPassesMade,
                AssistPtsCreatedPerPass=TotalAstPointsCreated/TotalPassesMade) -> new_vars
merge(new_vars,turnovers) -> new_vars

#tables
t1=new_vars %>% select("Player Name"=Player_Name,ProductivePasses_per_game) %>% arrange(desc(ProductivePasses_per_game)) %>% head()
t2=new_vars %>% select("Player Name"=Player_Name,ProductivePassRate) %>% arrange(desc(ProductivePassRate)) %>% head()
t3=new_vars %>% select("Player Name"=Player_Name,AssistPtsCreatedPerPass) %>% arrange(desc(AssistPtsCreatedPerPass)) %>% head()
cbind(t1,breaks,t2,breaks,t3)

#calculating passer rating
new_vars %>% filter(MP >= quantile(new_vars$MP,probs=0.2),
                    TOV > 0) %>%
  mutate(TORate=TOV/TotalPassesMade,
         PP_to_TO_ratio=ProductivePassRate/TORate,
         logPPTO=log(PP_to_TO_ratio),
         tAPCPP=(1+AssistPtsCreatedPerPass),
         Value_per_Pass=(log(1+PP_to_TO_ratio)+(1+ProductivePassRate)^2)^tAPCPP,
         Passer_Rating_uT=log(Value_per_Pass*PassesMade_PG)) -> Filtered_PassRtg

#transforming the distribution
adjust_normal = function(col,goal_mean,goal_sd){
  round(goal_mean + (col-mean(col)) * goal_sd/sd(col),1)
}
Filtered_PassRtg$Passer_Rating=adjust_normal(Filtered_PassRtg$Passer_Rating_uT,10,2)

mean(Filtered_PassRtg$Passer_Rating) # 10.00207
sd(Filtered_PassRtg$Passer_Rating) #1.999246
skewness(Filtered_PassRtg$Passer_Rating) #-0.04574408

ggplot(Filtered_PassRtg,aes(x=Passer_Rating)) + geom_histogram(binwidth = 0.75)
hist(Filtered_PassRtg$Passer_Rating)

#bottom 2.5%
Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% 
  filter(Passer_Rating>=quantile(Passer_Rating,.975)) %>% arrange(desc(Passer_Rating))
#bottom 2.5%
Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% 
  filter(Passer_Rating<=quantile(Passer_Rating,.025)) %>% arrange(desc(Passer_Rating)) 


# Finding minutes vs players in 2020
library(ggplot2)
formula = y ~ x + I(x^2)
nbaDecade2020 <- nbaDecade %>% filter(yearSeason == 2020 & minutes > 25) %>% 
  mutate(ptsDiff = dplyr::case_when(pts > 25 ~ "Greater than 25", pts < 18 ~ "Less than 18", TRUE ~ "In between"))
ggplot(nbaDecade2020, aes(x= minutes, y = pts, label = namePlayer)) + 
  geom_point(size = 3, alpha = 0.8, color = dplyr::case_when(nbaDecade2020$pts > 25 ~ "#1b9e77", 
                                                             nbaDecade2020$pts < 18 ~ "#d95f02",
                                                             TRUE ~ "#7570b3")) + 
  geom_smooth(formula = formula, method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = formula, parse = T) +
  geom_text_repel(data = subset(nbaDecade2020, pts > 25),
                  nudge_y = max(nbaDecade2020$pts) - subset(nbaDecade2020, pts > 25)$pts,
                  size = 3,
                  box.padding = 1.5,
                  point.padding = 0.5,
                  force         = 1,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "both") +
  geom_label_repel(data = subset(nbaDecade2020, pts < 18),
                   nudge_y       = -8,
                   size          = 2,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 1,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x") +
  scale_x_continuous(expand = expand_scale(mult = c(0.2, .2))) +
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1))) +
  theme_classic(base_size = 16)






# Old Code
# lakers_shots <- teams_shots(teams = "Los Angeles Lakers", seasons = c(2020))
# view(lakers_shots)
# lakersPlot <- lakers_shots %>%
#   ggplot(aes(x = locationX, y = locationY, color = isShotMade)) +
#   geom_point(alpha = 0.5) +
#   scale_color_manual(values = c("gold2", "slateblue3")) +
#   theme_bw() + theme(legend.position = "bottom") +
#   labs(title = "Shot attempts by Los Angeles Lakers in 2019-20 NBA season",
#        caption = "Data accessed via nbastatR") + 
#   geom_text(aes(label = ifelse(locationY > 300, as.character(namePlayer),'')), hjust = 0, vjust = 0)
# lakersPlot
# 
# blazers_shots <- teams_shots(teams = "Portland Trail Blazers", seasons = c(2020))
# 
# blazersPlot <- blazers_shots %>%
#   ggplot(aes(x = locationX, y = locationY, color = isShotMade)) +
#   geom_point(alpha = 0.5) +
#   scale_color_manual(values = c("black", "red3")) +
#   theme_bw() + theme(legend.position = "bottom") +
#   labs(title = "Shot attempts by Portland Trail Blazers in 2019-20 NBA season",
#        caption = "Data accessed via nbastatR")
# grid.arrange(lakersPlot, blazersPlot, ncol = 2)




# Salary Data by Player
all_team_data <- nba_insider_salaries()


# team_cap_space <-
#   all_team_data %>%
#   mutate(idSeason = idSeason %>% factor(ordered = T)) %>%
#   dplyr::filter(nameItem %in% c('amountCapSpace', 'amountCapHold')) %>%
#   dplyr::select(idSeason, slugTeamYahoo, nameItem, value) %>%
#   group_by(idSeason, slugTeamYahoo, nameItem) %>%
#   summarise(value = sum(value, na.rm = T) / 1000000) %>%
#   ungroup %>%
#   spread(nameItem, value) %>%
#   replace_na(list(amountCapHold = 0)) %>%
#   mutate(amountSpaceLessHold = amountCapHold + amountCapSpace) %>%
#   dplyr::select(idSeason, slugTeamYahoo, amountSpaceLessHold) %>%
#   gather(item, amountSpaceLessHold, -c(idSeason, slugTeamYahoo)) %>%
#   dplyr::select(-item) %>%
#   spread(idSeason, amountSpaceLessHold)
# row.names(team_cap_space) <-
#   team_cap_space$slugTeamYahoo


# plot_data <-
#   team_cap_space %>%
#   dplyr::select(-slugTeamYahoo)

# Cool dunk statistics
lakers_shots1 <- lakers_shots %>% dplyr::filter(grepl('Dunk', typeAction)) %>% dplyr::group_by(namePlayer) %>% 
  dplyr::summarize(makes = sum(typeEvent == "Made Shot"), misses = sum(typeEvent == "Missed Shot"), percent = (makes / (makes + misses)) * 100)

lebronData <- players_careers(players = c("LeBron James")) #Incredible level of data here
