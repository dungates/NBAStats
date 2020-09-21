library(rvest)
library(curl)
library(tidyverse)
library(httr)

# get all objects to loop in the urls. All teams since 96/97 are in tm_abr and only active teams will be identified later on
tm_abr <- c("ATL","BOS","BRK","NJN","CHA", "CHH","CHO","CHI",
            "CLE","DAL","DEN","DET","GSW","HOU","IND",
            "LAC","LAL","MEM","MIA","MIL","MIN","NOH", "NOK",
            "NOP","NYK","OKC","ORL","PHI","PHO","POR",
            "SEA","SAC","SAS","TOR","UTA","VAN", "WAS","WSB")

days <- c("01","02","03","04","05","06","07","08","09",
          "10","11","12","13","14","15","16","17","18",
          "19","20","21","22","23","24","25","26","27",
          "28","29","30","31")

mnth <- c("01","02","03","04","05","06","07","08","09",
          "10","11","12")


#1996 scrape

url <- "https://www.basketball-reference.com/boxscores/pbp/1996{month}{date}0{home_team}.html"
url2 <- map(.x = c(tm_abr),
            .f = function(x){gsub(x = url, pattern = "\\{home_team\\}", 
                                  replacement = x)}) %>% 
  unlist

url3 <- map(.x = c(days),
            .f = function(x){gsub(x = url2, pattern = "\\{date\\}", 
                                  replacement = x)}) %>% 
  unlist

url4 <- map(.x = c(mnth),
            .f = function(x){gsub(x = url3, pattern = "\\{month\\}", 
                                  replacement = x)}) %>% 
  unlist

#this will take a while, but it ID's which of the urls are active pages
check_link <- sapply(url4, url_success) %>%
  as.data.frame() 

#this stores the indices that are active
link_hit_index <- which(check_link == TRUE)

#now reduce to only links that are active so we don't get an error scraping pages not active
url_final <- url4[link_hit_index]
saveRDS(url_final, "activeURLs_1996.rds")

#create vector with all the active urls for the first year of pbp data available - 1996
url_final <- readRDS("activeURLs_1996.rds")

#map .x is the vector of active urls to run the function on. .f is the giant mess of crap that remains

nba_1996 <- map_dfr(.x = url_final,
                    .f = function(x) {
                      Sys.sleep(2); cat(1); 
                      df <- read_html(curl(x, handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>% 
                        html_nodes("table") %>% 
                        html_table(fill = T) %>%
                        .[[1]] %>%
                        setNames(paste0(c("time","away_play","away_score",
                                          "combined_Score","home_score", "home_play")))
                      
  away_tm <- df[1,2]
  home_tm <- df[1,6]
                      
  df$away_team <- away_tm
  df$home_team <- home_tm
  df$date <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{8}")
  df$year <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{4}")
  df$month <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{4})\\d{2}")
  df$day <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{6})\\d{2}")
  df$playid <- c(1:nrow(df))
  df$gameid <- paste0(df$away_team,df$home_team,df$date)
  df$gameid <- str_replace_all(df$gameid, " ", "")
                      
  df 
                      
})


saveRDS(nba_2012, "nba_pbp_calendar_1996.rds")

#Repeat this process for every year. Simply replace teh 1996 in url with 1997 and rerun everything from there with appropriate name changes of 
#objects to 1997 instead of anything referencing 1996 - especially when it comes to the final saveRDS command so you don't save over 1996. Not that 
#I've done this on more than one occasion. 

#After you complete saving an RDS file for each calendar year data from 96 to 2020, 
#you can combine it all in a single massive df. But, you might find R doesn't have the 
#memory for it. To see how much memory, run:

memory.limit()

#Mine was about 8k and this was not sufficient to bind all data together. So, some stack overflow person 
#recommended:

memory.limit(size = 56000)

all_years <- bind_rows(nba_1996, nba_1997, nba_1998, nba_1999, nba_2000, nba_2001, nba_2002,
                       nba_2003, nba_2004, nba_2005, nba_2006, nba_2007, nba_2008, nba_2009,
                       nba_2010, nba_2011, nba_2012, nba_2013, nba_2014, nba_2015, nba_2016,
                       nba_2017, nba_2018, nba_2019, nba_2020)


# Here's the rest of the data engineering

# library(tidyverse)
# 
# #load the behemoth all_years data frame from the scrape file of this repo and get ready to wait while this massive calls does all the work. If you're better at R than I am you could probs
# #do this using data.frame syntax and would actually be cool if someone did as this would take less than like 8 hours to run...
# 
# NBAScrapR <- all_years %>%
#   mutate_at(c("month", "day", "playid"), as.numeric) %>%
#   filter(time != "Time") %>%
#   separate(time, c("minutes","seconds","tenths"), remove = FALSE) %>%
#   mutate_at(c("minutes","seconds","tenths"), as.numeric) %>%
#   mutate(quarter = case_when(
#     str_detect(play, "End of 1st quarter") ~ 1,
#     str_detect(play, "End of 2nd quarter") ~ 1,
#     str_detect(play, "End of 3rd quarter") ~ 1,
#     str_detect(play, "End of 4th quarter") ~ 1,
#     str_detect(play, "End of 1st overtime") ~ 1,
#     str_detect(play, "End of 2nd overtime") ~ 1,
#     str_detect(play, "End of 3rd overtime") ~ 1,
#     str_detect(play, "End of 4th overtime") ~ 1,
#     str_detect(play, "End of 5th overtime") ~ 1,
#     str_detect(play, "End of 6th overtime") ~ 1,
#     str_detect(play, "End of 7th overtime") ~ 1,
#     TRUE ~ 0)) %>%
#   mutate(quarter = ifelse(is.na(quarter), 0, quarter)) %>%
#   group_by(gameid) %>%
#   mutate(quarter = case_when(
#     cumsum(quarter) == 0 ~ 1,
#     cumsum(quarter) == 1 ~ 2,
#     cumsum(quarter) == 2 ~ 3,
#     cumsum(quarter) == 3 ~ 4,
#     cumsum(quarter) == 4 ~ 5,
#     cumsum(quarter) == 5 ~ 6,
#     cumsum(quarter) == 6 ~ 7,
#     cumsum(quarter) == 7 ~ 8,
#     cumsum(quarter) == 8 ~ 9,
#     cumsum(quarter) == 9 ~ 10,
#     cumsum(quarter) == 10 ~ 11
#   )) %>%
#   mutate(play = case_when(
#     nchar(away_play) > 0 ~ away_play,
#     nchar(away_play) == 0 ~ home_play
#   )) %>%
#   mutate(player_shot = str_extract(
#     play, "^.*?(?=\\s+(?:misses|makes)\\b)")) %>%
#   #see the code at the bottom of this file I ran to figure out all the possible foul types the data contains
#   mutate(foul_type = case_when(
#     str_detect(play, "^Personal foul.*") ~ "Personal",
#     str_detect(play, "^Shooting foul.*") ~ "Shooting",
#     str_detect(play, "^Loose ball foul.*") ~ "Loose Ball",
#     str_detect(play, "^Technical foul.*") ~ "Technical",
#     str_detect(play, "^Offensive foul.*") ~ "Offensive",
#     str_detect(play, "^Def 3 sec tech foul.*") ~ "Def Three Seconds",
#     str_detect(play, "^Flagrant foul type 1.*") ~ "Flagrant 1",
#     str_detect(play, "^Away from play foul.*") ~ "Away From Play",
#     str_detect(play, "^Unkown foul.*") ~ "Unknown Foul",
#     str_detect(play, "^Ill def tech foul.*") ~ "Ill Def Technical Foul",
#     str_detect(play, "^Double Personal Foul.*") ~ "Double Personal Foul",
#     str_detect(play, "^Clear path foul.*") ~ "Clear Path",
#     str_detect(play, "^Hanging tech foul.*") ~ "Hanging Technical",
#     str_detect(play, "^Taunting technical foul.*") ~ "Taunting",
#     str_detect(play, "^Inbound foul.*") ~ "Inbound Foul",
#     str_detect(play, "^Delay tech foul.*") ~ "Delay Technical Foul",
#     str_detect(play, "^Non unsport tech foul.*") ~ "Non Unsport Technical Foul",
#     str_detect(play, "^Elbow foul.*") ~ "Elbow Foul",
#     str_detect(play, "^Punching foul.*") ~ "Punching Foul",
#     str_detect(play, "^Offensive charge foul.*") ~ "Offensive Charge",
#     str_detect(play, "^Personal block foul.*") ~ "Personal Block",
#     str_detect(play, "^Personal take foul.*") ~ "Personal Take",
#     str_detect(play, "^Shooting block foul.*") ~ "Shooting Block",
#     str_detect(play, "^Excess timeout tech foul.*") ~ "Excess Timeout Technical Foul",
#     str_detect(play, "^Flagrant foul type 2.*") ~ "Flagrant 2")) %>%
#   mutate(free_throw_foul_type = case_when(
#     str_detect(play, ".*?[makes|misses]\\s.*?free\\sthrow.*") ~ lag(foul_type))) %>%
#   mutate(free_throw_make = case_when(
#     str_detect(play, ".*?makes\\s.*?free\\sthrow.*") ~ "1",
#     str_detect(play, ".*?misses\\s.*?free\\sthrow.*") ~ "0",
#     str_detect(play, ".*?[^[misses|makes]].*") ~ "na")) %>%
#   mutate_at(vars(free_throw_make), as.numeric)  %>%
#   #neutral plays are listed in both teams so this parses those out
#   mutate(possession = if_else(nchar(away_play) > 0 & nchar(home_play) > 0, 
#                               "", if_else(nchar(away_play) > 0, 
#                                           away_team,
#                                           home_team))) %>%
#   mutate(away_pts_added = as.numeric(sub("\\D\\+", "",
#                                          away_score))) %>%
#   mutate(home_pts_added = as.numeric(sub("\\D\\+", "",
#                                          home_score))) %>%
#   replace_na(list(away_pts_added = 0)) %>%
#   replace_na(list(home_pts_added = 0)) %>%
#   mutate(away_score_after_shot = cumsum(away_pts_added),
#          home_score_after_shot = cumsum(home_pts_added)) %>%
#   mutate(away_score_before_shot = lag(cumsum(away_pts_added)),
#          home_score_before_shot = lag(cumsum(home_pts_added))) %>%
#   mutate(difference_after_play = if_else(possession == home_team,
#                                          home_score_after_shot - away_score_after_shot, away_score_after_shot - home_score_after_shot),
#          difference_before_play = if_else(possession == home_team,
#                                           home_score_before_shot - away_score_before_shot, away_score_before_shot - home_score_before_shot)) %>%
#   mutate(fg_length = str_extract(play, "(?<=from ).*?(?= ft)")) %>%
#   mutate(fg_result = case_when(
#     str_detect(play, "misses") & is.na(free_throw_make) ~ "0",
#     str_detect(play, "makes") & is.na(free_throw_make)  ~ "1",
#     TRUE ~ "NA"
#   )) %>% 
#   mutate_at(c("fg_result", "fg_length"), as.numeric) %>%
#   #shots at rim need to be length of 0 which is fixed below
#   mutate(fg_length = if_else(is.na(fg_length) & is.na(free_throw_make) & !is.na(player_shot), 0, fg_length)) %>%
#   #extract shots with ft lengths
#   mutate(shot_type = str_extract(play, "(?<=-pt ).*?(?= from)")) %>%
#   #combine with shots w no ft length (like layups)
#   mutate(shot_type = if_else(is.na(shot_type), str_extract(play, "(?<=-pt ).*"), shot_type)) %>% 
#   #find parentheses and remove leaving just shot type
#   mutate(shot_type = if_else(str_detect(shot_type, ".*?\\("), str_extract(shot_type, ".*?(?= \\()"), shot_type)) %>%
#   mutate(shot_type = trimws(shot_type)) %>%
#   mutate(player_assist = str_extract(play, "(?<=\\(assist by ).*?(?=\\))")) %>%
#   mutate(player_steal = str_extract(play, "(?<=steal by ).*?(?=\\))" )) %>%
#   mutate(player_turnover = str_extract(play, "(?<=Turnover by ).*?(?=\\(.*)")) %>%
#   mutate(player_block = str_extract(play, "(?<=block by ).*?(?=\\))")) %>%
#   mutate(player_offensive_rebound = str_extract(play, "(?<=Offensive rebound by ).*")) %>%
#   mutate(player_defensive_rebound = str_extract(play, "(?<=Defensive rebound by ).*")) %>%
#   mutate(player_travelling = str_extract(play, "(?<=Turnover by ).*? (?=\\(traveling\\))")) %>%
#   mutate(player_foul = if_else(!is.na(foul_type), str_extract(play, "(?<=foul by |foul type [12] by )[^()]*[^()\\s]"), "")) %>%
#   mutate(player_draw_foul = str_extract(play, "(?<=\\(drawn by ).*?(?=\\))")) %>%
#   mutate(season = case_when(
#     (year == 1996) | (year == 1997 & month < 10) ~ "96/97",
#     (year == 1997 & month > 9) | (year == 1998 & month < 10) ~ "97/98",
#     (year == 1998 & month > 9) | (year == 1999 & month < 10) ~ "98/99",
#     (year == 1999 & month > 9) | (year == 2000 & month < 10) ~ "99/00",
#     (year == 2000 & month > 9) | (year == 2001 & month < 10) ~ "00/01",
#     (year == 2001 & month > 9) | (year == 2002 & month < 10) ~ "01/02",
#     (year == 2002 & month > 9) | (year == 2003 & month < 10) ~ "02/03",
#     (year == 2003 & month > 9) | (year == 2004 & month < 10) ~ "03/04",
#     (year == 2004 & month > 9) | (year == 2005 & month < 10) ~ "04/05",
#     (year == 2005 & month > 9) | (year == 2006 & month < 10) ~ "05/06",
#     (year == 2006 & month > 9) | (year == 2007 & month < 10) ~ "06/07",
#     (year == 2007 & month > 9) | (year == 2008 & month < 10) ~ "07/08",
#     (year == 2008 & month > 9) | (year == 2009 & month < 10) ~ "08/09",
#     (year == 2009 & month > 9) | (year == 2010 & month < 10) ~ "09/10",
#     (year == 2010 & month > 9) | (year == 2011 & month < 10) ~ "10/11",
#     (year == 2011 & month > 9) | (year == 2012 & month < 10) ~ "11/12",
#     (year == 2012 & month > 9) | (year == 2013 & month < 10) ~ "12/13",
#     (year == 2013 & month > 9) | (year == 2014 & month < 10) ~ "13/14",
#     (year == 2014 & month > 9) | (year == 2015 & month < 10) ~ "14/15",
#     (year == 2015 & month > 9) | (year == 2016 & month < 10) ~ "15/16",
#     (year == 2016 & month > 9) | (year == 2017 & month < 10) ~ "16/17",
#     (year == 2017 & month > 9) | (year == 2018 & month < 10) ~ "17/18",
#     (year == 2018 & month > 9) | (year == 2019 & month < 10) ~ "18/19",
#     (year == 2019 & month > 9) | (year == 2020 & month < 10) ~ "19/20")) %>%
#   select(-c(away_play, home_play, home_score, away_score, combined_Score)) %>%
#   filter(!str_detect(play, "^Start of .*")) %>%
#   mutate(playoffs = case_when(
#     season == "96/97" & (month > 4 & month < 9) | season == "96/97" & (month == 4 & day >= 24) ~ 1,
#     season == "97/98" & (month > 4 & month < 9) | season == "97/98" & (month = 4 & day >= 23) ~ 1,
#     season == "98/99" & (month > 5 & month < 9) | season == "98/99" & (month = 5 & day >= 8) ~ 1,
#     season == "99/00" & (month > 4 & month < 9) | season == "99/00" & (month = 4 & day >= 22) ~ 1,
#     season == "00/01" & (month > 4 & month < 9) | season == "00/01" & (month = 4 & day >= 21) ~ 1,
#     season == "01/02" & (month > 4 & month < 9) | season == "01/02" & (month = 4 & day >= 20) ~ 1,
#     season == "02/03" & (month > 4 & month < 9) | season == "02/03" & (month = 4 & day >= 19) ~ 1,
#     season == "03/04" & (month > 4 & month < 9) | season == "03/04" & (month = 4 & day >= 17) ~ 1,
#     season == "05/06" & (month > 4 & month < 9) | season == "04/05" & (month = 4 & day >= 23) ~ 1,
#     season == "06/07" & (month > 4 & month < 9) | season == "05/06" & (month = 4 & day >= 22) ~ 1,
#     season == "07/08" & (month > 4 & month < 9) | season == "06/07" & (month = 4 & day >= 19) ~ 1,
#     season == "08/09" & (month > 4 & month < 9) | season == "07/08" & (month = 4 & day >= 18) ~ 1,
#     season == "09/10" & (month > 4 & month < 9) | season == "08/09" & (month = 4 & day >= 17) ~ 1,
#     season == "10/11" & (month > 4 & month < 9) | season == "09/10" & (month = 4 & day >= 17) ~ 1,
#     season == "11/12" & (month > 4 & month < 9) | season == "10/11" & (month = 4 & day >= 28) ~ 1,
#     season == "12/13" & (month > 4 & month < 9) | season == "11/12" & (month = 4 & day >= 20) ~ 1,
#     season == "13/14" & (month > 4 & month < 9) | season == "12/13" & (month = 4 & day >= 19) ~ 1,
#     season == "14/15" & (month > 4 & month < 9) | season == "13/14" & (month = 4 & day >= 18) ~ 1,
#     season == "15/16" & (month > 4 & month < 9) | season == "14/15" & (month = 4 & day >= 16) ~ 1,
#     season == "16/17" & (month > 4 & month < 9) | season == "15/16" & (month = 4 & day >= 15) ~ 1,
#     season == "17/18" & (month > 4 & month < 9) | season == "16/17" & (month = 4 & day >= 14) ~ 1,
#     season == "18/19" & (month > 4 & month < 9) | season == "17/18" & (month = 4 & day >= 13) ~ 1,
#     season == "19/20" & (month > 4 & month < 9) | season == "18/19" & (month = 4 & day >= 24) ~ 1,
#     TRUE ~ 0
#   )) %>%
#   mutate(qtr_seconds_remaining = (minutes * 60) + seconds) %>%
#   select(-quarter) %>% 
#   select(season, year, playoffs, date, month, day, gameid, home_team, away_team, quarter = qtr, 
#          qtr_seconds_remaining, minutes, seconds, playid, possession, play, shot_type, fg_result, 
#          fg_length, free_throw_make, free_throw_foul_type, player_shot, player_assist, player_defensive_rebound, 
#          player_offensive_rebound, player_steal, player_block, player_draw_foul, player_foul, foul_type, 
#          player_travelling, player_turnover, home_score_before_shot, away_score_before_shot, difference_before_play, 
#          home_score_after_shot, away_score_after_shot, difference_after_play, home_pts_added, away_pts_added)





