### for wnba scraping stats :]
library(httr)
library(dplyr)
library(rvest)
library(xml2)
library("rvest")
library(XML)
library(readr)

#x = read_csv("https://raw.githubusercontent.com/eric-thiel/w_mins_matrix/master/new_final_results.csv")
#x = subset(x, !is.na(x$Player_id))
#x = subset(x, !is.na(x$Names))

## use this first one to get starters. Then get total mins / stats.
## get games from pbpstats/games, append starters and mins (usage?) to each game.
## will have to keep cumulative update as all we have are totals. run every morning.


## teams 
# 0: {id: "1611661330", text: "ATL"}
# 1: {id: "1611661329", text: "CHI"}
# 2: {id: "1611661323", text: "CON"}
# 3: {id: "1611661321", text: "DAL"}
# 4: {id: "1611661325", text: "IND"}
# 5: {id: "1611661320", text: "LAS"}
# 6: {id: "1611661319", text: "LVA"}
# 7: {id: "1611661324", text: "MIN"}
# 8: {id: "1611661313", text: "NYL"}
# 9: {id: "1611661317", text: "PHO"}
# 10: {id: "1611661328", text: "SEA"}
# 11: {id: "1611661322", text: "WAS"}

## oopsie dont need this

# team_ids = c("1611661330", "1611661329", "1611661323", "1611661321",
#             "1611661325", "1611661320","1611661319","1611661324",
#             "1611661313","1611661317","1611661328","1611661322")

 team_abbrevs = c("ATL","CHI","CON","DAL","IND","LAS","LVA","MIN",
                 "NYL","PHO","SEA","WAS")

#get_teams = data.frame(team_ids, team_abbrevs)
#for i in team_ids{
#}

## all starters

g = jsonlite::fromJSON("https://api.pbpstats.com/get-totals/wnba?Season=2020&SeasonType=Regular%2BSeason&StarterState=5v5&Type=Player")
Player_id = g[["multi_row_table_data"]][["RowId"]]
Names = g[["multi_row_table_data"]][["Name"]]
Minutes = g[["multi_row_table_data"]][["Minutes"]]


Player_id = as.data.frame(Player_id)
Names = as.data.frame(Names)
Minutes = as.data.frame(Minutes)

starters = cbind(Player_id, Names)
starters = cbind(starters, Minutes)
starters$Player_id = as.character(starters$Player_id)
starters$Names  = as.character(starters$Names)
starters$Player_id = as.numeric(starters$Player_id)
starters$Minutes = as.character(starters$Minutes)
starters$Minutes = as.numeric(starters$Minutes)

starters$started = 1

## all starters done, now get normal minutes & usage,
## this will be what actually goes in the matrix. Will join with starters


g = jsonlite::fromJSON("https://api.pbpstats.com/get-totals/wnba?Season=2020&SeasonType=Regular%2BSeason&Type=Player")
Player_id = g[["multi_row_table_data"]][["RowId"]]
Names = g[["multi_row_table_data"]][["Name"]]
Minutes = g[["multi_row_table_data"]][["Minutes"]]
Usage = g[["multi_row_table_data"]][["Usage"]]
TeamAbbreviation = g[["multi_row_table_data"]][["TeamAbbreviation"]]
GamesPlayed = g[["multi_row_table_data"]][["GamesPlayed"]]


Player_id = as.data.frame(Player_id)
Names = as.data.frame(Names)
Minutes = as.data.frame(Minutes)
Usage = as.data.frame(Usage)
TeamAbbreviation = as.data.frame(TeamAbbreviation)
GamesPlayed = as.data.frame(Games_Played)

to_this_date = cbind(Player_id, Names)
to_this_date = cbind(to_this_date, Minutes)
to_this_date = cbind(to_this_date, Usage)
to_this_date = cbind(to_this_date, TeamAbbreviation)
to_this_date = cbind(to_this_date, GamesPlayed)
to_this_date$Player_id = as.character(to_this_date$Player_id)
to_this_date$Names  = as.character(to_this_date$Names)
to_this_date$TeamAbbreviation  = as.character(to_this_date$TeamAbbreviation)
to_this_date$Player_id = as.numeric(to_this_date$Player_id)
to_this_date$Minutes = as.character(to_this_date$Minutes)
to_this_date$Minutes = as.numeric(to_this_date$Minutes)
to_this_date$Usage = as.character(to_this_date$Usage)
to_this_date$Usage = as.numeric(to_this_date$Usage)
to_this_date$Usage = ifelse(is.na(to_this_date$Usage),0,to_this_date$Usage)
to_this_date$GamesPlayed = as.character(to_this_date$GamesPlayed)
to_this_date$GamesPlayed = as.numeric(to_this_date$GamesPlayed)

### here is where I am going to have to subtract the previous days totals to get single game totals.
### total - running count == new statistics


previous_gamelog = vroom::vroom("https://raw.githubusercontent.com/eric-thiel/w_mins_matrix/master/new_final_results.csv")
hold_previous_gamelog = vroom::vroom("https://raw.githubusercontent.com/eric-thiel/w_mins_matrix/master/new_final_results.csv")
previous_gamelog$GamesPlayed = 1 ## comment these out eventually. Fixing shit
hold_previous_gamelog$GamesPlayed = 1 ## comment these out eventually. Fixing shit lol


#wnba_helper <- read_csv("~/Downloads/wnba_helper - Sheet1.csv")
#starters = left_join(starters, wnba_helper[c("First_game_start_mins","Name")], by = c("Names"="Name"))
### ideally here we are comparing the "starter" minutes in the previous gamelog to the current ones. to determine who the starters for the last game were


summarised_prev = previous_gamelog %>% group_by(Player_id,Names)%>%
  summarise(sum_mins = sum(Minutes), sum_usage = sum(Usage), sum_starts = sum(started), sum_games = sum(GamesPlayed), sum_start_mins = sum(minutes_as_starter))


#starters$started = ifelse(starters$Minutes - starters$First_game_start_mins == 0, 0,1)
starters = left_join(starters, summarised_prev[c("sum_start_mins","Player_id", "sum_starts")], by = c("Player_id"="Player_id"))
starters$newest_start = ifelse(starters$sum_start_mins == starters$Minutes, 0, 1) + starters$sum_starts
starters$new_minutes_for_starters = starters$Minutes - starters$sum_start_mins


joined = left_join(to_this_date, starters[c("new_minutes_for_starters","newest_start","Player_id")], by = c("Player_id"="Player_id"))

joined = left_join(joined, summarised_prev[c("sum_mins","sum_usage","sum_starts","sum_games","Player_id")], by = c("Player_id"="Player_id"))
joined[is.na(joined)] = 0

joined$newest_game = joined$GamesPlayed - joined$sum_games
joined = subset(joined, newest_game == 1)

joined$Minutes_new = joined$Minutes - joined$sum_mins
joined$started_new = joined$newest_start - joined$sum_starts
joined$Usage_new = (joined$GamesPlayed * joined$Usage) - (joined$sum_games * joined$sum_usage)
joined$minutes_as_starter_new = joined$new_minutes_for_starters


## minutes as starter doesn't really matter WRONG 

joined$game_number = Sys.Date() - 
  as.Date(as.character("2020/01/01"), format="%Y/%m/%d")

g = jsonlite::fromJSON("https://api.pbpstats.com/get-games/wnba?Season=2020&SeasonType=Regular%2BSeason")

Home = g[["results"]][["HomeTeamAbbreviation"]]
Away = g[["results"]][["AwayTeamAbbreviation"]]
Home_score = g[["results"]][["HomePoints"]]
Away_score = g[["results"]][["AwayPoints"]]
Date = g[["results"]][["Date"]]

Home = as.data.frame(Home)
Away = as.data.frame(Away)
Home_score = as.data.frame(Home_score)
Away_score = as.data.frame(Away_score)
Date = as.data.frame(Date)

games = cbind(Home, Away)
games = cbind(games, Home_score)
games = cbind(games, Away_score)
games = cbind(games, Date)

games[, 1:5] = sapply(games[, 1:5], as.character)
games$Home_score = as.numeric(games$Home_score)
games$Away_score = as.numeric(games$Away_score)
games$Date = as.Date(games$Date, format = "%Y-%m-%d")
games = games %>% arrange(Date)


#### WIP, tough to visualize without another day of data. Attack after tomorrows games
# previous_gamelog = vroom::vroom("https://raw.githubusercontent.com/eric-thiel/w_mins_matrix/master/new_final_results.csv")

# last = previous_gamelog %>% select(Player_id, Names, Minutes, Usage, TeamAbbreviation, started, game_number)
# to_compare_to = joined %>% select(Player_id, Names, Minutes, Usage, TeamAbbreviation, started, game_number)
# to_compare_to = subset(to_compare_to, to_compare_to$game_number != last$game_number) ## this is useless, will never come true. 

## essentially we want to subtract the total from the previous. Likely need to summarise the previous gamelog by player and then left join that 
  ## to "to_compare_to" subtract "new" minutes from "old minutes" that are stored in gamelog. 

joined = joined %>% select(Player_id,Names, Minutes_new, Usage_new, TeamAbbreviation, started_new, game_number, minutes_as_starter_new)
joined = joined %>% rename("Minutes"="Minutes_new", "Usage"="Usage_new", "started"="started_new", "minutes_as_starter"="minutes_as_starter_new")


hold_final_results = c("Player_id", "Names","Minutes","Usage","TeamAbbreviation","started","game_number","minutes_as_starter","matchup","description")
hold_final_results = data.frame(hold_final_results)
t1 <- t(hold_final_results)
my.names <- t1[1,]
colnames(t1) <- my.names
t1 = as.data.frame(t1)
t1 = t1[0,]
hold_final_results = t1
teams = unique(joined$TeamAbbreviation)
teams = as.data.frame(teams)
for(i in teams$teams){
grab_a_team = subset(joined, TeamAbbreviation == i)
grab_latest_game = subset(games, games$Away == i | games$Home == i)
grab_latest_game = tail(grab_latest_game,1)
grab_latest_game$home_win = ifelse(grab_latest_game$Home_score > grab_latest_game$Away_score,1,0)
grab_latest_game$description = ifelse(grab_latest_game$Home == i & grab_latest_game$home_win == 1,
                                      paste("W", grab_latest_game$Home_score," - ",grab_latest_game$Away_score),
                                      ifelse(grab_latest_game$Home ==i & grab_latest_game$home_win == 0,
                                      paste("L", grab_latest_game$Home_score, " - ",grab_latest_game$Away_score),
                                      ifelse(grab_latest_game$Away == i & grab_latest_game$home_win == 1,
                                             paste("L", grab_latest_game$Away_score, " - ",grab_latest_game$Home_score),
                                                   ifelse(grab_latest_game$Away == i & grab_latest_game$home_win ==0,
                                                          paste("W",grab_latest_game$Away_score, " - ", grab_latest_game$Home_score), "oops u messed up"))))
  
grab_latest_game$matchup = ifelse(grab_latest_game$Home == i, paste(grab_latest_game$Home, " v ", grab_latest_game$Away),
                                  paste(grab_latest_game$Away, " v ", grab_latest_game$Home))

matrix_test = grab_a_team %>% select(Player_id,Names, Minutes, Usage, TeamAbbreviation, started, game_number, minutes_as_starter)
matrix_test = cbind(matrix_test, grab_latest_game$matchup)
matrix_test = cbind(matrix_test, grab_latest_game$description)
matrix_test$matchup = matrix_test$`grab_latest_game$matchup`
matrix_test$`grab_latest_game$matchup` = NULL
matrix_test$description = matrix_test$`grab_latest_game$description`
matrix_test$`grab_latest_game$description` = NULL
matrix_test[, 8:9] = sapply(matrix_test[, 8:9], as.character)
hold_final_results = rbind(hold_final_results, matrix_test)
print(i)
}


hold_previous_gamelog$GamesPlayed = NULL
hold_final_results = rbind(hold_final_results, hold_previous_gamelog)


#### sparks
### seattle
### washington


write.csv(hold_final_results, file = "new_final_results.csv", row.names = FALSE)







