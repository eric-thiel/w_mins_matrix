### for wnba scraping stats :]
library(httr)
library(dplyr)
library(rvest)
library(xml2)
library("rvest")
library(XML)

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

# team_abbrevs = c("ATL","CHI","CON","DAL","IND","LAS","LVA","MIN",
#                 "NYL","PHO","SEA","WAS")

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


Player_id = as.data.frame(Player_id)
Names = as.data.frame(Names)
Minutes = as.data.frame(Minutes)
Usage = as.data.frame(Usage)
TeamAbbreviation = as.data.frame(TeamAbbreviation)

to_this_date = cbind(Player_id, Names)
to_this_date = cbind(to_this_date, Minutes)
to_this_date = cbind(to_this_date, Usage)
to_this_date = cbind(to_this_date, TeamAbbreviation)
to_this_date$Player_id = as.character(to_this_date$Player_id)
to_this_date$Names  = as.character(to_this_date$Names)
to_this_date$TeamAbbreviation  = as.character(to_this_date$TeamAbbreviation)
to_this_date$Player_id = as.numeric(to_this_date$Player_id)
to_this_date$Minutes = as.character(to_this_date$Minutes)
to_this_date$Minutes = as.numeric(to_this_date$Minutes)
to_this_date$Usage = as.character(to_this_date$Usage)
to_this_date$Usage = as.numeric(to_this_date$Usage)
to_this_date$Usage = ifelse(is.na(to_this_date$Usage),0,to_this_date$Usage)


### here is where I am going to have to subtract the previous days totals to get single game totals.
### total - running count == new statistics






joined = left_join(to_this_date, starters[c("Minutes","started","Player_id")], by = c("Player_id"="Player_id"))
## minutes as starter doesn't really matter

joined = joined %>% rename("Minutes"="Minutes.x","minutes_as_starter"="Minutes.y")
joined$started = ifelse(is.na(joined$started),0,joined$started)
joined$minutes_as_starter = ifelse(is.na(joined$minutes_as_starter),0,joined$minutes_as_starter)


g = jsonlite::fromJSON("https://api.pbpstats.com/get-games/wnba?Season=2020&SeasonType=Regular%2BSeason")

Home = g[["results"]][["HomeTeamAbbreviation"]]
Away = g[["results"]][["AwayTeamAbbreviation"]]
Home_score = g[["results"]][["HomePoints"]]
Away_score = g[["results"]][["AwayPoints"]]

Home = as.data.frame(Home)
Away = as.data.frame(Away)
Home_score = as.data.frame(Home_score)
Away_score = as.data.frame(Away_score)

games = cbind(Home, Away)
games = cbind(games, Home_score)
games = cbind(games, Away_score)

games[, 1:4] = sapply(games[, 1:4], as.character)

games$game_number = rownames(games)





