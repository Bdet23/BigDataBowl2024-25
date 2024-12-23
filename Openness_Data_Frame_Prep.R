library(tidyverse)
library(RSQLite)

dcon <- dbConnect(SQLite(), dbname = "BDB2025.db")

dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM all_weeks AS a
LEFT JOIN plays AS b
ON a.gameId=b.gameId 
AND a.playId=b.playId
WHERE a.frameType = 'AFTER_SNAP'
AND b.isDropback = 1;")

data <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, nflId, routeRan
FROM player_play
WHERE wasRunningRoute=1
;")

f_player_play <- dbFetch(res, -1)
dbClearResult(res)

unique(data$event)
track_data <- data[,-c(19,20)] %>% select(gameId,playId,nflId,frameId,x,y,s)

oplayer_data <- track_data %>% merge(f_player_play, by=c("gameId", "playId", "nflId"))

def_data <- data[,-c(19,20)] %>% filter(club==defensiveTeam) %>% arrange(gameId,playId, desc(frameId))

def_data$defLabel <- rep(1:11)

defense_pos_data <- def_data %>% pivot_wider(
                      id_cols = c(gameId, playId, frameId),
                      names_from = defLabel,
                      values_from = c(x, y, s),
                      names_sep = "_"
                      )

oplayer_data <- oplayer_data %>% merge(defense_pos_data, by=c("gameId", "playId", "frameId"))

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, pff_primaryDefensiveCoverageMatchupNflId, pff_defensiveCoverageAssignment
FROM player_play;")

coverage_data <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM players;")

players <- dbFetch(res, -1)
dbClearResult(res)

coverage_data <- coverage_data[!is.na(coverage_data$pff_primaryDefensiveCoverageMatchupNflId),]

oplayer_data <- oplayer_data %>% merge(coverage_data, by.x = c("playId", "gameId", "nflId"), by.y = c("playId", "gameId", "pff_primaryDefensiveCoverageMatchupNflId"))

football_data <- data[,-c(19,20)] %>% filter(displayName=="football")




# Get rows with 'forward_pass' and the next 5 rows
throws <- football_data %>%
  filter(row_number() %in% unlist(
    lapply(which(event == "pass_forward"), function(x) (x+2):(x + 7))
  ))

team_data <- data[,-c(19,20)] %>% select(possessionTeam, nflId, gameId, playId) %>% merge(players, by="nflId") %>%
  filter(position=="QB") 

rm(data)
rm(def_data)
rm(def_pos_data)

team_data <- team_data %>% distinct() %>% select(possessionTeam, gameId, playId, QB = displayName)

throws <- throws %>% merge(team_data, by = c("gameId", "playId", "possessionTeam"))

qb_velo <- throws %>% group_by(QB) %>% 
  summarize(throw_speed = median(s, na.rm=TRUE))

throws <- throws %>% merge(qb_velo, by="QB")

throws <- throws %>% select(gameId, playId, throw_speed)

oplayer_data <- oplayer_data %>% merge(throws, by=c("gameId", "playId"))
oplayer_data <- oplayer_data %>% distinct()

fpos <- football_data %>% select(gameId, playId, frameId, fx=x, fy=y)
oplayer_data <- oplayer_data %>% merge(fpos, by=c("gameId", "playId", "frameId")) %>% distinct()
rm(track_data)

dbWriteTable(dcon, "cleaned_player_data", oplayer_data)
dbListTables(dcon)
