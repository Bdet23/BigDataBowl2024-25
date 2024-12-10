
library(dplyr)
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

filtered_data <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, nflId, routeRan, teamAbbr
FROM player_play
WHERE wasRunningRoute=1
;")

f_player_play <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, pff_passCoverage, possessionTeam
FROM plays
WHERE isDropback=1
;")
f_play <- dbFetch(res, -1)
dbClearResult(res)

group_play <- f_player_play %>% group_by(gameId, playId, teamAbbr) %>%
  summarise(
    route_tree = paste(routeRan, collapse=" ")
  )

cov_route <- f_play %>% merge(group_play, by=c("playId", "gameId"))

matchups <- cov_route %>% group_by(route_tree, pff_passCoverage) %>% 
  summarise(matchups = n())

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM all_weeks AS a
LEFT JOIN plays AS b
ON a.gameId=b.gameId 
AND a.playId=b.playId
WHERE a.event = 'ball_snap'
AND b.isDropback = 1;")

game_data <- dbFetch(res, -1)
dbClearResult(res)

game_data <- game_data[,-c(19,20)]

testing_info <- game_data %>% merge(f_player_play, by = c("gameId","playId", "nflId"))
testing_info <- testing_info %>% select("gameId","playId","nflId","y","routeRan", "club") %>% arrange(gameId, playId, y)

group_play <- testing_info %>% group_by(gameId, playId, club) %>%
  summarise(
    route_tree = paste(routeRan, collapse=" ")
  )

cov_route <- f_play %>% merge(group_play, by=c("playId", "gameId"))

matchups <- cov_route %>% group_by(route_tree, pff_passCoverage) %>% 
  summarise(matchups = n())
library(stringr)
reverse_route <- function(route) {
  paste(rev(unlist(str_split(route, " "))), collapse = " ")
}

mirror <- matchups %>%
  mutate(reversed_route = sapply(route_tree, reverse_route))

mirror_match <- matchups %>%
  # Join the data frame to itself to find matching route trees and reversed routes
  left_join(mirror, by = c("pff_passCoverage", "route_tree" = "reversed_route"))

mirror_match$matchups.y[is.na(mirror_match$matchups.y)] <- 0

mirror_match$total_matchups <- mirror_match$matchups.x + mirror_match$matchups.y

unique(mirror_match$pff_passCoverage)

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, pff_primaryDefensiveCoverageMatchupNflId, pff_defensiveCoverageAssignment
FROM player_play;")

coverage_data <- dbFetch(res, -1)
dbClearResult(res)

test <- f_player_play %>% left_join(coverage_data, by=c("gameId", "playId", "nflId"="pff_primaryDefensiveCoverageMatchupNflId"))
test <- test %>% filter(!is.na(pff_defensiveCoverageAssignment))

test<-test %>% mutate(route_def_combo = str_c(routeRan, pff_defensiveCoverageAssignment, sep = " - "))

matchups_info <- test %>% group_by(route_def_combo) %>%
  summarise(num = n())
library(ggplot2)

play_data <- testing_info %>% filter(playId == 1102)  # replace with actual playId

# Plot each player's position and label with the route ran
ggplot(play_data, aes(x = x, y = y, label = routeRan)) +
  geom_point(color = "blue", size = 3) +               # Plot points at each (x, y) coordinate
  geom_text(vjust = -0.5, hjust = 0.5, color = "red") + # Add route label slightly offset from point
  labs(title = paste("Player Routes for Play ID:", unique(play_data$playId)), 
       x = "X Coordinate", y = "Y Coordinate") +
  xlim(0, 120) +                                      
  ylim(0, 53.3) +
  theme_minimal()

f_player_play %>% filter(gameId==2022090800, playId==1102)
                                     