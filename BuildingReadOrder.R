library(tidyverse)
library(RSQLite)

dcon <- dbConnect(SQLite(), dbname = "BDB2025.db")

dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM radius_data;
")


data <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, pff_primaryDefensiveCoverageMatchupNflId, pff_defensiveCoverageAssignment
FROM player_play;")

coverage_data <- dbFetch(res, -1)
dbClearResult(res)
coverage_data <- coverage_data[!is.na(coverage_data$pff_primaryDefensiveCoverageMatchupNflId),]

data <- data %>% merge(coverage_data, by.x = c("playId", "gameId", "nflId"), by.y = c("playId", "gameId", "pff_primaryDefensiveCoverageMatchupNflId"))

vs_coverage <- data %>% group_by(routeRan, pff_defensiveCoverageAssignment, secondId) %>%
  summarise(
    avg_openness = mean(open_count, na.rm=T)
  )

vs_coverage$time_in_sec <- ifelse(vs_coverage$secondId<=10, 1, 
                                  ifelse(vs_coverage$secondId<=20, 2,
                                         ifelse(vs_coverage$secondId<=30, 3,
                                                ifelse(vs_coverage$secondId<=40, 4,
                                                       ifelse(vs_coverage$secondId<=50, 5, 6)))))

by_sec_cov <- vs_coverage %>% group_by(routeRan, pff_defensiveCoverageAssignment, time_in_sec) %>%
  summarise(
    openness = mean(avg_openness, na.rm=T)
  )

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM plays;
")


plays <- dbFetch(res, -1)
dbClearResult(res)

dropback <- plays %>% select(gameId, playId, dropbackType, dropbackDistance)

data <- data %>% merge(dropback, by=c("gameId", "playId"))

data <- data %>% filter(!dropbackType %in% c("DESIGNED_ROLLOUT_RIGHT", "DESIGNED_ROLLOUT_LEFT", 
                                             "QB_SNEAK", "DESIGNED_RUN"))
unique(data$dropbackType)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM all_weeks AS a
LEFT JOIN plays AS b
ON a.gameId=b.gameId 
AND a.playId=b.playId
WHERE a.frameType = 'AFTER_SNAP'
AND b.isDropback = 1
AND b.dropbackType NOT IN ('DESIGNED_ROLLOUT_RIGHT', 'DESIGNED_ROLLOUT_LEFT', 
                           'QB_SNEAK', 'DESIGNED_RUN');")

df <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM players;")
players <- dbFetch(res, -1)
dbClearResult(res)

players <- players %>% select(nflId, position)
df <- df[,-c(19,20)] %>% select(gameId, playId, nflId, frameId, s,a,dropbackDistance )

df <- df %>% merge(players, by='nflId')
df <- df %>% filter(position == "QB")
# Compute the cumulative distance for each row

df <- df %>% mutate(distance_step = s * 0.1 + 0.5 * a * (0.1^2))
df <- df %>%
  group_by(gameId, playId) %>%  # Group by gameId and playId to reset for each unique pair
  mutate(
    cumulative_distance = cumsum(distance_step)   # Cumulative distance within each group
  ) %>%
  ungroup() %>%  # Ungroup to avoid any unintended grouping in subsequent operations
  arrange(gameId, playId, frameId)  # Ensure the data is ordered correctly

# Group by each target distance and compute time to complete dropback
df <- df %>%
  group_by(gameId, playId) %>%
  mutate(
    time_to_complete_dropback = ifelse(
      cumulative_distance >= dropbackDistance, 
      min(frameId[cumulative_distance >= dropbackDistance]), 
      NA
    )
  ) %>%
  ungroup()

df <- df %>% group_by(gameId, playId) %>% mutate(
    adj_time = time_to_complete_dropback - min(frameId)
  ) %>%
  ungroup()

complete_dropback <- df %>% select(gameId, playId, top_drop_frame = adj_time) %>%
  filter(!is.na(df$time_to_complete_dropback)) %>% unique()

complete_dropback <- complete_dropback %>% mutate(
  firstRead = top_drop_frame,
  secondRead = top_drop_frame +5, 
  thirdRead = top_drop_frame +10,
  fourthRead = top_drop_frame +15,
  fifthRead  = top_drop_frame +20
) %>% select(-top_drop_frame)

data <- data %>% merge(complete_dropback, by=c("gameId","playId"))

build_reads <- function(init_data, cover_data){
  #Looks at the first frame of each play
  filt <- init_data %>% group_by(gameId, playId) %>%
    filter(frameId == min(frameId)) %>%
    ungroup()
  
  #looks at the outside two receivers
  Outside <- filt %>%
    group_by(gameId, playId) %>%
    filter(y == min(y) | y == max(y)) %>% 
    mutate(firstSide = ifelse(y == min(y), "L", "R")) %>% 
    ungroup()
  
  Outside <- Outside %>% left_join(cover_data, by=c("routeRan", "pff_defensiveCoverageAssignment", "firstRead"="secondId"))
  
  result <- Outside %>%
    group_by(gameId, playId) %>%
    filter(avg_openness == max(avg_openness, na.rm = TRUE)) %>%
    slice(1) %>% # In case of ties, select the first one
    ungroup() %>%
    mutate(firstTarget = nflId) %>% select(gameId, playId, firstTarget, firstSide)
  
  reads <- filt %>% select(gameId, playId, nflId, y) %>% 
    merge(result, by=c("gameId", "playId")) %>% arrange(gameId, playId, y) %>% unique()
  
  final_reads <- reads %>%
    group_by(gameId, playId) %>%
    arrange(gameId, playId, if_else(firstSide == "R", -y, y)) %>% # Sort by x or -x depending on direction
    mutate(
      read_order = row_number(),
    ) %>%
    ungroup()
  
  final_reads$target_order <- ifelse(final_reads$read_order == 1, "firstTarget", 
                                     ifelse(final_reads$read_order == 2, "secondTarget",
                                            ifelse(final_reads$read_order == 3, "thirdTarget", 
                                                   ifelse(final_reads$read_order == 4, "fourthTarget", "fifthTarget"))))
  
  first_reads <- final_reads %>% select(gameId, playId, firstTarget)
  second_reads <- final_reads %>% filter(target_order =="secondTarget") %>% 
    mutate(secondTarget = nflId) %>% select(gameId, playId, secondTarget)
  third_reads <- final_reads %>% filter(target_order =="thirdTarget") %>% 
    mutate(thirdTarget = nflId) %>% select(gameId, playId, thirdTarget)
  fourth_reads <- final_reads %>% filter(target_order =="fourthTarget") %>% 
    mutate(fourthTarget = nflId) %>% select(gameId, playId, fourthTarget)
  fifth_reads <- final_reads %>% filter(target_order =="fifthTarget") %>% 
    mutate(fifthTarget = nflId) %>% select(gameId, playId, fifthTarget)
  
  complete_reads <- first_reads %>% 
    merge(second_reads, by=c("gameId", "playId"), all.x=T) %>% 
    merge(third_reads, by=c("gameId", "playId"), all.x=T) %>% 
    merge(fourth_reads, by=c("gameId", "playId"), all.x=T) %>% 
    merge(fifth_reads, by=c("gameId", "playId"), all.x=T)
  
  
  return(complete_reads)
}


reads <- build_reads(data, vs_coverage)

second_ids <- data %>% select(gameId, playId, frameId, secondId) %>% unique()

dbWriteTable(dcon, "reads_data", reads)
dbListTables(dcon)

dbWriteTable(dcon, "seconds_data", second_ids)
dbListTables(dcon)

dbWriteTable(dcon, "dropback_timing", complete_dropback)
dbListTables(dcon)
