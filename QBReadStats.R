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
SELECT *
FROM dropback_timing;
")

dropback <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM seconds_data;
")

seconds <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM reads_data;
")

reads <- dbFetch(res, -1)
dbClearResult(res)

reads <- reads %>% unique()

read_info <- dropback %>% merge(reads, by=c("gameId", "playId"), all.x=T) %>%
  merge(seconds, by=c("gameId", "playId"), all.y=T)  

read_info <- read_info %>% arrange(gameId, playId, frameId)

data <- data[,-c(19,20)]

data <- data %>% merge(read_info, by=c("gameId", "playId", "frameId")) %>%
  filter(dropbackType == "TRADITIONAL" & unblockedPressure == 0)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM player_play;
")

player_play <- dbFetch(res, -1)
dbClearResult(res)

player_play <- player_play %>% select(gameId, playId, nflId, wasTargettedReceiver)

data <- data %>% merge(player_play, by=c("gameId", "playId", "nflId"))

unique(data$event)

fill_na_with_nearest <- function(df, col_range) {
  # Step 1: Extract the target columns
  target_cols <- df[, col_range]
  
  # Step 2: Replace NAs row-wise with nearest column values
  filled_cols <- t(apply(target_cols, 1, function(row) {
    for (i in seq_along(row)) {
      if (is.na(row[i])) {
        # Find the nearest non-NA column in the range
        distances <- abs(seq_along(row) - i)
        non_na_indices <- which(!is.na(row))
        if (length(non_na_indices) > 0) {
          nearest_index <- non_na_indices[which.min(distances[non_na_indices])]
          row[i] <- row[nearest_index]  # Replace NA with nearest value
        }
      }
    }
    return(row)
  }))
  
  # Step 3: Replace the original columns with the processed columns
  df[, col_range] <- filled_cols
  return(df)
}


throw <- data %>% filter(event == "pass_arrived" & wasTargettedReceiver == 1)

test <- throw %>% select(gameId, playId, nflId, timeToThrow, firstRead, secondRead, thirdRead, fourthRead, fifthRead, firstTarget, secondTarget, thirdTarget, fourthTarget, fifthTarget)
  # Step 1: Compute throw_id
throw$throw_id <- round(throw$timeToThrow, digits = 1) * 10
  
  # Step 2: Get the columns to compare (67 to 71)
target_cols <- throw[, c(67, 68, 69, 70, 71)]
  
  # Step 3: Compute absolute differences
diffs <- abs(target_cols - throw$throw_id)
  
  # Step 4: Find the index of the column with the minimum difference for each row
min_indices <- max.col(-diffs, ties.method = "first")  # max.col works with matrix-like structures
  
  # Step 5: Map to actual column numbers
actual_columns <- c(67, 68, 69, 70, 71)[min_indices]
  
  # Add the expected column to the data
throw$expected_column <- actual_columns + 5

throw <- fill_na_with_nearest(throw, 72:76)
  # Step 7: Check if the expected player matches the actual receiver
throw$is_expected_receiver <- with(throw, {
  nflId_expected <- mapply(function(row, col) throw[row, col], seq_len(nrow(throw)), throw$expected_column)
  nflId_expected == throw$nflId
})
  
# Step 8: Return the relevant columns
throw <- throw %>% select(gameId, playId, is_expected_receiver)

sum(is.na(throw$is_expected_receiver))

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM players;
")

players <- dbFetch(res, -1)
dbClearResult(res)

players <- players %>% select(nflId, position)

qb_data <- data %>% merge(players, by="nflId") %>% filter(position == "QB")  %>%
  distinct(gameId, playId, .keep_all = TRUE) %>% select(gameId, playId, nflId)
qb_data <- qb_data %>% merge(throw, by = c("gameId", "playId"))

results <- qb_data %>% group_by(nflId) %>% summarise(
  expected_read_percentage = sum(is_expected_receiver)/n()
)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM players
WHERE position = 'QB';
")

qbs <- dbFetch(res, -1)
dbClearResult(res)

results <- results %>% merge(qbs, by="nflId")
