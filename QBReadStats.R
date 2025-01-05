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
mthrow <- throw %>% select(gameId, playId, is_expected_receiver)

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
qb_data <- qb_data %>% merge(mthrow, by = c("gameId", "playId"))

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

mean(results$expected_read_percentage)
################ Generating Effective Read Percentage ##################
#This is looking for false positives - their was a better read at time of throw even though QB took expected
#Also looks for bad reads -  the expected read was more open than the actual target

rm(throw_filled)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM radius_data;
")

open_data <- dbFetch(res, -1)
dbClearResult(res)


throw$expected_receiver <- with(throw, {
  nflId_expected <- mapply(function(row, col) throw[row, col], seq_len(nrow(throw)), throw$expected_column)
  nflId_expected
})

needed_throws <- throw %>% select(gameId, playId, target = nflId, expected_receiver, throw_id, is_expected_receiver)

open_data <- open_data %>% merge(needed_throws, by = c("gameId", "playId")) %>% filter(throw_id == secondId)

most_open <- open_data %>% group_by(gameId, playId) %>%
  slice_max(open_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(gameId, playId, most_open_rec = nflId, most_open_count = open_count)

actual_open <- open_data %>% filter(target==nflId) %>% 
  select(gameId, playId, target, target_open_count = open_count)

expected_open <- open_data %>% filter(expected_receiver==nflId) %>% 
  select(gameId, playId, expected_receiver, expected_open_count = open_count)

openness <- most_open %>% merge(actual_open, by=c("gameId", "playId")) %>%
  merge(expected_open, by=c("gameId", "playId"))

openness$correct_read <- ifelse(openness$expected_open_count > openness$target_open_count, 0, 
                                ifelse(openness$target_open_count > openness$expected_open_count, 1,
                                       ifelse(openness$expected_receiver==openness$target & openness$most_open_count > openness$expected_open_count, 0,1)))

openness$fail_to_leave <- ifelse(openness$expected_receiver==openness$target & openness$most_open_count > openness$expected_open_count, 1,0)
openness$hit_pre_snap <- openness$expected_receiver == openness$target

needed_openness <- openness %>% select(gameId, playId, correct_read, fail_to_leave, hit_pre_snap)

qb_data <- qb_data %>% merge(needed_openness, by = c("gameId", "playId"))

effective_read_percentage <- qb_data %>% group_by(nflId) %>% summarise(
  effective_read_percentage = sum(correct_read)/n(),
  stuck_to_pre_rate = sum(fail_to_leave)/n(),
  throw_to_pre_rate = sum(hit_pre_snap)/n(),
  snaps = n()
)


effective_read_percentage <- effective_read_percentage %>% merge(qbs, by="nflId")

openness$is_exp_max <- ifelse(openness$expected_receiver == openness$most_open_rec, 1, 0)

sum(openness$is_exp_max)/5393

effective_read_percentage$PRESS <- (effective_read_percentage$effective_read_percentage / mean(effective_read_percentage$effective_read_percentage)) * 100

qualified_data <- effective_read_percentage %>% filter(snaps >= 100)

qualified_data$qualified_press <- (qualified_data$effective_read_percentage / mean(qualified_data$effective_read_percentage)) * 100

library(gt)
library(htmltools)

# Top 10 rows
top_table <- qualified_data %>% select(Name = displayName, Snaps = snaps, qualified_press) %>%
  arrange(desc(qualified_press)) %>%
  slice(1:10) %>%
  gt() %>%
  tab_header(
    title = "Top 10 QBs"
  )  %>%
  as_raw_html()

# Bottom 10 rows
bottom_table <- qualified_data %>% select(Name = displayName, Snaps = snaps, qualified_press) %>%
  arrange(desc(qualified_press)) %>%
  slice(24:33) %>%
  gt() %>%
  tab_header(
    title = "Bottom 10 QBs"
  )  %>%
  as_raw_html()

# Combine tables side by side
html_output <- htmltools::tags$div(
  style = "display: flex; justify-content: center; gap: 10px;", # Reduced gap for closer tables
  htmltools::tags$div(style = "margin-right: 5px;", HTML(top_table)),
  htmltools::tags$div(style = "margin-left: 5px;", HTML(bottom_table))
)

# Render HTML in Kaggle notebook
html_output
htmltools::save_html(html_output, "side_by_side_tables.html")

# Open in the browser
browseURL("side_by_side_tables.html")

write.csv(qualified_data, file = "press_data.csv", row.names = FALSE)

############## Looking at Motion #####################

res <- dbSendQuery(conn = dcon, "
SELECT gameId, playId, motionSinceLineset
FROM player_play
WHERE motionSinceLineset = True;
")

motion <- dbFetch(res, -1)
dbClearResult(res)

motion <- motion %>% unique()

motion_open <- needed_openness %>% merge(motion, by=c("gameId", "playId")) 

(motion_press <- mean(motion_open$correct_read)/mean(effective_read_percentage$effective_read_percentage) * 100)

############# Looking in the Redzone #################

res <- dbSendQuery(conn = dcon,"
SELECT gameId, playId, absoluteYardlineNumber
FROM plays
WHERE absoluteYardlineNumber <= 20;
")

redzone_plays <-  dbFetch(res, -1)
dbClearResult(res)

redzone_open <- needed_openness %>% merge(redzone_plays, by=c("gameId", "playId"))

(redzone_press <- mean(redzone_open$correct_read)/mean(effective_read_percentage$effective_read_percentage) * 100)

res <- dbSendQuery(conn = dcon,"
SELECT gameId, playId, absoluteYardlineNumber, pff_passCoverage
FROM plays
WHERE absoluteYardlineNumber <= 20;
")

redzone_coverage <-  dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon,"
SELECT gameId, playId, absoluteYardlineNumber, pff_passCoverage
FROM plays;
")

allsit_coverage <-  dbFetch(res, -1)
dbClearResult(res)

redzone_coverage <- redzone_coverage %>% group_by(pff_passCoverage) %>% 
  summarise(times_ran = n()) %>% arrange(times_ran)

allsit_coverage <- allsit_coverage %>% group_by(pff_passCoverage) %>% 
  summarise(times_ran = n()) %>% arrange(times_ran)

redzone_coverage$ratio <- redzone_coverage$times_ran / sum(redzone_coverage$times_ran)
allsit_coverage$ratio <- allsit_coverage$times_ran / sum(allsit_coverage$times_ran)


(num_coverages<-length(unique(redzone_coverage$pff_passCoverage)))

(num_coverages<-length(unique(allsit_coverage$pff_passCoverage)))

plot(redzone_coverage$ratio, cumsum(redzone_coverage$ratio))

library(entropy)

# Normalize frequencies to probabilities
probs_redzone <- redzone_coverage$times_ran / sum(redzone_coverage$times_ran)
probs_normal <- allsit_coverage$times_ran / sum(allsit_coverage$times_ran)

# Calculate entropy
entropy_redzone <- entropy(probs_redzone, unit = "log2") # Entropy in bits
entropy_normal <- entropy(probs_normal, unit = "log2")

# Compare
print(paste("Redzone Entropy:", entropy_redzone))
print(paste("Normal Entropy:", entropy_normal))

cv_redzone <- sd(redzone_coverage$times_ran) / mean(redzone_coverage$times_ran)
cv_normal <- sd(allsit_coverage$times_ran) / mean(allsit_coverage$times_ran)

# Compare
print(paste("Redzone CV:", cv_redzone))
print(paste("Normal CV:", cv_normal))

####### Looking at Components #######


####### Expected Throw EPA ###########

