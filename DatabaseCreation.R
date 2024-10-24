# Install and load necessary libraries
install.packages("RSQLite")
install.packages("readr")
install.packages("dplyr")

library(RSQLite)
library(readr)
library(dplyr)

# Create a connection to SQLite database
con <- dbConnect(SQLite(), dbname = "BDB2025.db")

# 1. Import games.csv
games <- read_csv("data/games.csv")
dbWriteTable(con, "games", games, overwrite = TRUE)

# 2. Import plays.csv
plays <- read_csv("data/plays.csv")
dbWriteTable(con, "plays", plays, overwrite = TRUE)

# 3. Import players.csv
players <- read_csv("data/players.csv")
dbWriteTable(con, "players", players, overwrite = TRUE)

# 4. Import player_play.csv
player_play <- read_csv("data/player_play.csv")
dbWriteTable(con, "player_play", player_play, overwrite = TRUE)

dbWriteTable(con, "data", player_play, overwrite = TRUE)
# 5. Import tracking_week_[week].csv files
# Assume you have tracking data for multiple weeks in a folder
tracking_files <- list.files(path = "path_to_tracking", pattern = "tracking_week_.*\\.csv", full.names = TRUE)
for (file in tracking_files) {
  tracking_data <- read_csv(file)
  dbWriteTable(con, "tracking", tracking_data, append = TRUE)
}

# Size of each chunk to be inserted (e.g., 1 million rows at a time) 
chunk_size <- 300000
# Number of rows in the data frame 
n_rows <- nrow(data) 
data <- data %>% select(-X)

# Create the table in the database by writing the first chunk 
dbWriteTable(con, "all_weeks", data[1:chunk_size, ], overwrite = TRUE)
# Start a transaction for better performance when appending chunks 
dbBegin(con) 
# Write remaining chunks to the database 
for (i in seq(chunk_size + 1, n_rows, by = chunk_size)) { 
  # Define the end of the current chunk 
  end_row <- min(i + chunk_size - 1, n_rows) 
  # Append the chunk to the database 
  dbWriteTable(con, "all_weeks", data[i:end_row, ], append = TRUE) 
  } 
# Commit the transaction 
dbCommit(con)

# Add primary and foreign key relationships
# Primary keys
dbExecute(con, "ALTER TABLE games ADD PRIMARY KEY (gameId);")
dbExecute(con, "ALTER TABLE plays ADD PRIMARY KEY (gameId, playId);")
dbExecute(con, "ALTER TABLE players ADD PRIMARY KEY (nflId);")
dbExecute(con, "ALTER TABLE player_play ADD PRIMARY KEY (gameId, playId, nflId);")
dbExecute(con, "ALTER TABLE tracking ADD PRIMARY KEY (gameId, playId, nflId, frameId);")

# Foreign keys
dbExecute(con, "ALTER TABLE plays ADD FOREIGN KEY (gameId) REFERENCES games(gameId);")
dbExecute(con, "ALTER TABLE player_play ADD FOREIGN KEY (gameId) REFERENCES games(gameId);")
dbExecute(con, "ALTER TABLE player_play ADD FOREIGN KEY (playId) REFERENCES plays(playId);")
dbExecute(con, "ALTER TABLE player_play ADD FOREIGN KEY (nflId) REFERENCES players(nflId);")
dbExecute(con, "ALTER TABLE tracking ADD FOREIGN KEY (gameId) REFERENCES games(gameId);")
dbExecute(con, "ALTER TABLE tracking ADD FOREIGN KEY (playId) REFERENCES plays(playId);")
dbExecute(con, "ALTER TABLE tracking ADD FOREIGN KEY (nflId) REFERENCES players(nflId);")

# Close the connection
dbDisconnect(con)