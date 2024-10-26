#install.packages("farver")
#install.packages("teamcolors")
library(farver)
library(teamcolors)

team_name_mapping <- list(
  ARI = "Arizona Cardinals", ATL = "Atlanta Falcons", BAL = "Baltimore Ravens",
  BUF = "Buffalo Bills", CAR = "Carolina Panthers", CHI = "Chicago Bears",
  CIN = "Cincinnati Bengals", CLE = "Cleveland Browns", DAL = "Dallas Cowboys",
  DEN = "Denver Broncos", DET = "Detroit Lions", GB  = "Green Bay Packers",
  HOU = "Houston Texans", IND = "Indianapolis Colts", JAX = "Jacksonville Jaguars",
  KC  = "Kansas City Chiefs", LAC = "Los Angeles Chargers", LAR = "Los Angeles Rams",
  LV  = "Las Vegas Raiders", MIA = "Miami Dolphins", MIN = "Minnesota Vikings",
  NE  = "New England Patriots", NO  = "New Orleans Saints", NYG = "New York Giants",
  NYJ = "New York Jets", PHI = "Philadelphia Eagles", PIT = "Pittsburgh Steelers",
  SEA = "Seattle Seahawks", SF  = "San Francisco 49ers", TB  = "Tampa Bay Buccaneers",
  TEN = "Tennessee Titans", WAS = "Washington Commanders")

hex_to_rgb <- function(hex) {
  rgb <- col2rgb(hex) / 255
  return(as.numeric(rgb))
}

lab_distance <- function(color1, color2) {
  rgb1 <- as.numeric(col2rgb(color1))
  rgb2 <- as.numeric(col2rgb(color2))

  lab1 <- farver::convert_colour(matrix(rgb1, nrow = 1), from = "rgb", to = "lab")
  lab2 <- farver::convert_colour(matrix(rgb2, nrow = 1), from = "rgb", to = "lab")

  return(sqrt(sum((lab1 - lab2) ^ 2)))
}

nfl_teams <- subset(teamcolors, league == "nfl")
nfl_team_colors <- setNames(nfl_teams$primary, nfl_teams$name)
nfl_team_secondary_colors <- setNames(nfl_teams$secondary, nfl_teams$name)

plot_play_movement <- function(game_id, play_id, data) {
  play_data <- subset(data, gameId == game_id & playId == play_id)

  teams <- unique(play_data$club[play_data$displayName != "football"])

  full_team_names <- sapply(teams, function(code) team_name_mapping[[code]])
  team_colors <- nfl_team_colors[full_team_names]
  
  for (i in 1:(length(team_colors) - 1)) {
    for (j in (i + 1):length(team_colors)) {
      if (lab_distance(team_colors[i], team_colors[j]) < 40) {
        team_name <- names(team_colors)[i]
        team_colors[i] <- nfl_team_secondary_colors[team_name]
      }
    }
  }
  
  football_color <- "brown"
  
  frame_ids <- sort(unique(play_data$frameId))
  
  for (frame in frame_ids) {
    frame_data <- subset(play_data, frameId <= frame)

    plot.new()
    plot.window(xlim = c(0, 140), ylim = c(0, 53.3))

    rect(0, 0, 120, 53.3, border = "darkgreen", lwd = 2)
    rect(0, 0, 10, 53.3, border = "black", lwd = 2)
    rect(110, 0, 120, 53.3, border = "black", lwd = 2)

    yard_labels <- c(10, 20, 30, 40, 50, 40, 30, 20, 10)
    yard_positions <- seq(20, 100, by = 10)

    for (i in seq_along(yard_positions)) {
      yard <- yard_positions[i]
      abline(v = yard, col = "lightgray", lty = "dashed")
      text(x = yard, y = 1.7, labels = yard_labels[i], col = "darkgray", cex = 1.3, font = 2)
      text(x = yard, y = 51.6, labels = yard_labels[i], col = "darkgray", cex = 1.3, font = 2)
    }

    title(main = paste("Play", play_id, "from", full_team_names[1], 
                       "vs.", full_team_names[2]))

    for (player in unique(frame_data$displayName)) {
      player_data <- subset(frame_data, displayName == player)
      if (player == "football") {
        player_color <- football_color
      } else {
        player_team <- unique(player_data$club)
        full_team_name <- team_name_mapping[[player_team]]
        player_color <- team_colors[full_team_name]
      }
      lines(player_data$x, player_data$y, col = player_color, lwd = 2)
      points(player_data$x[nrow(player_data)], player_data$y[nrow(player_data)], 
             col = player_color, pch = 16)
    }

    legend_labels <- c(teams, "Football")
    legend_colors <- c(team_colors, football_color)
    legend(x = 122, y = 53.3, legend = legend_labels, col = legend_colors, lwd = 2, 
           bg = "white", xpd = TRUE, bty = "n", cex = 1, text.col = "black")

    Sys.sleep(0.1)
  }
}

plot_play_movement(2022101603, 346, data)
