library(shiny)
library(dplyr)
library(grid)
library(teamcolors)
library(farver)

#Below commented code allows yall to create the data df

#cleaned_player_data <- read.csv("/Users/wyattbellinger/Downloads/cleaned_player_data.csv")
#week1 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_1.csv")
#week2 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_2.csv")
#week3 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_3.csv")
#week4 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_4.csv")
#week5 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_5.csv")
#week6 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_6.csv")
#week7 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_7.csv")
#week8 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_8.csv")
#week9 <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/tracking_week_9.csv")

#tracking_data <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9)

#cleaned_tracking_data <- tracking_data %>% semi_join(cleaned_player_data, by = c("gameId", "playId"))

#data <- cleaned_tracking_data %>%
#  left_join(
#    cleaned_player_data %>%
#      select(gameId, playId, frameId, nflId, throw_speed, fx, fy),
#    by = c("gameId", "playId", "nflId", "frameId")
#  ) %>%
#  select(gameId, playId, nflId, displayName, frameId, frameType, time, jerseyNumber,club, x, y, s, event, throw_speed, fx, fy)

#write.csv(data, "cleaned_data.csv", row.names = FALSE)

partial_radius <- function(player_speed, 
                           player_x, player_y, ball_speed, ball_x, ball_y){
  radius <- c(numeric(360))
  times <- c(numeric(360))
  pos_x <- c(numeric(360))
  pos_y <- c(numeric(360))
  for (i in 1:360){
    
    player_angle <- i * (pi / 180) 
    objective <- function(vars){
      time <- vars[1]
      ball_angle <- vars[2]
      if (time < 0){
        return (1e10)
      }
      player_x_int <- cos(player_angle)*player_speed*time + player_x
      ball_x_int   <- cos(ball_angle)*ball_speed*time + ball_x
      player_y_int <- sin(player_angle)*player_speed*time + player_y
      ball_y_int   <- sin(ball_angle)*ball_speed*time + ball_y
      
      sum((player_x_int-ball_x_int)^2 + (player_y_int-ball_y_int)^2)
    }  
    
    # finding a good guess
    t <- sqrt((player_x - ball_x)^2 + (player_y - ball_y)^2) / 
      max(player_speed, ball_speed, 1e-6)
    angle_guess <- atan2(player_y - ball_y, player_x - ball_x)
    init_guess <- c(t, angle_guess)
    
    solution <- optim(
      par = init_guess,
      fn = objective,
      method = "L-BFGS-B",
      lower = c(0.000001, -pi),
      upper = c(Inf, pi)
    )
    
    time       <- solution$par[1]
    ball_angle <- solution$par[2]
    
    # Proposed intercept
    x1 <- cos(player_angle)*player_speed*time + player_x
    y1 <- sin(player_angle)*player_speed*time + player_y

    rad <- sqrt((x1 - player_x)^2 + (y1 - player_y)^2)
    radius[i] <- rad
    times[i] <- time
    pos_x[i] <- x1
    pos_y[i] <- y1
  }
  
  df <- data.frame(angle = 1:360, radius = radius)
  return(df)
}

data <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/cleaned_data.csv")
plays <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/plays.csv")
games <- read.csv("/Users/wyattbellinger/Projects/BigDataBowl2024-25/Data/games.csv")

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
  TEN = "Tennessee Titans", WAS = "Washington Commanders"
)

nfl_teams <- subset(teamcolors, league == "nfl")
nfl_team_colors <- setNames(nfl_teams$primary, nfl_teams$name)
nfl_team_secondary_colors <- setNames(nfl_teams$secondary, nfl_teams$name)

time_to_seconds <- function(time_str) {
  parts <- unlist(strsplit(time_str, ":"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

seconds_to_time <- function(total_seconds) {
  if (total_seconds < 0) total_seconds <- 0
  minutes <- floor(total_seconds / 60)
  seconds <- floor(total_seconds %% 60)
  time_str <- sprintf("%d:%02d", minutes, seconds)
  return(time_str)
}

lab_distance <- function(color1, color2) {
  rgb1 <- as.numeric(col2rgb(color1))
  rgb2 <- as.numeric(col2rgb(color2))
  lab1 <- farver::convert_colour(matrix(rgb1, nrow = 1), from = "rgb", to = "lab")
  lab2 <- farver::convert_colour(matrix(rgb2, nrow = 1), from = "rgb", to = "lab")
  return(sqrt(sum((lab1 - lab2) ^ 2)))
}

plot_play_movement <- function(game_id, play_id, data, plays, games, frame_number,
                               new_page = TRUE) {
  
  if (new_page) grid.newpage()

  play_data <- subset(data, gameId == game_id & playId == play_id)
  if (nrow(play_data) == 0) {
    stop("No data available for the specified gameId and playId.")
  }

  play_info <- subset(plays, gameId == game_id & playId == play_id)
  if (nrow(play_info) == 0) {
    stop("No matching play found in 'plays' data frame.")
  }

  possession_team_code <- play_info$possessionTeam[1]
  defensive_team_code  <- play_info$defensiveTeam[1]

  game_info <- subset(games, gameId == game_id)
  if (nrow(game_info) == 0) {
    stop("No matching game found in 'games' data frame.")
  }
  
  home_team_abbr <- toupper(game_info$homeTeamAbbr[1])
  away_team_abbr <- toupper(game_info$visitorTeamAbbr[1])
  
  preSnapVisitorScore <- play_info$preSnapVisitorScore[1]
  preSnapHomeScore    <- play_info$preSnapHomeScore[1]

  if (possession_team_code == home_team_abbr) {
    offense_score <- preSnapHomeScore
    defense_score <- preSnapVisitorScore
    offense_team  <- team_name_mapping[[home_team_abbr]]
    defense_team  <- team_name_mapping[[away_team_abbr]]
  } else if (possession_team_code == away_team_abbr) {
    offense_score <- preSnapVisitorScore
    defense_score <- preSnapHomeScore
    offense_team  <- team_name_mapping[[away_team_abbr]]
    defense_team  <- team_name_mapping[[home_team_abbr]]
  } else {
    stop("Possession team does not match home or away team in 'games' data frame.")
  }

  possession_team <- team_name_mapping[[possession_team_code]]
  defensive_team  <- team_name_mapping[[defensive_team_code]]
  
  team_colors <- nfl_team_colors[c(possession_team, defensive_team)]
  names(team_colors) <- c(possession_team, defensive_team)

  team_names <- c(possession_team, defensive_team)
  for (i in 1:(length(team_colors) - 1)) {
    for (j in (i + 1):length(team_colors)) {
      if (lab_distance(team_colors[i], team_colors[j]) < 40) {
        team_name <- team_names[i]
        team_colors[i] <- nfl_team_secondary_colors[[team_name]]
      }
    }
  }
  
  football_color <- "brown"
  frame_ids <- sort(unique(play_data$frameId))

  if (!frame_number %in% frame_ids) {
    stop(paste("frame_number must be one of:", paste(frame_ids, collapse = ", ")))
  }

  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = unit(c(4, 1), "null"))))

  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  padding_left   <- unit(5, "mm")
  padding_right  <- unit(5, "mm")
  padding_bottom <- unit(5, "mm")
  padding_top    <- unit(5, "mm")
  
  pushViewport(viewport(x = 0.5, y = 0.5, 
                        width  = unit(1, "npc") - (padding_left + padding_right),
                        height = unit(1, "npc") - (padding_bottom + padding_top),
                        xscale = c(0, 120), yscale = c(-5, 58.3), name = "main_vp"))

  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(120, "native"), height = unit(53.3, "native"),
            just = c("left","bottom"), gp = gpar(fill = "white", col = "darkgreen", lwd = 2))

  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left","bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))
  grid.rect(x = unit(110, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left","bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))

  yard_labels    <- c(10, 20, 30, 40, 50, 40, 30, 20, 10)
  yard_positions <- seq(20, 100, by = 10)
  for (i in seq_along(yard_positions)) {
    yard <- yard_positions[i]
    grid.lines(x = unit(c(yard, yard), "native"),
               y = unit(c(0, 53.3), "native"),
               gp = gpar(col = "lightgray", lty = "dashed"))
    grid.text(label = yard_labels[i], x = unit(yard, "native"), 
              y = unit(1.7, "native"), gp = gpar(col = "darkgray", fontsize = 13, fontface = "bold"))
    grid.text(label = yard_labels[i], x = unit(yard, "native"), 
              y = unit(51.6, "native"), gp = gpar(col = "darkgray", fontsize = 13, fontface = "bold"))
  }

  title_label <- paste0(offense_team, ": ", offense_score, " vs. ", 
                        defense_team, ": ", defense_score)
  grid.text(label = title_label,
            x = unit(60, "native"), y = unit(58.3 - 1, "native"),
            gp = gpar(fontsize = 14, fontface = "bold"))

  initial_gameClock_str <- play_info$gameClock[1]
  total_seconds         <- time_to_seconds(initial_gameClock_str)
  updated_gameClock_str <- seconds_to_time(total_seconds - (frame_number - 1) * 0.1)
  
  game_clock_grob <- textGrob(label = paste("Game Clock:", updated_gameClock_str),
                              x = unit(60, "native"), y = unit(58.3 - 3, "native"),
                              gp = gpar(col = "black", fontsize = 12, fontface = "bold"),
                              name = "game_clock")
  grid.draw(game_clock_grob)
  
  popViewport()
  popViewport()

  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  
  legend_padding <- unit(5, "mm")
  legend_frame <- frameGrob(
    name = "legend_frame", 
    vp   = viewport(width = unit(1, "npc") - 2 * legend_padding, 
                    height= unit(1, "npc") - 2 * legend_padding)
  )

  legend_labels <- c(offense_team, defense_team, "Football")
  legend_colors <- c(team_colors, football_color)
  legend_items  <- list()
  
  for (i in seq_along(legend_labels)) {
    if (legend_labels[i] != "Football") {

      symbol_grob <- linesGrob(x = unit.c(unit(0, "mm"), unit(10, "mm")),
                               y = unit(0.5, "npc"), 
                               gp = gpar(col = legend_colors[i], lwd = 2))
    } else {

      symbol_grob <- circleGrob(x = unit(5, "mm"), y = unit(0.5, "npc"), r = unit(3, "mm"),
                                gp = gpar(fill = legend_colors[i], col = legend_colors[i]))
    }
    label_grob <- textGrob(label = legend_labels[i], x = unit(15, "mm"), y = unit(0.5, "npc"),
                           just = "left", gp = gpar(col = "black", fontsize = 10))
    legend_item <- gTree(children = gList(symbol_grob, label_grob))
    legend_items[[i]] <- legend_item
  }
  
  for (i in seq_along(legend_items)) {
    legend_frame <- packGrob(legend_frame, legend_items[[i]], side = "top", height = unit(1, "null"))
  }
  
  grid.draw(legend_frame)
  popViewport() 

  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  pushViewport(viewport(x = 0.5, y = 0.5, 
                        width  = unit(1, "npc") - (padding_left + padding_right),
                        height = unit(1, "npc") - (padding_bottom + padding_top),
                        xscale = c(0, 120), yscale = c(-5, 58.3), name = "dynamic_vp"))

  frame_data <- subset(play_data, frameId == frame_number)
  if (nrow(frame_data) == 0) {
    stop(paste("No data available for frame number", frame_number))
  }
  
  dynamic_grob <- gTree(name = "dynamic_content")

  unique_players <- unique(frame_data$displayName)
  
  for (player in unique_players) {

    player_data <- subset(frame_data, displayName == player)

    player_data <- player_data[1, ]

    x_pos <- player_data$x
    y_pos <- player_data$y
    jersey <- player_data$jerseyNumber
    
    if (player == "football") {
      dynamic_grob <- addGrob(dynamic_grob,
                              pointsGrob(x = unit(x_pos, "native"),
                                         y = unit(y_pos, "native"),
                                         pch = 16, size = unit(2, "native"),
                                         gp = gpar(col = football_color))
      )
    } else {
      player_team_code <- player_data$club
      player_team_name <- team_name_mapping[[player_team_code]]
      if (!is.null(player_team_name) && player_team_name %in% names(team_colors)) {
        player_color <- team_colors[player_team_name]
      } else {
        player_color <- "black"
      }

      if (!is.na(player_data$throw_speed) &&
          !is.na(player_data$fx) &&
          !is.na(player_data$fy)) {
        
        df_poly <- partial_radius(
          player_speed = player_data$s,
          player_x     = x_pos,
          player_y     = y_pos,
          ball_speed   = player_data$throw_speed,
          ball_x       = player_data$fx,
          ball_y       = player_data$fy
        )

        angles_rad <- df_poly$angle * pi / 180
        x_coords <- x_pos + df_poly$radius * cos(angles_rad)
        y_coords <- y_pos + df_poly$radius * sin(angles_rad)

        x_coords[x_coords < 0]      <- 0
        x_coords[x_coords > 120]    <- 120
        y_coords[y_coords < 0]      <- 0
        y_coords[y_coords > 53.3]   <- 53.3
        
        dynamic_grob <- addGrob(dynamic_grob,
                                polygonGrob(
                                  x = unit(x_coords, "native"),
                                  y = unit(y_coords, "native"),
                                  gp = gpar(fill = "blue", col = "blue", alpha = 0.2)
                                )
        )
      }

      dynamic_grob <- addGrob(dynamic_grob,
                              circleGrob(x = unit(x_pos, "native"),
                                         y = unit(y_pos, "native"),
                                         r = unit(4, "mm"),
                                         gp = gpar(fill = player_color, col = player_color))
      )
      dynamic_grob <- addGrob(dynamic_grob,
                              textGrob(label = jersey,
                                       x = unit(x_pos, "native"),
                                       y = unit(y_pos, "native"),
                                       gp = gpar(col = "white", fontsize = 8, fontface = "bold"))
      )
    }
  }

  grid.draw(dynamic_grob)
  
  popViewport()
  popViewport()
  popViewport()
}


ui <- fluidPage(
  titlePanel("NFL Play Movement Visualization (With partial_radius Shapes)"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("prev_frame", "Previous Frame"),
      actionButton("next_frame", "Next Frame"),
      br(),
      textOutput("frame_info")
    ),
    
    mainPanel(
      plotOutput("playPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {

  current_gameId <- 2022091200
  current_playId <- 109

  rv <- reactiveValues(frame = 1)

  total_frames <- reactive({
    max(subset(data, gameId == current_gameId & playId == current_playId)$frameId, na.rm = TRUE)
  })
  
  observeEvent(input$next_frame, {
    if (rv$frame < total_frames()) {
      rv$frame <- rv$frame + 1
    }
  })
  
  observeEvent(input$prev_frame, {
    if (rv$frame > 1) {
      rv$frame <- rv$frame - 1
    }
  })
  
  output$frame_info <- renderText({
    paste("Current Frame:", rv$frame, "of", total_frames())
  })
  
  output$playPlot <- renderPlot({
    req(data, plays, games)
    frame <- rv$frame
    
    plot_play_movement(
      game_id     = current_gameId,
      play_id     = current_playId,
      data        = data,
      plays       = plays,
      games       = games,
      frame_number= frame,
      new_page    = FALSE
    )
  }, height = 600, width = 800)
}

shinyApp(ui = ui, server = server)
