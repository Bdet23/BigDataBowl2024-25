library(RSQLite)
library(nleqslv)
library(stats)
library(dplyr)
dcon <- dbConnect(SQLite(), "BDB2025.db")

dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
                   SELECT * FROM all_weeks as a
                   LEFT JOIN plays as b ON a.playId = b.playId
                   AND a.gameId = b.gameId 
                   WHERE isDropback == 1 AND
                   frameType = 'AFTER_SNAP'")


data <- dbFetch(res, -1)



player_speed <- 10
player_x <- 15
player_y <- 8
ball_speed <- 20
ball_x <- 0
ball_y <- 0
i <- 30
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
      
      ball_x_int <- cos(ball_angle)*ball_speed*time + ball_x
      
      player_y_int <- sin(player_angle)*player_speed*time + player_y
      ball_y_int <- sin(ball_angle)*ball_speed*time + ball_y
      
      sum((player_x_int-ball_x_int)^2 + (player_y_int-ball_y_int)^2)
    }  
    
    
    #finding a good guess
    t <- 2
    player_x_guess <- cos(player_angle)*player_speed*t + player_x
    player_y_guess <- sin(player_angle)*player_speed*t + player_y
    
    angle_guess <- atan2(player_y_guess - ball_y, player_x_guess - ball_x)
    t <- sqrt((player_x - ball_x)^2 + (player_y - ball_y)^2) / max(player_speed, ball_speed)
    angle_guess <- atan2(player_y - ball_y, player_x - ball_x)
    init_guess <- c(t, angle_guess)
    
    
    
    solution <- optim(
      par = init_guess,
      fn = objective,
      method = "L-BFGS-B",
      lower = c(0.000001, -pi),
      upper = c(Inf, pi)
      
    )
    
    time <- solution$par[1]
    ball_angle <- solution$par[2]
    
    y1 <- sin(player_angle)*player_speed*time + player_y
    y2 <- sin(ball_angle)*ball_speed*time + ball_y
    
    x1 <- cos(player_angle)*player_speed*time + player_x
    x2 <- cos(ball_angle)*ball_speed*time + ball_x
    
    
    intercept_x <- x1
    intercept_y <- y1
    
    rad <- sqrt((intercept_x - player_x)^2 + (intercept_y - player_y)^2)
    
    radius[i] <- rad
    times[i] <- time
    pos_x[i] <- intercept_x
    pos_y[i] <- intercept_y
    
  }
  
  return(list(radius, times, pos_x, pos_y))
}



offensive_data <- partial_radius(10, 15, 8, 20, 3, 5)



#revised version
#defensive player input will be a vector with n elements
#each element will have 3 elements within the defensive player speed
#dplayer_speed, the defensive player x and y; dplayer_x & dplayer_y
#to calculate time we will see the intersection point and calculate 
#the time it would take the ball to get there

#this function assumes that in denfensive player data the order is 
#dplayer_x, dplayer_y, and dplayer_speed

oplayer_speed <- 10
oplayer_x <- 15
oplayer_y <- 8


overlap <- function(oplayer_speed, 
                    oplayer_x, oplayer_y, defensive_player_data, 
                    ball_speed, ball_x, ball_y){
  
  #calculating offensive players radius 
  offensive_data <- partial_radius(oplayer_speed, oplayer_x, oplayer_y,
                                   ball_speed, ball_x, ball_y)
  
  #setting values based on the offensive data
  rad <- offensive_data[[1]]
  #time <- offensive_data[2]
  #a vector where the 1st element is the x positions 
  #and the 2nd is the y positions
  off_pos_x <- offensive_data[[2]]
  off_pos_y <- offensive_data[[3]]
  
  #max radius to help set the box
  omax <- max(rad, 1)
  
  #setting the simulated restrictions with the field as restrictions also
  minx <- max(min(oplayer_x - omax), 0)

  maxx <- min(max(oplayer_x + omax), 120)

  miny <- max(min(oplayer_y - omax), 0)

  maxy <- min(max(oplayer_y + omax), 53.3)

  
  #taking area of restricted box with 100 points per square yard
  num_sims <- abs((maxx - minx) * (maxy - miny) * 100)

  #running x and y simulated data
  xsim <- runif(num_sims, min = minx, max = maxx)
  ysim <- runif(num_sims, min = miny, max = maxy)
  
  #finding the time it would take the ball to reach each simulated point
  time <- sqrt((xsim - ball_x)^2 + (ysim - ball_y)^2) / ball_speed
  
  #go through each defender to define if they can reach the simulated point
  #before the ball
  dopen <- data.frame(matrix(nrow=num_sims, ncol=length(defensive_player_data)))
  for (i in 1:length(defensive_player_data)){
    #defensive player distance to ball
    def_rad <- sqrt((defensive_player_data[[i]][[1]] - xsim)^2 +
                      (defensive_player_data[[i]][[2]] - ysim)^2)
    def_player_speed <- defensive_player_data[[i]][[3]]
    dopen[[i]] <- time * def_player_speed > def_rad
    
  }
  
  #calculating the offesive player distanct to simulated point
  o_xdist <- oplayer_x - xsim
  o_ydist <- oplayer_y - ysim
  
  #angle between off player and bal turned output into 0-360 instead of -180-180
  o_angle <- ifelse(atan2(o_ydist, o_xdist) >=0, atan2(o_ydist, o_xdist), 2*pi+atan2(o_ydist, o_xdist))
  
  #measuring if the offsive player can reach the simulated point
  #o_radius <- partial_radius(oplayer_speed, oplayer_x, 
                             #oplayer_y, ball_speed, ball_x, ball_y)
  o_radius <- rad
  
  o_dist_to_point <- sqrt(o_xdist^2 + o_ydist^2)
  o_open <- sapply(seq_along(o_dist_to_point), function(i) {
    # Ensure the index is within bounds
    index <- pmax(1, pmin(length(o_radius), round(o_angle[i] * 180 / pi)))
    # Compare the radius at the computed index with the distance to point
    o_radius[index] >= o_dist_to_point[i]
  })
  
  #creating a binary matrix 1st col is the offensive player openness for each point
  #other columns is the denfensive players openess
  openness_matrix <- data.frame(o_open, dopen)
  open_cond <- ifelse(openness_matrix[, 1] == 1, 
                      ifelse(rowSums(openness_matrix[, -1]) == 0, 1, 
                             ifelse(rowSums(openness_matrix[, -1]) > 1, 
                                    -0.2 * rowSums(openness_matrix[, -1]), 0)), 0)
  
  
  open_count <- sum(open_cond)
  
  
  return(open_count)
}

defensive_player_data <- list(c(0, 0, 7), c(10, 12, 5), c(16, 30, 8))
defensive_player_data[[1]][[1]]
overlap(10, 15, 8, defensive_player_data, 20, 0, 0)


res <- dbSendQuery(conn = dcon, "
SELECT *
FROM cleaned_player_data")

data <- dbFetch(res, -1)
dbClearResult(res)

data <- data %>% arrange(gameId, playId, frameId)


library(future.apply)

# Plan for parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Chunk size for iterative processing
chunk_size <- 100

# Function to process a single chunk
process_chunk <- function(data_chunk) {
  data_chunk$open_count <- apply(data_chunk, 1, function(row) {
    oplayer_speed <- as.numeric(row["s"])
    oplayer_x <- as.numeric(row["x"])
    oplayer_y <- as.numeric(row["y"])
    
    # Defensive player data
    defensive_player_data <- lapply(1:11, function(i) {
      list(as.numeric(row[paste0("x_", i)]), 
           as.numeric(row[paste0("y_", i)]), 
           as.numeric(row[paste0("s_", i)]))
    })
    print(is.numeric(defensive_player_data[[1]][1]))
    # Ball parameters
    ball_speed <- as.numeric(row["throw_speed"])
    ball_x <- as.numeric(row["fx"])
    ball_y <- as.numeric(row["fy"])
    
    # Run the overlap function
    overlap(oplayer_speed, oplayer_x, oplayer_y, defensive_player_data, ball_speed, ball_x, ball_y)
  })
  return(data_chunk)
}

# Split data into chunks
split_indices <- seq(1, nrow(data), by = chunk_size)

chunks <- lapply(split_indices, function(i) {
  data[i:min(i + chunk_size - 1, nrow(data)), ]
})

# Process chunks in parallel
result_chunks <- future_lapply(chunks, process_chunk)
result_chunks[[1]]$open_count
# Combine chunks into a single DataFrame
final_data <- do.call(rbind, result_chunks)

# Clean up parallel resources
plan(sequential)

#dbWriteTable(dcon, "radius_data", final_data)
#dbListTables(dcon)

final_data <- final_data %>%
  arrange(gameId, playId, frameId) %>% # Ensure data is sorted
  group_by(gameId, playId) %>%
  mutate(secondId = dense_rank(frameId) - 1) %>% # Assign rank based on frameId
  ungroup()

test <- final_data %>% filter(is.na(final_data$open_count))

vs_coverage <- final_data %>% group_by(routeRan, pff_defensiveCoverageAssignment, secondId) %>%
  summarise(
    avg_openness = mean(open_count, na.rm=T)
  )

g <- process_chunk(chunks)


data <- a[sample(nrow(a), 100), ]

