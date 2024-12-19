library(RSQLite)
setwd("/Users/devin/Desktop/BDB/Big_Data_Bowl/")
dcon <- dbConnect(SQLite(), "BDB2025.db")

dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
                   SELECT * FROM all_weeks as a
                   LEFT JOIN plays as b ON a.playId = b.playId
                   AND a.gameId = b.gameId 
                   WHERE isDropback == 1 AND
                   frameType = 'AFTER_SNAP'")


data <- dbFetch(res, -1)

#derive the distance the player can run between the throw time 
#and catch time
radius <- function(playerx, playery, footballx, footbally){
  
  dist <- sqrt((playerx - footballx)^2 + (playery - footbally)^2)
  #in yard/sec. used 16 mph
  player_speed <- 7.822
  
  #avg_ball_speed in yards/sec. Used 40 mph
  ball_speed <- 19.55
  
  #calculation to get the time until intersection and assuming the player
  #is running fully away from the ball
  time <- dist / (ball_speed - player_speed)
  
  #the point at where the ball and player will intersect
  intersection <- ball_speed * time
  
  #the distance travelled between throw and catch 
  radius <- intersection - dist
  
  return(radius)
  
}

radius(10, 12, 0, 0)

debug(radius(10, 12, 0, 0))


over_lapping_radius <- function(gameId, playId, frameId){
  query <- paste("SELECT * FROM all_weeks 
              WHERE gameId ==", gameId, "AND playId ==",
                 playId, "AND frameId == ", frameId)
  res <- dbSendQuery(conn = dcon, "
                   SELECT * FROM all_weeks
                   WHERE gameId ==  AND
                   frameType = 'AFTER_SNAP'")
  
  
}

player_speed <- 10
player_x <- 9
player_y <- 8
ball_speed <- 20
i <- 35

partial_radius <- function(player_speed, 
                           player_x, player_y, ball_speed){
  radius <- c()
  times <- c()
  pos_x <- c()
  pos_y <- c()
  for (i in 1:360){
    
    player_angle <- i * (pi / 180) + pi
    
    
    ball_angle <- acos((player_speed / ball_speed) * 
                         (player_y * cos(player_angle) - player_x * sin(i)) / 
                         sqrt(player_x^2 + player_y^2)) - atan(-player_x / player_y)
    #ball_angle <- ball_angle + pi/2                   
    time <- player_x / ((ball_speed * cos(ball_angle)) - 
                          (cos(i) * player_speed))
    intercept_x <- cos(i) * player_speed * time 
    intercept_y <- sin(i) * player_speed * time 
    
    rad <- sqrt((intercept_x - player_x)^2 + (intercept_y - player_y)^2)
    
    radius <- append(radius, rad)
    times <- append(times, time)
    pos_x <- append(pos_x, intercept_x)
    pos_y <- append(pos_y, intercept_y)
  
  }
  
  
  return(c(radius, times, pos_x, pos_y))
}

partial_radius(10, 15, 8, 10)

overlap <- function(oplayer_speed, 
                    oplayer_x, oplayer_y,  
                    dplayer_speed, dplayer_x, dplayer_y, ball_speed){
  
  
  offensive_data <- partial_radius(angles, oplayer_speed, oplayer_x, oplayer_y, ball_speed)
  
  rad <- offensive_data[1]
  time <- offensive_data[2]
  #a vector where the 1st element is the x positions 
  #and the 2nd is the y positions
  off_pos_x <- offensive_data[3]
  off_pos_y <- offensive_data[4]
  
  
  
  
  
  dcoord <- partial_radius(angles, dplayer_speed, dplayer_x, dplayer_y, ball_speed)
  omax <- max(ocoord)
  dmax <- max(dcoord)
  
  minx <- min(oplayer_x - omax, dplayer_x - dmax)
  maxx <- max(oplayer_x + omax, dplayer_x + dmax)
  
  miny <- min(oplayer_y - omax, dplayer_y - dmax)
  maxy <- max(oplayer_y + omax, dplayer_y + dmax)
  
  num_sims <- (maxx - minx) * (maxy - miny) * 100
  
  xsim <- runif(num_sims, min = minx, max = maxx)
  ysim <- runif(num_sims, min = miny, max = maxy)
  
  o_xdist <- oplayer_x - xsim
  o_ydist <- oplayer_y - ysim
  
  d_xdist <- dplayer_x - xsim
  d_ydist <- dplayer_y - ysim
  
  o_angle <- atan2(o_ydist, o_xdist)
  d_angle <- atan2(d_ydist, d_xdist)
  
  o_radius <- partial_radius(o_angle, oplayer_speed, oplayer_x, oplayer_y, ball_speed)
  d_radius <- partial_radius(d_angle, dplayer_speed, dplayer_x, dplayer_y, ball_speed)
  
  o_dist_to_point <- sqrt(o_xdist^2 + o_ydist^2)
  d_dist_to_point <- sqrt(d_xdist^2 + d_ydist^2)
  
  o_open <- sapply(seq_along(o_dist_to_point), function(i, o_radius) {
    ifelse(o_radius[i] >= o_dist_to_point[i], 1, 0)
  }, o_radius = o_radius)
  
  d_open <- sapply(seq_along(d_dist_to_point), function(i, d_radius) {
    ifelse(d_radius[i] >= d_dist_to_point[i], 1, 0)
  }, d_radius = d_radius)
  
  overlap_open <- o_open - d_open
  num_open <- sum(overlap_open[overlap_open == 1])
  
  return(num_open)
}



#revised version
#defensive player input will be a vector with n elements
#each element will have 3 elements within the defensive player speed
#dplayer_speed, the defensive player x and y; dplayer_x & dplayer_y
#to calculate time we will see the intersection point and calculate 
#the time it would take the ball to get there

#this function assumes that in denfensive player data the order is 
#dplayer_x, dplayer_y, and dplayer_speed
overlap <- function(oplayer_speed, 
                    oplayer_x, oplayer_y, defensive_player_data, 
                    ball_speed, ball_x, ball_y){
  
  #calculating offensive players radius 
  offensive_data <- partial_radius(angles, oplayer_speed, oplayer_x, oplayer_y,
                                   ball_speed, ball_x, ball_y)
  
  #setting values based on the offensive data
  rad <- offensive_data[1]
  #time <- offensive_data[2]
  #a vector where the 1st element is the x positions 
  #and the 2nd is the y positions
  off_pos_x <- offensive_data[3]
  off_pos_y <- offensive_data[4]
  
  #max radius to help set the box
  omax <- max(ocoord)
  
  #setting the simulated restrictions
  minx <- min(off_pos[1] - omax)
  maxx <- max(off_pos[1] + omax)
  
  miny <- min(off_pos[2] - omax)
  maxy <- max(off_pos[2] + omax)
  
  #taking area of restricted box with 100 points per square yard
  num_sims <- (maxx - minx) * (maxy - miny) * 100
  
  #running x and y simulated data
  xsim <- runif(num_sims, min = minx, max = maxx)
  ysim <- runif(num_sims, min = miny, max = maxy)
  
  #finding the time it would take the ball to reach each simulated point
  time <- sqrt((xsim - ball_x)^2 + (ysim - ball_y)^2) / ball_speed
  
  #go through each defender to define if they can reach the simulated point
  #before the ball
  for (i in 1:length(defensive_player_data)){
    #defensive player distance to ball
    def_rad <- sqrt((defensive_player_data[i][1] - off_pos_x)^2 +
                      (defensive_player_data[i][2] - off_pos_y)^2)
    def_player_speed <- defensive_player_data[i][3]
    paste("def", i, sep="") <- time * def_player_speed > def_rad
  }
  
  #calculating the offesive player distanct to simulated point
  o_xdist <- oplayer_x - xsim
  o_ydist <- oplayer_y - ysim
  
  #angle between off player and bal;
  o_angle <- atan2(o_ydist, o_xdist)
  
  #measuring if the offsive player can reach the simulated point
  o_radius <- partial_radius(o_angle, oplayer_speed, oplayer_x, 
                             oplayer_y, ball_speed)
  
  o_dist_to_point <- sqrt(o_xdist^2 + o_ydist^2)
  
  o_open <- sapply(seq_along(o_dist_to_point), function(i, o_radius) {
    ifelse(o_radius[i] >= o_dist_to_point[i], 1, 0)
  }, o_radius = o_radius)
  
  #creating a binary matrix 1st col is the offensive player openness for each point
  #other columns is the denfensive players openess
  openness_matrix <- data.frame(o_open, paste("def", c(1:length(defensive_player_data)),
                                              sep="", collapse = ","))
  open_cond <- (openness_matrix[ ,1]==1) & rowSums(openness_matrix[ ,-1] == 0)
  
  open_count <- sum(open_cond)
  
 
  
  return(open_count)
}



#partial radius function original

partial_radius <- function(player_angle, player_speed, 
                           player_x, player_y, ball_speed){
  
  player_angle <- player_angle * (pi / 180) + pix
  
  
  ball_angle <- acos((player_speed / ball_speed) * 
                       (player_y * cos(player_angle) - player_x * sin(player_angle)) / 
                       sqrt(player_x^2 + player_y^2)) - atan(-player_x / player_y)
  #ball_angle <- ball_angle + pi/2                   
  time <- player_x / ((ball_speed * cos(ball_angle)) - 
                        (cos(player_angle) * player_speed))
  print(time)
  intercept_x <- cos(player_angle) * player_speed * time 
  intercept_y <- sin(player_angle) * player_speed * time 
  
  radius <- sqrt(intercept_x^2 + intercept_y^2)
  
  return(radius)
}

vec <- rep(seq(1, 180, length = 180))

vecs <- partial_radius(vec, 7.8, 10, 12, 20)




