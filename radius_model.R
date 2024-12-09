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


over_lapping_radius <- function(gameId, playId, frameId){
  query <- paste("SELECT * FROM all_weeks 
              WHERE gameId ==", gameId, "AND playId ==",
                 playId, "AND frameId == ", frameId)
  res <- dbSendQuery(conn = dcon, "
                   SELECT * FROM all_weeks
                   WHERE gameId ==  AND
                   frameType = 'AFTER_SNAP'")
  
  
}

partial_radius <- function(player_angle, player_speed, 
                           player_x, player_y, ball_speed){
 
  player_angle <- player_angle * (pi / 180) + pi
  
  
  ball_angle <- acos((player_speed / ball_speed) * 
                       (player_y * cos(player_angle) - player_x * sin(player_angle)) / 
                       sqrt(player_x^2 + player_y^2)) - atan(-player_x / player_y)
  #ball_angle <- ball_angle + pi/2                   
  time <- player_x / ((ball_speed * cos(ball_angle)) - 
                        (cos(player_angle) * player_speed))
  intercept_x <- cos(player_angle) * player_speed * time 
  intercept_y <- sin(player_angle) * player_speed * time 
  
  radius <- sqrt(intercept_x^2 + intercept_y^2)
  
  return(radius)
}

overlap <- function(oplayer_speed, 
                    oplayer_x, oplayer_y,  
                    dplayer_speed, dplayer_x, dplayer_y, ball_speed){
  
  angles <- seq(1, 360, length.out = 360)
  ocoord <- partial_radius(angles, oplayer_speed, oplayer_x, oplayer_y, ball_speed)
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


overlap(5, 4, 4, 5, 2, 2, 10)
oplayer_speed <- 5
dplayer_speed <- 5
oplayer_x <- 1
oplayer_y <- 1
dplayer_x <- 2
dplayer_y <- 2
ball_speed <- 10


vec <- rep(seq(1, 180, length = 180))

(vecs <- partial_radius(vec, 7.8, 10, 12, 20))

dbDisconnect()
SELECT * FROM all_weeks
WHERE gameId = 2022091200
AND playId = 64 AND
frameId = 1
print(radius(10, 20, 0, 0))




x <- runif(100)
y <- runif(100)
plot(x, y, xlim = c(0, 5), ylim=c(0, 5))
