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
    
    
    if (x1 < 0){
      x1 <- 0
    }
    else if (x1 > 120) {
      x1 <- 120
    }
    
    if (y1 < 0){
      y1 <- 0
    }
    else if (y1 > 53.34){
      y1 <- 53.33
    }
    
    intercept_x <- x1
    intercept_y <- y1
    
    rad <- sqrt((intercept_x - player_x)^2 + (intercept_y - player_y)^2)
    
    radius[i] <- rad
    times[i] <- time
    pos_x[i] <- intercept_x
    pos_y[i] <- intercept_y
    
    df <- data.frame(angle = c(1:360), radius = radius)
  }
  
  return(df)
}


a <- data[51, ]

partial_radius(20, 110, 45, 20, 80, 40)
