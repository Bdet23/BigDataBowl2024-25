#this will be the script to initially clean all the data. 

library(dplyr)

w1 <- read.csv("tracking_week_1.csv")
w2 <- read.csv("tracking_week_2.csv")
w3 <- read.csv("tracking_week_3.csv")
w4 <- read.csv("tracking_week_4.csv")
w5 <- read.csv("tracking_week_5.csv")
w6 <- read.csv("tracking_week_6.csv")
w7 <- read.csv("tracking_week_7.csv")
w8 <- read.csv("tracking_week_8.csv")
w9 <- read.csv("tracking_week_9.csv")

players <- read.csv("players.csv")
plays <- read.csv("plays.csv")

player_play<- read.csv("player_play.csv")

data <- rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9)

data <- data %>% merge(plays, by=c("gameId", "playId"))
