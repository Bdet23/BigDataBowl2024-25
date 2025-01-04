setwd("/Users/devin/desktop/BDB/Big_Data_Bowl")

library(dplyr)
library(stringr)

data <- read.csv("coverage_predictions.csv")

right <- data[data$predicted_coverage == data$actual_coverage, ]


data$sum <- rowSums(data[, 5:22])

wrong <- data[data$predicted_coverage != data$actual_coverage, ]


colnames(data)[colnames(data) == "prob_2-Man"] <- "prob_2.Man"

data <- data %>%
      mutate(actual_coverage = str_replace_all(actual_coverage, "[ -]", "."),
             predicted_coverage = str_replace_all(predicted_coverage, "[ -]", "."))

# Subset rows based on dynamic column values

dig <- numeric(nrow(data))

for (row in seq_len(nrow(data))){
  
  cov <- data[row, "actual_coverage"]
  col <- paste("prob_", cov, sep="" )
  if (data[row, col] > .2 & data[row, "actual_coverage"] == data[row, "predicted_coverage"]) {
    dig[row] <- 0
  } else {
    dig[row] <- 1
  }
}

sum(dig)





