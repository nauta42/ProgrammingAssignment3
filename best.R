best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (length(data$State[data$State==state])==0) {
    stop("invalid state")
  }
  switch(EXPR = outcome, `heart attack` = colsel <- 11, `heart failure` = colsel <- 17,
         `pneumonia` = colsel <- 23, stop("invalid outcome"))
  #outcome numerico y con NA
  data[, colsel] <- as.numeric(data[, colsel])
  #index de filas sin NA
  ok <- complete.cases(data[, colsel])
  #data sin NA en outcome
  data <- data[ok, ]
  okstate <- data$State==state
  data <- data[okstate, ]
  data <- data[order(data$Hospital.Name), ]
  mins <- which.min(data[[colsel]])

  data$Hospital.Name[mins]

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}