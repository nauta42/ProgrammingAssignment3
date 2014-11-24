# num: list("best", "worst", 1:n)
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!any(data$State == state)) {
    stop("invalid state")
  }
  switch(EXPR = outcome, `heart attack` = colsel <- 11, `heart failure` = colsel <- 17,
         `pneumonia` = colsel <- 23, stop("invalid outcome"))
  # filter rows with the chosen state
  okstate <- data$State == state
  data <- data[okstate, ]
  # select only two columns: Hospital.name & outcome rate
  data <- data[, c(2,colsel)]
  names(data)[2] <- "Rate"
  # convert chosen outcome column to numeric & NAs
  # suppress 'Warning message:  In best("MD", "pneumonia") : NAs introduced by coercion'
  data[, 2] <- suppressWarnings(as.numeric(data[, 2]))
  # select rows without NAs in outcome column
  noNA <- complete.cases(data[, 2])
  data <- data[noNA, ]
  data <- data[order(data[[2]],data[[1]]), ]
  #
  last <- length(data[[2]])
  rank <- list(Rank = 1:last)
  data <- data.frame(data, rank)

  # comprobar que num=[1,last]
  num <- if (num == "best") {
    1
  } else if (num == "worst") {
    last 
  } else if (num > last) {
    return(NA)
  } else {
    num
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  #print(data[num, 1])
  data[num, 1]
}