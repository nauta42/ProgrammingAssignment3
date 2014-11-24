#
rankall <- function(outcome, num = "best") {
  #
  if (num == "best") {
    num <- 1
  }
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!any(data$State==state)) {
    stop("invalid state")
  }
  switch(EXPR = outcome, `heart attack` = colsel <- 11, `heart failure` = colsel <- 17,
         `pneumonia` = colsel <- 23, stop("invalid outcome"))
  # select three columns: Hospital.name , outcome rate, state
  data <- data[, c(2, colsel, 7)]
  names(data) <- c("hospital", "rate","state")
  # convert chosen outcome column to numeric & NAs
  # suppress 'Warning message:  In best("MD", "pneumonia") : NAs introduced by coercion'
  data[, 2] <- suppressWarnings(as.numeric(data[, 2]))
  # select rows without NAs in outcome column
  noNA <- complete.cases(data[, 2])
  data <- data[noNA, ]
  data <- data[order(data$state, data$rate, data$hospital), ]
  # factor
  g <- data$state
  datasplit <- split(data, g)
  ## For each state, find the hospital of the given rank
  reslist <- lapply(datasplit, function(d) {
          last <- length(d[[2]])
          if (num == "worst") {
            num <- last
          }
          # comprobar que num=[1,last]
          if (num > last) {
            num <- 1
            d[num, 1] <- "<NA>"
          }
          d[num, c(1,3)]
  })
  data <- do.call(rbind, reslist)
  #data <- unsplit(reslist, g)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  # names() <- c("hospital","state")
  #return(data)
}