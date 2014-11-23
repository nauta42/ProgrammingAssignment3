#
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  # others, with prefix ! -NOT-: match(), is.element(), %in% , any()
  if (!any(data$State==state)) {
    stop("invalid state")
  }
  switch(EXPR = outcome, `heart attack` = colsel <- 11, `heart failure` = colsel <- 17,
         `pneumonia` = colsel <- 23, stop("invalid outcome"))
  # convert chosen outcome column to numeric & NAs
  # suppress 'Warning message:  In best("MD", "pneumonia") : NAs introduced by coercion'
  data[, colsel] <- suppressWarnings(as.numeric(data[, colsel]))
  # select rows without NAs in outcome column
  noNA <- complete.cases(data[, colsel])
  data <- data[noNA, ]
  # select rows with the chosen state
  okstate <- data$State==state
  data <- data[okstate, ]
  data <- data[order(data$Hospital.Name), ]
  mins <- which.min(data[[colsel]])

  data$Hospital.Name[mins]

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}