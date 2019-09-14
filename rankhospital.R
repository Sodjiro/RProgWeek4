##Ranking hospitals by outcome in a state


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  x <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  if (!state %in% x$State)
    stop("invalid state")
  if (!outcome %in% c("heart failure", "heart attack", "pneumonia"))
    stop("invalid outcome")
  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  if (outcome == "heart failure") {
    p <-
      subset(
        x,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",
        select = c(
          State,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          Hospital.Name
        )
      )
    p[, 2] <- as.numeric(p[, 2])
    p <- p[p$State == state,]
    p[order(p[, 2]),]
    
    if (num == "best") {
      p <- p[which(p[, 2] %in% min(p[, 2])), 3]
      p <- sort(p)
      p[1]
    } else if (num == "worst") {
      p <- p[which(p[, 2] %in% max(p[, 2])), 3]
      p <- sort(p)
      tail(p, 1)
      
    } else {
      ordered <- p[order(p[,2],p[,3]),]
      ordered[num,3]
      
    }
  } else if (outcome == "heart attack") {
    p <-
      subset(
        x,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",
        select = c(
          State,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          Hospital.Name
        )
      )
    p[, 2] <- as.numeric(p[, 2])
    p <- p[p$State == state,]
    p[order(p[, 2]),]
    
    if (num == "best") {
      p <- p[which(p[, 2] %in% min(p[, 2])), 3]
      p <- sort(p)
      p[1]
    } else if (num == "worst") {
      p <- p[which(p[, 2] %in% max(p[, 2])), 3]
      p <- sort(p)
      tail(p, 1)
      
    } else {
      ordered <- p[order(p[, 2], p[, 3]),]
      ordered[num,3]
      
    }
    
  } else if (outcome == "pneumonia") {
    p <-
      subset(
        x,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia != "Not Available",
        select = c(
          State,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia,
          Hospital.Name
        )
      )
    p[, 2] <- as.numeric(p[, 2])
    p <- p[p$State == state,]
    p[order(p[, 2]),]
    
    if (num == "best") {
      p <- p[which(p[, 2] %in% min(p[, 2])), 3]
      p <- sort(p)
      p[1]
    } else if (num == "worst") {
      p <- p[which(p[, 2] %in% max(p[, 2])), 3]
      p <- sort(p)
      tail(p, 1)
      
    } else {
      ordered <- p[order(p[, 2], p[, 3]),]
      ordered[num,3]
      
    }
    
  }
}