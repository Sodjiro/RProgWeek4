##Finding the best hospital in a state

best <- function(state, outcome) {
  
  ## Read outcome data
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  if(!state %in% x$State) stop("invalid state")
  if(!outcome %in% c("heart failure","heart attack","pneumonia")) stop("invalid outcome")
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome=="pneumonia"){
    
    p<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available",
              select = c(State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name))
    p[,2]<-as.numeric(p[,2])
    p<-split(p,p$State)
    p<-p[[state]]
    p<-p[which(p[,2] %in% min(p[,2])),3]
    p<-sort(p)
    p[1]
    
  } else if (outcome=="heart failure"){
    p<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available",
              select = c(State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name))
    p[,2]<-as.numeric(p[,2])
    p<-split(p,p$State)
    p<-p[[state]]
    p<-p[which(p[,2] %in% min(p[,2])),3]
    p<-sort(p)
    p[1]
    
  } else if (outcome=="heart attack"){
    p<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available",
              select = c(State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name))
    p[,2]<-as.numeric(p[,2])
    p<-split(p,p$State)
    p<-p[[state]]
    p<-p[which(p[,2] %in% min(p[,2])),3]
    p<-sort(p)
    p[1]
  }
  
}






