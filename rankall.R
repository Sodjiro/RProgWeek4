##Ranking hospitals in all states


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  x <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
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
    p<-split(p,p$State)
    st<-data.frame(matrix(nrow = 54,ncol = 3), row.names = unique(x[,7]))
    names(st)[c(1,2,3)]<-colnames(x[c(7,17,2)])
    if (num == "best") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
    } else if (num == "worst") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(-dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
    } else {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[num,]
        st[i,1]<-i
      } 
      
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
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
    p<-split(p,p$State)
    st<-data.frame(matrix(nrow = 54,ncol = 3), row.names = unique(x[,7]))
    names(st)[c(1,2,3)]<-colnames(x[c(7,11,2)])
    if (num == "best") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
    } else if (num == "worst") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(-dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
    } else {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[num,]
        st[i,1]<-i
      } 
      
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
    } 
  } else if (outcome == "pneumonia") {
    p <-
      subset(
        x,
        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",
        select = c(
          State,
          Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
          Hospital.Name
        )
      )
    p[, 2] <- as.numeric(p[, 2])
    p<-split(p,p$State)
    st<-data.frame(matrix(nrow = 54,ncol = 3), row.names = unique(x[,7]))
    names(st)[c(1,2,3)]<-colnames(x[c(7,23,2)])
    if (num == "best") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
    } else if (num == "worst") {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(-dt[,2],dt[,3]),]
        st[i,]<-ordered[1,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
    } else {
      for(i in unique(x[,7])){
        dt<-data.frame( p[[i]]) 
        ordered <- dt[order(dt[,2],dt[,3]),]
        st[i,]<-ordered[num,]
        st[i,1]<-i
      } 
      stordered<-st[order(st[,1]),]
      stordered[,c(1,3)]
      
    }
  }
}