# balance table function for main variables

balancetable <- function(balanceoutput) {
  
  before <- balanceoutput$BeforeMatching
  after <- balanceoutput$AfterMatching
  
  n <- length(balanceoutput$BeforeMatching)
  
  # create vectors of pre-matching means
  
  meanTrBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanTrBefore[i] <- round(before[[i]]$mean.Tr, digits = 2)
  }
  
  meanCoBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanCoBefore[i] <- round(before[[i]]$mean.Co, digits = 2)
  }
  
  # create vector of pre-matching p-values
  
  meanPBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanPBefore[i] <- round(before[[i]]$p.value, digits = 3)
  }
  
  # create vectors of post-matching means
  
  meanTrAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanTrAfter[i] <- round(after[[i]]$mean.Tr, digits = 2)
  }
  
  meanCoAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanCoAfter[i] <- round(after[[i]]$mean.Co, digits = 2)
  }
  
  # create vector of post-matching p-values
  
  meanPAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanPAfter[i] <- round(after[[i]]$p.value, digits = 3)
  }
  
  # you can include TrBefore and/or TrAfter depending on whether or not many treated observations are dropped
  # get your varnames from the MatchBalance function (it's not clear whether they can be extracted from the output)
  
  varnames <- c("American Indian", "Asian", "Black", "Latino", "Multiracial", "Male", "Math/Reading", "Retain", "SES", "Single Parent")
  balancetable <- data.frame(varnames, meanTrBefore, meanCoBefore, meanCoAfter, meanPBefore, meanPAfter)
  return(balancetable)
  
}

# balance table function for extra variables

extrabalancetable <- function(balanceoutput) {
  
  before <- balanceoutput$BeforeMatching
  after <- balanceoutput$AfterMatching
  
  n <- length(balanceoutput$BeforeMatching)
  
  # create vectors of pre-matching means
  
  meanTrBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanTrBefore[i] <- round(before[[i]]$mean.Tr, digits = 2)
  }
  
  meanCoBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanCoBefore[i] <- round(before[[i]]$mean.Co, digits = 2)
  }
  
  # create vector of pre-matching p-values
  
  meanPBefore <- rep(NA, n)
  
  for(i in 1:n) {
    meanPBefore[i] <- round(before[[i]]$p.value, digits = 3)
  }
  
  # create vectors of post-matching means
  
  meanTrAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanTrAfter[i] <- round(after[[i]]$mean.Tr, digits = 2)
  }
  
  meanCoAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanCoAfter[i] <- round(after[[i]]$mean.Co, digits = 2)
  }
  
  # create vector of post-matching p-values
  
  meanPAfter <- rep(NA, n)
  
  for(i in 1:n) {
    meanPAfter[i] <- round(after[[i]]$p.value, digits = 3)
  }
  
  # you can include TrBefore and/or TrAfter depending on whether or not many treated observations are dropped
  # get your varnames IN ORDER from the MatchBalance function (it's not clear whether they can be extracted from the output)
  
  varnames <- c("Native English", "First-Gen Immigrant", "Sec-Gen Immigrant", "Wealth", "AP Class", "IB Program", "Risk Factors", 
                "Parent English", "IEP", "School SES", "School Math/Reading", "School Uni Rate", "School Suspend Rate")
  balancetable <- data.frame(varnames, meanTrBefore, meanCoBefore, meanCoAfter, meanPBefore, meanPAfter)
  return(balancetable)
  
}

