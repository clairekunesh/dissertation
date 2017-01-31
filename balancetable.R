# balance table function for main variables

# the function accepts the object balanceoutput, which is the result of the MatchBalance() function 
# varnames is like this c("American Indian", "Asian", "Black", "Latino", "Multiracial", "Male", "Math/Reading", "Retain", "SES", "Single Parent")
# in the Matching package

balancetable <- function(varnames, balanceoutput) {
  
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
  
  # create vector of pre-matching t-test p-values
  
  pBefore <- rep(NA, n)
  
  for(i in 1:n) {
    pBefore[i] <- round(before[[i]]$p.value, digits = 3)
  }
  
  # create vector of pre-matching ks-test p-values
  
  ksBefore <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksBefore[i] <- round(before[[i]]$ks$ks.boot.pvalue, digits = 3)
    }
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
  
  # create vector of post-matching t-test p-values
  
  pAfter <- rep(NA, n)
  
  for(i in 1:n) {
    pAfter[i] <- round(after[[i]]$p.value, digits = 3)
  }
  
  # create vector of post-matching ks-test p-values
  
  ksAfter <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksAfter[i] <- round(after[[i]]$ks$ks.boot.pvalue, digits = 3)
    }
  }
  
  # you can include TrBefore and/or TrAfter depending on whether or not many treated observations are dropped
  # get your varnames from the MatchBalance function (it's not clear whether they can be extracted from the output)
  
  balancetable <- data.frame(varnames, meanTrBefore, meanCoBefore, meanCoAfter, pBefore, pAfter,
                             ksBefore, ksAfter)
  return(balancetable)
  
}

# balance table function for extra variables

# note that the KS test output for the imputed and school-level variables is not meaningful, and should be removed from the table

extrabalancetable <- function(extravarnames, balanceoutput) {
  
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
  
  pBefore <- rep(NA, n)
  
  for(i in 1:n) {
    pBefore[i] <- round(before[[i]]$p.value, digits = 3)
  }
  
  ksBefore <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksBefore[i] <- round(before[[i]]$ks$ks.boot.pvalue, digits = 3)
    }
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
  
  pAfter <- rep(NA, n)
  
  for(i in 1:n) {
    pAfter[i] <- round(after[[i]]$p.value, digits = 3)
  }
  
  # create vector of post-matching ks-test p-values
  
  ksAfter <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksAfter[i] <- round(after[[i]]$ks$ks.boot.pvalue, digits = 3)
    }
  }
  
  # you can include TrBefore and/or TrAfter depending on whether or not many treated observations are dropped

  extrabalancetable <- data.frame(varnames, meanTrBefore, meanCoBefore, meanCoAfter, pBefore, pAfter,
                                  ksBefore, ksAfter)
  return(extrabalancetable)
  
}


