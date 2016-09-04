# call ggplot before running the function
library(ggplot2)

# balanceoutput is the output of the MatchBalance() function in the Matching package
# varnames is a vector of strings; your variables names MUST be in the same order as they were in the matching function!

balanceplot <- function(balanceoutput, varnames) {
  
  before <- balanceoutput$BeforeMatching
  after <- balanceoutput$AfterMatching
  
  n <- length(balanceoutput$BeforeMatching)
  
  # create vector of pre-matching t-test p-values
  
  pBefore <- rep(NA, n)
  
  for(i in 1:n) {
    pBefore[i] <- before[[i]]$p.value
  }
  
  # create vector of pre-matching ks-test p-values
  
  ksBefore <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksBefore[i] <- before[[i]]$ks$ks.boot.pvalue
    }
  }
  
  
  # create vector of post-matching t-test p-values
  
  pAfter <- rep(NA, n)
  
  for(i in 1:n) {
    pAfter[i] <- after[[i]]$p.value
  }
  
  # create vector of post-matching ks-test p-values
  
  ksAfter <- rep(NA, n)
  
  for(i in 1:n) {
    if(!is.null(before[[i]]$ks)) {
      ksAfter[i] <- after[[i]]$ks$ks.boot.pvalue
    }
  }
  
  time <- rep(c("Pre t-test", "Post t-test", "Pre KS-test", "Post KS-test"), each = length(varnames))
  namesrep <- rep(varnames, 4)
  pvalues <- c(pBefore, pAfter, ksBefore, ksAfter)
  balance <- as.data.frame(cbind(namesrep, time, pvalues))
  balance <- na.omit(balance)
  balance$pvalues <- as.numeric(as.character(balance$pvalues))
  
  balanceplot <- ggplot(balance, aes(namesrep, pvalues)) +
                    geom_point(aes(color = time, shape = time, size = 3)) +
                    # see http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=4 for color options
                    scale_color_manual(values = c("#4daf4a", "#377eb8", "#ff7f00", "#e41a1c"), breaks = c("Pre t-test", "Post t-test", "Pre KS-test", "Post KS-test")) +
                    scale_shape_manual(values = c(19, 17, 19, 17), breaks = c("Pre t-test", "Post t-test", "Pre KS-test", "Post KS-test")) +
                    coord_flip() +
                    labs(x = NULL, y = "p-values", color = "Imbalance", shape = "Imbalance") +
                    scale_y_continuous(limits = c(0, 1)) +
                    scale_x_discrete(limits = rev(levels(balance$namesrep))) +
                    geom_segment(aes(x = 0, xend = (length(varnames) + 1), y = .05, yend = .05), linetype = 2, size = 0.5) +
                    theme(text = element_text(size = 20), axis.text = element_text(colour = "black")) +
                    guides(colour = "legend",  shape = "legend", size = "none")
                  
  return(balanceplot)
}