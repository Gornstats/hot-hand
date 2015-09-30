# Test Hot-Hand using coin flips
# Based on code & discussion from:
# http://andrewgelman.com/2015/07/09/hey-guess-what-there-really-is-a-hot-hand/

rep <- 1e6
n <- 4
data <- array(sample(c(0,1), rep*n, replace=TRUE), c(rep,n))
prob <- rep(NA, rep)
for (i in 1:rep){
  heads1 <- data[i,1:(n-1)]==1
  heads2 <- data[i,2:n]==1
  prob[i] <- sum(heads1 & heads2)/sum(heads1)
}
# Average proportion of Heads following Heads
# With All-Tails results removed
mean(prob, na.rm=TRUE)
# = ~0.4
