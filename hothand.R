# Test Hot-Hand using coin flips
# Based on code & discussion from:
# http://andrewgelman.com/2015/07/09/hey-guess-what-there-really-is-a-hot-hand/
# From Miller & Sanjurjo's paper
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2627354
# WSJ article on the paper
# http://www.wsj.com/articles/the-hot-hand-debate-gets-flipped-on-its-head-1443465711

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

# Test with larger sequence
# Avg NBA team has roughly 80 shots per game, 8 per 10 player rotation.
n <- 8
data <- array(sample(c(0,1), rep*n, replace=TRUE), c(rep,n))
prob <- rep(NA, rep)
for (i in 1:rep){
  heads1 <- data[i,1:(n-1)]==1
  heads2 <- data[i,2:n]==1
  prob[i] <- sum(heads1 & heads2)/sum(heads1)
}
# Sanity Check: NA results
# All-Tails and TTTTTTTH both NA results
# 2/(2^8) = .0078
sum(is.na(prob))/rep
# ~= .0079 Looks reasonable
mean(prob, na.rm=TRUE)
# ~= 0.433 in this simulation

# Finally test for a high shot volume player
# Kobe & LeBron average around 20 FGA.
n <- 20
data <- array(sample(c(0,1), rep*n, replace=TRUE), c(rep,n))
prob <- rep(NA, rep)
for (i in 1:rep){
  heads1 <- data[i,1:(n-1)]==1
  heads2 <- data[i,2:n]==1
  prob[i] <- sum(heads1 & heads2)/sum(heads1)
}
# NA check
sum(is.na(prob))/rep
# expected NA permutations is 2/2^20*1000000 ~= 2
# in this simulation, no NA's occur. This is reasonable

# chart distribution of ratio of Heads-following-Heads 
hist(prob, col='blue')
# Final test of average proportion of heads-following-heads
mean(prob, na.rm=TRUE)
# ~= 0.47 in this simulation

# We see the proportion of heads-following-heads or a "shot streak"
# trending towards the conventionally expected 50% as we increase the
# size of the shot/test sequence.

# Implications? With shorter shot sequence samples, the baseline
# for a hot streak should be set closer to 40% rather than the
# commonly assumed 50%- changes state of hot-hand analysis in all sports!
# Is this counter-intuitive result due to censoring (ie - not counting
# possible streaks started with a H/shot-made at end of sequence)?
# Hopefully further review of the Miller&Sanjurjo will tell in time.


