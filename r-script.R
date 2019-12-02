##############################
##############################
######   QUESTION 1 ##########
##############################
##############################
n = 20
pi = 0.87
# (a) Conduct an exact binomial test and construct a Clopper-Pearson 95% confidence interval.
greater_than_x <- function(x, n, pi) {
  range <- 0:n
  cumm_probs <- c(0, cumsum(dbinom(range, size = n, prob = pi)))
  return(1 - cumm_probs[x+1])
}

# obtain values for the table
dbinom(0:20, size = 20, prob = 0.87)
greater_than_x(0:20, n = 20, pi = 0.87)

# CI 
# install.packages('binom')
library(binom)
binom.confint(x = 20, n = 20, conf.level = 0.95, method = 'exact')

# (b)coverage probability for a 95% confidence interval for pi (n=20, population pi = 0.87) 
# (1) a Clopper-Pearson exact interval
binomial_probs_of_samples_containing_pi <- rep(0, n)
for(i in 0:n){
  ucl = binom.confint(x = i, n = 20, conf.level = 0.95, method = 'exact')$upper
  lcl = binom.confint(x = i, n = 20, conf.level = 0.95, method = 'exact')$lower
  if (lcl <= pi && pi <= ucl){
    binomial_probs_of_samples_containing_pi[i] <- dbinom(i, size = n, prob = pi)
  }
}
sum(binomial_probs_of_samples_containing_pi)

# (2) a Wald interval
binomial_probs_of_samples_containing_pi <- rep(0, n)
for(i in 0:n){
  ucl = binom.confint(x = i, n = 20, conf.level = 0.95, method = 'asymptotic')$upper
  lcl = binom.confint(x = i, n = 20, conf.level = 0.95, method = 'asymptotic')$lower
  if (lcl <= pi && pi <= ucl){
    binomial_probs_of_samples_containing_pi[i] <- dbinom(i, size = n, prob = pi)
  }
}
sum(binomial_probs_of_samples_containing_pi)

##############################
##############################
######   QUESTION 2 ##########
##############################
##############################
tutor.matrix <- matrix( c(5, 10, 5, 30), ncol=2, 
                          dimnames=list(Support=c("Tutor support","No support"), Successful=c("Yes","No")))
proportion_tutor_support <- sum(tutor.matrix[1,1]) / sum(tutor.matrix[1,])
proportion_no_support <- sum(tutor.matrix[2,1]) / sum(tutor.matrix[2,])

# test, tests the difference between tutor support proportion and no support proportion 
# H0: tutor_support = no_support
# A: tutor_support > no support

test <- prop.test(tutor.matrix, correct = FALSE, alternative = 'greater')
test$estimate # estimated probability of success
test$conf.int # the confidence interval for the probability of success
test$p.value  # the p-value of the test

