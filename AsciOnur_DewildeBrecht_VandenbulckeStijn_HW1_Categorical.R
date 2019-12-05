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
binom.confint(x = 20, n = 20, conf.level = 0.05, method = 'exact')

# (b)coverage probability for a 95% confidence interval for pi (n=20, population pi = 0.87) 
# (1) a Clopper-Pearson exact interval
binomial_probs_of_samples_containing_pi <- rep(0, n)
for(i in 0:n){
  ucl = binom.confint(x = i, n = 20, conf.level = 0.05, method = 'exact')$upper
  lcl = binom.confint(x = i, n = 20, conf.level = 0.05, method = 'exact')$lower
  if (lcl <= pi && pi <= ucl){
    binomial_probs_of_samples_containing_pi[i] <- dbinom(i, size = n, prob = pi)
  }
}
sum(binomial_probs_of_samples_containing_pi)

# (2) a Wald interval
binomial_probs_of_samples_containing_pi <- rep(0, n)
for(i in 0:n){
  ucl = binom.confint(x = i, n = 20, conf.level = 0.05, method = 'asymptotic')$upper
  lcl = binom.confint(x = i, n = 20, conf.level = 0.05, method = 'asymptotic')$lower
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
barplot(tutor.matrix, beside = TRUE)
proportion_tutor_support <- sum(tutor.matrix[1,1]) / sum(tutor.matrix[1,])
proportion_no_support <- sum(tutor.matrix[2,1]) / sum(tutor.matrix[2,])

# test, tests the difference between tutor support proportion and no support proportion 
# H0: tutor_support = no_support
# A: tutor_support > no support

# Note that, by default, the function prop.test() used the Yates continuity correction, 
# which is really important if either the expected successes or failures is < 5. None are less than 5
test <- prop.test(tutor.matrix, correct = FALSE, alternative = 'greater')

# estimated probability of success
test$estimate

# the confidence interval for the probability of success
test$conf.int

# the p-value of the test
test$p.value # as the p value of the test is higher than the significande level alpha = 0.05 
# we can conclude that the proportion of people that succeeded is not significantly different in the
# two groups with a p-value of ... 

##############################
##############################
######   QUESTION 3 ##########
##############################
##############################
library("effects")
setwd("D:/Ugent/MaNaMa/categorical")
migraine <- read.table("migraine.txt")
summary(migraine)
migraine$trt <- factor(migraine$trt)
migraine$gender <- factor(migraine$gender)
migraine$bassev <- factor(migraine$bassev)
migraine$relief <- factor(migraine$relief)

#a) A logistic regression model for headache relief with factors for treatment, baseline severity, gender and age will
#be used to assess the effect of treatment." Perform this analysis in which the effect
#of the 2 doses as compared to placebo is assessed and summarize your findings.
#logistic regression relief <- trt bassev gender age

#family is binomial because the link is "logit"
fit_0 <- glm(relief~age + trt + gender + bassev, data = migraine,
             family = binomial)
summary(fit_0)

fit_00 <- glm(relief~age + gender + bassev, data = migraine,
              family = binomial)
summary(fit_00)

#Use anova to compare the model with and without treatment predictor
anova(fit_00, fit_0, test="Chisq")

#(b) Compare the effect of both doses and report a confidence interval for this effect.
#Use the effect library to check the effect of the treatment predictor
e <- effect("trt",fit_0)
summary(e)

#save the figure
png("headache.png", width = 7, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()


#(c) The analysis plan also mentioned the following: \To check to consistency of the
#treatment eect across baseline severity, gender and age, logistic regression models
#similar to the primary model will be fitted with additional interaction terms for
#treatment-by-baseline severity, treatment-by-gender and treatment-by-age, respec-
#tively." Fit these models and summarize your findings.

#interactie between trt bassev
fit_1 <- glm(relief~age + trt + gender + bassev + trt*bassev, data = migraine,
             family = binomial)
summary(fit_1)
e <- effect("trt:bassev", fit_1)
summary(e)

png("headache_trtbassev.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment bassev interaction term on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()

#interactie tss trt gender
fit_2 <- glm(relief~age + trt + gender + bassev + trt*gender, data = migraine,
             family = binomial)
summary(fit_2)
e <- effect("trt:gender", fit_2)
summary(e)

png("headache_trtgender.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment gender interaction term on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()

#Interaction between trt age
fit_3 <- glm(relief~age + trt + gender + bassev + trt*age, data = migraine,
             family = binomial)
summary(fit_3)
e <- effect("age:trt", fit_3)
summary(e)

png("headache_trtage.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment age interaction term on headache relief", xlab = "age", ylab = "relief")
dev.off()


##############################
##############################
######   QUESTION 4 ##########
##############################
##############################


library(DescTools)

# Odds ratio function
OR<-function(x, alpha=0.05) {
  # the odds ratio
  odds.ratio <- (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
  # large-sample Wald interval
  z.alpha <- qnorm(1-alpha/2)
  se.OR <- sqrt(sum(x^{-1}))
  ci.OR <- exp( log(odds.ratio) + c(-1,+1)*z.alpha*se.OR )
  list(OR=odds.ratio, OR.wald.ci=ci.OR) }



# create dataset
trt<-rep(c(1,1,0,0),8)
death<-rep(c(1,0,1,0),8)
trial<-rep(seq(1,8,by=1),each=4)
count<-c(75,3429-75,47,1710-47,21,1621-21,23,814-23,16,1856-16,14,1855-14,10,1009-10,19,1026-19,
         87,2545-87,104,2540-104,15,1262-15,19,1277-19,25,638-25,31,638-31,78,1675-78,90,1675-90)
aspirin.data<-as.data.frame(cbind(trt,death,trial,count))

# convert variables to factor
aspirin.data$trt <-as.factor(aspirin.data$trt) 
aspirin.data$trial <-as.factor(aspirin.data$trial) 
aspirin.data$death <-as.factor(aspirin.data$death) 


# calculate Odds Ratio and Confidence Interval for each trial
aspirin.array <- xtabs(count ~ trt + death + trial, data=aspirin.data)
unlist(apply(aspirin.array, 3, OR))


# Calculate pooled estimate, confidence interval and p-value
mantelhaen.test(aspirin.array)

# test for homogeneity
BreslowDayTest(aspirin.array,correct=F)


# create logistic regression model including interaction term
fit_all <- glm(death ~ trt * trial, weight=count, data=aspirin.data, family=binomial)

# odds ratio of Trials 2 and 3
Trial2_OR <- exp(predict(fit_all, newdata = data.frame(trt="1", trial="2")) -
                   predict(fit_all, newdata = data.frame(trt="0", trial="2")))

Trial3_OR <- exp(predict(fit_all, newdata = data.frame(trt="1", trial="3")) -
                   predict(fit_all, newdata = data.frame(trt="0", trial="3")))

Trial2_OR
Trial3_OR 

# standard error and confidence intervals for trials 2 and 3
T2_weights <- matrix(c(0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,-1), ncol=1) 
T2_SE <- sqrt(t(T2_weights) %*% vcov(fit_all) %*% T2_weights)
T2_CI <- exp(as.vector(coef(fit_all)%*%T2_weights)+qnorm(c(.025, .975))*as.vector(T2_SE))

T2_SE
T2_CI

# create logistic regression model without interaction term
fit.full  <- glm(death ~ trt + trial, weight=count, data=aspirin.data, family=binomial)
summary(fit.full)

# pooled estimate
exp(coef(fit.full)["trt1"])

# confidence interval of pooled estimate
exp(confint.default(fit.full)[2,1])
exp(confint.default(fit.full)[2,2])


# create model with only trial main effect
fit.restricted <- glm(death ~ trial, weight=count, data=aspirin.data, family=binomial)

# p-value for pooled estimate
anova(fit.restricted, fit.full, test="Chisq")

# test for homogeneity
anova(fit_all, fit.full, test="Chisq")



