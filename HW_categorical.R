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
head(migraine)
summary(migraine)
migraine$trt <- factor(migraine$trt)
migraine$gender <- factor(migraine$gender)
migraine$bassev <- factor(migraine$bassev)
migraine$relief <- factor(migraine$relief)

#a) A logistic regression model for headache relief with factors for treatment, baseline severity, gender and age will
#be used to assess the effect of treatment." Perform this analysis in which the effect
#of the 2 doses as compared to placebo is assessed and summarize your findings.
#logistic regression relief <- trt bassev gender age

#family is binomial want link = "logit"
fit_0 <- glm(relief~age + trt + gender + bassev, data = migraine,
           family = binomial)
summary(fit_0)

fit_00 <- glm(relief~age + gender + bassev, data = migraine,
              family = binomial)
summary(fit_00)

anova(fit_00, fit_0, test="Chisq")



#(b) Compare the effect of both doses and report a confidence interval for this effect.
e <- effect("trt",fit_0)
summary(e)

png("headache.png", width = 7, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()


#(c) The analysis plan also mentioned the following: \To check to consistency of the
#treatment eect across baseline severity, gender and age, logistic regression models
#similar to the primary model will be fitted with additional interaction terms for
#treatment-by-baseline severity, treatment-by-gender and treatment-by-age, respec-
#tively." Fit these models and summarize your findings.

#interactie tss trt bassev
fit_1 <- glm(relief~age + trt + gender + bassev + trt*bassev, data = migraine,
           family = binomial)
summary(fit_1)
effect("trt:bassev", fit_1)
e <- effect("trt:bassev", fit_1)
summary(e)
png("headache_trtbassev.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment bassev interaction term on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()

#interactie tss trt gender
fit_2 <- glm(relief~age + trt + gender + bassev + trt*gender, data = migraine,
             family = binomial)
summary(fit_2)
effect("trt:gender", fit_2)
#summary(effect("trt:gender",fit_2))
e <- effect("trt:gender", fit_2)
png("headache_trtgender.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment gender interaction term on headache relief", xlab = "treatment (mg)", ylab = "relief")
dev.off()


#interactie tss trt age
fit_3 <- glm(relief~age + trt + gender + bassev + trt*age, data = migraine,
             family = binomial)
summary(fit_3)
effect("age:trt", fit_3)
plot(effect("age:trt", fit_3))
summary(effect("age:trt",fit_3))


e <- effect("age:trt", fit_3)
png("headache_trtage.png", width = 10, height = 5, units = 'in',res = 200)
plot(e, main = "Effect of treatment age interaction term on headache relief", xlab = "age", ylab = "relief")
dev.off()






#is dit nodig?
#interactie tss trt bassev and gender 
fit_4 <- glm(relief~age + trt + gender + bassev + trt*bassev + trt*gender, data = migraine,
             family = binomial)
summary(fit_4)

#interactie tss trt bassev and age 
fit_5<- glm(relief~age + trt + gender + bassev + trt*bassev + trt*age, data = migraine,
             family = binomial)
summary(fit_5)

#interactie tss trt gender and age 
fit_6 <- glm(relief~age + trt + gender + bassev + trt*gender + trt*age, data = migraine,
             family = binomial)
summary(fit_6)

#interactie tss trt gender and age and bassev
fit_7 <- glm(relief~age + trt + gender + bassev + trt*gender + trt*age + trt*bassev, data = migraine,
             family = binomial)
summary(fit_7)
