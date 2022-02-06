> plot(sex == "B" ~ length, data = KidsFeet, pch = 16, col = ifelse(sex == "B", "blue", "pink"))
> Kids.lm <- glm(sex == "B" ~ length, data = KidsFeet, family = "binomial")
> curve(exp(-12.4860+0.5074*x)/(1+exp(-12.4860+0.5074*x)), add = TRUE)
curve(exp(Kids.lm$coefficients[1]+Kids.lm$coefficients[2]*x)/(1+exp(Kids.lm$coefficients[1]+Kids.lm$coefficients[2]*x)))
predict(Kids.lm, data.frame(length = 25), type = "response")
exp(Kids.lm$coefficients[2])
#Make factors
is.na()
!is.na()

x ~ x + x
unique(x$x)
table(x$x)
table(foodstamp$income, foodstamp$tenancy) #Helps see if there are lots of repeated x values for goodness test

Sleep.Test <- with(sleep, t.test(extra[group==1], extra[group==2], paired = TRUE, mu = 0))
# Get the test statistic from the test:
observedTestStat <- Sleep.Test$statistic

N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permuteData <- sample(x=c(-1,1), size=10, replace=TRUE) 
  permutedTest <- with(sleep, t.test(permuteData*(extra[group==1] - extra[group==2]), mu = 0))
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat, col='skyblue', lwd=3)



myTest <- t.test(values ~ group, data = theData, mu = 0)
observedTestStat <- myTest$statistic




N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=theData$group)
  permutedTest <- t.test(values ~ permutedData, data = theData, mu = 0)
  permutedTestStats[i]  <-  permutedTest$statistic
}


hist(permutedTestStats)
abline(v=observedTestStat, col="skyblue")
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N

sum(permutedTestStats != observedTestStat)/N


myTest <- t.test(values ~ group, data = theData, paired = TRUE, mu = 0)
observedTestStat <- myTest$statistic

N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=c(-1,1), size = 30, replace = TRUE)
  permutedTest <- with(theData, t.test(permutedData*(values[group == 1] - values[group == 2]), mu = 0))
  permutedTestStats[i]  <-  permutedTest$statistic
}


hist(permutedTestStats)
abline(v=observedTestStat, col = "skyblue")
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N


#Class activity

#Step 1
myTest <- t.test(wt ~ cyl, data = mtcars1)
observedTestStat <- myTest$statistic

#Step 2
N <- 2000      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(mtcars1$cyl)
  permutedTest <- t.test(wt ~ permutedData, data = mtcars1, mu = 0)
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N



#Step 1
myTest <- aov(price ~ clarity, data = diamonds)
observedTestStat <- summary(myTest)[[1]]$`F value`[1]

#Step 2
N <- 100      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(diamonds$price)
  permutedTest <- aov(permutedData ~ clarity, data = diamonds)
  permutedTestStats[i] <- summary(permutedTest)[[1]]$`F value`[1]
}
hist(permutedTestStats)
abline(v=observedTestStat, col = "red")

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
2*sum(permutedTestStats >= observedTestStat)/N



#Step 1
myTest <- glm(sat > 1000 ~ expend, data = SAT, family = binomial)
observedTestStat <- summary(myTest)[[12]][2,3]

#Step 2
N <- 100      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(SAT$sat)
  permutedTest <- glm(permutedData > 1000 ~ expend, data = SAT, family = binomial)
}
hist(permutedTestStats)
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N




# The test and then the test statistic is found in a similar way to that of an ANOVA (this is the t statistic)
myTest <- lm(OT ~ G, data = Juv)
observedTestStat <- summary(myTest)[[4]][2,3]

# The permutation part is set up in this way
N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permutedData <- sample(trees$Height)
  permutedTest <- lm(permutedData ~ Girth, data = trees)
  permutedTestStats[i] <- summary(permutedTest)[[4]][2,3]
}

# Here, as before, is the histogram of the distribution of the test statistics
hist(permutedTestStats, col = "skyblue")
abline(v = observedTestStat, col = "red", lwd = 3)

# Less-than p-value:
sum(permutedTestStats <= observedTestStat)/N

# Greater-than p-value:
sum(permutedTestStats >= observedTestStat)/N

# Two-Sided p-value:
2*sum(permutedTestStats >= observedTestStat)/N