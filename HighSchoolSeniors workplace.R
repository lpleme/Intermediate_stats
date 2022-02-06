#Two sided t test
t.test(CO2.chilled.250.M$uptake, CO2.chilled.250.Q$uptake, mu = 0, alternative = "two.sided", conf.level = 0.95)

#QQ
library(car)
Miss <- subset(CO2, Treatment == "chilled" & conc == 250 & Type == "Mississippi")
> Queb <- subset(CO2, Treatment == "chilled" & conc == 250 & Type == "Quebec")
> par(mfrow=c(1,2))
> qqPlot(Miss$uptake)
qqPlot(Queb$uptake)


library(car)
Fem <- subset(HSS, Gender == "Female")
Male <- subset(HSS, Gender == "Male")

FemS <- Fem %>%
  sample_n(30)
MaleS <- Male %>% 
  sample_n(30)

par(mfrow=c(1,2))
qqPlot(MaleS$Reaction_time, ylab = "Male Reaction Time")
qqPlot(FemS$Reaction_time, ylab = "Female Reaction Time")

t.test(HighSchoolSeniorsM$Reaction_time, HighSchoolSeniorsF$Reaction_time, mu = 0, alternative = "two.sided", conf.level = 0.95)
t.test(Male$Reaction_time, Fem$Reaction_time, mu = 0, alternative = "two.sided", conf.level = 0.95)
t.test(MaleS$Reaction_time, FemS$Reaction_time, mu = 0, alternative = "two.sided", conf.level = 0.95)


$$
  H_0: \mu_\text{Male Memory Test Score} - \mu_\text{Female Memory Test Score} = 0
$$
  
$$  
  H_a: \mu_\text{Female Memory Test Score} - \mu_\text{Female Memory Test Score} \neq 0
$$
  
  
  
stripchart(Score_in_memory_game ~ Gender, data=HSS, pch=16, col=c("skyblue","firebrick"), 
             vertical = TRUE, xlim=c(0.5,2.5), xlab="Gender", 
             ylab="Reaction Time", main="Memory Test Scores Between Genders")
abline(h=0, lty=2, col="gray")
reactionmeans <- mean(Score_in_memory_game ~ Gender, data=HSS)
lines(reactionmeans ~ c(1,2), lty=2, lwd=2, col="darkgray")
points(reactionmeans ~ c(1,2), pch=3, cex=2, col=c("skyblue","firebrick"))
legend("topleft", bty="n", pch=3, col=c("skyblue","firebrick"), title="Gender", legend=c("Male", "Female"))



pander(t.test(Score_in_memory_game ~ Gender, data = HSS, mu = 0, alternative = "two.sided", conf.level = 0.95), caption="Independent Sample t-Test: Score in Memory Game Based on Gender")

HSS <- HSS %>%
  mutate(good_memory = ifelse(Score_in_memory_game <= "60")
         
HSS_sleep <- HSS %>% 
  filter(Sleep_Hours_Schoolnight >= "7")

qqPlot(HSS_sleep$Score_in_memory_game)

pander(t.test(HSS_sleep$Score_in_memory_game, mu = 0, alternative = "less", conf.level = 0.95), caption="One Sample t-Test: Hours of Extra Sleep")




#SUBSETS
Male

Fem


boxplot(Male$Languages_spoken, Fem$Languages_spoken)

qqplot(Male$Languages_spoken, main = "Languages Spoken from Males")