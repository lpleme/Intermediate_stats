SFR <- subset(Friendly, condition == "SFR")
Before <- subset(Friendly, condition == "Before")
Meshed <- subset(Friendly, condition == "Meshed")

x <- c(Meshed$correct)
y <- c(Before$correct)
z <- c(SFR$correct)


ggplot(Friendly, aes(x=factor(condition), y=correct, )) +
  geom_boxplot(fill="cadetblue1",color="black")+
  labs(title = "TEST", x = "Condition", y = "Correctness")

outputTable <- rbind(SFR=favstats(SFR$correct), `Meshed`=favstats(Meshed$correct), `Before`=favstats(Before$correct))
pander(outputTable[c("min","Q1","median","mean","Q3","max","n")], caption="Study System Summary")

#Paired wilcoxon signed-rank test (maybe)
wilcox.test(x, y, mu = 0, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#Mann-Whitney (maybe)
wilcox.test(x, y, mu = 0, alternative = "two.sided", conf.level = 0.95)

