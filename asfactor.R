as.factor(x)
myaov <- aov(Wind ~ as.factor(Month), data = airquality)
aq1 <- airquality
aq1$Monthf <- aq1 %>% 
  mutate(Monthf = factor(Month, levels = c(9, 8, 7, 6, 5), labels = c("Sept", "Aug", "July", "June", "May")))
str(aq1)




