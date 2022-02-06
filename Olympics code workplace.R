#first 6 rows
head(Olympics)
#first 10
head(Olympics, 10)
#last 6
tail(Olympics)
#shows data type
class(Olmypics)
class(Olympics$sex)
#shows structure
str(Olympics)

## Don't filter with team, instead use NOC variable ##

OlympicsWinter <- Olympics %>% 
  filter(Season == "Winter")

OlympicsSummer <- Olympics %>%
  filter(Season == "Summer")

# != "NA"

#Makes a subset of the Winter set to show people that achieved medals. (No N/A)
OlympicsWinterMedal <- OlympicsWinter %>% 
  filter(Medal != "NA")
#Makes a subset of the Summer set to show people that achieved medals. (No N/A)
OlympicsSummerMedal <- OlympicsSummer %>% 
  filter(Medal != "NA")

#Subet of gold medals in the summer

OlympicsSummerMedalGold <- OlympicsSummerMedal %>% 
  filter(Medal == "Gold")

#Subset of silver medals in the summer

OlympicsSummerMedalSilver <- OlympicsSummerMedal %>% 
  filter(Medal == "Silver")

#Subset of bronze medals in the summer

OlympicsSummerMedalSilver <- OlympicsSummerMedal %>% 
  filter(Medal == "Bronze")

#Subset of gold medals in the winter

OlympicsWinterMedalGold <- OlympicsWinterMedal %>% 
  filter(Medal == "Gold")

#Subset of silver medals in the winter

OlympicsWinterMedalSilver <- OlympicsWinterMedal %>% 
  filter(Medal == "Silver")

#Subset of bronze medals in the winter

OlympicsWinterMedalBronze <- OlympicsWinterMedal %>% 
  filter(Medal == "Bronze")

#Plot of gold medals

hist(OlympicsSummerMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals Based on Age (Summer Olympics")

hist(OlympicsWinterMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals Based on Age (Winter Olympics)")

hist(OlympicsWinterFemaleMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals in Females Based on Age (Winter Olympics)")

hist(OlympicsWinterMaleMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals in Males Based on Age (Winter Olympics)")

hist(OlympicsSummerFemaleMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals in Females Based on Age (Summer Olympics")

hist(OlympicsSummerMaleMedal$Age, col = "skyblue", xlab = "Age", main = "Frequency of Medals in Males Based on Age (Summer Olympics")

outputTable <- rbind(Age=favstats(OlympicsSummerMedal$Age))
pander(outputTable[c("min","Q1","median","mean","Q3","max","n")], caption="Medals Based on Age for the Summer Olympics")

