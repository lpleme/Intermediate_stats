#Is not filtered by price yet#
RentClose <- Rent %>%
  + filter(WalkingMinutes < 10, Gender == "F") %>%
  + select(Apartment, Gender, Address, Price, PrivateRoomPrice, Deposit, MilesToCampus, WalkingMinutes, Capacity, Website)

#RentSubset is where everything is subset (Gender, Distance, and Price)#
RentSubset <- RentClose %>% 
  + filter(WalkingMinutes < 10, Gender == "F", Price <= 1200)

#We need to show capacity for her request of having something with a lot of people around#

#histogram of all the apartments within a 10 minute walk to campus with a rent price of $1200/semester or less#
hist(RentSubset$Price, xlab = "Rent (In U.S. Dollars)", main = "Pricing of Housing Within a 10 Minute Walk", col = "skyblue")

#Interactive plot of prices compared with distance#
#We need to change the DF to "RentClose" once we subset the gender#
plot_ly(RentSubset) %>% 
  add_markers(y= ~Price, x= ~MilesToCampus, text= ~Apartment) %>% 
  layout(title="BYU-Idaho Housing Price Comparitive to Distance", xaxis=list(title="Distance to Campus"), yaxis=list(title="Semester Basic Contract Prices"))


#Data Table Summary of the standard rent compared with the private room rent#
outputTable <- rbind(`Standard Price`=favstats(RentSubset$Price), `Private Room Price`=favstats(RentSubset$PrivateRoomPrice))
pander(outputTable[c("min","Q1","median","mean","Q3","max","n")], caption="BYU-Idaho Semester Rent Contract Price Summary For Standard Room and Private Room")

#Displayed our data in a table for the viewer to read in depth#
datatable(RentSubset, options=list(lengthMenu = c(3,10,30)), extensions="Responsive")

##

plot(Capacity ~ Price, data = RentSubset, xlab = "Price of Apartment (USD)", ylab = "Number of Students in Apartment", main = "The Capacity of the Apartment Complex", col = "skyblue", pch = 18)

