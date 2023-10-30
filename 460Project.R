rm(list=ls())
library(tidyverse)
library(dplyr)

#Read in all city data into a dataframe

Asheville <- read.csv("AshevilleSeptember.csv")
Austin <- read.csv("AustinSeptember.csv")
Boston <- read.csv("BostonSeptember.csv")
Cambridge <- read.csv("CambridgeSeptember.csv")
Chicago <- read.csv("ChicagoSeptember.csv")
Columbus <- read.csv("ColumbusSeptember.csv")
Dallas <- read.csv("DallasSeptember.csv")
Denver <- read.csv("DenverSeptember.csv")
FortWorth <- read.csv("FortWorthSeptember.csv")
FtLauderdale <- read.csv("FtLauderdaleSeptember.csv")
Hawaii <- read.csv("HawaiiSeptember.csv")
JerseyCity <- read.csv("JerseyCitySeptember.csv")
LasVegas <- read.csv("LasVegasSeptember.csv")
LosAngeles <- read.csv("LosAngelesSeptember.csv")
Nashville <- read.csv("NashvilleSeptember.csv")
Newark <- read.csv("NewarkSeptember.csv")
NewOrleans <- read.csv("NewOrleansSeptember.csv")
NewYorkCity <- read.csv("NewYorkCitySeptember.csv")
Oakland <- read.csv("OaklandSeptember.csv")
PacificGrove <- read.csv("PacificGroveSeptember.csv")
Asheville <- read.csv("AshevilleSeptember.csv")
RhodeIsland <- read.csv("RhodeIslandSeptember.csv")
Rochester <- read.csv("RochesterSeptember.csv")
Salem <- read.csv("SalemSeptember.csv")
SanDiego <- read.csv("SanDiegoSeptember.csv")
SanFrancisco <- read.csv("SanFranciscoSeptember.csv")
SanMateo <- read.csv("SanMateoSeptember.csv")
SantaClara <- read.csv("SantaClaraSeptember.csv")
SantaCruz <- read.csv("SantaCruzSeptember.csv")
Seattle <- read.csv("SeattleSeptember.csv")
TwinCities <- read.csv("TwinCitiesSeptember.csv")
WashingtonDC <- read.csv("WashingtonDCSeptember.csv")

#Add a "Location" column to each data frame to maintain a reference once all have been binded together

Asheville <- Asheville %>%
  mutate(Location = "Asheville") %>%
  relocate(Location)

Austin <- Austin %>%
  mutate(Location = "Austin") %>%
  relocate(Location)

Boston <- Boston %>%
  mutate(Location = "Boston") %>%
  relocate(Location)

Cambridge <- Cambridge %>%
  mutate(Location = "Cambridge") %>%
  relocate(Location)

Chicago <- Chicago %>%
  mutate(Location = "Chicago") %>%
  relocate(Location)

Columbus <- Columbus %>%
  mutate(Location = "Columbus") %>%
  relocate(Location)

Dallas <- Dallas %>%
  mutate(Location = "Dallas") %>%
  relocate(Location)

Denver <- Denver %>%
  mutate(Location = "Denver") %>%
  relocate(Location)

FortWorth <- FortWorth %>%
  mutate(Location = "FortWorth") %>%
  relocate(Location)

FtLauderdale <- FtLauderdale %>%
  mutate(Location = "FtLauderdale") %>%
  relocate(Location)

Hawaii <- Hawaii %>%
  mutate(Location = "Hawaii") %>%
  relocate(Location)

JerseyCity <- JerseyCity %>%
  mutate(Location = "JerseyCity") %>%
  relocate(Location)

LasVegas <- LasVegas %>%
  mutate(Location = "LasVegas") %>%
  relocate(Location)

LosAngeles <- LosAngeles %>%
  mutate(Location = "LosAngeles") %>%
  relocate(Location)

Nashville <- Nashville %>%
  mutate(Location = "Nashville") %>%
  relocate(Location)

Newark <- Newark %>%
  mutate(Location = "Newark") %>%
  relocate(Location)

NewOrleans <- NewOrleans %>%
  mutate(Location = "NewOrleans") %>%
  relocate(Location)

NewYorkCity <- NewYorkCity %>%
  mutate(Location = "NewYorkCity") %>%
  relocate(Location)

Oakland <- Oakland %>%
  mutate(Location = "Oakland") %>%
  relocate(Location)

PacificGrove <- PacificGrove %>%
  mutate(Location = "PacificGrove") %>%
  relocate(Location)

RhodeIsland <- RhodeIsland %>%
  mutate(Location = "RhodeIsland") %>%
  relocate(Location)

Rochester <- Rochester %>%
  mutate(Location = "Rochester") %>%
  relocate(Location)

Salem <- Salem %>%
  mutate(Location = "Salem") %>%
  relocate(Location)

SanDiego <- SanDiego %>%
  mutate(Location = "SanDiego") %>%
  relocate(Location)

SanFrancisco <- SanFrancisco %>%
  mutate(Location = "SanFrancisco") %>%
  relocate(Location)

SanMateo <- SanMateo %>%
  mutate(Location = "SanMateo") %>%
  relocate(Location)

SantaClara <- SantaClara %>%
  mutate(Location = "SantaClara") %>%
  relocate(Location)

SantaCruz <- SantaCruz %>%
  mutate(Location = "SantaCruz") %>%
  relocate(Location)

Seattle <- Seattle %>%
  mutate(Location = "Seattle") %>%
  relocate(Location)

TwinCities <- TwinCities %>%
  mutate(Location = "TwinCities") %>%
  relocate(Location)

WashingtonDC <- WashingtonDC %>%
  mutate(Location = "WashingtonDC") %>%
  relocate(Location)

#Create list of all cities
citynames <- list(Asheville, Austin, Boston, Cambridge, Chicago, Columbus, Dallas, Denver, FortWorth, FtLauderdale,
                  Hawaii, JerseyCity, LasVegas, LosAngeles, Nashville, Newark, NewOrleans, NewYorkCity, Oakland, PacificGrove,
                  RhodeIsland, Rochester, Salem, SanDiego, SanFrancisco, SanMateo, SantaClara, SantaCruz, Seattle, TwinCities, WashingtonDC)

#Create final dataframe by rowbinding all data frames together at one time
finalDF <- do.call("rbind", citynames)

#Remove useless columns
finalDF <- finalDF[-c(2,5,14,18,19)]



############################################################################################################################################################
#Building Graphs


#Bar graph build
locations <- c(
  "Asheville", "Austin", "Boston", "Cambridge", "Chicago", "Columbus",
  "Dallas", "Denver", "finalDF", "FortWorth", "FtLauderdale", "Hawaii",
  "JerseyCity", "LasVegas", "LosAngeles", "Nashville", "Newark",
  "NewOrleans", "NewYorkCity", "Oakland", "PacificGrove", "RhodeIsland",
  "Rochester", "Salem", "SanDiego", "SanFrancisco", "SanMateo",
  "SantaClara", "SantaCruz", "Seattle", "TwinCities", "WashingtonDC"
)
mean_prices <- numeric(length(locations))

for (i in 1:length(locations)){
  mean_prices[i] <- mean(get(locations[i])$price)
}

sorted_index <- order(mean_prices)
sorted_locations <- locations[sorted_index]
sorted_mean_prices <- mean_prices[sorted_index]
num_bars <- length(sorted_mean_prices)
color_palette <- colorRampPalette(c("blue", "red"))(num_bars)
barplot(sorted_mean_prices, main = "Mean Prices in Increasing Order", names.arg = sorted_locations, las = 2, cex.names = 0.9, col = color_palette)



#Scatterplot build
mean_reviews <- numeric(length(locations))

# Calculate the mean price for each location and store it in the vector
for (i in 1:length(locations)){
  mean_reviews[i] <- mean(get(locations[i])$number_of_reviews)
}

sorted_index <- order(mean_reviews)
sorted_locations <- locations[sorted_index]
sorted_mean_reviews <- mean_reviews[sorted_index]
plot(mean_reviews,mean_prices,xlab = "Mean Number of Reviews", ylab = "Mean Price per Night", )

#Creating a simmilar bar graph based on the tax rate of short term rentals by state
#https://realtorparty.realtor/wp-content/uploads/2018/11/HTA-Chart-State-Short-Term-Rental-Tax-Rate.pdf
State_Tax_Rate = c(5,0,5.5,7.875,0,2.91,15,0,14.80,6,4,10.25,8,6,7,5,6.5,7,4.5,9,6,5,6,6.875,7,4.255,7,6.5,0,9,11.625,5.125,4,4.75,5,5.75,4.5,1.8,6,10.5,7,4.5,7,6,5.02,9,4.3,6.971,6,5,4)
States_Alpha_Order <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
  "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming","District of Columbia"
)


sorted_index_tax <- order(State_Tax_Rate)
sorted_locations_tax <- States_Alpha_Order[sorted_index_tax]
sorted_tax_rate <- State_Tax_Rate[sorted_index_tax]
num_bars <- length(sorted_index_tax)
color_palette <- colorRampPalette(c("purple", "green"))(num_bars)

barplot(sorted_tax_rate, main = "Tax Rate in Increasing Order", names.arg = sorted_locations_tax, las = 2, cex.names = 0.6, col = color_palette)

#Creating a scatterplot of the states tax rate against the average rental price within the state.
city_state_vector <- c(
  "North Carolina", "Texas", "Massachusetts", "Massachusetts", "Illinois",
  "Ohio", "Texas", "Colorado", "Texas", "Florida", "Hawaii", "New Jersey",
  "Nevada", "California", "Tennessee", "New Jersey", "Louisiana", "New York",
  "California", "California", "Rhode Island", "New York", "Oregon", "California",
  "California", "California", "California", "California", "Washington",
  "Minnesota", "District of Columbia"
)
city_state_tax_rate = c(4.75,6,5,5,6,5.75,6,2.91,6,6,10.25,11.625,0,0,7,11.625,0,4.5,4,0,0,10.5,4,1.8,0,0,0,0,0,6.971,6.875,14.80)
plot(city_state_tax_rate, mean_prices, xlab = "Mean Tax Rate of State", ylab = "Mean Price per Night within City")

