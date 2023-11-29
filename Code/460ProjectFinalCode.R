rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)

#Create list of all data frames
my_files <- list.files(pattern = "\\.csv$")
citynames <- list()
citynames <- lapply(my_files, read.csv)
#Remove files created that are not data sets
citynames <- citynames[-c(1,8,9,25:28,38,39,40)]

#Creates a list of locations we are looking into in the form of a string
locationsdataframe <- read.csv("All_Locations.csv")
locations = locationsdataframe$Locations

#Creates a list of file path names
file_paths <- setNames(paste0(locations, "September.csv"), locations)

#Creates data frames iteratively
for (location in locations) {
  myFiles <- assign(paste(location), read.csv(file_paths[[location]]))
}

# Might be able to do the process iteratively
locationstest <- c(
  "Asheville", "Austin", "Boston", 'Bozeman', "Cambridge", "Chicago", "Columbus",
  "Dallas", "Denver", "FortWorth", "FtLauderdale", "Hawaii",
  "JerseyCity", "LasVegas", "LosAngeles", "Nashville", "Newark",
  "NewOrleans", "NewYorkCity", "Oakland", "PacificGrove", "RhodeIsland",
  "Rochester", "Salem", "SanDiego", "SanFrancisco", "SanMateo",
  "SantaClara", "SantaCruz", "Seattle", "TwinCities", "WashingtonDC"
)
for(i in 1:length(citynames)) {
  Location <- rep(locationstest[i], times = length(citynames[[i]]$id))
  citynames[[i]] <- cbind(citynames[[i]], Location)
}

Asheville <- as.data.frame(citynames[[1]])
#Add a "Location" column to each data frame to maintain a reference once all have been bound together
#Process cannot easily be made iterative as looping from one data frame to another is impractical

Asheville <- Asheville %>%
  mutate(Location = "Asheville") %>%
  relocate(Location)

Austin <- Austin %>%
  mutate(Location = "Austin") %>%
  relocate(Location)

Boston <- Boston %>%
  mutate(Location = "Boston") %>%
  relocate(Location)

Bozeman <- Bozeman %>%
  mutate(Location = "Bozeman") %>%
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


#Create final data frame by row binding all data frames together at one time
finalDF <- do.call("rbind", citynames1)

#Remove useless columns
finalDF <- finalDF[-c(2,5,14,18,19)]

#Save data set
save(finalDF, file = "finalData.RData")

############################################################################################################################################################
#Building Graphs

#Bar graph build
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
plot(mean_reviews,mean_prices,xlab = "Mean Number of Reviews", ylab = "Mean Price per Night",main = "Relationship between Mean Reviews and Mean Prices" )

#Creating a similar bar graph based on the tax rate of short term rentals by state
#https://realtorparty.realtor/wp-content/uploads/2018/11/HTA-Chart-State-Short-Term-Rental-Tax-Rate.pdf
States_Alpha_Order_DF <- read.csv("States_Alpha_Order.csv")
State_Tax_Rate_Df = read.csv("State_Tax_Rate.csv")
States_Alpha_Order = States_Alpha_Order_DF$State
State_Tax_Rate = State_Tax_Rate_Df$Rate

sorted_index_tax <- order(State_Tax_Rate)
sorted_locations_tax <- States_Alpha_Order[sorted_index_tax]
sorted_tax_rate <- State_Tax_Rate[sorted_index_tax]
num_bars <- length(sorted_index_tax)
color_palette <- colorRampPalette(c("purple", "green"))(num_bars)

barplot(sorted_tax_rate, main = "Tax Rate in Increasing Order", names.arg = sorted_locations_tax, las = 2, cex.names = 0.6, col = color_palette)

#Creating a scatterplot of the states tax rate against the average rental price within the state.
city_state_vector_df <- read.csv("city_state.csv")
city_state_vector <- city_state_vector_df$Column1
#Source: https://www.airbnb.com/help/article/2509
city_state_tax_rate = read.csv("CityStateTaxRate.csv")

#Locations that require either an operating license or are zoning restricted
#https://www.airbnb.com/help?audience=guest
restrictedList<- read.csv("RestrictedZoning.csv")
licenseList <- read.csv("RequireLicense.csv")
restrictedStates <- read.csv("RestrictedStates.csv")
licenseStates <- read.csv("StatesRequiringLicense.csv")

#Compute mean number of days vacant by city
finalDF <- finalDF %>%
  group_by(Location) %>%
  mutate(VacancyMean = mean(availability_365))

vacMean <- unique(finalDF$VacancyMean)

#Create data frame of data by single state
taxedCities <- data.frame(city_state_vector, mean_prices, vacMean)
taxedCities <- cbind(taxedCities, city_state_tax_rate)
colnames(taxedCities) <- c("Location", "MeanPrice", "AvgVacancy", "TaxRate")

#Create new binary variable for grouping the variables


taxedCities <- taxedCities %>%
  mutate(IsRestricted = ifelse(Location %in% restrictedStates$RS,T,F)) %>%
  mutate(IsLicensed = ifelse(Location %in% licenseStates$SRL,T,F))

#Scatterplot of Mean Price and Average Vacancy at each location
ggplot(taxedCities, aes(MeanPrice, AvgVacancy)) + geom_point() + geom_smooth(method = "lm")

#Remove zero tax rate from sample to look at the impact of "treated" cities
taxedCitiesSub <- subset(taxedCities, city_state_tax_rate!=0)

p = taxedCitiesSub %>% 
  ggplot(aes(x = TaxRate, y = MeanPrice, label = Location)) +
  geom_point(size = 2) +
  geom_text(nudge_y = 15, size = 3) + 
  xlab("Tax Rate by State") + 
  ylab("Mean Price of STR by State") +
  ggtitle("Tax Rate to Air BNB Price by State")

p + geom_point(aes(color = IsLicensed), size = 3) + geom_smooth(method = "lm", se = F)
p + geom_point(aes(color = IsRestricted), size = 3) + geom_smooth(method = "lm", se = F)

#Plot tax rate and mean prices
ggplot(taxedCitiesSub, aes(TaxRate, MeanPrice)) + geom_point() + geom_smooth(method = "lm")

################################

#Revising final scatter plots for better messaging
#Create more convenient data frame
cityStates <- cbind(taxedCities, locationsdataframe)
cityStates <- cityStates %>%
  relocate(Locations, .before = Location)
colnames(cityStates) <- c("City",'State', "MeanPrice", "AvgVacancy", "TaxRate", 'isRestricted',
                          'isLicensed')
#Create bar chart of each city and its mean price with license indicator
f <- ggplot(cityStates, aes(x = City, y = MeanPrice))
f <- f + coord_flip()
f + geom_col(aes(color = isLicensed, fill = isLicensed))

#Filter for a group mean
cityStates <- cityStates %>%
  group_by(isLicensed) %>%
  mutate(GroupMeanL = mean(MeanPrice))

#Bar chart for the group means of prices
b <- ggplot(cityStates[1:2,], aes(x = isLicensed, y = GroupMeanL))
b + geom_col(aes(color = isLicensed, fill = isLicensed))

#Repeat process for restrictions
g <- ggplot(cityStates, aes(x = City, y = MeanPrice))
g <- g + coord_flip()
g + geom_col(aes(fill = isRestricted))


#Filter for a group mean
cityStates <- cityStates %>%
  group_by(isRestricted) %>%
  mutate(GroupMeanR = mean(MeanPrice))

#Bar chart for the group means of prices
b <- ggplot(cityStates[c(1,3),], aes(x = isRestricted, y = GroupMeanR))
b + geom_col(aes(fill = isRestricted))
