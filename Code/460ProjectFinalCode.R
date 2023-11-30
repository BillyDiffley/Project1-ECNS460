rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

#Create list of all data frames
my_files <- list.files(pattern = "\\.csv$")
citynames <- list()
citynames <- lapply(my_files, read.csv)
#Remove files created that are not data sets
citynames <- citynames[-c(1,8:10,26:29,39:41)]

#Creates a list of locations we are looking into in the form of a string
locationsdataframe <- read.csv("All_Locations.csv")
locations = locationsdataframe$Locations

#Creates a list of file path names
file_paths <- setNames(paste0(locations, "September.csv"), locations)

#Creates data frames iteratively
for (location in locations) {
  myFiles <- assign(paste(location), read.csv(file_paths[[location]]))
}

#Read in the list of cities
locationstmp <- read.csv('CityList.csv')
cities <- list()

#Convert to list
for(i in 1:nrow(locationstmp)) {       
  cities[[i]] <- locationstmp[i,]     
} 

#Create "Location column for each data set to maintain a reference once all are bound together

for(i in 1:length(citynames)) {
  Location <- rep(cities[[i]], times = length(citynames[[i]]$id))
  citynames[[i]] <- cbind(citynames[[i]], Location)
}

#Row Bind all data frames together
finalDF <- do.call("rbind", citynames)

#Remove useless columns
finalDF <- finalDF[-c(5:8)]

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
barplot(sorted_mean_prices, main = "Mean Prices in Increasing Order",
        names.arg = sorted_locations, las = 2, cex.names = 0.9, col = color_palette
        ,horiz = T, las = 1)

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

barplot(sorted_tax_rate, main = "Tax Rate in Increasing Order",
        names.arg = sorted_locations_tax, las = 2, cex.names = 0.6, col = color_palette,
        horiz = T, las = 1)

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
ggplot(taxedCities, aes(MeanPrice, AvgVacancy)) + geom_point() + geom_smooth(method = "lm") +ggtitle("Relationship between Mean Price and Average Vacancy")

#Remove zero tax rate from sample to look at the impact of "treated" cities
taxedCitiesSub <- subset(taxedCities, city_state_tax_rate!=0)

#Plot tax rate and mean prices
ggplot(taxedCitiesSub, aes(TaxRate, MeanPrice)) + geom_point() + geom_smooth(method = "lm") +ggtitle("Relationship between Tax Rate and Mean Price")

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
f + geom_col(aes(color = isLicensed, fill = isLicensed)) + ggtitle("Relationship between Mean Price by City")

#Filter for a group mean
cityStates <- cityStates %>%
  group_by(isLicensed) %>%
  mutate(GroupMeanL = mean(MeanPrice))

#Bar chart for the group means of prices
b <- ggplot(cityStates[1:2,], aes(x = isLicensed, y = GroupMeanL))
b + geom_col(aes(color = isLicensed, fill = isLicensed)) +ggtitle("Relationship between Licensed and Group Mean")

#Repeat process for restrictions
g <- ggplot(cityStates, aes(x = City, y = MeanPrice))
g <- g + coord_flip()
g + geom_col(aes(fill = isRestricted))+ggtitle("Relationship between Mean Price and Restrictions")

#Filter for a group mean
cityStates <- cityStates %>%
  group_by(isRestricted) %>%
  mutate(GroupMeanR = mean(MeanPrice))

#Bar chart for the group means of prices
b <- ggplot(cityStates[c(1,3),], aes(x = isRestricted, y = GroupMeanR))
b + geom_col(aes(fill = isRestricted)) +ggtitle("Comparison of Restricted against Non-Restricted")
