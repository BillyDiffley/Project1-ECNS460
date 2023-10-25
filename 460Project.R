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
