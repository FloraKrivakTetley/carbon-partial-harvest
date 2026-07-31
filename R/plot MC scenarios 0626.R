## Build script to read in summarized output from MC runs and visualize
## This script will read in and plot time series results (or comparisons of Year 50) for a
## set of Scenarios

## Scenario Sets:
## Burn Scenarios
## 1. 150 AAC Burn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 2. 150 Low Burn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 3. 300 AAC Burn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 4. 300 Low Burn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## NoBurn Scenarios
## 5. 150 AAC NoBurn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 6. 150 Low NoBurn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 7. 300 AAC NoBurn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)
## 8. 300 AAC NoBurn (NoHarvest, CC, PH30, PH30Plus, PH60, PH60Plus)

## Total Scenario sets will be 8*6=48. But NoHarvest is identical for all 150s and all 300s

NoHarvest150<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/Baseline NoHarvest150/mean_by_year.csv")
NoHarvest150<-NoHarvest150%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="NoHarvest", StandAge=150, HarvestRate="NA", Burn="NA")

NoHarvest300<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/Baseline NoHarvest300/mean_by_year.csv")
NoHarvest300<-NoHarvest300%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="NoHarvest", StandAge=300, HarvestRate="NA", Burn="NA")

## Scenario Set 1- Leave out PH30 for now. 150 AAC Burn.

CC_150AACBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150AACBurn/mean_by_year.csv")
CC_150AACBurn<- CC_150AACBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="CC", StandAge=150, HarvestRate="AAC", Burn="Yes")

PH60_150AACBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_150AACBurn/mean_by_year.csv")
PH60_150AACBurn<- PH60_150AACBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60", StandAge=150, HarvestRate="AAC", Burn="Yes")

PH60Plus_150AACBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150AACBurn/mean_by_year.csv")
PH60Plus_150AACBurn<- PH60Plus_150AACBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60Plus", StandAge=150, HarvestRate="AAC", Burn="Yes")

## Scenario Set 2 150 Low Burn

CC_150LowBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150LowBurn/mean_by_year.csv")
CC_150LowBurn<- CC_150LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="CC", StandAge=150, HarvestRate="Low", Burn="Yes")

PH60_150LowBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_150LowBurn/mean_by_year.csv")
PH60_150LowBurn<- PH60_150LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60", StandAge=150, HarvestRate="Low", Burn="Yes")

PH60Plus_150LowBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150LowBurn/mean_by_year.csv")
PH60Plus_150LowBurn<- PH60Plus_150LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60Plus", StandAge=150, HarvestRate="Low", Burn="Yes")

## Scenario Set 3 300 AAC Burn

## Scenario Set 4 300 Low Burn

CC_300LowBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_300LowBurn/mean_by_year.csv")
CC_300LowBurn<- CC_300LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="CC", StandAge=300, HarvestRate="Low", Burn="Yes")

PH60_300LowBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_300LowBurn/mean_by_year.csv")
PH60_300LowBurn<- PH60_300LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60", StandAge=300, HarvestRate="Low", Burn="Yes")

PH60Plus_300LowBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_300LowBurn/mean_by_year.csv")
PH60Plus_300LowBurn<- PH60Plus_300LowBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60Plus", StandAge=300, HarvestRate="Low", Burn="Yes")

## Scenario Set 6 150 Low NoBurn

CC_150LowNoBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150LowNoBurn/mean_by_year.csv")
CC_150LowNoBurn<- CC_150LowNoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="CC", StandAge=150, HarvestRate="Low", Burn="No")

PH60_150LowNoBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_150LowNoBurn/mean_by_year.csv")
PH60_150LowNoBurn<- PH60_150LowNoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60", StandAge=150, HarvestRate="Low", Burn="No")

PH60Plus_150LowNoBurn<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150LowNoBurn/mean_by_year.csv")
PH60Plus_150LowNoBurn<- PH60Plus_150LowNoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Scenario="PH60Plus", StandAge=150, HarvestRate="Low", Burn="No")


##
Scenarios_150AACBurn<- bind_rows(NoHarvest150, CC_150AACBurn, PH60_150AACBurn, PH60Plus_150AACBurn)
Scenarios_150LowBurn<- bind_rows(NoHarvest150, CC_150LowBurn, PH60_150LowBurn, PH60Plus_150LowBurn)
Scenarios_150LowNoBurn<- bind_rows(NoHarvest150, CC_150LowNoBurn, PH60_150LowNoBurn, PH60Plus_150LowNoBurn)
Scenarios_300LowBurn<-bind_rows(NoHarvest300, CC_300LowBurn, PH6_300LowBurn, PH60Plus_300LowBurn)

Scenarios_150Burn<-bind_rows(Scenarios_150AACBurn, Scenarios_150LowBurn)
Scenarios_150Low<-bind_rows(Scenarios_150LowBurn, Scenarios_150LowNoBurn)
Sceanrios_LowBurn<-bind_rows(Scenarios_150LowBurn, Scenarios_300LowBurn)


## Harvest rate: AAC versus Low
ggplot(data=Scenarios_150Burn, aes(x=Time, y=ABio, colour=Scenario, lty=HarvestRate))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Biomass (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs")

##
ggplot(data=Scenarios_150Burn, aes(x=Time, y=ToFPS, colour=Scenario, lty=HarvestRate))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs- ForCS Output")
