## Edited June 2024

library(tidyverse)
library(ggplot2)
library(dplyr)

FCIDataFullPH%>% filter(Burn=="Burn")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Year, y=ToFPS_m3Total, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_classic(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  # scale_fill_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  # scale_colour_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  theme_classic(base_size = 14)+
  xlab("Year")+
  ylab(bquote(paste("Total Volume Harvested (m "^3,")")))

FCIDataFullPH%>% filter(Burn=="Burn")%>% 
  ggplot(aes(x=Year, y=ToFPS_m3harvested, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_classic(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  # scale_fill_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  # scale_colour_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  theme_classic(base_size = 14)+
  xlab("Year")+
  ylab(bquote(paste("Total Volume Harvested (m "^3,")")))

max(FCIDataFullPH$ToFPS_m3harvested) ## 582.6
min(FCIDataFullPH$ToFPS_m3harvested[FCIDataFullPH$ToFPS_m3harvested>0]) ## 106.2









## OLD###

## Step 1. #################################################################################
## Read in data.This pulls in output from data summary of MC runs for quick & dirty plotting
############################################################################################

Baseline<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/Baseline NoHarvest150/mean_by_year.csv")
Baseline$Scenario<- "NoHarvest"

CC<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150AACBurn/mean_by_year.csv")
CC$Scenario<- "ClearCut"

CC2<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150AACBurnADJUSTED/mean_by_year.csv")
CC$Scenario<- "ClearCut2" ## this harvests CC at target size 14

PH30<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH30_150AACBurn/mean_by_year.csv")
PH30$Scenario<- "PH30"

PH30Plus<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH30Plus_150AACBurn/mean_by_year.csv")
PH30Plus$Scenario<- "PH30Plus"

PH60<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_150AACBurn/mean_by_year.csv")
PH60$Scenario<- "PH60"

PH60Plus<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150AACBurn/mean_by_year.csv")
PH60Plus$Scenario<- "Ph60Plus"

PH60Plus2<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150AACBurnADJUSTED/mean_by_year.csv")
PH60Plus2$Scenario<- "Ph60Plus2"

## Create on data frame
Scenarios_150AACBurn<- bind_rows(Baseline, CC, CC2,PH30, PH30Plus, PH60, PH60Plus, PH60Plus2)

## Plot variables of interest

## Live C
ggplot(data=Scenarios_150AACBurn, aes(x=Time, y=ABio, colour=Scenario))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Biomass (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs")

##
ggplot(data=Scenarios_150AACBurn, aes(x=Time, y=TotalDOM, colour=Scenario))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("Total DOM (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs")

##
ggplot(data=Scenarios_150AACBurn, aes(x=Time, y=ToFPS, colour=Scenario))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs- ForCS Output")

## Step 2. ###############################
## Do the same thing with HWP model values
##########################################

## Omit Baseline, all 0s.

CCHWP<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/CC_150AACBurn/Mean_HWP.csv")
CCHWP$Scenario<- "ClearCut"

PH30HWP<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH30_150AACBurn/Mean_HWP.csv")
PH30HWP$Scenario<- "PH30"

PH30PlusHWP<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH30Plus_150AACBurn/Mean_HWP.csv")
PH30PlusHWP$Scenario<- "PH30Plus"

PH60HWP<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60_150AACBurn/Mean_HWP.csv")
PH60HWP$Scenario<- "PH60"

PH60PlusHWP<-read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS/PH60Plus_150AACBurn/Mean_HWP.csv")
PH60PlusHWP$Scenario<- "Ph60Plus"

## Create dataframes for NoHarvest Scenario
mylist<- list(1:50, rep(0,50), rep("NoHarvest", 50))
NoHarvestHWP<- data.frame(mylist)
names(NoHarvestHWP)<- c("Time", "Total", "Scenario")

## Create on data frame
HWP_150AACBurn<- bind_rows(NoHarvestHWP, CCHWP, PH30HWP, PH30PlusHWP, PH60HWP, PH60PlusHWP)

##
ggplot(data=HWP_150AACBurn, aes(x=Time, y=Total, colour=Scenario))+
  geom_line()+
  xlim(1,50)+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (tonnes CO2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs- HWP Model Output")
