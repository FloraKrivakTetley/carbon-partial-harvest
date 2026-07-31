## Waste output comparison

library(tidyverse)
library(ggplot2)

## Step 1. ################################################
## Read in data.
###########################################################


## FPI modelling waste values
FPI150Burn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run67/log_Summary.csv")
FPI150Burn<-
  FPI150Burn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

FPI150Burn_yrs<- FPI150Burn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0),Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Mature", Burn="Yes", Waste="FPI")
##
FPI150NoBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run69/log_Summary.csv")
FPI150NoBurn<-
  FPI150NoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

FPI150NoBurn_yrs<- FPI150NoBurn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0),Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Mature", Burn="No", Waste="FPI")

## FPI 300
FPI300Burn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run72/log_Summary.csv")
FPI300Burn<-
  FPI300Burn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

FPI300Burn_yrs<- FPI300Burn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0),Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=300, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Old Growth", Burn="Yes", Waste="FPI")
##

FPI300NoBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run74/log_Summary.csv")
FPI300NoBurn<-
  FPI300NoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

FPI300NoBurn_yrs<- FPI300NoBurn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0),Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=300, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Old Growth", Burn="No", Waste="FPI")

## Northern data waste values; Burn scenarios

North150Burn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run68/log_Summary.csv")
North150Burn<-
  North150Burn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

North150Burn_yrs<- North150Burn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0), Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Mature", Burn="Yes", Waste="North")

North300Burn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run71/log_Summary.csv")
North300Burn<-
  North300Burn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

North300Burn_yrs<- North300Burn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0), Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=300, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Old Growth", Burn="Yes", Waste="North")

## North No Burn Scenarios

North150NoBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run70/log_Summary.csv")
North150NoBurn<-
  North150NoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

North150NoBurn_yrs<- North150NoBurn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0), Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Mature", Burn="No", Waste="North")

## North 300 No Burn
North300NoBurn<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_TestWaste/Run73/log_Summary.csv")
North300NoBurn<-
  North300NoBurn%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

North300NoBurn_yrs<- North300NoBurn%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), Rh=mean(Rh),
            NBP=mean(NBP), NPP=mean(NPP), FPS=sum(ToFPS)/sum(ToFPS !=0), HaCut=sum(ToFPS !=0), Age=mean(Age), Year=mean(Age)-150, 
            PercentCut=0.2, StartAge=300, Scenario="Clearcut", Type="Standard", Volume="Baseline", AgeClass="Mature", Burn="No", Waste="North")

###################################
## GGPLOT GRAPHING ################
## Compare aboveground live carbon
ggplot(data=FPI150Burn_yrs, aes(x=Year, y=ABio, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=ABio), size=1.5)+#, colour="darkorange")+
  geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=ABio), size=1.5)+#, colour="pink")+
  geom_line(data=North150NoBurn_yrs, aes(x=Year, y=ABio), size=1.5)+ #colour="magenta")+
  geom_line(data=North300Burn_yrs, aes(x=Year, y=ABio), size=1.5)+
  geom_line(data=FPI300Burn_yrs, aes(x=Year, y=ABio), size=1.5)+
  geom_line(data=FPI300NoBurn_yrs, aes(x=Year, y=ABio), size=1.5)+
  ## ylim(0, 30000)+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Carbon (g/m^2)"))

## Compare Total C

ggplot(data=FPI150Burn_yrs, aes(x=Year, y=TotalC, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=TotalC), size=1.5)+ #colour="darkorange")+
  geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=TotalC), size=1.5)+ #colour="red")+
  geom_line(data=North150NoBurn_yrs, aes(x=Year, y=TotalC), size=1.5)+ #colour="magenta")+
  geom_line(data=North300Burn_yrs, aes(x=Year, y=TotalC), size=1.5)+
  geom_line(data=FPI300Burn_yrs, aes(x=Year, y=TotalC), size=1.5)+
  geom_line(data=North300NoBurn_yrs, aes(x=Year, y=TotalC), size=1.5)+
  geom_line(data=FPI300NoBurn_yrs, aes(x=Year, y=TotalC), size=1.5)+
  ## ylim(0, 30000)+
  xlab("Simulation Year")+
  ylab(expression("Total Carbon (g/m^2)"))

# Plot Rh

ggplot(data=FPI150Burn_yrs, aes(x=Year, y=Rh, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=Rh), size=1.5)+ #colour="darkorange")+
  geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=Rh), size=1.5)+ #colour="red")+
  geom_line(data=North150NoBurn_yrs, aes(x=Year, y=Rh), size=1.5)+ #colour="magenta")+
  ylim(400, 700)+
  xlab("Simulation Year")+
  ylab(expression("Rh (g/m^2)"))

## Look at NPP

ggplot(data=FPI150Burn_yrs, aes(x=Year, y=NPP, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=NPP), size=1.5)+ #colour="darkorange")+
  geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=NPP), size=1.5)+ #colour="red")+
  geom_line(data=North150NoBurn_yrs, aes(x=Year, y=NPP), size=1.5)+ #colour="magenta")+
  ## ylim(0, 30000)+
  xlab("Simulation Year")+
  ylab(expression("NPP (g/m^2)"))

## DOM over time
ggplot(data=FPI150Burn_yrs, aes(x=Year, y=DOM, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=DOM), size=1.5)+ #colour="darkorange")+
  geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=DOM), size=1.5)+ #colour="red")+
  geom_line(data=North150NoBurn_yrs, aes(x=Year, y=DOM), size=1.5)+ #colour="magenta")+
  ## ylim(0, 30000)+
  xlab("Simulation Year")+
  ylab(expression("DOM (g/m^2)"))

## Validation Step Looking at ToFPS

## 150 Year old stands
ggplot(data=FPI150Burn_yrs, aes(x=Year, y=FPS, linetype=AgeClass, colour=Waste))+
  geom_line(size=1.5)+
  # geom_line(data=FPI150NoBurn_yrs, aes(x=Year, y=FPS), size=1.5)+
  geom_line(data=North150Burn_yrs, aes(x=Year, y=FPS), size=1.5)+ 
  geom_line(data=North300Burn_yrs, aes(x=Year, y=FPS), size=1.5)+
  geom_line(data=FPI300Burn_yrs, aes(x=Year, y=FPS), size=1.5)+
  # geom_line(data=North150NoBurn_yrs, aes(x=Year, y=FPS), size=1.5)+ 
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ #conversion factor for just hemlock boles
  geom_hline(yintercept=9423.771)+
  annotate("text", x=10, y=9550, label="Date Creek Merch Volume")+
  geom_hline(yintercept=12096)+
  annotate("text", x=10, y=12200, label="Gitanyow Max Merch Volume Estimate")+
  ## ylim(8000, 100000)+
  xlab("Simulation Year")+
  ylab(expression("Carbon to FPS in Harvested Areas (g/m^2)"))

## 300 Year old stands
ggplot(data=FPI300Burn_yrs, aes(x=Year, y=FPS, linetype=Burn, colour=Waste))+
  geom_line(size=1.5)+
  geom_line(data=North300Burn_yrs, aes(x=Year, y=FPS), size=1.5)+
  geom_line(data=FPI300Burn_yrs, aes(x=Year, y=FPS), size=1.5)+
  geom_line(data=FPI300NoBurn_yrs, aes(x=Year, y=FPS), size=1.5)+
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ #conversion factor for just hemlock boles
  geom_hline(yintercept=9423.771)+
  annotate("text", x=10, y=9550, label="Date Creek Merch Volume")+
  geom_hline(yintercept=12096)+
  annotate("text", x=10, y=12200, label="Gitanyow Max Merch Volume Estimate")+
  ## ylim(8000, 100000)+
  xlab("Simulation Year")+
  ylab(expression("Carbon to FPS in Harvested Areas (g/m^2)"))

ggplot(data=FPI150Burn_yrs, aes(x=Year, y=FPS, shape=Burn, colour=Waste, size=Waste))+
  geom_point()+
  geom_point(data=North150Burn_yrs, aes(x=Year, y=FPS))+ ##, size=1.5)+ #colour="darkorange")+
  geom_point(data=FPI150NoBurn_yrs, aes(x=Year, y=FPS))+ ##, size=1.5)+ #colour="red")+
  geom_point(data=North150NoBurn_yrs, aes(x=Year, y=FPS))+ ##, size=1.5)+ #colour="magenta")+
  geom_point(data=North300Burn_yrs, aes(x=Year, y=FPS))+ ##, size=1.5)+
  geom_point(data=FPI300Burn_yrs, aes(x=Year, y=FPS))+ ##, size=1.5)+
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ #conversion factor for just hemlock boles
  geom_hline(yintercept=9423.771)+
  annotate("text", x=10, y=9550, label="Date Creek Merch Volume")+
  geom_hline(yintercept=12096)+
  annotate("text", x=10, y=12200, label="Gitanyow Max Merch Volume Estimate")+
  ## ylim(8000, 100000)+
  xlab("Simulation Year")+
  ylab(expression("Carbon to FPS in Harvested Areas (g/m^2)"))

## messing around trying to get all variables on the same graph

ggplot(data=North300Burn_yrs, aes(x=Year, y=FPS, colour=AgeClass, size=Waste, shape=Burn))+
  geom_point()+
  geom_point(data=FPI300Burn_yrs, aes(x=Year, y=FPS))+
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ #conversion factor for just hemlock boles
  xlab("Simulation Year")+
  ylab(expression("Carbon to FPS in Harvested Areas (g/m^2)"))
