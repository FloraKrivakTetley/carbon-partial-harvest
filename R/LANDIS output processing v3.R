## This is a simplified script for processing Phase 1 output
library(tidyverse)
library(ggplot2)

## Step 1. ##################################################################################################################
## Read in data.This is long and clunky but I want to be able to tweak individual folders for each scenario combination.
#############################################################################################################################

##############################
## 150 Year old landscape ####
##############################

Baseline<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run38/log_Summary.csv")
Baseline<-
  Baseline%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

Baseline_yrs<- Baseline%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0, StartAge=150, Scenario="NoHarvest", Type="Standard", Volume="None")

Clearcut<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run37/log_Summary.csv")
Clearcut<-
  Clearcut%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

Clearcut_yrs<- Clearcut%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0.2, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline")

PH30<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run39/log_Summary.csv")
PH30<-
  PH30%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

PH30_yrs<- PH30%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0.2, StartAge=150, Scenario="PH30", Type="Standard", Volume="Reduced")

PH30Plus<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run40/log_Summary.csv")
PH30Plus<-
  PH30Plus%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

PH30Plus_yrs<- PH30Plus%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0.6667, StartAge=150, Scenario="PH30Plus", Type="Plus", Volume="Baseline")

PH60<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run41/log_Summary.csv")
PH60<-
  PH60%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

PH60_yrs<- PH60%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0.60, StartAge=150, Scenario="PH60", Type="Standard", Volume="Reduced")

PH60Plus<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run42/log_Summary.csv")
PH60Plus<-
  PH60Plus%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

PH60Plus_yrs<- PH60Plus%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=0.60, StartAge=150, Scenario="PH60Plus", Type="Plus", Volume="Baseline")

#############################
## 300 Year Old landscape ### 
#############################

BaselineOG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run45/log_Summary.csv")
BaselineOG<-
  BaselineOG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

BaselineOG_yrs<- BaselineOG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0, StartAge=300, Scenario="NoHarvest", Type="Standard", Volume="None")

ClearcutOG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run46/log_Summary.csv")
ClearcutOG<-
  ClearcutOG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

ClearcutOG_yrs<- ClearcutOG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Rh=mean(Rh), NEP=mean(NEP), Age=mean(Age), Year=mean(Age)-300, PercentCut=100, StartAge=300, Scenario="Clearcut", Type="Standard", Volume="Baseline")

PH30OG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run47/log_Summary.csv")
PH30OG<-
  PH30OG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

PH30OG_yrs<- PH30OG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0.2, StartAge=300, Scenario="PH30", Type="Standard", Volume="Reduced")

PH30PlusOG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run48/log_Summary.csv")
PH30PlusOG<-
  PH30PlusOG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

PH30PlusOG_yrs<- PH30PlusOG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0.6667, StartAge=300, Scenario="PH30Plus", Type="Plus", Volume="Baseline")

PH60OG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run49/log_Summary.csv")
PH60OG<-
  PH60OG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

PH60OG_yrs<- PH60OG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0.60, StartAge=300, Scenario="PH60", Type="Standard", Volume="Reduced")

PH60PlusOG<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run50/log_Summary.csv")
PH60PlusOG<-
  PH60PlusOG%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

PH60PlusOG_yrs<- PH60PlusOG%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0.60, StartAge=300, Scenario="PH60Plus", Type="Plus", Volume="Baseline")

########################
## AAC Target Harvest ##
########################

ClearcutAAC<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run54/log_Summary.csv")
ClearcutAAC<-
  ClearcutAAC%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

ClearcutAAC_yrs<- ClearcutAAC%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=100, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline")

###############################
## Clearcut Max Harvest rate ##
###############################

ClearcutMax<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_MAIN/Run31/log_Summary.csv")
ClearcutMax<-
  ClearcutMax%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+150)

ClearcutMax_yrs<- ClearcutMax%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC),ABio=mean(ABio),BBio=mean(BBio),DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-150, PercentCut=100, StartAge=150, Scenario="Clearcut", Type="Standard", Volume="Baseline")

#########################
## Mixed Age Landscape ##
#########################

BaselineMixed<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1b/Run52/log_Summary.csv")
BaselineMixed<-
  BaselineMixed%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=NA)

BaselineMixed_yrs<- BaselineMixed%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=NA, Year=max(Time), PercentCut=0, StartAge="Mixed", Scenario="NoHarvest", Type="Standard", Volume="None")

ClearcutMixed<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1b/Run51/log_Summary.csv")
ClearcutMixed<-
  ClearcutMixed%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=NA)

ClearcutMixed_yrs<- ClearcutMixed%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=NA, Year=max(Time), PercentCut=100, StartAge="Mixed", Scenario="Clearcut", Type="Standard", Volume="Baseline")

##############################################################

Harvest_clearcut<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run37/harvest/Summarylog.csv")

Harvest_clearcut$HarvestMgperha<- Harvest_clearcut$TotalBiomassHarvested/(40000)
Harvest_clearcut$TotalVolume<- Harvest_clearcut$TotalBiomassHarvested/0.5811
Harvest_clearcut$Volume<- Harvest_clearcut$HarvestMgperha/0.5811

ggplot(data=Harvest_clearcut, aes(x=Time, y=TotalVolume))+
  geom_point()+
  theme_bw()+
  ylab(bquote(paste("Volume Harvested (m"^3,")")))
      

## GGPLOT FIGURES USING LONG FORM DATA

Phase1_150<- rbind(Baseline_yrs, Clearcut_yrs, PH30_yrs, PH30Plus_yrs, PH60_yrs, PH60Plus_yrs)
Phase1_150$Scenario<- factor(Phase1_150$Scenario, levels=c("NoHarvest","PH30", "PH60","PH30Plus", "PH60Plus", "Clearcut"))

Phase1_300<- rbind(BaselineOG_yrs, ClearcutOG_yrs, PH30OG_yrs, PH30PlusOG_yrs, PH60OG_yrs, PH60PlusOG_yrs)
Phase1_300$Scenario<- factor(Phase1_300$Scenario, levels=c("NoHarvest","PH30", "PH60","PH30Plus", "PH60Plus", "Clearcut"))

Phase1_Mix<- rbind(ClearcutMixed_yrs)

Phase1ALL<- rbind(Phase1_150, Phase1_300)

Yr50<-
  Phase1ALL%>%
  filter(Year==50)
Yr50$StartAge<- as.factor(Yr50$StartAge)
vars(Yr50)
  
## Boxplot

ggplot(data=Yr50, aes(x=Scenario, y=ABio))+
  facet_grid(rows=vars(Age))+
  geom_boxplot()+
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Standing Volume (m"^3," ha"^-1,")"))))+
  # ylim(15000,21000)+
  ylab(bquote(paste("Aboveground Live Carbon "," (g m"^-2,") at Year 50")))

## Figure prep for NSC presentation

## Fig. 1 Plot 50 years with no harvest.
## Use Baseline_yrs
## Fig. 2 Add Clearcut at 0.2%/yr
TwoRuns<- rbind(Baseline_yrs, Clearcut_yrs)
## Fig. 3 Add in the two partial cutting scenarios
FourRuns<- rbind(Baseline_yrs, Clearcut_yrs, PH30_yrs, PH60_yrs)
## Fig. 4 Compare just what happens with 30PH
PH30Set<- rbind(Baseline_yrs, Clearcut_yrs, PH30_yrs, PH30Plus_yrs)

PH60Set<- rbind(Baseline_yrs, Clearcut_yrs, PH60_yrs, PH60Plus_yrs)
## Compare clearcut harvests at 0.2, 0.6 and 1%/yr
ClearcutSet<- rbind(Clearcut_yrs, ClearcutMax_yrs)
## ClearcutAAC_yrs,

## Next we can combine all these rows and use ggplot to create a stacked 2-panel figure


## Color palette
library(wesanderson)
pal<- wes_palette("Zissou1", 6, type="continuous")

## This ggplot figure combines the two stand ages (comment out the facet grid line and edit data to plot single age runs)
## Aboveground Carbon
ggplot(data=Clearcut_yrs, aes(x=Year, y=ABio, color=Scenario, lty=Volume, size=Type))+
  facet_grid(StartAge~., labeller=label_both)+
  geom_line()+
  # scale_fill_gradientn(colours=pal)+
  scale_color_manual(values= c("NoHarvest"="black",
                               "Clearcut"="brown1",
                               "PH30"="chartreuse2",
                               "PH30Plus"="chartreuse2",
                               "PH60"="blue2",
                               "PH60Plus"="blue2"))+
  scale_linetype_manual(name="Harvest Volume",
                        values=c("None"=1,
                                 "Baseline"=1,
                                 "Reduced"=2))+
  scale_size_manual(values=c("None"=2,
                             "Standard"=1,
                             "Plus"=1))+
  # labs(color=Scenario, linetype=) ## work on composite label
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ ## this is for bole only
  guides(size="none")+
  #ylim(19000,24000)+
  theme_bw()+
  xlab("Simulation Year")+
  ylab(bquote(paste("Aboveground Live Carbon "," (g m"^-2,")")))
  # ylab(bquote(paste("Net Primary Productivity ", " (g m"^-2, "per year)")))

## For harvest rate comparisons

ggplot(data=Baseline_yrs, aes(x=Year, y=ABio))+
  geom_line(size=1)+
  facet_grid(StartAge~., labeller=label_both)+
  geom_line(data=Clearcut_yrs, colour="red", size=1)+
  annotate("text", x=45, y=19750, label="Harvest rate: 0.2%/yr")+
  geom_line(data=ClearcutMax_yrs, aes(x=Year, y=ABio), colour="darkred", size=1)+
  annotate("text", x=40, y=14200, label="Harvest rate: 1%/yr")+
  geom_line(data=ClearcutAAC_yrs, aes(x=Year, y=ABio), colour="orange", size=1)+
  annotate("text", x=42, y=17000, label="Harvest rate: 0.6%/yr")+
  # ylim(14000, 24000)+
  theme_bw()+
  scale_y_continuous(lim=c(14000,24000), sec.axis=sec_axis(~.*0.0344, name = bquote(paste("Standing Volume (m"^3," ha"^-1,")"))))+ ## for bole plus branches plus foliage
  xlab("Simulation Year")+
  ylab(bquote(paste("Aboveground Live Carbon "," (g m"^-2,")")))

## See notes for calculation of conversion factor for second axis. Uses 4 regression coefficients from Kivari et al. and conversion 
## between g C per sq m and tonnes biomess per ha.

## NPP
ggplot(data=Phase1ALL, aes(x=Year, y=NPP, color=Scenario))+
  facet_grid(StartAge~.)+
  geom_line()+
  scale_fill_gradientn(colours=pal)+
  theme_bw()+
  # ylim(590, 670)+
  xlab("Simulation Year")+
  ylab(bquote(paste("Net Primary Productivity ", "(g m"^-2, "per year)")))

## NBP
ggplot(data=Phase1ALL, aes(x=Year, y=NBP, color=Scenario))+
  facet_grid(StartAge~.)+
  geom_line()+
  scale_fill_gradientn(colours=pal)+
  theme_bw()+
  # ylim(-250, 100)+
  xlab("Stand Age")+
  ylab(expression("Net Biome Productivity (g/m^2/yr)"))

## DOM
ggplot(data=Phase1ALL, aes(x=Year, y=TotalC, color=Scenario))+
  facet_grid(StartAge~.)+
  geom_line()+
  scale_fill_gradientn(colours=pal)+
  theme_bw()+
  # ylim(-250, 100)+
  xlab("Stand Age")+
  ylab(expression("Total Carbon (g/m^2)"))

## GGPLOT FIGURES USING INDIVIDUAL DATA SETS

ggplot(data=Clearcut_yrs, aes(x=Age, y=LiveC))+
  geom_line()+
  geom_line(data=BL300seed_yrs, aes(x=Age, y=LiveC), colour="lightblue")+
  geom_line(data=BL300_yrs, aes(x=Age, y=LiveC), colour="darkblue")+
  geom_line(data=Clearcut_yrs, aes(x=Age, y=LiveC), colour="red")+
  geom_line(data=PH30_yrs, aes(x=Age, y=LiveC), colour="darkorange")+
  geom_line(data=PH60_yrs, aes(x=Age, y=LiveC), colour="pink")+
  geom_line(data=PH60Plus_yrs, aes(x=Age, y=LiveC), colour="magenta")+
  ## geom_line(data=PH33_yrs, aes(x=Age, y=LiveC), colour="purple")+ ## this is at 1%/yr harvest rate
  geom_line(data=PH30Plus_yrs, aes(x=Age, y=LiveC), colour="purple")+
  ## ylim(0, 30000)+
  xlab("Stand Age")+
  ylab(expression("Live Carbon (g/m^2)"))

ggplot(data=Baseline300_yrs, aes(x=Age, y=ABio))+
  geom_line()+
  ##geom_line(data=BL300seed_yrs, aes(x=Age, y=ABio), colour="lightblue")+
  geom_line(data=Clearcut300seed_yrs, aes(x=Age, y=ABio), colour="blue")+
  geom_line(data=Clearcut300_yrs, aes(x=Age, y=ABio), colour="red")+
  ##geom_line(data=PH30_yrs, aes(x=Age, y=ABio), colour="darkorange")+
  ##geom_line(data=PH60_yrs, aes(x=Age, y=ABio), colour="pink")+
  ##geom_line(data=PH60Plus_yrs, aes(x=Age, y=ABio), colour="magenta")+
  ## geom_line(data=PH33_yrs, aes(x=Age, y=ABio), colour="purple")+ ## this is at 1%/yr harvest rate
  ##geom_line(data=PH30Plus_yrs, aes(x=Age, y=ABio), colour="purple")+
  ## ylim(0, 30000)+
  xlab("Stand Age")+
  ylab(expression("Aboveground Carbon (g/m^2)"))

ggplot(data=Baseline_yrs, aes(x=Time, y=NBP))+
  geom_line()+
  geom_line(data=BL300_yrs, aes(x=Time, y=NBP), colour="darkblue")+
  ##geom_line(data=PH30_yrs, aes(x=Time, y=NBP), colour="darkorange")+
  geom_line(data=Clearcut_yrs, aes(x=Time, y=NBP), colour="red")+
  ##geom_line(data=PH60_yrs, aes(x=Time, y=NBP), colour="pink")+
  geom_line(data=PH60Plus_yrs, aes(x=Time, y=NBP), colour="magenta")+
  geom_line(data=NoHarvest_yrs, aes(x=Time, y=NBP), colour="purple")+
  xlab("Simulation Year")+
  ylab(expression("Net Biome Production (g/m^2/yr)"))

ggplot(data=Baseline_yrs, aes(x=Age, y=NPP))+
  geom_line()+
  geom_line(data=BL300seed_yrs, aes(x=Age, y=NPP), colour="lightblue")+
  geom_line(data=BL300_yrs, aes(x=Age, y=NPP), colour="darkblue")+
  geom_line(data=PH30_yrs, aes(x=Age, y=NPP), colour="darkorange")+
  geom_line(data=Clearcut_yrs, aes(x=Age, y=NPP), colour="red")+
  geom_line(data=PH60_yrs, aes(x=Age, y=NPP), colour="pink")+
  geom_line(data=PH60Plus_yrs, aes(x=Age, y=NPP), colour="magenta")+
  geom_line(data=NoHarvest_yrs, aes(x=Age, y=NPP), colour="purple")+
  xlab("Stand Age")+
  ylab(expression("Net Primary Production (g/m^2/yr)"))


## Comparison for Caren

ggplot(data=ClearcutOG_yrs, aes(x=Age, y=NPP), colour="blue")+
  geom_line()+
  annotate("text", x=350, y=600, label="NPP")+
  geom_line(data=ClearcutOG_yrs, aes(x=Age, y=Rh), colour="darkorange")+
  annotate("text", x=304, y=615, label="Rh")+
  geom_line(data=ClearcutOG_yrs, aes(x=Age, y=NEP), colour="purple")+
  annotate("text", x=310, y=20, label="NEP")+
  geom_line(data=ClearcutOG_yrs, aes(x=Age, y=NBP), colour="green")+
  annotate("text", x=305, y=-25, label="NBP")+
  xlab("Stand Age")+
  ylab(expression("g/m^2/yr"))


## OLD CODE FROM THIS ROUND
Clearcut300seed<- read.csv("C:/Users/fkrivakt/OneDrive - UBC/Desktop/Scenarios_Phase1/Run44/log_Summary.csv")
Clearcut300seed<-
  BL300seed%>%
  mutate(TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, Age=Time+300)

Clearcut300seed_yrs<- Clearcut300seed%>%
  group_by(Time)%>%
  summarise(TotalC=mean(TotalC), LiveC=mean(LiveC), ABio=mean(ABio), BBio=mean(BBio), DOM=mean(TotalDOM), 
            NBP=mean(NBP), NPP=mean(NPP), Age=mean(Age), Year=mean(Age)-300, PercentCut=0, StartAge=300, Scenario="Clearcut_seed")
