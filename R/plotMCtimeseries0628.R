## Load environment using code in "Load MC scenarios 0629.R" or load in compiled file
## Once those are complete, create a master file that has all scenarios and then add code here to load that file
## in before plotting
library(ggplot2)
library(tidyverse)

## If data must be loaded
better_merge<- read.csv("AllScenarios091323.csv") ## Adjust as needed

better_merge$Harvest<- factor(better_merge$Harvest, levels=c("NoHarvest", "PH30", "PH30Plus", "PH60", "PH60Plus", "Clearcut"))
better_merge$Burn<-factor(better_merge$Burn, levels=c("NA", "Burn", "NoBurn"))
better_merge$Rate<- factor(better_merge$Rate, levels=c("NA", "Low", "High"))

## Default 4 color palette in ggplot: "#F8766D" "#7CAE00" "#00BFC4" "#C77CFF"

## Simple plot of one set
better_merge%>%
  filter(Burn=="Burn"|Burn=="NA", StandAge=="150", Rate=="Low"|Rate=="NA")%>%
  ggplot(aes(x=Time, y=ABioMgha, colour=Harvest))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  scale_color_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  scale_linetype_manual(values=c(5, 3, 1))+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Biomass (Mg C/ha)"))

## Ribbon plot version

## better_merge%>%
scenarios2$Harvest<- factor(x=scenarios2$Harvest, levels=c("NoHarvest", "PH30", "PH30Plus", "PH60", "PH60Plus", "Clearcut"))
scenarios2$Rate<- factor(x=scenarios2$Rate, levels=c("NA", "Low", "High"))

scenarios2%>%mutate(Burn=replace_na(Burn, "NA"), Rate=replace_na(Rate, "NA"))%>%
  filter(Burn=="Burn"|Burn=="NA")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Time, y=ABioMgha, fill=Harvest, colour=Harvest))+
  facet_grid(StandAge~Rate)+
  geom_ribbon(aes(x=Time,ymin=ABioMgha-ABio_sd/100, ymax=ABioMgha+ABio_sd/100, fill=Harvest))+
  scale_fill_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  scale_colour_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  geom_line(size=0.2)+
  theme_grey(base_size = 14)+
  scale_linetype_manual(values=c(5, 3, 1))+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Biomass (Mg C/ha)"))

scenarios2%>%mutate(Burn=replace_na(Burn, "NA"), Rate=replace_na(Rate, "NA"))%>%
  filter(Burn=="Burn"|Burn=="NA")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Year, y=TotalCMgha, fill=Harvest, colour=Harvest))+
  facet_grid(StandAge~Rate)+
  ## geom_ribbon(aes(x=Time,ymin=TotalCMgha-TotalC_sd/100, ymax=TotalCMgha+TotalC_sd/100, fill=Harvest))+
  scale_fill_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  scale_colour_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  geom_line(size=0.2)+
  theme_grey(base_size = 14)+
  scale_linetype_manual(values=c(5, 3, 1))+
  xlab("Year")+
  ylab(expression("Total Ecosystem Carbon (Mg C/ha)"))

## Show scenarios for 150Burn at two different harvest rates
better_merge%>%
  filter(Burn=="Burn"|Burn=="NA", StandAge=="150")%>%
ggplot(aes(x=Time, y=ABioMgha, colour=Harvest, lty=Rate))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  scale_color_manual(values=c("#7CAE00","lightblue","#00BFC4","violet","purple","#F8766D" ))+
  scale_linetype_manual(values=c(5, 1, 3))+
  xlab("Simulation Year")+
  ylab(expression("Aboveground Live Biomass (Mg C/ha)"))
  
## Make plot with both stand ages
better_merge%>%
    filter(Burn=="NoBurn"|Burn=="NA")%>%
    ggplot(aes(x=Time, y=ABioMgha, colour=Harvest, lty=Rate))+
    geom_line(size=1)+
  theme_grey(base_size = 14)+
    facet_grid(.~StandAge)+
  scale_color_manual(values=c("#7CAE00", "lightblue","#00BFC4","violet","purple","#F8766D" ))+
    scale_linetype_manual(values=c(5, 1, 3))+
    xlab("Simulation Year")+
    ylab(expression("Aboveground Live Biomass (Mg C/ha)"))

## Plot Total C instead of ABio
better_merge%>%
  filter(Burn=="Burn"|Burn=="NA")%>%
  ggplot(aes(x=Time, y=TotalCMgha, colour=Harvest, lty=Rate))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  facet_grid(.~StandAge)+
  scale_color_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  scale_linetype_manual(values=c(5, 1, 3))+
  xlab("Simulation Year")+
  ylab(expression("Total Carbon (Mg C/ha)"))

## Burn vs No Burn for 150 Year old landscape
better_merge%>%
  filter(StandAge=="150")%>%
  ggplot(aes(x=Time, y=TotalCMgha, colour=Harvest, lty=Rate))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  facet_grid(.~Burn)+
  scale_color_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  scale_linetype_manual(values=c(5, 1, 3))+
  xlab("Simulation Year")+
  ylab(expression("Total Carbon (Mg C/ha)"))

## Burn vs No Burn for 300 year old landscape
better_merge%>%
  filter(StandAge=="300")%>%
  ggplot(aes(x=Time, y=TotalCMgha, colour=Harvest, lty=Rate))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  facet_grid(.~Burn)+
  scale_color_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  scale_linetype_manual(values=c(5, 1, 3))+
  xlab("Simulation Year")+
  ylab(expression("Total Carbon (Mg C/ha)"))



## Examine ToFPS

better_merge%>%
  filter(Burn=="Burn"|Burn=="NA")%>%
  ggplot(aes(x=Time, y=ToFPS, colour=Harvest, lty=Rate))+
  geom_line(size=1)+
  geom_ribbon(aes(x=Time,ymin=ToFPS-ToFPS_sd, ymax=ToFPS+ToFPS_sd), fill="lightgrey")+
  theme_grey(base_size = 14)+
  facet_grid(.~StandAge)+
  scale_color_manual(values=c("#7CAE00", "lightblue","#00BFC4","violet","purple","#F8766D" ))+
  scale_linetype_manual(values=c(5, 1, 3))+
  scale_y_continuous(sec.axis=sec_axis(~.*0.04649, name = bquote(paste("Volume (m"^3," ha"^-1,")"))))+ #conversion factor for just hemlock boles
  ## conversion includes *0.01 to convert units*2 to convert C to biomass, and the Kivari bole coefficient of 0.4302 for hemmlock
  ## a coefficient of 20% cedar, 80% hemlock contribution would be (0.2*0.3233+0.8*0.4302)=0.40882
  xlab("Simulation Year")+
  ylab(expression("Carbon to Forest Products Sector (g/m^2)"))


## Try plotting with Ribbon function




##
ggplot(data=Scenarios_150Burn, aes(x=Time, y=ToFPS, colour=Scenario, lty=HarvestRate))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (g C/m^2)"))+
  ggtitle("AAC 150 Burn: Means from 20 runs- ForCS Output")

## Burn VS NoBurn: 150 Low

ggplot(data=Scenarios_150Low, aes(x=Time, y=TotalC, colour=Scenario, lty=Burn))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("Total C (g C/m^2)"))+
  ggtitle("Low 150 Burn vs NoBurn: Means from 20 runs")

ggplot(data=Scenarios_150Low, aes(x=Time, y=ToFPS, colour=Scenario, lty=Burn))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (g C/m^2)"))+
  ggtitle("Low 150 Burn vs NoBurn: Means from 20 runs- ForCS Output")

## Forest Age: 150 vs 300 for Low Harvest Rate

ggplot(data=Scenarios_LowBurn, aes(x=Time, y=TotalC, colour=Scenario, lty=Age))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("Total C (g C/m^2)"))+
  ggtitle("Low 150 Burn vs NoBurn: Means from 20 runs")

ggplot(data=Scenarios_LowBurn, aes(x=Time, y=ToFPS, colour=Scenario, lty=Age))+
  geom_line()+
  xlab("Simulation Year")+
  ylab(expression("ToFPS (g C/m^2)"))+
  ggtitle("Low 150 Burn vs NoBurn: Means from 20 runs- ForCS Output")
