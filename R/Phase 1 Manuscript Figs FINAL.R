## Code for Figures for FCI Phase 1 Manuscript
## Author: Flora Krivak-Tetley
## Contact: florakrivaktetley@gmail.com


library(tidyverse)
library(viridis)
library(colorblindcheck)
library(ggplot2)

# Check 

pools_pal<-c("#7D26CD", "#FFFF00","#7CAE00", "#8B3626", "#D2B48C") ## good for all colorblind types
scenarios_pal<- c("#30123BFF","#3E9BFEFF", "#46F884FF","#E1DD37FF", "#F05B12FF","#7A0403FF")

## to test colorblind interpretation of a graph, name graph "gg" then run the following:

palette_check(pools_pal, plot=TRUE)
palette_check(scenarios_pal, plot=TRUE)

## for image rendering
info<-ggplot_build(gg)$data

Drive<- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2"
setwd(paste0(Drive))

## FCIData<- read.csv("ScenariosWithHWP04Oct.csv") ## note this doesn't have all flux calculations added
## FCIDataFullPH<- read.csv("ScenariosWithHWPandAllNH26Apr24.csv")
FCIDataFullPH<- read.csv("ScenarioOutputCorrected09Oct24.csv")

## Check factor ordering, use some version of this as needed
FCIDataFullPH$Harvest<- factor(FCIDataFullPH$Harvest, levels=c("NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus", "Clearcut"))
FCIDataFullPH$Rate<- factor(FCIDataFullPH$Rate, levels=c("Low", "High"))

## To create a version with extra rows removed (as needed)
FCIDataFullPHNoDuplicates <- FCIDataFullPH%>%
  select(-X.2)%>%
  distinct(Time, Scenario, .keep_all=TRUE)


write.csv(FCIDataFullPHNoDuplicated, "FCIDataFullPHNoDuplicates13Feb2025.csv")
## Read this file for use in analysis and making figures OR rerun the code above

age_labels <- c("Mature Forest", "Old Forest")
names(age_labels)<- c("150", "300")
rate_labels<- c("Low Baseline", "High Baseline")
names(rate_labels)<- c("Low", "High")
flux_labels<- c("Net Ecosystem Productivity", "Substitution", "Slash Burning", "Harvested Wood Products")
names(flux_labels) <- c("NEP_CO2eha", "sub1", "ToAirBurn_CO2e", "yrly_HWPfluxhaNoFossil")
harvest_labels <- c("No Harvest", "PH60", "Clearcut")
names(harvest_labels)<- c("NoHarvest", "PH60", "Clearcut")

## Figure 1 ##
## MAP. See ArcGIS and Figure 1c raster script.

## Figure 2. Carbon pools in unharvested and clearcut landscapes over 50 years

CarbonPools<-
  FCIDataFullPH%>%
  select("Scenario", "Year", "Time", "StandAge", "Burn", "Rate", "Harvest", "HarvestIntensity", "PH", "NEP", 
         "ABio_CO2eha", "BBio_CO2eha","TotalDOM_CO2eha", "ProductsCO2eha", "LandfillCO2eha")%>%
  pivot_longer(cols=c(ABio_CO2eha, BBio_CO2eha, TotalDOM_CO2eha, ProductsCO2eha, LandfillCO2eha), names_to="CarbonPool")%>%
  mutate(Year=as.numeric(Year))

CarbonPools$CarbonPool<- factor(CarbonPools$CarbonPool, levels=c("LandfillCO2eha", "ProductsCO2eha",
                                                               "ABio_CO2eha", "BBio_CO2eha", "TotalDOM_CO2eha"))

## Graph just two scenarios
CarbonPools%>%filter(StandAge==150, Rate=="High", Burn=="Burn", Harvest=="NoHarvest"|Harvest=="Clearcut"|Harvest=="PH60")%>%
  ggplot(aes(x=Year, y=value, group=CarbonPool))+
  geom_area(aes(fill=CarbonPool), position='stack')+
  facet_grid(.~Harvest, labeller=labeller(Harvest=harvest_labels))+
  scale_fill_manual(values=c("#7D26CD", "#FFFF00","#7CAE00", "#8B3626", "#D2B48C"), 
                    labels=c("Landfills", "Wood Products", "Aboveground Live", "Belowground Live", "DOM and Soils"),
                    name="Carbon Pool")+ # checked colorblind safe
  theme_classic(base_size=14)+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))+
  scale_y_continuous(sec.axis=sec_axis(~.*(12/44), name=(bquote(paste("Mg C ha"^-1)))))+
  ylab(bquote(paste("tonnes CO "[2],"e ha"^-1)))

## Figure 3. Total System C- Burn scenarios

FCIDataFullPH%>% filter(Burn=="Burn")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Year, y=TotalSystemCo2eha, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_classic(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  # scale_fill_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  # scale_colour_manual(values=c("#7CAE00","lightblue", "#00BFC4","violet", "purple", "#F8766D" ))+
  theme_classic(base_size = 14)+
  ylim(1800, 2300)+
  xlab("Year")+
  ylab(bquote(paste("Total system carbon (tonnes CO "[2],"e ha"^-1,")")))

## Figure 4 Burn-No Burn

FCIDataFullPH%>% filter(StandAge==300, Rate=="High", Harvest != "NoHarvest")%>% 
  ggplot(aes(x=Year, y=TotalSystemCo2eha, fill=Harvest, colour=Harvest, lty=Burn))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  scale_fill_manual(values=c("#3E9BFEFF", "#46F884FF","#E1DD37FF", "#F05B12FF","#7A0403FF" ))+
  scale_colour_manual(values=c("#3E9BFEFF", "#46F884FF","#E1DD37FF","#F05B12FF","#7A0403FF" ))+
  theme_bw(base_size = 14)+
  scale_linetype_manual(values=c(1,3))+
  xlab("Year")+
  ylab(bquote(paste("Total system carbon (tonnes CO "[2],"e ha"^-1,")")))


## Figure 6
## Cumulative flux figure

CumFluxFig <- 
  FCIDataFullPH%>%
  filter(Burn=="Burn", Year==2030|Year==2050|Year==2075)%>%
  mutate(Year=factor(Year, levels=c("2030", "2050", "2075")))%>%
  ggplot(aes(x=Year, y=CumNSPCO2ehaNoFossil, fill=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw(base_size=14)+
  geom_hline(yintercept=0, color="black", lty=2)+
  scale_fill_viridis(discrete=TRUE, option="H")+
  ylab(bquote(paste("Cumulative NSP (tonnes CO "[2],"e ha"^-1,")")))+
  ylim(-300, 150)
print(CumFluxFig)

## Figure 5 Compare flux sources

stacked_data <- FCIDataFullPH %>%
  select(Time, Year, Scenario, Harvest, Percentage, HarvestIntensity, StandAge, Rate, Burn, NEP_CO2eha, ToAirBurn_CO2eha,
         yrly_HWPfluxhaNoFossil, sub1)%>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075)) %>%
  mutate(ToAirBurn_CO2eha = -1*ToAirBurn_CO2eha, yrly_HWPfluxhaNoFossil = -1*yrly_HWPfluxhaNoFossil, sub1 = sub1/40000, 
         TotalFlux = NEP_CO2eha+ToAirBurn_CO2eha + yrly_HWPfluxhaNoFossil + sub1, TotalNSP=NEP_CO2eha+ToAirBurn_CO2eha + yrly_HWPfluxhaNoFossil)%>%
  pivot_longer(cols = c('NEP_CO2eha', 'ToAirBurn_CO2eha', 'yrly_HWPfluxhaNoFossil', 'sub1'),
               names_to = 'FluxSource', values_to = 'CarbonFlux')

stacked_data$Rate<- factor(stacked_data$Rate, levels=c("Low", "High"))
stacked_data$FluxSource<- factor(stacked_data$FluxSource, levels =c("sub1", "yrly_HWPfluxhaNoFossil", "ToAirBurn_CO2eha", "NEP_CO2eha"))

CC <- stacked_data%>%filter(Harvest=="Clearcut", Burn=="Burn")
  
ggplot(data=CC)+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  theme_bw(base_size=14)+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(data = CC, aes(x=Year, y=TotalNSP))+
  geom_line(data = CC, aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Carbon flux (tonnes CO "[2],"e ha"^-1,"yr"^-1,")")))


PH_30<- stacked_data%>%filter(Harvest=="PH30", Burn=="Burn")

# ggplot(aes(fill = FluxSource, y = CarbonFlux, x=Year))+
ggplot(data=PH_30)+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(data = PH_30, aes(x=Year, y=TotalNSP))+
  geom_line(data = PH_30, aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Annual NSP (tonnes CO  "[2],"e ha"^-1,")")))

## Figure 8

## REGULAR VERSION ##
Fig8Data<- FCIDataFullPH%>%mutate(Year=as.numeric(Year))%>% ##, HarvestIntensity=as.factor(HarvestIntensity))%>%
  filter(Time==50, Burn=="Burn", StandAge=="150")%>%
  select("PH", "AvgFPSMgha", "CumNSPCO2ehaNoFossil", "Scenario")
Fig8Data[1,1] <- "Yes"
Fig8Data<- Fig8Data%>%
  mutate(ScenarioType = ifelse(PH=="Yes", "Partial Harvest", "Clearcut"),
         AvgFPSTotal = AvgFPSMgha*40000)

  ggplot(data=Fig8Data, aes(x=AvgFPSTotal, y=CumNSPCO2ehaNoFossil, shape=ScenarioType, lty=ScenarioType))+
  geom_point(size=3)+ ##, shape=HarvestIntensity))+
  geom_smooth(method="lm", linewidth=1, color = "black",fullrange=TRUE, se=FALSE)+
  theme_classic(base_size = 14)+
  scale_shape_manual(values=c(15,1), name="Scenario Type", labels=c("Clearcut", "Partial Harvest"))+
  scale_linetype_manual(values=c(1,1), name="Scenario Type")+
  theme(legend.position=c(0.85,0.85), plot.margin=margin(0,2,0 ,0 , "cm"))+
  # scale_shape_manual(values=c(16,17))+
  geom_hline(yintercept=0, color="black", lty=2, size=1)+
  scale_x_continuous(sec.axis=sec_axis(~.*2/0.40882*1/1000, 
    name = bquote(paste("Mean annual volume to forest products sector (1000 m"^3,")"))), expand=c(0,0, 0, 150))+
  xlab("Mean annual biomass to forest products sector (Mg C)")+
  ylab(bquote(paste("Cumulative NSP at year 2075 (tonnes CO  "[2],"e ha"^-1,")")))
  
## updated stats 27 Feb 2025  
library(jtools)
    
anova(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal * ScenarioType, data=Fig8Data))
summ(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal * ScenarioType, data=Fig8Data))
coef(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal * ScenarioType, data=Fig8Data))

## F statistics in paper from anova(lm)

## Difference between slopes

## get regression coefficients for clearcut line
Fig8DataCC<- Fig8Data%>%
  filter(ScenarioType=="Clearcut")
  summary(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal, data=Fig8DataCC))
  
## Clearcut: y = 149.8 - 0.009258x
  
  Fig8DataPH<- Fig8Data%>%
    filter(ScenarioType=="Partial Harvest")
  summary(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal, data=Fig8DataPH))

## PH: y = 150.3 -0.007155x

library(car)
car::Anova(lm(CumNSPCO2ehaNoFossil ~ AvgFPSTotal*ScenarioType, data=Fig8Data))


## Figure 9


AllRoadsCombined<- read.csv(file="AllRoadsCombined22May24.csv")  
AllRoadsCombined$Harvest<- factor(AllRoadsCombined$Harvest, levels=c("PH30", "PH60", "PH30Plus", "PH60Plus", "Clearcut"))

AllRoadsCombined%>%filter(StandAge==150, Harvest%in%c("PH30", "PH60", "PH30Plus", "PH60Plus", "Clearcut"))%>%
  mutate(PH=case_when(Harvest=="NoHarvest"|Harvest=="Clearcut" ~ "No",Harvest=="CC"~ "No", Harvest=="PH30"~"Yes",
                      Harvest=="PH30Plus"~"Yes", Harvest=="PH60"~"Yes",Harvest=="PH60Plus" ~ "Yes"))%>%
  ggplot(aes(x=Percentage, y=Road.density.km, colour=Harvest))+
  geom_point(size=4, shape=16)+
  theme_classic(base_size = 14)+
  scale_colour_manual(values=c("#3E9BFEFF", "#46F884FF","#E1DD37FF","#F05B12FF","#7A0403FF" ))+
  # scale_shape_manual(values=c(16:18,15))+
  geom_hline(yintercept=0.6,lty=2)+
  geom_hline(yintercept=1.6, lty=2)+
  ylim(c(0,1.8))+
  xlab("Percentage of landscape harvested per year (%)")+
  ylab(bquote(paste("2075 Road density (km km"^-2,")")))


## SUPPLEMENTAL FIGURES AND TABLES

## Appendix S2 Table 1
## Summarize mean flux values for master table
FluxSummary <- FCIDataFullPH%>%
  select(Time, Year, Scenario, Harvest, Percentage, HarvestIntensity, StandAge, Rate, Burn, NSPCO2ehaNoFossil, ToFPS_CO2eha, ToFPSMgha,
         ToFPS_m3ha, ToFPS_m3harvested, NPPMgha, RhMgha)%>%
  mutate(Stand=ifelse(StandAge == 150, "Mature", "Old"))%>%
  
  group_by(Scenario)%>%
  summarise(across((NSPCO2ehaNoFossil:RhMgha), mean))

write.csv(FluxSummary, file="FluxSummary13Feb2025.csv")

CumFluxSummary <- FCIDataFullPH %>%
  select(Time, Year, Scenario, Harvest, Percentage, HarvestIntensity, StandAge, Rate, Burn, CumNSPCO2ehaNoFossil,CumNSPCO2ehaWithFossil, CumNSPCO2ehaNoFossilWithSub1)%>%
  filter(Time==50)
write.csv(CumFluxSummary, file="CumFluxSummary13Feb2025.csv")
  
summarise(by=c(Scenario))
# Full Pool graphs for 4 sets. 

PoolColors<- c("#7D26CD", "#FFFF00","#7CAE00", "#8B3626", "#D2B48C")

CarbonPools<-
  FCIDataFullPH%>%
  select("Scenario", "Year", "Time", "StandAge", "Burn", "Rate", "Harvest", "HarvestIntensity", "PH", "NEP", 
         "ABio_CO2eha", "BBio_CO2eha","TotalDOM_CO2eha", "ProductsCO2eha", "LandfillCO2eha" )%>%
  pivot_longer(cols=c(ABio_CO2eha, BBio_CO2eha, TotalDOM_CO2eha, ProductsCO2eha, LandfillCO2eha), names_to="CarbonPool")
mutate(Year=as.numeric(Year))

CarbonPools$CarbonPool<- factor(CarbonPools$CarbonPool, levels=c("LandfillCO2eha", "ProductsCO2eha",
                                                                 "ABio_CO2eha", "BBio_CO2eha", "TotalDOM_CO2eha"))

#Appendix S2.Figure S1-S4. All pool for Mature-Low.
## Swap out filters 150/300 or "Low"/"High"
CarbonPools%>%filter(StandAge==300, Rate=="High")%>%
  ggplot(aes(x=Year, y=value, group=CarbonPool))+
  geom_area(aes(fill=CarbonPool), position='stack')+
  facet_grid(Burn~Harvest, labeller=labeller(StandAge=age_labels, Harvest=harvest_labels))+
  scale_fill_manual(values=c("#7D26CD", "#FFFF00","#7CAE00", "#8B3626", "#D2B48C"), 
                    labels=c("Landfills", "Wood Products", "Aboveground Live", "Belowground Live", "DOM and Soils"),
                    name="Carbon Pool")+ # checked colorblind safe
  theme_classic(base_size=12)+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))+
  scale_y_continuous(sec.axis=sec_axis(~.*(12/44), name=(bquote(paste("Mg C ha"^-1)))))+
  ylab(bquote(paste("tonnes CO "[2],"e ha"^-1)))

# Appendix S2. Figure S5. Aboveground live biomass. Note Burn/NoBurn are identical.

FCIDataFullPH%>% filter(Burn=="Burn")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Year, y=ABioMgha, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_bw(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  # ylim(0,230)+
  xlab("Year")+
  ylab(bquote(paste("Aboveground live biomass (Mg C ha"^-1,")")))

# Appendix S2. Figure S6. Total landscape C.

FCIDataFullPH%>% # filter(Burn=="Burn")%>% ##, Rate=="High"|Rate=="NA")%>%
  ggplot(aes(x=Year, y=TotalCMgha, fill=Harvest, colour=Harvest, lty=Burn))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_bw(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  # ylim(0,230)+
  xlab("Year")+
  ylab(bquote(paste("Total landscape carbon (Mg C ha"^-1,")")))

# Appendix S2. Figure S7. NPP
## Omitting year 1 to remove anomaly that's left from correcting NEP/NPP

FCIDataFullPH%>% filter(Burn=="Burn", Time!=1)%>% 
  ggplot(aes(x=Year, y=NPPMgha, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_bw(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  xlab("Year")+
  ylab(bquote(paste("NPP (Mg C ha"^-1,")")))

# Appendix S2. Figure S8. Rh

FCIDataFullPH%>% filter(Burn=="Burn")%>% 
  ggplot(aes(x=Year, y=RhMgha, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  theme_bw(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  xlab("Year")+
  ylab(bquote(paste("Rh (Mg C ha"^-1,")")))

# Appendix S2. Figure S9. NEP
## Omitting Year 1 due to odd artifact from NEP correction

FCIDataFullPH%>% filter(Burn=="Burn", Time!=1)%>% 
  ggplot(aes(x=Year, y=NEPMgha, fill=Harvest, colour=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_line(size=1)+
  geom_hline(yintercept=0,lty=2)+
  theme_bw(base_size=14)+
  scale_colour_viridis(discrete=TRUE, option="H")+
  scale_fill_viridis(discrete=TRUE, option="H")+
  xlab("Year")+
  ylab(bquote(paste("NEP (Mg C ha"^-1,")")))

# Appendix S2. Figure S10A. NSP + Burn

CumFluxFig <- 
  FCIDataFullPH%>%
  filter(Burn=="Burn", Year==2030|Year==2050|Year==2075)%>%
  mutate(Year=factor(Year, levels=c("2030", "2050", "2075")))%>%
  ggplot(aes(x=Year, y=CumNSPCO2ehaNoFossil, fill=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw(base_size=14)+
  geom_hline(yintercept=0, color="black", lty=2)+
  scale_fill_viridis(discrete=TRUE, option="H")+
  ylab(bquote(paste("Cumulative flux (tonnes CO "[2],"e ha"^-1,")")))+
  ylim(-300, 150)
print(CumFluxFig)

# Appendix S2. Figure S10B. NSP + NoBurn

CumFluxFigb <- 
  FCIDataFullPH%>%
  filter(Burn=="NoBurn", Year==2030|Year==2050|Year==2075)%>%
  mutate(Year=factor(Year, levels=c("2030", "2050", "2075")))%>%
  ggplot(aes(x=Year, y=CumNSPCO2ehaNoFossil, fill=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw(base_size=14)+
  geom_hline(yintercept=0, color="black", lty=2)+
  scale_fill_viridis(discrete=TRUE, option="H")+
  ylab(bquote(paste("Cumulative flux (tonnes CO "[2],"e ha"^-1,")")))+
  ylim(-300, 150)
print(CumFluxFigb)


# Appendix S2. Figure S10C. NSP + Substitution + Burn

CumFluxFigSub <- 
  FCIDataFullPH%>%
  filter(Burn=="Burn", Year==2030|Year==2050|Year==2075)%>%
  mutate(Year=factor(Year, levels=c("2030", "2050", "2075")))%>%
  ggplot(aes(x=Year, y=CumNSPCO2ehaNoFossilWithSub1, fill=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw(base_size=14)+
  geom_hline(yintercept=0, color="black", lty=2)+
  scale_fill_viridis(discrete=TRUE, option="H")+
  ylab(bquote(paste("Cumulative flux (tonnes CO "[2],"e ha"^-1,")")))+
  ylim(-300, 150)
print(CumFluxFigSub)

# Appendix S2. Figure S10D. NSP + Substitution + NoBurn

CumFluxFigSub <- 
  FCIDataFullPH%>%
  filter(Burn=="NoBurn", Year==2030|Year==2050|Year==2075)%>%
  mutate(Year=factor(Year, levels=c("2030", "2050", "2075")))%>%
  ggplot(aes(x=Year, y=CumNSPCO2ehaNoFossilWithSub1, fill=Harvest))+
  facet_grid(Rate~StandAge, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw(base_size=14)+
  geom_hline(yintercept=0, color="black", lty=2)+
  scale_fill_viridis(discrete=TRUE, option="H")+
  ylab(bquote(paste("Cumulative flux (tonnes CO "[2],"e ha"^-1,")")))+
  ylim(-300, 150)
print(CumFluxFigSub)

# Appendix S2. Figure S11-S14
## SEE FIGURE 5 to CREATE stacked_data

# PH30
stacked_data%>%filter(Harvest=="PH30", Burn=="Burn")%>%
ggplot()+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  theme_bw(base_size=14)+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(aes(x=Year, y=TotalNSP))+
  geom_line(aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Carbon flux (tonnes CO "[2],"e ha"^-1,"yr"^-1,")")))

# PH60
stacked_data%>%filter(Harvest=="PH60", Burn=="Burn")%>%
  ggplot()+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  theme_bw(base_size=14)+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(aes(x=Year, y=TotalNSP))+
  geom_line(aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Carbon flux (tonnes CO "[2],"e ha"^-1,"yr"^-1,")")))

# PH30Plus
stacked_data%>%filter(Harvest=="PH30Plus", Burn=="Burn")%>%
  ggplot()+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  theme_bw(base_size=14)+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(aes(x=Year, y=TotalNSP))+
  geom_line(aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Carbon flux (tonnes CO "[2],"e ha"^-1,"yr"^-1,")")))

# PH60Plus
stacked_data%>%filter(Harvest=="PH60Plus", Burn=="Burn")%>%
  ggplot()+
  geom_bar(aes(y=CarbonFlux, x = Year, fill=FluxSource), position = "stack", stat = "identity")+
  geom_hline(yintercept=0, col="black")+
  theme_bw(base_size=14)+
  scale_fill_manual(values = c("darkgreen", "orange", "pink", "lightgreen"), 
                    labels =c("Substitution", "Biogenic + Landfill", "Residue Burning", "NEP"),
                    name="Flux Source")+
  geom_line(aes(x=Year, y=TotalNSP))+
  geom_line(aes(x=Year, y=TotalFlux), lty=2)+
  facet_grid(Rate~StandAge, labeller = labeller(StandAge=age_labels, Rate=rate_labels))+
  ylab(bquote(paste("Carbon flux (tonnes CO "[2],"e ha"^-1,"yr"^-1,")")))

