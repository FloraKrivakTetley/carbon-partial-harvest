## This script is a combination of previous versions FluxPrep.R, FluxPrep2.R and FluxPrep3.R.
## This simplification is Step 1 in preparing a final version for publication on GitHub. 
## Here, we double check all calculations made in the FluxPrep scripts, add a couple needed variables, and add annotatation

## For FCI Phase 1, this script is set up to work directly with the output from LoadMCScenarios091323.R
## The saved output file from that script is "AllScenarios091323.csv".
## In the future, variable naming should happen in just one place, but for now, we'll leave it split.

## The bug fix for ForCS NBP calculations has been added here. See "FluxPrepFixForCSBug.R" or "Year_zero_biomass_ForCS.R" for script that pulled
## data from raw files and yielded the file year_zero_means

## NEED TO EDIT PER TIDYVERSE STYLE GUIDE


library(tidyverse)

Drive <- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2"
setwd(paste0(Drive))

## Load in data frame saved from processing in LoadMCScenarios091323.R

ScenarioData <- read.csv("AllScenarios091323.csv")

## Calculate additional landscape scale variables
## CONSIDER ADDING Mg/ha CONVERSIONS FOR EVERY OUTPUT VARIABLE FROM LANDIS

## Fix variable types

ScenarioData <- ScenarioData%>%
  mutate(Burn = replace_na(Burn, "NA"), Rate = replace_na(Rate, "NA"))

##########################################################################################
## INSERTED SECTION: ForCS Bug Fix. Correct NBP values at this stage.
## Flora Krivak-Tetley, Sep 2024
##########################################################################################

## First, use script Year_zero_biomass_ForCS.R to get year 0 values from all runs.

ScenarioDataCorrected <- ScenarioData%>%
  split(.$Scenario)%>%
  imap_dfr(~ add_row(.x, Scenario= .y, Time = 0, .before = 1))

## get Year 0 means
year_zero <- read.csv("year_zero_biomass_ALL.csv")

year_zero_means <- 
  year_zero%>%
  filter(Time==0)%>%
  group_by(Scenario)%>%
  summarise(TotalC=mean(TotalBiomass))%>%
  select(Scenario, TotalC)

## for testing
year_one_means <- 
  year_zero%>%
  filter(Time==1)%>%
  group_by(Scenario)%>%
  summarise(TotalC=mean(TotalBiomass))%>%
  select(Scenario, TotalC)

## add time the year 0 means
year_zero_means$Time<- rep(0)

## Get values from Year 0 means and add to Scenario data, combine 2 columns into one corrected Total C column
ScenarioDataCorrected<- ScenarioDataCorrected%>%
  left_join(year_zero_means, by = c("Scenario", "Time"))%>%
  mutate(NewTotalC=coalesce(TotalC.x, TotalC.y))

## calculate corrected NBP columm
ScenarioDataCorrected<- ScenarioDataCorrected%>%
  group_by(Scenario)%>%
  mutate(NBP_corrected = NewTotalC-lag(NewTotalC))%>%
  ungroup()

## check
First_check <- ScenarioDataCorrected %>%
  filter(Time==0|Time==1)%>%
  select(Scenario, Time, NewTotalC, NBP, NBP_corrected)

## calculate corrected D, NEP, NPP, Net Growth

ScenarioDataCorrected<- ScenarioDataCorrected%>%
  mutate(D = NEP-NBP,
         NEP_corrected = NBP_corrected + D,
         NPP_corrected = NEP_corrected+Rh,
         NetGrowth_corrected = NPP_corrected - Turnover)%>%
  select(-c(X, TotalC.x, TotalC.y))%>%
  filter(Time!=0)

## check w/ quick visualization

ggplot(ScenarioDataCorrected, aes(x=Time, y=NEP_corrected, fill=Scenario))+
  geom_line()+
  geom_line(aes(x=Time, y=NEP), colour="red")+
  facet_grid(Rate~StandAge) #, labeller=labeller(StandAge=age_labels, Rate=rate_labels))+
xlab("Year")+
  ylab("NBP")


## UNITS CONVERSION
## All output from LANDIS-II summary csv is in gC/m^2/yr.
## To convert to tonnes/ha, multiply by 0.01
## To convert C to CO2e, multiply by 3.667

## Add Mg/h columns for fluxes
ScenarioData2 <- ScenarioDataCorrected%>%
  mutate(ToFPSMgha = ToFPS * 0.01,
         ToAirBurn = case_when(Burn=="Burn" ~ NEP_corrected-NBP_corrected-ToFPS, Burn=="NoBurn"|Burn=="NA" ~ 0), ## added condition so sites with no burning will be 0
         ToAirBurnMgha = ToAirBurn * 0.01,
         NPPMgha = NPP_corrected * 0.01,
         NEPMgha = NEP_corrected * 0.01,
         NBPMgha = NBP_corrected * 0.01,
         RhMgha = Rh * 0.01,
         LiveCMgha = LiveC * 0.01,
         Year = 2025 + Time)%>%
  mutate(HarvestIntensity = as.factor(case_when(Harvest=="NoHarvest"~ 0, Harvest=="PH30"|Harvest == "PH30Plus" ~ 30, Harvest == "PH60"|Harvest == "PH60Plus" ~ 60, Harvest == "Clearcut" ~ 100)), 
         PH=case_when(Harvest=="NoHarvest"|Harvest=="Clearcut" ~ "No",Harvest=="CC"~ "No", Harvest=="PH30"~"Yes",
                      Harvest=="PH30Plus"~"Yes", Harvest=="PH60"~"Yes",Harvest=="PH60Plus" ~ "Yes"))



## Add columns for CO2e

ScenarioData2 <- ScenarioData2%>%
  mutate(
    ABio_CO2eha = ABioMgha * (44/12),
    BBio_CO2eha = BBioMgha * (44/12),
    Live_CO2eha = ABio_CO2eha+BBio_CO2eha,
    TotalDOM_CO2eha = TotalDOMMgha * (44/12),
    TotalC_CO2eha = TotalCMgha * (44/12),
    NPP_CO2eha = NPPMgha * (44/12),
    NEP_CO2eha = NEPMgha * (44/12),
    ToFPS_CO2eha = ToFPSMgha * (44/12),
    ToAirBurn_CO2eha = ToAirBurnMgha*(4.437998333), ## See "GHG conversion factors.xlsx"
  )


## Add 3 columns for averages
ScenarioData2 <- ScenarioData2%>%
  group_by(Scenario)%>%
  mutate(AvgFPS = mean(ToFPS),
         AvgFPSMgha = mean(ToFPSMgha),
         AvgTotalCMgha = mean(TotalCMgha))

## Add Percentage harvested (rate) column

## Rate sets. In the future, build this into Load MC Scenarios or use a separate script.

Zero <- c("NoHarvest150", "NoHarvest300")
PointTwo <- c("CC_150LowBurn", "PH30_150LowBurn", "PH60_150LowBurn", "CC_300LowBurn", "PH30_300LowBurn", "PH60_300LowBurn",
              "CC_150LowNoBurn", "PH30_150LowNoBurn", "PH60_150LowNoBurn", "CC_300LowNoBurn", "PH30_300LowNoBurn", "PH60_300LowNoBurn")
OneThird <- c("PH60Plus_150LowBurn", "PH60Plus_300LowBurn", "PH60Plus_150LowNoBurn", "PH60Plus_300LowNoBurn")
PointSix <- c("CC_150AACBurn", "PH30_150AACBurn", "PH60_150AACBurn", "CC_300AACBurn", "PH30_300AACBurn", "PH60_300AACBurn",
              "CC_150AACNoBurn", "PH30_150AACNoBurn", "PH60_150AACNoBurn", "CC_300AACNoBurn", "PH30_300AACNoBurn", "PH60_300AACNoBurn")
TwoThirds <- c("PH30Plus_150LowBurn", "PH30Plus_300LowBurn", "PH30Plus_150LowNoBurn", "PH30Plus_300LowNoBurn")
One <- c("PH60Plus_150AACBurn", "PH60Plus_300AACBurn", "PH60Plus_150AACNoBurn", "PH60Plus_300AACNoBurn")
Two <- c("PH30Plus_150AACBurn", "PH30Plus_300AACBurn", "PH30Plus_150AACNoBurn", "PH30Plus_300AACNoBurn")

ScenarioData2 <- ScenarioData2%>%
  mutate(Percentage = case_when(Scenario %in% Zero ~ 0, Scenario %in% PointTwo ~ 0.2, Scenario %in% OneThird ~ 0.333,
                                Scenario %in% PointSix ~ 0.6, Scenario %in% TwoThirds ~ 0.667,
                                Scenario %in% One ~ 1, Scenario %in% Two ~ 2))

## Add ToFPS Calculations

ScenarioData2 <- ScenarioData2 %>%
  mutate(
    ToFPS_m3ha = ToFPSMgha * 2 / 0.40882,
    ToFPS_m3Total = ToFPS_m3ha * 40000,
    ToFPS_m3harvested = ToFPS_m3Total / (40000*Percentage*0.01)# ToFPS per harvested site - do I need to do a better calculation of this by querying # sites harvested from each run?
  )

## Combine with HWP output. Read in output from "Load HWP 030124.R". 

better_HWP <- read.csv("better_HWP_01March24.csv")

ScenariosFlux <- dplyr::inner_join(ScenarioData2, better_HWP, by = c("Scenario"="Scenario", "Time"="year", "Rate"="Rate",
                                                                     "StandAge"="StandAge", "Burn"="Burn", "Harvest"="Harvest"))

NoHarvestRows <- ScenarioData2 %>% filter(Burn=="NA")
ScenariosFlux <- bind_rows(NoHarvestRows, ScenariosFlux)

## ScenariosFlux <- ScenariosFlux %>% select(-c(X.1, X, X.y, Group.1, Time.y, ToFPS.y)) ## remove extra columns
ScenariosFlux[is.na(ScenariosFlux)]<-0

## Do important flux calculations
# First create all yearly columns for HWP
# tested yrly calculations by using cumsum on them and comparing to totals from HWP model

ScenariosFlux <- ScenariosFlux%>%
  group_by(Scenario)%>%
  mutate(yrly_landfill = c(total_CO2eEmissionsfromLandfillandDumps[1], diff(total_CO2eEmissionsfromLandfillandDumps)),
         yrly_biogenic = c(total_BiogenicManufacturingEmissions[1], diff(total_BiogenicManufacturingEmissions)),
         yrly_fossil = c(total_CradletoGateFossilEmissions_CO2e[1], diff(total_CradletoGateFossilEmissions_CO2e)),
         yrly_HWPflux = yrly_landfill + yrly_biogenic + yrly_fossil, 
         yrly_HWPfluxha = yrly_HWPflux / 40000,
         yrly_HWPfluxNoFossil = yrly_landfill + yrly_biogenic,
         yrly_HWPfluxhaNoFossil = yrly_HWPfluxNoFossil / 40000,
         yrly_HWPflux_sub1 = yrly_landfill + yrly_biogenic + yrly_fossil - sub1, 
         yrly_HWPfluxha_sub1 = yrly_HWPflux_sub1 / 40000,
         yrly_HWPfluxNoFossil_sub1 = yrly_landfill + yrly_biogenic - sub1,
         yrly_HWPfluxhaNoFossil_sub1 = yrly_HWPfluxNoFossil_sub1 / 40000,
         yrly_HWPfluxNoFossil_sub2 = yrly_landfill + yrly_biogenic - sub2,
         yrly_HWPfluxhaNoFossil_sub2 = yrly_HWPfluxNoFossil_sub2 / 40000) ## these exclude fossil fuels burning from calcs

## Next, calculate additional columns for cumulative variables

ScenariosFlux <- ScenariosFlux%>%
  mutate(NSPCO2ehaNoFossil = NEP_CO2eha - ToAirBurn_CO2eha - yrly_HWPfluxhaNoFossil,
         NSPCO2ehaWithFossil = NEP_CO2eha - ToAirBurn_CO2eha - yrly_HWPfluxha,
         NSPCO2ehaNoFossilWithSub1 = NEP_CO2eha - ToAirBurn_CO2eha - yrly_HWPfluxhaNoFossil_sub1,
         NSPCO2ehaNoFossilWithSub2 = NEP_CO2eha - ToAirBurn_CO2eha - yrly_HWPfluxhaNoFossil_sub2)%>%
  group_by(Scenario)%>%
  mutate(CumNSPCO2ehaNoFossil = cumsum(NSPCO2ehaNoFossil),
         CumNSPCO2ehaWithFossil = cumsum(NSPCO2ehaWithFossil),
         CumNSPCO2ehaNoFossilWithSub1 = cumsum(NSPCO2ehaNoFossilWithSub1),
         CumNSPCO2ehaNoFossilWithSub2 = cumsum(NSPCO2ehaNoFossilWithSub2))

## Add column for NSP (CO2e/ha) difference from baseline and cumulative difference from baseline
## baseline is no harvest for the correct age (150 or 300)

baselineNSP150<- ScenariosFlux%>%filter(Scenario=="NoHarvest150")%>%pull(NSPCO2ehaNoFossil)
baselineNSP300<- ScenariosFlux%>%filter(Scenario=="NoHarvest300")%>%pull(NSPCO2ehaNoFossil)

ScenariosFlux2_150<- ScenariosFlux%>%filter(StandAge==150)%>% mutate(Baseline=rep(baselineNSP150))
ScenariosFlux2_300<- ScenariosFlux%>%filter(StandAge==300)%>% mutate(Baseline=rep(baselineNSP300))

ScenariosFlux<- rbind(ScenariosFlux2_150, ScenariosFlux2_300)
ScenariosFlux<- ScenariosFlux%>%
  mutate(NSPCO2ehaDiff=NSPCO2ehaNoFossil-Baseline)

ScenariosFlux<-ScenariosFlux%>%
  group_by(Scenario)%>%
  mutate(CumNSPCO2ehaDiff=cumsum(NSPCO2ehaDiff))

## Calculate additional storage variables

ScenariosFlux <- ScenariosFlux%>%
  mutate(ProductsCO2eha = total_CO2eStorageinProducts / 40000,
         LandfillCO2eha = total_CO2eStorageInLandfillsandDumps / 40000,
         TotalCO2eStorageha = ProductsCO2eha+LandfillCO2eha, 
         TotalSystemCo2eha = TotalC_CO2eha+TotalCO2eStorageha,
         TotalSystemMgha = TotalSystemCo2eha/(44/12))

## Adjust variables for plotting  
ScenariosFlux$Harvest <- factor(ScenariosFlux$Harvest, levels=c("NoHarvest", "PH30", "PH30Plus", "PH60", "PH60Plus", "Clearcut"))


## Create replicates of the NoHarvest scenario data that can be retained for group analyses- e.g., copies for 
## low-high harvest rates and copies for Burn/NoBurn. 

SF_NoHarvest150 <- ScenariosFlux %>% filter(Scenario=="NoHarvest150")
SF_NoHarvest300 <- ScenariosFlux %>% filter(Scenario=="NoHarvest300")
Other_Scenarios <- ScenariosFlux %>% filter(Scenario!= "NoHarvest150"& Scenario!="NoHarvest300")

## There is surely a more efficient way to do this but here goes

NH150HighBurn <- SF_NoHarvest150%>%mutate(Burn="Burn", Rate="High")
NH150LowBurn <- SF_NoHarvest150%>%mutate(Burn="Burn", Rate="Low")
NH150HighNoBurn <- SF_NoHarvest150%>%mutate(Burn="NoBurn", Rate="High")
NH150LowNoBurn <- SF_NoHarvest150%>%mutate(Burn="NoBurn", Rate="Low")

NH300HighBurn <-SF_NoHarvest300%>%mutate(Burn = "Burn", Rate = "High")
NH300LowBurn <- SF_NoHarvest300%>%mutate(Burn = "Burn", Rate = "Low")
NH300HighNoBurn <- SF_NoHarvest300%>%mutate(Burn = "NoBurn", Rate = "High")
NH300LowNoBurn <- SF_NoHarvest300%>%mutate(Burn = "NoBurn", Rate = "Low")

All_NH <- bind_rows(NH150HighBurn,NH150LowBurn, NH150HighNoBurn, NH150LowNoBurn, NH300HighBurn, NH300LowBurn,
                    NH300HighNoBurn, NH300LowNoBurn)

## Combine with other data

ScenariosFluxAllNH <- bind_rows(All_NH, Other_Scenarios)  

FCIDataFullPH<- ScenariosFluxAllNH

write.csv(ScenariosFluxAllNH, file="ScenarioOutputCorrected09Oct24.csv")

## 18 April 2024. Executed from scratch and everything worked. May be a couple mods to variables that need to be added.
## Suggest making another script that converts to long format data for pool analysis
## 26 April 2024. Added ToFPS volume calculations.