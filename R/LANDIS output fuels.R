## 1) Load required packages

library(readtext)
library(tidyverse)
library(ggplot2)

## 2) Define file paths (external drive where all runs are stored)

## Drive<- "D:/FCI Project UBC/SCENARIOS Phase 1/"
## setwd(paste0(Drive))

## test

Drive<- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2/"
setwd(paste0(Drive))

## 3) Define list of scenarios= folder names

scenarios <- c("NoHarvest150", "NoHarvest300", "CC_300AACBurn", "CC_300AACNoBurn","CC_300LowBurn", "CC_300LowNoBurn",
              "PH30_300AACBurn", "PH30_300AACNoBurn","PH30_300LowBurn", "PH30_300LowNoBurn",
              "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn","PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn", 
              "PH60_300AACBurn", "PH60_300AACNoBurn","PH60_300LowBurn", "PH60_300LowNoBurn",
              "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn","PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn", 
              "CC_150AACBurn", "CC_150AACNoBurn","CC_150LowBurn", "CC_150LowNoBurn",
              "PH30_150AACBurn", "PH30_150AACNoBurn","PH30_150LowBurn", "PH30_150LowNoBurn",
              "PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn","PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn",
              "PH60_150AACBurn", "PH60_150AACNoBurn","PH60_150LowBurn", "PH60_150LowNoBurn",
              "PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn","PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn")

##test 
scenarios <- c('PH60Plus_150LowBurn', 'PH60Plus_300AACBurn')

## 4) Loop through folders to collect needed columns from output files. This code outputs one file to each scenario folder.
## These files contain rows for each time period for each Run.


for (scenario in scenarios){
## Create empty initial output files
PoolOutput <- matrix(NA,nrow=0,ncol=15) ## check # of columns

ThisDrive <- (paste0(Drive, scenario))

for (i in 1:20){
  
  setwd(paste0(ThisDrive,'/Run',i))  
  Summary <- read.csv("log_Pools.csv", sep=",")
  Summary_species <- Summary %>% 
    select(-c(species)) %>%
    group_by(Time, row, column, ecoregion) %>% 
    summarise_each(funs(sum))
  # Summary_species<-aggregate(Summary, by=list(Time=Summary$Time, row=Summary$row, column=Summary$column), FUN=sum, na.rm=TRUE)
  Summary_means_by_year <- aggregate(Summary_species, by=list(Summary_species$Time), FUN=mean, na.rm=TRUE)
  Summary_means_by_year$Time <- NULL
  Summary_means_by_year$ecoregion <- NULL
  names(Summary_means_by_year)[1] <- "Time"
  write.csv(Summary_means_by_year, file = "Summary_means_by_year.csv")
  Run_sum <- cbind(Summary_means_by_year, i)
  PoolOutput <- rbind(PoolOutput,Run_sum)
}

setwd(ThisDrive)
write.csv(PoolOutput, file = "PoolOutputScenarioSummary.csv")  

}

## 5) Combine these output files, add Scenario names in a column

setwd(paste0(Drive))

combined_pools <- data.frame()

for (scenario in scenarios){
  file_path <- file.path(scenario, "PoolOutputScenarioSummary.csv")
  df <- read.csv(file_path)
  df$Scenario <- scenario
  combined_pools <- bind_rows(combined_pools, df)
}

write.csv(combined_pools, file = "CombinedPools.csv")

## 6) Find means and sd across 20 Runs for each Scenario (could be done above, but saving intermediate step in case we need it)

PoolSummary <-
combined_pools %>% 
  select(-c(X.1, row, column, X)) %>%
  group_by(Scenario, Time) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean=mean, SD=sd),
    .names = "{col}_{fn}"
  ))

PoolSummary <-
  PoolSummary %>%
  select(-c(i_Mean, i_SD))

write.csv(PoolSummary, file="PoolSummaryAllScenarios.csv")

## 7) Convert units, calculate pool combinations, combine with primary data file FCIDataFullPH
## Include quick validation step to check whether the totals match up with a relevant value from log_Summary (e.g., TotalDOM)

PoolSummary <- read.csv("PoolSummaryAllScenarios.csv")

MorePools <- PoolSummary %>%
mutate(SnagsMgha = 0.01*(Sng_Stem_Mean + Sng_Oth_Mean), 
       CWDMgha = 0.01*(MED_Mean),
       FSWDMgha = 0.01*(Fast_A_Mean), 
       LitterMgha = 0.01*(VF_A_Mean),
       PotentialFuelsMgha = SnagsMgha + CWDMgha + FSWDMgha + LitterMgha,
       TotalSoilsMgha = 0.01*(VF_B_Mean + Fast_B_Mean + Slow_B_Mean + Slow_A_Mean),
       TotalDOMFromPoolsMgha = 0.01*(VF_A_Mean + VF_B_Mean + Fast_A_Mean + Fast_B_Mean + MED_Mean + 
                                       Slow_A_Mean + Slow_B_Mean + Sng_Stem_Mean + Sng_Oth_Mean),
       SnagsCO2eha = 3.667*SnagsMgha,
       CWDCO2eha = 3.667*CWDMgha,
       FSWDCO2eha = 3.667*FSWDMgha,
       LitterCO2eha = 3.667*LitterMgha,
       PotentialFuelsCO2eha = 3.667*PotentialFuelsMgha,
       TotalSoilsCO2eha = 3.667*TotalSoilsMgha,
       TotalDOMFromPoolsCO2eha = 3.667*TotalDOMFromPoolsMgha)

## Plot for visualization and validation

MorePools %>%
  filter(Time %in% c(0,10,20,30,40,50)) %>%
  ggplot(aes(x=Time, y=TotalDOMFromPoolsMgha))+
  geom_point()

## Save Year 0 values

YearZero <- MorePools %>%
  filter(Time==0)
write.csv(YearZero, "YearZeroDOMpools06Mar24.csv")

## VALIDATION: Yes, TotalDOMFromPoolsMgha equals TotalDOM/0.01 in FCIDataFullPH for Year 1. Checked for subset of Scenarios.

## 8) Combine with main data set.

FCIDataFullPH <- read.csv("ScenariosWithHWPandAllNH06Mar24.csv")
JoinTest <- full_join(FCIDataFullPH, MorePools, by=c("Scenario", "Time"))

## Summarize- get Year 0, Year 50 for each scenario.

FuelsTable <- JoinTest %>%
  filter(Time %in% c(0,50)) %>%
  select("Scenario", "Year", "Time", "StandAge", "Burn", "Rate", "Harvest", "HarvestIntensity", "PH",
         "SnagsMgha","CWDMgha", "FSWDMgha", "LitterMgha", "PotentialFuelsMgha")
write.csv(FuelsTable, file="FuelsTable06Mar.csv")  
## this could be improved in R. Consider subtracting start values from yr 50 values (so we'd get change in fuels)


## To Plot Carbon Pools. Note probably need to do a couple more calculations

library(ggbreak)

CarbonPools <-
  JoinTest %>%
  filter(Time %in% c(1,10,20,30,40,50)) %>%
  select("Scenario", "Year", "Time", "StandAge", "Burn", "Rate", "Harvest", "HarvestIntensity", "PH", "NEP", 
         "Live_CO2eha","TotalSoilsCO2eha", "PotentialFuelsCO2eha", "TotalDOMFromPoolsCO2eha", "ProductsCO2eha", "LandfillCO2eha" ) %>%
  pivot_longer(cols=c(Live_CO2eha, PotentialFuelsCO2eha, TotalSoilsCO2eha, ProductsCO2eha, LandfillCO2eha), names_to="CarbonPool") %>%
  mutate(Year=as.numeric(Year))
# fct_relevel not working (maybe bc it's a )
CarbonPools$CarbonPool <- factor(CarbonPools$CarbonPool, levels=c("LandfillCO2eha", "ProductsCO2eha",
                                                                 "Live_CO2eha", "PotentialFuelsCO2eha", "TotalSoilsCO2eha"))
CarbonPools$Harvest <- factor(CarbonPools$Harvest, levels = c("NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus", "Clearcut"))


write.csv(CarbonPools, file="CarbonPools06Mar24withZero.csv")

CarbonPools %>%
  filter(StandAge==300, Rate=="High") %>%
  ggplot(aes(x=Year, y=value, group=CarbonPool))+
  geom_area(aes(fill=CarbonPool), position='stack')+
  facet_grid(Burn~Harvest)+
  scale_fill_manual(values=c("orange","yellow", "#7CAE00", "brown", "tan"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))+
  ylab(bquote(paste("Carbon pools (tonnes CO "[2],"e ha"^-1,")")))




## Graph subset
CarbonPools %>%
  filter(StandAge==300, Rate=="High", Burn=="Burn", Harvest %in% c("NoHarvest","Clearcut")) %>%
  ggplot(aes(x=Year, y=value, group=CarbonPool))+
  geom_area(aes(fill=CarbonPool), position='stack')+
  facet_grid(.~Harvest)+
  scale_fill_manual(values=c("#7D26CD", "#FFFF00","#7CAE00", "#8B3626", "#D2B48C"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))+
  # scale_y_break(c(500,1000))+
  ylab(bquote(paste("Carbon pools (tonnes CO "[2],"e ha"^-1,")")))

## Output for Tables

## Summarize Pool Data.
CarbonSummaryYr50 <-
  CarbonPools %>%
  filter(Time==50) %>%
  group_by(Scenario) %>%
  summarize(TotalCarbon=sum(value))

CarbonLandscapeYr50 <- 
  CarbonPools %>%
  filter(Time==50, CarbonPool %in% c("Live_CO2eha", "PotentialFuelsCO2eha", "TotalSoilsCO2eha")) %>%
  group_by(Scenario) %>%
  summarize(TotalCarbon=sum(value))

CarbonSummaryProducts<- full_join(CarbonSummaryYr50, CarbonLandscapeYr50, by="Scenario")
CarbonSummaryProducts$Diff <- CarbonSummaryProducts$TotalCarbon.x-CarbonSummaryProducts$TotalCarbon.y

CC150AACBurn <- CarbonPools %>%
  filter(Scenario=="CC_150AACBurn", Time==50)

  
  
