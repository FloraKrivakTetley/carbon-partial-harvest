## Build script to read in summarized output from MC runs and visualize
## This script will read in MC output files and combine them so they are ready for analysis

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


## Read in all means

library(dplyr)

Drive<- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2"
setwd(paste0(Drive))

## For all scenarios with NoHarvest
folders<- c("NoHarvest150", "NoHarvest300", "CC_300AACBurn", "CC_300AACNoBurn","CC_300LowBurn", "CC_300LowNoBurn",
             "PH30_300AACBurn", "PH30_300AACNoBurn","PH30_300LowBurn", "PH30_300LowNoBurn",
            "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn","PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn", "PH60_300AACBurn", "PH60_300AACNoBurn","PH60_300LowBurn", "PH60_300LowNoBurn",
            "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn","PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn", 
            "CC_150AACBurn", "CC_150AACNoBurn","CC_150LowBurn", "CC_150LowNoBurn",
            "PH30_150AACBurn", "PH30_150AACNoBurn","PH30_150LowBurn", "PH30_150LowNoBurn",
            "PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn","PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn",
            "PH60_150AACBurn", "PH60_150AACNoBurn","PH60_150LowBurn", "PH60_150LowNoBurn",
            "PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn","PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn")

combined_means<- data.frame()

for (folder in folders){
  file_path<- file.path(folder, "Mean_by_year.csv")
  df<- read.csv(file_path)
  df$Scenario<- folder
  combined_means<- bind_rows(combined_means, df)
}

print(combined_means)
remove1<- c("Group.1", "X", "row", "column", "i")
clean_means<- combined_means[, !colnames(combined_means) %in% remove1]
write.csv(clean_means, "clean_means091323.csv")


## Read in all SD csvs (need to assign folders above)

combined_sd<- data.frame()

for (folder in folders){
  file_path<- file.path(folder, "SD_by_year.csv")
  df<- read.csv(file_path)
  df$Scenario<- folder
  combined_sd<- bind_rows(combined_sd, df)
}

print(combined_sd)

write.csv(combined_sd, "combined_sd091323.csv")

## remove weird columns
remove2<- c("X.1", "X", "row", "column", "i", "Time")
clean_sd<- combined_sd[, !colnames(combined_sd) %in% remove2]
## change name of weird X column to match time column and add _sd to each of the column names
clean_sd<- rename(clean_sd, Time=Group.1, ABio_sd=ABio, BBio_sd=BBio, TotalDOM_sd=TotalDOM, DelBio_sd=DelBio,
                  Turnover_sd=Turnover, NetGrowth_sd=NetGrowth, NPP_sd=NPP, Rh_sd=Rh, NEP_sd=NEP, NBP_sd=NBP,
                  ToFPS_sd=ToFPS, Scenario=Scenario)

write.csv(clean_sd, "clean_sd091323.csv")

## Combine data frames

## Scenarios_150AACBurnALL<- merge(x=Scenarios_150AACBurn, y=clean_sd, by=c("Time"="Time", "Scenario"="Scenario"), all=TRUE) ## this is the base R way
merged_test<-full_join(x=clean_means, y=clean_sd, by=c("Time"="Time", "Scenario"="Scenario")) ## this is the dplyr way

## Add lists of scenarios to ID column values we need to add

## Harvest Sets
NoHarvest<- c("NoHarvest150", "NoHarvest300")
CC<- c("CC_150AACBurn", "CC_150AACNoBurn", "CC_300AACBurn", "CC_300AACNoBurn", 
       "CC_150LowBurn", "CC_150LowNoBurn", "CC_300LowBurn", "CC_300LowNoBurn")

PH30<- c("PH30_150AACBurn", "PH30_150AACNoBurn", "PH30_300AACBurn", "PH30_300AACNoBurn",
         "PH30_150LowBurn", "PH30_150LowNoBurn", "PH30_300LowBurn", "PH30_300LowNoBurn")

PH30Plus<- c("PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn", "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn",
             "PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn", "PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn")

PH60<- c("PH60_150AACBurn", "PH60_150AACNoBurn", "PH60_300AACBurn", "PH60_300AACNoBurn",
         "PH60_150LowBurn", "PH60_150LowNoBurn", "PH60_300LowBurn", "PH60_300LowNoBurn")

PH60Plus<- c("PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn", "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn",
             "PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn", "PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn")

## Age Sets
OldGrowth<- c("NoHarvest300", "CC_300AACBurn", "CC_300AACNoBurn","CC_300LowBurn", "CC_300LowNoBurn",
              "PH30_300AACBurn", "PH30_300AACNoBurn", "PH30_300LowBurn", "PH30_300LowNoBurn",
              "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn","PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn",
           "PH60_300AACBurn", "PH60_300AACNoBurn","PH60_300LowBurn", "PH60_300LowNoBurn",
           "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn","PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn")
Mature150<- c("NoHarvest150","CC_150AACBurn", "CC_150AACNoBurn","CC_150LowBurn", "CC_150LowNoBurn",
              "PH30_150AACBurn", "PH30_150AACNoBurn", "PH30_150LowBurn", "PH30_150LowNoBurn",
              "PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn","PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn",
              "PH60_150AACBurn", "PH60_150AACNoBurn","PH60_150LowBurn", "PH60_150LowNoBurn",
              "PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn","PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn")

## Burn Sets
## Use NoHarvest to assign Burn/NoBurn to NoHarvest scenarios
Burn<-c("CC_300AACBurn", "CC_300LowBurn","CC_150AACBurn","CC_150LowBurn",
        "PH30_300AACBurn", "PH30_300LowBurn", "PH30_150AACBurn","PH30_150LowBurn",
        "PH30Plus_300AACBurn", "PH30Plus_300LowBurn", "PH30Plus_150AACBurn", "PH30Plus_150LowBurn",
        "PH60_300AACBurn", "PH60_300LowBurn", "PH60_150AACBurn","PH60_150LowBurn",
        "PH60Plus_300AACBurn", "PH60Plus_300LowBurn", "PH60Plus_150AACBurn", "PH60Plus_150LowBurn")
NoBurn<- c("CC_300AACNoBurn", "CC_300LowNoBurn","CC_150AACNoBurn","CC_150LowNoBurn",
           "PH30_300AACNoBurn", "PH30_300LowNoBurn", "PH30_150AACNoBurn","PH30_150LowNoBurn",
           "PH30Plus_300AACNoBurn", "PH30Plus_300LowNoBurn", "PH30Plus_150AACNoBurn", "PH30Plus_150LowNoBurn",
           "PH60_300AACNoBurn", "PH60_300LowNoBurn", "PH60_150AACNoBurn","PH60_150LowNoBurn",
           "PH60Plus_300AACNoBurn", "PH60Plus_300LowNoBurn", "PH60Plus_150AACNoBurn", "PH60Plus_150LowNoBurn")

## Harvest Rate
High<- c("CC_300AACBurn", "CC_150AACBurn", "PH60_300AACBurn",  "PH60_150AACBurn",
        "PH30Plus_300AACBurn", "PH30Plus_150AACBurn", "PH30_300AACBurn",  "PH30_150AACBurn",
        "PH30_300AACNoBurn", "PH30_150AACNoBurn", "PH30Plus_300AACNoBurn", "PH30Plus_150AACNoBurn",
        "PH60Plus_300AACBurn", "PH60Plus_150AACBurn", "CC_300AACNoBurn", "CC_150AACNoBurn",
        "PH60_300AACNoBurn", "PH60_150AACNoBurn", "PH60Plus_300AACNoBurn", "PH60Plus_150AACNoBurn")
Low<- c("CC_300LowBurn","CC_150LowBurn", "PH30Plus_300LowBurn", "PH30Plus_150LowBurn",
        "PH30_300LowBurn", "PH30_150LowBurn",
        "PH60_300LowBurn", "PH60_150LowBurn","PH60Plus_300LowBurn", "PH60Plus_150LowBurn",
        "CC_300LowNoBurn","CC_150LowNoBurn", 
        "PH30_300LowNoBurn", "PH30_150LowNoBurn", "PH30Plus_300LowNoBurn", "PH30Plus_150LowNoBurn",
        "PH60_300LowNoBurn", "PH60_150LowNoBurn", "PH60Plus_300LowNoBurn", "PH60Plus_150LowNoBurn")

## Add additional columns we need for analysis

better_merge<- merged_test%>%
  mutate(Burn=case_when(Scenario %in% Burn ~ "Burn", Scenario %in% NoBurn ~ "NoBurn", Scenario %in% NoHarvest ~"NA"),
         StandAge=case_when(Scenario %in% OldGrowth ~ 300, Scenario %in% Mature150 ~ 150),
         Harvest=case_when(Scenario %in% NoHarvest ~ "NoHarvest", Scenario %in% CC ~ "Clearcut", 
                           Scenario %in% PH30 ~ "PH30", Scenario %in% PH30Plus ~ "PH30Plus",
                           Scenario %in% PH60 ~ "PH60", Scenario %in% PH60Plus ~ "PH60Plus"),
         Rate=case_when(Scenario%in% High ~ "High", Scenario %in% Low ~ "Low", Scenario %in% NoHarvest ~ "NA"),
         HarvestIntensity=case_when(Scenario %in% NoHarvest ~ 0, Scenario %in% CC ~ 100, Scenario %in% PH30 ~ 30,
                                    Scenario %in% PH30Plus ~ 30, Scenario %in% PH60 ~ 60, Scenario %in% PH60Plus ~60),
         TotalC=ABio+BBio+TotalDOM, LiveC=ABio+BBio, ABioMgha=ABio/100, BBioMgha=BBio/100, TotalCMgha=TotalC/100,
         TotalDOMMgha=TotalDOM/100)

## consider taking the Mg/ha conversions shown above out of this script and adding to FluxPrepFINAL.R
  
better_merge$Time<- as.numeric(better_merge$Time)
better_merge$Harvest<- as.factor(better_merge$Harvest)

write.csv(better_merge, "AllScenarios091323.csv")
