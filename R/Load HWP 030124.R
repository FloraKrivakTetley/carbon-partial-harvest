library(dplyr)

Drive<- "D:/FCI Project UBC/SCENARIOS Phase 1"
setwd(paste0(Drive))

## For all scenarios as of Sep 2023
folders<- c("CC_300AACBurn", "CC_300AACNoBurn","CC_300LowBurn", "CC_300LowNoBurn",
            "PH30_300AACBurn", "PH30_300AACNoBurn","PH30_300LowBurn", "PH30_300LowNoBurn",
            "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn","PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn", "PH60_300AACBurn", "PH60_300AACNoBurn","PH60_300LowBurn", "PH60_300LowNoBurn",
            "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn","PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn", 
            "CC_150AACBurn", "CC_150AACNoBurn","CC_150LowBurn", "CC_150LowNoBurn",
            "PH30_150AACBurn", "PH30_150AACNoBurn","PH30_150LowBurn", "PH30_150LowNoBurn",
            "PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn","PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn",
            "PH60_150AACBurn", "PH60_150AACNoBurn","PH60_150LowBurn", "PH60_150LowNoBurn",
            "PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn","PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn")


combined_HWP<- data.frame()

for (folder in folders){
  file_path<- file.path(folder, "Mean_HWP.csv")
  df<- read.csv(file_path)
  df$Scenario<- folder
  combined_HWP<- bind_rows(combined_HWP, df)
}

print(combined_HWP)

## Assign Sets
## Harvest Sets
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
OldGrowth<- c("CC_300AACBurn", "CC_300AACNoBurn","CC_300LowBurn", "CC_300LowNoBurn",
              "PH30_300AACBurn", "PH30_300AACNoBurn", "PH30_300LowBurn", "PH30_300LowNoBurn",
              "PH30Plus_300AACBurn", "PH30Plus_300AACNoBurn","PH30Plus_300LowBurn", "PH30Plus_300LowNoBurn",
              "PH60_300AACBurn", "PH60_300AACNoBurn","PH60_300LowBurn", "PH60_300LowNoBurn",
              "PH60Plus_300AACBurn", "PH60Plus_300AACNoBurn","PH60Plus_300LowBurn", "PH60Plus_300LowNoBurn")
Mature150<- c("CC_150AACBurn", "CC_150AACNoBurn","CC_150LowBurn", "CC_150LowNoBurn",
              "PH30_150AACBurn", "PH30_150AACNoBurn", "PH30_150LowBurn", "PH30_150LowNoBurn",
              "PH30Plus_150AACBurn", "PH30Plus_150AACNoBurn","PH30Plus_150LowBurn", "PH30Plus_150LowNoBurn",
              "PH60_150AACBurn", "PH60_150AACNoBurn","PH60_150LowBurn", "PH60_150LowNoBurn",
              "PH60Plus_150AACBurn", "PH60Plus_150AACNoBurn","PH60Plus_150LowBurn", "PH60Plus_150LowNoBurn")

## Burn Sets
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


## consider leaving 0s in for some plots
## combined_HWP<-combined_HWP%>%
##  filter(year!=0)

better_HWP<- combined_HWP%>%
  mutate(Burn=case_when(Scenario %in% Burn ~ "Burn", Scenario %in% NoBurn ~ "NoBurn"),
         StandAge=case_when(Scenario %in% OldGrowth ~ 300, Scenario %in% Mature150 ~ 150),
         Harvest=case_when(Scenario %in% CC ~ "Clearcut", 
                           Scenario %in% PH30 ~ "PH30", Scenario %in% PH30Plus ~ "PH30Plus",
                           Scenario %in% PH60 ~ "PH60", Scenario %in% PH60Plus ~ "PH60Plus"),
         Rate=case_when(Scenario%in% High ~ "High", Scenario %in% Low ~ "Low"))

better_HWP$Harvest<- as.factor(better_HWP$Harvest)
better_HWP$Scenario<- as.factor(better_HWP$Scenario)

write.csv(better_HWP, file="better_HWP_01March24.csv")

Drive<- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2"
setwd(paste0(Drive))


library(ggplot2)

better_HWP%>%
  filter(Burn=="Burn", StandAge=="150", Rate=="Low")%>%
ggplot(aes(x=year, y=total_CO2eStorageinProducts/10000, fill=Harvest))+
  geom_area()+
  facet_grid(Harvest~.)+
  theme_grey(base_size = 14)+
  ylim(0,100)+
  xlab("Simulation Year")+
  ylab("Total Carbon Storage in Products (10,000 tonnes C)")

better_HWP%>%
  filter(Burn=="Burn", StandAge=="150", Rate=="Low")%>%
ggplot(aes(x=year, y=total_CO2eStorageInLandfillsandDumps/10000, fill=Harvest))+
  geom_area()+
  facet_grid(Scenario~.)+
  theme_grey(base_size = 14)+
  ## scale_fill_manual(values=c("#04DF91"))+
  ylim(0,160)+
  xlab("Simulation Year")+
  ylab("Total Carbon Storage in Landfills and Dumps (10,000 tonnes C)")

ggplot(data=combined_HWP,aes(x=year, y=Total/10000, lty=Scenario))+
  geom_line(size=1)+
  theme_grey(base_size = 14)+
  xlab("Simulation Year")+
  ylab("Total Carbon to FPS (10,000 tonnes C)")
       