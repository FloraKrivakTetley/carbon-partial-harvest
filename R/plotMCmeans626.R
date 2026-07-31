## Load environment using code in "Load MC scenarios 0629.R" or load in compiled file
## Once those are complete, create a master file that has all scenarios and then add code here to load that file
## in before plotting
library(ggplot2)
library(tidyverse)

## Filtering Years needed

better_merge$Harvest<- factor(better_merge$Harvest, levels=c("NoHarvest", "PH60", "PH60Plus", "Clearcut"))
better_merge$Burn<-factor(better_merge$Burn, levels=c("NA", "Burn", "NoBurn"))
better_merge$Rate<- factor(better_merge$Rate, levels=c("NA", "Low", "AAC"))
better_merge$StandAge<-factor(better_merge$StandAge, levels=c("150", "300"))

Year5<- better_merge%>%
  filter(Time==5)

Year50<- better_merge%>%
  filter(Time==50)

## set up facet labels
  
ratelabels<- list("NoHarvest", "Current", "AAC Estimate")
rate_labeler<- function(variable, value){
  return(ratelabels[value])
}

#### Plot Aboveground biomass across 4 main scenarios: 150LowBurn
Year50%>%
  filter(Burn=="Burn"|Burn=="NA", StandAge=="150", Rate=="Low"|Rate=="NA")%>%
  ggplot(aes(fill=Harvest, y=ABioMgha, x=Harvest))+
  geom_col(position="dodge")+
  facet_grid(.~Rate, scales="free", space="free", labeller=rate_labeler)+
  theme_classic(base_size = 14)+
  scale_fill_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  # scale_y_continuous(limits=c(0,30), expand = c(0,0))+
  xlab("")+
  ylab("Aboveground Live Biomass (Mg C/ha)")+
  geom_errorbar(aes(ymin=ABioMgha-ABio_sd/100, ymax=ABioMgha+ABio_sd/100, width=.2))+
  ggtitle("Mature (150 years) Landscape: Current Harvest Rate")



Year50%>%
  filter(Burn=="Burn"|Burn=="NA", StandAge=="150")%>%
ggplot(aes(fill=Harvest, y=ABioMgha, x=Harvest))+
  geom_col(position="dodge")+
  facet_grid(.~Rate, scales="free", space="free", labeller=rate_labeler)+
  theme_classic(base_size = 14)+
  scale_fill_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  # scale_y_continuous(limits=c(0,30), expand = c(0,0))+
  xlab("")+
  ylab("Aboveground Biomass (Mg C/ha)")+
  geom_errorbar(aes(ymin=ABioMgha-ABio_sd/100, ymax=ABioMgha+ABio_sd/100, width=.2))+
  ggtitle("Mature (150 years) Landscape: Compare Harvest Rates")

## Let's look at NPP

ggplot(Year50, aes(fill=Harvest, y=NPP, x=Harvest))+
  geom_col(position="dodge")+
  facet_grid(.~Rate, scales="free", space="free", labeller=rate_labeler)+
  theme_classic(base_size = 14)+
  scale_fill_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  # scale_y_continuous(limits=c(0,30), expand = c(0,0))+
  # xlab("Harvest Intensity")+
  ylab("NPP (g C/ m^2)")+
  geom_errorbar(aes(ymin=NPP-NPP_sd, ymax=NPP+NPP_sd, width=.2))+
  ggtitle("150Burn: Year 50 Means from 20 runs")

## Try showing ABio patterns in 150 vs 300 year old stands: still all Burn Scenarios

Year50%>%
  filter(Burn=="Burn"|Burn=="NA")%>%
  ggplot(aes(fill=Harvest, y=ABioMgha, x=StandAge))+
  # geom_col(position="dodge")+
  geom_col(position=position_dodge2(preserve="single"))+
  facet_grid(.~Rate, scales="free", space="free", labeller=rate_labeler)+
  theme_classic(base_size = 14)+
  scale_fill_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  xlab("")+
  ylab("Aboveground Biomass (Mg C/ha)")+
  geom_errorbar(aes(ymin=ABioMgha-ABio_sd/100, ymax=ABioMgha+ABio_sd/100, width=.2), position=position_dodge(0.9))+
  ggtitle("Compare Harvest Rates & Mature vs. Old Growth")

## Compare Total C at the end of 50 Years across Stand age and Harvest intensity
Year50%>%
  filter(Burn=="Burn"|Burn=="NA")%>%
  ggplot(aes(fill=Harvest, y=TotalCMgha, x=StandAge))+
  geom_col(position=position_dodge2(preserve="single"))+
  facet_grid(~Rate, scales="free", space="free", labeller=rate_labeler)+
  theme_classic(base_size = 14)+
  scale_fill_manual(values=c("#7CAE00","#00BFC4","#C77CFF","#F8766D" ))+
  xlab("")+
  ylab("Total Carbon (Mg/ha)")+
  geom_errorbar(aes(ymin=TotalCMgha-ABio_sd/100, ymax=TotalCMgha+ABio_sd/100, width=.2, group=Harvest), position=position_dodge(0.9))+
  ggtitle("Compare Harvest Rates & Mature vs. Old Growth")
