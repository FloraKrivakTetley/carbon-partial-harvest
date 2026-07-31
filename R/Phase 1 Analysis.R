library(tidyverse)
library(ggplot2)
library(viridis)
library(effsize)

## Use linear models to get effect sizes for variables of interest

FCIDataFullPH$Harvest<- factor(FCIDataFullPH$Harvest, levels=c("Clearcut", "NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus"))

CumFluxValuesAll <- FCIDataFullPH%>%
  select(Time, Year, Scenario, Harvest, Percentage, HarvestIntensity, StandAge, Rate, Burn, CumNSPCO2ehaNoFossil, NSPCO2ehaNoFossil, ToFPS_CO2eha)%>%
  mutate(Stand=ifelse(StandAge == 150, "Mature", "Old"))

CumFluxValuesAll<- CumFluxValuesAll%>%
  mutate(Harvest=fct_relevel(Harvest, c("Clearcut", "NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus")),
         Rate=fct_relevel(Rate, c("High", "Low")),
         Stand = fct_relevel(Stand, c("Old", "Mature")))

CumFluxValues2030 <- CumFluxValuesAll %>%
  filter(Year==2030)

CumFluxValues2050 <- CumFluxValuesAll %>%
  filter(Year==2050)

CumFluxValues2075 <- CumFluxValuesAll %>%
  filter(Year==2075)

CumFluxModel <- lm(CumNSPCO2ehaNoFossil ~ Rate + Stand + Harvest + Burn, data= CumFluxValues2075)
summary(CumFluxModel)




CumFluxModel2<- lm(CumNSPCO2ehaNoFossil ~ Percentage + HarvestIntensity + Stand + Burn, data=CumFluxValues)
summary(CumFluxModel2)

CumFluxModel3<- lm(CumNSPCO2ehaNoFossil ~ Percentage * HarvestIntensity + Stand + Burn, data=CumFluxValues)
summary(CumFluxModel3)

CumFluxModel4<- lm(NSPCO2ehaNoFossil ~ Time + Percentage + HarvestIntensity + Stand + Burn, data=CumFluxValuesAll)
summary(CumFluxModel4)

## ANALYSIS WITH SUBSTITUTION included

FCIDataFullPH$Harvest<- factor(FCIDataFullPH$Harvest, levels=c("Clearcut", "NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus"))
FCIDataFullPH$Harvest<- factor(FCIDataFullPH$Rate)

CumFluxValuesSub <- FCIDataFullPH%>%
  select(Time, Year, Scenario, Harvest, Percentage, HarvestIntensity, StandAge, Rate, Burn, CumNSPCO2ehaNoFossilWithSub1, NSPCO2ehaNoFossilWithSub1, ToFPS_CO2eha)%>%
  mutate(Stand=ifelse(StandAge == 150, "Mature", "Old"))

CumFluxValuesSub<- CumFluxValuesSub%>%
  mutate(Harvest=fct_relevel(Harvest, c("Clearcut", "NoHarvest", "PH30", "PH60", "PH30Plus", "PH60Plus")),
         Rate=fct_relevel(Rate, c("High", "Low")),
         Stand = fct_relevel(Stand, c("Old", "Mature")))

CumFluxValuesSub2030 <- CumFluxValuesSub %>%
  filter(Year==2030)

CumFluxValuesSub2050 <- CumFluxValuesSub %>%
  filter(Year==2050)

CumFluxValuesSub2075 <- CumFluxValuesSub %>%
  filter(Year==2075)

CumFluxModelSub <- lm(CumNSPCO2ehaNoFossilWithSub1 ~ Rate + Stand + Harvest + Burn, data= CumFluxValuesSub2075)
summary(CumFluxModelSub)

