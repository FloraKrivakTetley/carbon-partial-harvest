## Version: June 16, 2023 
## Modified from code from Bing Xu, May 2023, Caren Dymond Pine Creek files and HWP model code

library(readtext)
library(dplyr)
library(ggplot2)

## Set up working directory
## Change as needed, make sure correct folder is used
Drive<- "C:/Users/fkrivakt/OneDrive - UBC/Desktop/SCENARIOS2/PH60_300AACNoBurn"
setwd(paste0(Drive))



## Create desired # folders with LANDIS input files, named by their run number
## To create a smaller # then add more later just adjust numbers here as needed
for (i in 1:20){
  Input_files <- list.files(paste0(Drive,'/Seed'))  
  dir.create(paste0('Run',i))
  file.copy(paste0(Drive,'/Seed/',Input_files),paste0('Run',i))
}

## Run replicates sequentially (as many as desired)
## To run more at once, open separate session in R and run a different set of replicates in parallel

for (i in 1:20){
  
  setwd(paste0(Drive,'/Run',i))
  
  shell("SimpleBatchFile.bat", wait=TRUE)
}

## After runs are complete, pull and summarize the desired information
## See Bing's code for an example of how to do this inside original for-loop. 

## Code to pull and summarize information needed from log_Summary

## Create empty initial output files
Output<-matrix(,nrow=0,ncol=15)

for (i in 1:20){
  
  setwd(paste0(Drive,'/Run',i))  
  Summary<-read.csv("log_Summary.csv", sep=",")
  Summary_means_by_year<-aggregate(Summary, by=list(Summary$Time), FUN=mean,na.rm=TRUE)
  Summary_means_by_year$Time <- NULL
  Summary_means_by_year$ecoregion <- NULL
  names(Summary_means_by_year)[1] <- "Time"
  write.csv(Summary_means_by_year, file = "Summary_means_by_year.csv")
  Run_sum<- cbind(Summary_means_by_year, i)
  Output<-rbind(Output,Run_sum)
}

setwd(paste0(Drive))
write.csv(Output, file = "Output.csv")  

### Visualize parts of output as desired

Output2<- Output
Output2$i<- as.factor(Output2$i)

ggplot(data=Output2, aes(x=Time, y=ToFPS, color=i))+
  geom_line()+
  theme_bw()+
  xlab("Simulation Year")+
  ylab(bquote(paste("To FPS "," (g m"^-2,")")))+
  ggtitle("Test plot ToFPS")

######### HWP ###################################

## Create empty initial output files
## Output<-matrix(,nrow=0,ncol=15) ## this is for previous output
OutputHWP<-matrix(,nrow=0,ncol=24)

###
for (r in 1:20){
  
  setwd(paste0(Drive,'/Run',r))
  #read in harvest stream
  
  full <- read.csv("log_fluxbio.csv")  #full input file
  flux <- full %>%
    group_by(Time)  %>%
    summarise(ToFPS=sum(BioToFPS))
  
  ## read in DOM to FPS value, prevent error from duplicated row name in the CSV file
  fullDOM <- read.csv("log_fluxDOM.csv",row.names = NULL)
  colnames(fullDOM) <- colnames(fullDOM)[2:ncol(fullDOM)]
  fullDOM <- fullDOM[ , - ncol(fullDOM)]       
  
  DOM <- fullDOM %>%
    group_by(Time)  %>%
    summarise(ToFPS=sum(DOMtoFPS))
  
  Snag <- fullDOM %>%
    group_by(Time)  %>%
    summarise(snagFPS=sum(SnagsToFPS))
  
  ## put together input values
  harv_orig <- merge(flux, DOM, by='Time')
  harv_orig <- merge(harv_orig, Snag, by='Time')
  harv <- harv_orig[harv_orig$ToFPS.x > 0,] ## If there was no harvest this code will yield no output!
  harv$Total <- (harv$ToFPS.x + harv$ToFPS.y + harv$snagFPS)/100
  
  #the harvest stream is from the first year of harvest (e.g., year 50)
  #Make a new column that has the year since first harvest (year)
  harv$year <- harv$Time - min(harv$Time) + 1
  
  #reverse it to make the  numbers work
  h_rev <- harv[order(nrow(harv):1),]
  
  #fraction remaining for different groups
  frac_rem_orig <- read.csv("RoundwoodRetention.csv")
  frac_rem_orig <- frac_rem_orig[,-1]
  
  #check that we have enough data in the retention curves
  if (max(frac_rem_orig$Year) < max(harv$year)) {
    #add years to make up the difference, and make the values the same as the last year of info
    lastyr <- max(frac_rem_orig$Year)
    for (i in (lastyr+1):max(harv$year) ) {
      frac_rem_orig[i,] <- frac_rem_orig[lastyr,]
      frac_rem_orig$Year[i] <- i
    }
  }
  
  frac_rem <- frac_rem_orig[frac_rem_orig$Year %in% harv$year,]
  
  #make a dataframe of the right length
  final1 <- data.frame(year = harv$year)
  for (icol in 2:ncol(frac_rem)) {
    final1[,paste0("total_",names(frac_rem[icol]))] <- NA
  }
  
  #loop over everything
  y <- nrow(harv)
  for (i in 1:y) {
    j <- y-i+1
    for (icol in 2:ncol(frac_rem)) {
      tmp <- h_rev$Total[j:y] * frac_rem[1:i,icol]  *44/12*(1-0.339763655553752)  #from the spreadsheet
      final1[final1$year==frac_rem$Year[i],paste0("total_",names(frac_rem[icol]))] <-sum(tmp)
    }
  }
  
  ## Don't need this but I'll leave it in in case we want to look at substitution w/ these values
  #Calculating substitution C emission reduction using SF=0.54 (based on RWE), Bing used Sub1 in her results
  harv$sub1<-harv$Total*44/12*0.54
  harv$sub2<-harv$Total*44/12*(1-0.339763655553752)*0.54
  
  
  ##including input harvest data in the final output 
  final2 <- merge(final1, harv, by='year')
  ##I've tried to add a row with all '0' for the year 0, but the following code didn't work. Any better solution?
  FirstRow <-rep(0,ncol(final2))
  final3 <- rbind(FirstRow,final2)
  ##extract every ten yr result
  ##final3 = final2[seq(0, nrow(final2), 10), ]
  
  write.csv(final3, "FPS_C_rentention1.csv")  
  Run_sumHWP<- cbind(final3, r)
  OutputHWP<-rbind(OutputHWP,Run_sumHWP)
}

setwd(paste0(Drive))
write.csv(OutputHWP, file = "OutputHWP.csv")

##########Summary of all the runs##############
## Output<- read.csv("Output.csv") ## IF you forgot to produce these previously
## summarize Landis-II output, mean and SD
Mean_by_year<-aggregate(Output, by=list(Output$Time), FUN=mean,na.rm=TRUE)
SD_by_year<-aggregate(Output, by=list(Output$Time), FUN=sd,na.rm=TRUE)
write.csv(Mean_by_year, file = "Mean_by_year.csv")
write.csv(SD_by_year, file = "SD_by_year.csv")

## summarize HWP output, mean and SD
Mean_HWP<-aggregate(OutputHWP, by=list(OutputHWP$year), FUN=mean,na.rm=TRUE)
SD_HWP<-aggregate(OutputHWP, by=list(OutputHWP$year), FUN=sd,na.rm=TRUE)
write.csv(Mean_HWP, file = "Mean_HWP.csv")
write.csv(SD_HWP, file = "SD_HWP.csv")

## Clear environment
rm(list=ls())
