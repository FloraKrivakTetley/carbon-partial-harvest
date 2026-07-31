library(dplyr)

#read in harvest stream
#harv <- read.csv("toFPS.txt")

full <- read.csv("log_fluxbio.csv")  #full input file
flux <- full %>%
  group_by(Time)  %>%
  summarise(ToFPS=sum(BioToFPS))

fullDOM <- read.csv("log_fluxDOM.csv")
DOM <- fullDOM %>%
  group_by(Time)  %>%
  summarise(ToFPS=sum(DOMtoFPS))

Snag <- fullDOM %>%
  group_by(Time)  %>%
  summarise(snagFPS=sum(SnagsToFPS))


harv_orig <- merge(flux, DOM, by='Time')
harv_orig <- merge(harv_orig, Snag, by='Time')
harv <- harv_orig[harv_orig$ToFPS.x > 0,]
harv$Total <- (harv$ToFPS.x + harv$ToFPS.y + harv$snagFPS)/100
#summarize all the harvested C and transfer unit from gC/m2/yr to tC/ha/yr)

write.csv(harv, "harv.csv")
#add in another 100 year of 0 harvest every 10 years
# (this just makes the calculations in the later step easier)
lastyr <- max(harv$Time)
for (i in 1:10 ) {
  harv[nrow(harv)+1,] <- c((lastyr + 10*i),0,0,0,0)
}

#the harvest stream is from the first year of harvest (e.g., year 50)
#Make a new column that has the year since first harvest (year)
harv$year <- harv$Time - min(harv$Time) + 1

#reverse it to make the  numbers work
#h_rev <- rev(harv)
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

#now make the matrix that is used only contain the years that have harvest
frac_rem <- frac_rem_orig[frac_rem_orig$Year %in% harv$year,]

#make a dataframe of the right length
final2 <- data.frame(year = harv$year)

for (icol in 2:ncol(frac_rem)) {
  final2[,paste0("total_",names(frac_rem[icol]))] <- NA
}

#loop over everything
y <- nrow(harv)
for (i in 1:y) {
  j <- y-i+1
  for (icol in 2:ncol(frac_rem)) {
    tmp <- h_rev$Total[j:y] * frac_rem[1:i,icol]  *44/12*(1-0.339763655553752)  #from the spreadsheet
    final2[final2$year==frac_rem$Year[i],paste0("total_",names(frac_rem[icol]))] <-sum(tmp)
  }
}

#Calculating substitution C emission reduction using SF=0.54 (based on RWE)
harv$sub1<-harv$Total*44/12*0.54
harv$sub2<-harv$Total*44/12*(1-0.339763655553752)*0.54


##including input harvest data in the final output 
final3 <- merge(final2, harv, by='year')
##extract every ten yr result
final4 = final3[seq(1, nrow(final3), 10), ]
  
write.csv(final4, "FPS_C_rentention1.csv")  
write.csv(final2, "FPS_C_rentention2.csv")



