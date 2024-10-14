#===== SWE grabber =====
# Downloads SNOTEL data files
# Parses for snow water equivalent SWE metrics pertinent to the Snake River basin using
# Grande Rhonde, Powder, Burnt, Imnaha, Clearwtaer abd Salmon basins
# computes various metrics that are cataloged (see code for data.frames)
# see SNOTEL file documentation/headers  for the meaning of the metrics
# writes out the peak swe date and value for each year
# Pick the days Everday from  March 1  to  May 15
#                             Day  60  to  Day 135  # 76 days to search        # Leap year is ignored
library(tidyverse)
library(lubridate)

dir.create("swe")

# Download SNOTEL reports for every day and every year during March- mid May back to 1987
# 1987 is first year with full set of sites.
# server-side errors are NOT UNCOMMON. restart the download sequence at blocked day as needed 
# This is slow.
# To update the library, change years to a single year or other subset of all years.
years <- 1987:2024

# Download raw files
holdtime <- Sys.time()

for( i in 1:90){
  if(i<=14 & i >= 1){ mo <- "February"; da <- i+14 } # no Feb 29 
  if(i<=45 & i >= 15){ mo <- "March" ;  da <- i-14 }
  if(i<=75 & i >= 46){ mo <- "April" ;  da <- i-45 }
  if(i<=90 & i >= 76){ mo <- "May"   ;  da <- i-75 }
  # Build the strings
  # https://wcc.sc.egov.usda.gov/reports/UpdateReport.html?textReport=Columbia+River+Basin&textRptKey=17&textFormat=SNOTEL+Snow%2FPrecipitation+Update+Report&StateList=Select+a+State&RegionList=17&SpecialList=Select+a+Special+Report&MonthList=April&DayList=8&YearList=1970&FormatList=N0&OutputFormatList=csv&textMonth=April&textDay=8&CompYearList=select+a+year
  string0 <- "https://wcc.sc.egov.usda.gov/reports/UpdateReport.html?textReport=Columbia+River+Basin&textRptKey=17&textFormat=SNOTEL+"
  string02 <- "Snow%2FPrecipitation+Update+Report&StateList=Select+a+State&RegionList=17&SpecialList=Select+a+Special+Report"
  # Keep and inspect all files
  for(year in years){
    string1 <- paste("&MonthList=",mo,"&DayList=",da,"&YearList=",year,"&FormatList=N0&OutputFormatList=csv&textMonth=",mo,"&textDay=",da,"&CompYearList=select+a+year",sep="")
    download.file(paste(string0,string02,string1,sep=""),method="auto",destfile=paste("swe/swe",year,".",mo,".",da,".txt",sep=""))
  }
}

holdtime2 <- Sys.time()
print(holdtime2 -holdtime)

# Run through each raw report (sweYYYY.MM.DD.txt) and grab desired output and write to temporary files.
i.MoDa.lu <- NULL  # We want this to have a running sequence of all the dates across Month bounds.

for( i in 1:90){
  if(i<=14 & i >= 1){ mo <- "February"; da <- i+14 } # no Feb 29 
  if(i<=45 & i >= 15){ mo <- "March" ;  da <- i-14 }
  if(i<=75 & i >= 46){ mo <- "April" ;  da <- i-45 }
  if(i<=90 & i >= 76){ mo <- "May"   ;  da <- i-75 }
  # Read each year and begin parsing for relevant data
  swes <- swesum <- sweN <-  NULL
  x <- NULL
  for(year in years){
    x <- read.table(paste("swe/swe",year,".",mo,".",da,".txt",sep=""),sep=",",skip=74,header=TRUE)
    I <- x$Basin_name == "GRAND RONDE, POWDER, BURNT, IMNAHA" | x$Basin_name == "CLEARWATER AND SALMON"
    swes <- rbind(swes,x[I,c(3,4,5,6,7,9,13)])
  }
  # Repair missing values
  swes$Wteq_amt[swes$Wteq_amt <= -99.0] <- NA
  swes$Prec_wytd_amt[swes$Prec_wytd_amt <= -99.0] <- NA
  # Sum SWE for an index for each year
  swesum <- swes %>% group_by(YYYYMMDD) %>% summarise(sum=sum(Wteq_amt,na.rm=TRUE))
  precipsum <- swes %>% group_by(YYYYMMDD) %>% summarise(sum=sum(Prec_wytd_amt,na.rm=TRUE))
  swemean <- swes %>% group_by(YYYYMMDD) %>% summarise(mean=mean(Wteq_amt,na.rm=TRUE))
  precipmean <- swes %>% group_by(YYYYMMDD) %>% summarise(mean=mean(Prec_wytd_amt,na.rm=TRUE))
  swemedian <- swes %>% group_by(YYYYMMDD) %>% summarise(median=median(Wteq_amt,na.rm=TRUE))
  precipmedian <- swes %>% group_by(YYYYMMDD) %>% summarise(median=median(Prec_wytd_amt,na.rm=TRUE))
  # Count reporting stations for each yearly index 
  # Set up the counts. This is falsely populated because n() counts NA. Will adjust.
  sweN  <- swes %>% group_by(YYYYMMDD) %>% summarise(N=n())
  # compute counts in each year
  for( ii in 1:dim(sweN)[1]){
    junk <- swes[swes$YYYYMMDD== sweN$YYYYMMDD[ii],]
    sweN$N[ii] <- length(junk$Wteq_amt[!is.na(junk$Wteq_amt)])
  }
  # Write out the index file with the date you asked for
  assign(paste("sweuse",mo,da,sep=""),cbind(swesum,sweN,swemean,swemedian)[,c(1,2,4,6,8)])
  assign(paste("sweusechron",i,sep=""),cbind(swesum,sweN,swemean,swemedian)[,c(1,2,4,6,8)])
  assign(paste("precipuse",mo,da,sep=""),cbind(precipsum,precipmean,precipmedian)[,c(1,2,4,6)])
  i.MoDa.lu <- rbind.data.frame(i.MoDa.lu,cbind(i=i,moda=paste(mo,da,sep=""))) 
  cat(i,mo,da,"\n")
  # write.table(sweuseApril1,"sweApril1data.csv",sep=",",row.names=FALSE)
}  # END  the loop for all the dates of interest

# Now open files in turn, by year and find the peak day and value  going through the days in sequence
swepeak <- NULL
for(year in years){
  now <- 0
  nowday <- 0
  for(i in 1:76){
    junk <- eval(as.name(paste("sweuse",i.MoDa.lu[ i.MoDa.lu[,1] == i,2],sep="")))
    test <- junk[match(year,years),]
    if(test$mean[1] > now){
      nowday <- i
      now <- test$mean
    }
  }
  j <- eval(as.name(paste("sweuse",i.MoDa.lu[i.MoDa.lu[,1]==nowday,2],sep="")))[match(year,years),]
  swepeak <- rbind.data.frame(swepeak,cbind.data.frame(year=year,date=i.MoDa.lu[nowday,2],
                                                       # day=nowday,
                                                       day=nowday + 45,  # Makes "day" = DOY not just day in sequence.
                                                                         # For precise day-of-year in leap years with date after Feb. 28 : add one
                                                       YYYMMDD=j[1,1],sum=j[1,2],N=j[1,3],mean=j[1,4],median=j[1,5]))
}
write.table(swepeak,file="swepeak.csv",row.names=FALSE,sep=",")
