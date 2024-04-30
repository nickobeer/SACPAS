# R code to obtain dewater percentage using USFWS 2006 method.
# Generate dewatering for historical WRCS redd distributions and collate.
# Available March 19, 2024
# install.packages(“tidyverse”)

mydirectoryname <- Sys.getenv("USERNAME") 
# mydirectoryname should be unique. It can be an arbitrary string of characters/numerals

library(tidyverse)
years <- 2013:2022
DWfracOut <- DWfracIn <- redds <- rep(NA,length=length(years)) 
base <- "https://cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dewater=onkwk"
base <- paste0(base,"&dirUseId=",mydirectoryname,"&redds=dbcarcass&tempsource=dbtemp")
for( i in 1:length(years)){
  cat(years[i])
  this <- paste0("&reddyear=",years[i],"&tempyear=",years[i])
  out <- read_csv(paste0(base,this,"&raw=13"),show_col_types = FALSE)
  DWfracOut[i]   <- as.numeric(out[14,2])
  redds[i]       <- as.numeric(out[2,2])
  out <- read_csv(paste0(base,this,"&raw=13&dewatertype=boards"),show_col_types = FALSE)
  DWfracIn[i]   <- as.numeric(out[14,2])
} 
dewaterUSFWS <- cbind.data.frame(years,redds,DWfracOut,DWfracIn)
print(dewaterUSFWS) 
