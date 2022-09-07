#==== Build DCCops ====

if(1){    # build DCCops Read in Delta Cross Channel operations posted as a .pdf
  #   MODIFIED  by WNB Sept 2022
  library(pdftools)
  library(tidyverse)
  
  todayday <- Sys.Date()
  download.file("https://www.usbr.gov/mp/cvo/vungvari/Ccgates.pdf", "Ccgates.pdf", "libcurl", mode="wb")
  DCCpdf <- pdf_text("Ccgates.pdf") # this yields a vector where each string is a page of the .pdf
  # split by newline to create a list of character vectors. Each list entry is a character vector representing the lines of that page
  DCCscript <- strsplit(DCCpdf, "\n")
  dateForm <- " %m/%d/%y"
  DCCops21 <- NULL
  for(i in 1:10){
    temp <- DCCscript[[i]][grep("^[[:blank:]]*[[:digit:]]+[[:punct:]][[:digit:]]+[[:punct:]][[:digit:]]", substr(DCCscript[[i]], 0, 8))]
    # the strsplit will yield a list where each entry is a character vector representing a sinl=gle gate status change
    # after dropping extra whitespace, each vector should be: [1] datetime of operation [2] gate status [3] comments
    temp <- strsplit(temp,"[[:blank:]]{2,}")
    #temp <- lapply(temp, dropBlank) # drop any extra whitespace
    tempTimes <- sapply(temp, '[', 1)
    tempTimes <- gsub(" ","",tempTimes,fixed=T)
    tempOps <- sapply(temp, '[', 3)
    DCCops21 <- as.data.frame(rbind(DCCops21,cbind("Date"=tempTimes,"GateOpen"=tempOps)))
  } # end of pages loop
  DCCops21$Date <- as.Date(DCCops21$Date, format = dateForm)
  
  DCCops21$GateInd <- as.integer(DCCops21$GateOpen == "open")
  DCCops21$Date <- as.Date(DCCops21$Date, format = dateForm)
  DCCops21 <- DCCops21[order(as.Date(DCCops21$Date)),]
  # Work backwards fom the end to eliminate the double dates since we have no time stamps on them
  hold <- DCCops21[nrow(DCCops21),]
  for(i in (nrow(DCCops21)-1):1){if(DCCops21$Date[i] != DCCops21$Date[i+1]){hold <- rbind.data.frame(DCCops21[i,],hold)}}
  DCCops21 <- hold
  newDates <- seq.Date(min(DCCops21$Date[1]), as.Date("2022-08-31"),"day")
  newInd <-   DCCops21$GateInd[findInterval(newDates,DCCops21$Date)]
  DCCops <- cbind.data.frame(Date=newDates,GateInd=newInd)
  while (as.Date(DCCops$Date[nrow(DCCops)]) < todayday){
    DCCops <- rbind(DCCops,c(as.character(DCCops$Date[nrow(DCCops)]+1),DCCops$GateInd[nrow(DCCops)]))
  }
  DCCops$GateInd <- as.integer(DCCops$GateInd)
  
  write_csv(DCCops,"DCCops.csv")
  cat("Date range of DCC operations data: \n")
  range(DCCops$Date, na.rm=T)
}

#==== End build DCCops ===
