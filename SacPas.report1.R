# Ideas to automate data base query SACPas results 
# WNBeer :: 3 Dec 2024
# nickbeer@uw.edu


library(tidyverse)

whatyear=2019
tmplocation="CP"          #  <YOUR CHOICE of a unique directory name to separate your results from others>
usemortality="hatchmort"  # "emergemort"
usedewater="onkwk"        # "off"
spawning="dbcarcass"      # "dbredds"

string <- paste0("https://www.cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dirUseId=",tmplocation,"&temponly=off&tempsource=dbtemp&mortality=",usemortality)
string1 <- paste0(string,"&reddyear=",whatyear,"&tempyear=",whatyear,"&dewater=",usedewater,"&redds=dbredds")
string2 <- paste0(string1,"&raw=13")
result <- read_csv(string2,show_col_types = FALSE)
print(result)


# AFTER the run is complete. Files can be obtained directly from the server's results directory
stringplot <- paste0("http://www.cbr.washington.edu/sacramento/tmp/RESULTS_",tmplocation)
plot1 <- paste0("tempplot.png")
plot2 <- paste0("temphatchplot.png")
plot3 <- paste0("tempemergeplot.png")
plot4 <- paste0("tempplot2.png")

# For WINDOWS need mode="wb" in the download.file function call
download.file(paste0(stringplot,"/",plot1),plot1,mode="wb")
download.file(paste0(stringplot,"/",plot2),plot2,mode="wb")
download.file(paste0(stringplot,"/",plot3),plot3,mode="wb")
download.file(paste0(stringplot,"/",plot4),plot4,mode="wb")

# How to obtain reach-by-reach survivals
survs <- read_csv(paste0(stringplot,"/survival.csv"),show_col_types = FALSE) # TDM dewater and density
tdmsurv <- read_csv(paste0(stringplot,"/tdmsurv.csv"),show_col_types = FALSE) 
redds <- read_csv(paste0(stringplot,"/redddistribution.csv"),show_col_types = FALSE)
denssurv <- read_csv(paste0(stringplot,"/densityonlysurv.csv"),show_col_types = FALSE)
denssurv.W.bg <- read_csv(paste0(stringplot,"/densitysurv.csv"),show_col_types = FALSE)

for(reach in colnames(survs)[-1]){
   if(reach==colnames(survs)[2]) cat("reach ","Count"," TotS"," TDMS","DensS"," D&BS","  DWS","\n")
    x <- sum(survs[reach]*redds[reach])/sum(redds[reach])
    y <- sum(tdmsurv[reach]*redds[reach])/sum(redds[reach])
    z <- sum(denssurv.W.bg[reach]*redds[reach])/sum(redds[reach])
    cat(reach,
        format(sum(redds[reach]),width=5),
        format(round(x,3),width=5),
        format(round(y,3),width=5),
        format(round(sum(denssurv[reach]*redds[reach])/sum(redds[reach]),3),width=5),
        format(round(z,3),width=5),
        format(round( x / y / z ,3),width=5),
    "\n")
}

  
  