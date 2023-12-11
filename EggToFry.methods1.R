# Example R code for Egg-to-Fry Fish Model runs
# Survivals are generated for user's parameters and data using the methods proscribed.
# 1 March 2021 &
# 8 December 2023

# W. Nicholas Beer CBR/SAFS/UW nickbeer@uw.edu

# parameter values for the development and survival formulations are the defaults shown on the FishModel page v.2.7.9
# Familiarity with the GUI fishmodel page is very helpful.
# THREE "modes" are possible with Egg-to-Fry modeling: historical, forecasts, and scenario
# 1. historical hindcasts use token=value pairs to retrieve data from the CBR database
# 2. forecasts are limited to temperature projections through end of Novemeber from CVTEMP. 
# 3. scenario mode is effectively unlimited with user-provided temperatures, flows, & redd distributions
# These are examples of historical runs. The methods are also the basis of forecasting and scenario runs.

#==== R tidyverse library is used here
# 1. install.packages("tidyverse")
library(tidyverse)

#==== workspace directory name
# 2. select a directory as a string of numerals and characters
myDIR <- "MYDIR1234"  # edit this value

	# use paste0() to assemble parts of a query string
	base <- paste0("https://cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dirUseId=",myDIR)
	# It is recommende to put the directory ID in the first position, subsequent token=value pairs 
	# can be in any order beginning with "&"

#==== 
# 3. Become familiar with querystring methods:
# - Can use the GUI and select "generate query strings" to see samples of queries
# - Can paste the result of this print statement into browser (without quotation marks!):
print(paste0(base,"&generatequery=1")) 

# the two methods aboveabove will display all of the "raw" codes used to obtain results directly from your model run
# this example will continue with "raw=13" which collects several key results

#==== 
# 4. Use the GUI to alter any of the input controls and they will be detailed in the query string, 
# (For example the aerial survey year, redd ditribution type, historical temperature year, ...)
# this example continues with various historical years using the winter carcass-survey-based winter redd distribution
# token=value pair examples: "&reddyear=2022" "&tempsource=dbtemp&tempyear=2022" "&redds=dbcarcass" 
# NOTE: &generatequery=1 is removed in order to obtain results directly
# example query. Paste print() results into a browser

# create a new base1 (
base1 <- paste0(base,"&tempsource=dbtemp&raw=13")
print(paste0(base1,"&reddyear=2022&tempyear=2022&redds=dbcarcass")) 

# make print line more compact:
string1 <- paste0(base1,"&reddyear=2022&tempyear=2022&redds=dbcarcass")
print(string1)

#===
# 5. capture results in R
# MOST results are multi-line, .csv text 
result <- read_csv(string1)
print(result)
#NOTE: Results with single value (raw=1, grand survival): captured with scan: 
print(scan(paste0(base,"&reddyear=2022&tempsource=dbtemp&tempyear=2022&redds=dbcarcass&raw=1")))

#==== 
# 6. Automate a run to compare hatching mortality model and incubation mortality model 
# ALSO use only during the "critical water years"
# relevant new token=value pairs  : &mortality=hatchmort (default, thus explicit use is same as omiting) or &mortality=emergemort
# include dwater mortality for the years for later analysis also.

 # initialize things
iterations <- 0 # initialize 
start <- Sys.time()
hatchmortresults <- emergemortresults <- NULL
years <- c(2008,2014,2015,2021,2022)
# base1 has a lot of what we need already
for(mort in c("hatchmort","emergemort")){
	out <- NULL
	for(i in 1:length(years)){
		string1 <- paste0(base1,"&mortality=",mort,"&reddyear=",years[i],"&tempyear=",years[i],"&redds=dbcarcass&dewater=onkwk")
		# print(string1)
		result <- read_csv(string1,show_col_types=FALSE)
		names(result) <- c("What",years[i])
		# glue together
		if(i==1) { out <- result } else { out <- cbind.data.frame(out,result[,2]) }
		iterations <- iterations + 1
	}
	if(mort== "hatchmort"){hatchmortresults <- out 
	}else{emergemortresults <- out}
}

end <- Sys.time()
cat(paste(iterations,"iterations\n"))
print(end-start) 

#==== 
#7. Compile some results
report <- hatchmortresults %>% filter(What=="TDM mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="TDM")
report2 <- emergemortresults %>% filter(What=="TDM mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="TDM")
print("Mortality at hatching period calibration")
print(report)
print("Mortality all incubation period calibration")
print(report2)

# 8. compile dewatering in these yeras
# show that it is identical for both TDM mortality computations
emergemortresults %>% filter(What=="Dewater mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="Dewater Mortality")
hatchmortresults %>% filter(What=="Dewater mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="Dewater Mortality")


#===
# 9. Example of a forecast run. At time of writing in December 2023,temperature forecast season is over so the 
#  future is filled in forecast with historical mean.
# These are equivalent:
test <- read_csv(paste0(base,"&redds=dbAll&reddyear=2023&tempyear=2023&raw=13"),show_col_types = FALSE)
print(test)
test <- read_csv(paste0(base,"&redds=dbAll&reddyear=2023&tempsource=latest&raw=13"),show_col_types = FALSE)
print(test)

#==== Scenario runs with user files require pre-loading user files and then applying similar token value pairs that reference the files as data sources
# Contact CBR staff at web@cbr.washington.edu