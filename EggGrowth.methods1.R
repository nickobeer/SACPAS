
# ===== Example R code for Egg Growth Modeling runs ==== 
# This is an introduction to some scripting methods
# It is NOT intended to be comprehensive
#
# Survivals are generated for user's parameters and data using the methods selected.
# Suitability for any particular purpose is NOT guaranteed.
# 15 January 2024
# W. Nicholas Beer CBR/SAFS/UW nickbeer@uw.edu


# https://cbr.washington.edu/sacramento/grow/index.html
# Two "modes" are possible with Egg Growth modeling: 
# --- Historical mode: modeling for American River Chinook based on the carcass survey data
#     in the CBR data base. Historical hindcasts are made by pairing temperature and redd timeseries by year.
#     Hindcasts use token=value pairs to retrieve data from the CBR database.
# --- Scenario mode: This is effectively unlimited with user-provided temperatures and redd or carcass distributions.

# These are examples of historical mode runs. The methods are also the basis of scenario mode runs.
# Parameter values for the development and survival formulations are the defaults shown on the Egg Growth page 
# This is version 2.1.5


#==== 1. Generate query strings ==== 
#  Familiarity with the Egg Growth page GUI is very helpful.
# Using a browser, navigate to https://cbr.washington.edu/sacramento/grow/index.html
# Select the "Generate Query Strings" in the "Analysis and Results Display" area.
# Click one of the "Run Emergence ..." buttons.

#==== 2.  Recover a 'raw' result ====
# Copy and paste one of the query strings from the previous step into a browser window

#==== 3. Modify query to customize it ====
# Return to the Egg Growth page in the browser window.
# Make as many changes as desired. 
# Ensure "Generate Query Strings" is selected.
# Click one of the "Run Emergence ..." buttons.
# Review and test the new query strings.

#==== 4. Prepare for bulk processing with R ====
# assemble parts of a query string
# Familiarity with the programming language: "R" is required.
# R tidyverse library is used here for procssing with R
# Note that generated query strings can be used with other software

# install.packages("tidyverse")
library(tidyverse)

base <- "https://cbr.washington.edu/sac-bin/grow/emergecontrols.pl"
string <- paste0(base,"?raw=cohort&rtnCSV=1")
# Run a specific year:
string <- paste0(string,"&reddyear=2021&tempyear=2021")

#==== 5. Run the model and capture the output ====
# Use R functionality to control modeling and post-processing
result <- read_csv(string)
print(result)

#==== 6. compare mortality models ====
# Select model combinations in turn and generate COMPLETE query strings.
# Capture differences as text snippets
# retain or regenerate these as needed
# MUST change them with each exponential model as a default, since the exponential family shares the same mathematical formulation.
#
# e.g.:
# https://tigger.cbr.washington.edu/sac-bin/grow/emergecontrols.pl?S0=0.347&TL=3&TU=15.4&TcritC=12.07&aTL=1&aTU=16.2&adultperredd=1
# &akL=27&akU=19&alevinSurvPar1=1.35&alevinSurvPar2=0.9054&alevinSurvParexp=8&atus=958&atushatch=417&carcassoffset=-12&carrycap=10000
# &dbredds=AmerRivSpnfem&densdeptype=none&eggSurvPar1=1.35&eggSurvPar2=0.9054&eggSurvParexp=8&eggmodel=zeug&eggparameter=200&
# eggperredd=3000&hatchmechanism=hatchatu&kL=25&kU=29&linparamdefault=martin&modeltemptype=degC&mortality=zeug&mu1=-0.0000188
# &paramdefault=zeug&redddatacolumn=2&reddyear=2022&rtnCSV=0&showcumulative=no&showdetails=no&slopeC=0.0239&smooth=no
# &spawningdataRredds=0&spawnsource=db&spawntextstring=Day,Redds
# 300,10&surv=0.26&tempdatacolumn=2&tempprojpoint=AFO:DailyAvg&tempsource=dbtemp&temptextstring=Day,Loc1,Loc2
# 1:730,10,12&tempyear=2022&timelim=buffered&units=centigrade&version=2.1.5&raw=cohort

# Next are the 3 default configurations of the exponential group of equations.
# They have been extracted from complete query strings (like above)
# The exponential models use the same 6 parameters.
# the "paramdefault" token is for your benefit. The model will run based on the parmeter values, 
# NOT the value of the paramdefault token.

salmoddefault <- paste0("&paramdefault=salmod",
                          "&alevinSurvPar1=2.521",
                          "&alevinSurvPar2=1.461",
                          "&alevinSurvParexp=12",
                          "&eggSurvPar1=1.475",
                          "&eggSurvPar2=1.392"
                         )

# controls for zeug exponential
zeugdefault <- paste0("&paramdefault=zeug",
                        "&alevinSurvPar1=1.35",
                        "&alevinSurvPar2=0.9054",
                        "&alevinSurvParexp=8",
                        "&eggSurvPar1=1.35",
                        "&eggSurvPar2=0.9054"
                        )

waterforumdefault <- paste0("&paramdefault=waterforum",
                              "&alevinSurvPar1=1.017554&alevinSurvPar2=1.24092&alevinSurvParexp=10",
                              "&eggSurvPar1=3.408488&eggSurvPar2=1.21122&eggSurvParexp=11"
                              )
# ensure it is the exponential group
expstring <- paste0(string,"&mortality=exp")

# run through the mortality models and cache results
# build up from base:

base   <- "https://cbr.washington.edu/sac-bin/grow/emergecontrols.pl"
string2 <- paste0(base,"?raw=cohort&rtnCSV=1")
string2 <- paste0(string2,"&reddyear=2021&tempyear=2021")
string2 <- paste0(string2,"&mortality=exp")

mortmodels <- c("waterforumdefault","zeugdefault","salmoddefault")

# run and cache results
resultsLIST <- list()
for(i in 1:length(mortmodels)){
  print(mortmodels[i])
  print(paste0(string2,eval(as.name(mortmodels[i]))))
  resultsLIST[[mortmodels[i]]] <-  read_csv(paste0(string2,eval(as.name(mortmodels[i]))))
}

# More compact and simpler outputs:
# NOTE: rtnCSV not needed nor wanted for single value (survival) return
base   <- "https://cbr.washington.edu/sac-bin/grow/emergecontrols.pl"
string3 <- paste0(base,"?raw=survival")
string3 <- paste0(string3,"&mortality=exp")


results.S <- cbind.data.frame(model=mortmodels,survival=NA)
for(i in 1:length(mortmodels)){
  what <- paste0(string3,"&rtnCSV=1&reddyear=2021&tempyear=2021",eval(as.name(mortmodels[i])))
  print(what)
  results.S[i,2] <-  scan(what)
}
print(results.S)

#==== Multi-change model runs ====
# 7. ALL EXP models and years 2011 to 2022 in CBR redd/carass Data Base
years <- 2015:2022
print(mortmodels)
results.EXP.S <- cbind.data.frame(year=years,"wf.S"=NA,"zeug.S"=NA,"salmod.S"=NA)
for(i in 1:length(mortmodels)){
  for(j in 1:length(years)){
    what <- paste0(string3,"&reddyear=",years[j],"&tempyear=",years[j],eval(as.name(mortmodels[i])))
    # print(what)
    results.EXP.S[j,i+1] <-  scan(what)
  }
}
print(results.EXP.S)

