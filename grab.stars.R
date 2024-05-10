# caitlin.pullSTARS.R
# COPY/MOVE or link to file:
# /var/httpd/www/shiny/apps/STARS/STARS.shinyinputs.Rdata
# heads up, this object has class: "xts" and "zoo"
library(xts)
load("STARS.shinyinputs.Rdata",verbose=T)

print(colnames(WR_xts))
print(head(index(WR_xts)))

print(colnames(RSM_xts)) # latefalls are different

WRwant <- WR_xts[,c("Survival Interior Delta Est", 
                    "Survival Interior Delta LCL 80", 
                    "Survival Interior Delta UCL 80",
                    "Routing Probability Interior Delta Est",    
                    "Routing Probability Interior Delta LCL 80",
                    "Routing Probability Interior Delta UCL 80",
                    "Survival Overall Est", 
                    "Survival Overall LCL 80",
                    "Survival Overall UCL 80")]
LFwant <- RSM_xts[,]  # Need to review how to handle these STARS results

write_csv(WRwant,"WR.stars.csv")
write_csv(LFwant,"LF.stars.csv")
