
### Set directory ########################
## if this Dir doesn't work, try assigning your full directory to Dir as shown in line 6. The folder ECL_Calculator should contain all the required files
rm(list = ls())
#Dir<-"~/ECL_Calculator"
Dir<- "C:/Users/narendra.sahu/Downloads/ECL_Calculator"
setwd(Dir)
##########################################

########   install missing packages automatically and load them  ##############################
list.of.packages <- c("shiny","readxl","writexl","dplyr","reshape2","ggplot2","plyr","scales","grid","gridExtra","shinycssloaders","DT",
                      "shinydashboard","tableHTML","editData","plotly","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 