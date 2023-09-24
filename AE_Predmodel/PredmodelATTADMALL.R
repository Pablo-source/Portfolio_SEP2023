# R Script: PredmodelATTADMALL.R
# PRODUCE ATTENDANCES FORECAST
# Install required packages
#install.packages("forecast",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("tidyverse",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("readxl",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("caret",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("partykit",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("data.table",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("janitor",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("forecastHybrid",repos="https://cloud.r-project.org",dependencies=TRUE)
#install.packages("pracma",repos="https://cloud.r-project.org",dependencies=TRUE)
# Load required packages . Note: ONCE Installed packages in local machine, just run library command
library(forecast)
library(tidyverse)
library(readxl)
# library(caret)
# library(partykit)
library(data.table)
library(xts)
library(janitor)
library(forecastHybrid)
library(pracma)
library(readr)
# TEST SCRIPT ON THE SERVER
# C:/TFS folder/24 FINAL R AND ALTERYX TABLEAU PRED MODEL/33 R model AUTOMATION/AUTOM OUTPUT
# Libraries in case the CLOUD CRAN PROJECT is down
# Check installed pacakges
Mypath<- .libPaths()
(.packages())
# ALL OUTPUT from the outomation will go to /R models output folder
# //irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models output

# Start running time for ATTENDANCES

#################
# ATTENDANCES ###
#################
# SET WD
setwd("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound")
getwd()

# Attendances data preparation
Prep_A <- read_csv("RMODELINPUT FIVET.csv",
col_types = cols(
ATTENDANCES_Type_1 = col_double(),
ADMISSIONS_Type_1 = col_skip(),
TEMPERATURE = col_skip(),
TRUSTCODE = col_skip(),
DATE = col_date(format = "%Y-%m-%d")
))

names(Prep_A)
head(Prep_A)
tail(Prep_A)
str(Prep_A)
# Rename this dataset as A_E
A_E <- Prep_A
# Check min and max dates
ST_date <- min(A_E$DATE)
END_date <-max(A_E$DATE)
ST_date
END_date
# colnames(A_E)<-A_E[1,]
# A_E<-A_E[(2:nrow(A_E)),]
# setnames(A_E,"NA","date")
setnames(A_E,"TRUSTNAME", "Name")
setnames(A_E, "DATE", "date")
setnames(A_E, "ATTENDANCES_Type_1", "Attendances")
names(A_E)
head(A_E)
tail(A_E)


A_E$Attendances<-as.numeric(A_E$Attendances)
oldNames<- unique(A_E$Name)
A_E$Name<-gsub("[[:punct:]]", "", A_E$Name)
A_E$Name<-gsub(" ","_",A_E$Name)
A_E$Name<-gsub("'","",A_E$Name)
A_E$Attendances<-ifelse(A_E$Attendances == 0, NA, A_E$Attendances)# Obtain list of sites
sites <- unique(A_E$Name)
numSites <- length(sites)
numSites

# Order the dataset
keycol <-c("Name","date")
setorderv(A_E, keycol)
# Sub missing values by
for (i in 1:nrow(A_E)) {
if(is.na(A_E$Attendances[i]) & (strcmp(A_E[i,]$Name,A_E[i-7,]$Name))){
A_E$Attendances[i] <- A_E$Attendances[i-7]
}
}
#Clean NA?s
A_E<-A_E[complete.cases(A_E), ]

#Define variable
# Forecast scenario 31 days
daysAhead<-31
results <- list()

for(i in 1:numSites){
  
## Generate TimeSeries and Test data
assign(paste0("ts",sites[i]), A_E[A_E$Name %in% sites[i],2])
assign(paste0("test",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[((nrow(",paste0("ts",sites[i]),")-
                                                                                ",daysAhead,"):nrow(",paste0("ts",sites[i]),")),]"))),frequency=7))
assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[(1:(nrow(",paste0("ts",sites[i]),")-
                                                                                 ",daysAhead,")),]"))),frequency=7))
assign(paste0("tss2",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=7))
## We run five models per Trust

# ARIMA
assign(paste0("autoarima",sites[i]), auto.arima(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predArima",sites[i]), forecast(eval(parse(text = paste0("autoarima",sites[i]))), h = daysAhead))
assign(paste0("autoarima2",sites[i]), auto.arima(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2Arima",sites[i]), forecast(eval(parse(text = paste0("autoarima2",sites[i]))), h = daysAhead))
# TBATS
assign(paste0("tbats",sites[i]), tbats(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predtbats",sites[i]), forecast(eval(parse(text = paste0("tbats",sites[i]))), h = daysAhead))
assign(paste0("tbats2",sites[i]), tbats(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2tbats",sites[i]), forecast(eval(parse(text = paste0("tbats2",sites[i]))), h = daysAhead))
# ETS
assign(paste0("ets",sites[i]), ets(eval(parse(text = paste0("tss",sites[i]))), model="MAM")) #Holt-Winters
assign(paste0("predets",sites[i]), forecast(eval(parse(text = paste0("ets",sites[i]))),h=daysAhead,method ='ets'))
assign(paste0("ets2",sites[i]), ets(eval(parse(text = paste0("tss2",sites[i]))), model="MAM")) #Holt-Winters
assign(paste0("pred2ets",sites[i]), forecast(eval(parse(text = paste0("ets2",sites[i]))),h=daysAhead,method ='ets'))
# ENSAMBLE
assign(paste0("predEnsemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(predArima",sites[i],")")))))+((1/3)*
(eval(parse(text = paste0("data.frame(predtbats",sites[i],")")))))+((1/3)*(eval(parse(text =
paste0("data.frame(predets",sites[i],")")))))))
assign(paste0("pred2Ensemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(pred2Arima",sites[i],")")))))+((1/3)*
(eval(parse(text = paste0("data.frame(pred2tbats",sites[i],")")))))+((1/3)*(eval(parse(text =
paste0("data.frame(pred2ets",sites[i],")")))))))
# HYBRID
assign(paste0("hybrid",sites[i]), hybridModel(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predhybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid",sites[i]))), h = daysAhead))
assign(paste0("hybrid2",sites[i]), hybridModel(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2hybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid",sites[i]))), h = daysAhead))
## Compare models
assign(paste0("Accuracy",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i]))))))
assign(paste0("Accuracy",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i]))))))
assign(paste0("Accuracy",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i]))))))
assign(paste0("Accuracy",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i]))))))
assign(paste0("MAPE",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"Ensemble") ,data.table(colMeans(abs(eval(parse(text = paste0("data.frame(test",sites[i],")")))-
(eval(parse(text = paste0("data.frame(predEnsemble",sites[i],")$Point.Forecast")))))/(eval(parse(text =
paste0("data.frame(test",sites[i],")")))))*100))
# RMSE accuracy measures
assign(paste0("RMSE",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"Ensemble") ,data.table(colMeans(abs(eval(parse(text = paste0("data.frame(test",sites[i],")")))-
(eval(parse(text = paste0("data.frame(predEnsemble",sites[i],")$Point.Forecast")))))/(eval(parse(text =
paste0("data.frame(test",sites[i],")")))))*100))
}
# Matrix to be populateda<-data.frame(matrix(NA, nrow = numSites, ncol = 5))
# Collect RMSE values
for(i in 1:numSites){
for(j in 1:5){
## Generate the csv's
a[i,j]<-min(eval(parse(text=data.table(ls(pattern=paste0("^RMSE",sites[i])))[j,]$V1)))
names(a)<-c("Arima", "Ensemble", "ets","hybrid","tbats")
rownames(a)<-sites
names(min(a[1,]))
colnames(a)[apply(a[j,],1,which.min)]
}
}
# Export ouptut to shared drive
write.csv(a, '//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/MAPEByModelNHSI_ATT.csv', row.names=T)
# Still output without any time stamp into AUTOM OUTPUT TO TEST THE LAST STEP
#

# ATTENDANCES FORECASTED VALUES
# Run from line 209 to 340 to obtain FORECAST using ARIMA ENSAMBLE ETS HYBRID AND ETBATS MODELS for Attendances
# Output file with predicted values:AE_PredictionNHSI_ATT.csv

for(i in 1:numSites){
if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
graphics::plot(data.frame(cbind(data.frame(rownames(data.frame(rbind.data.frame(eval(parse(text =
paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)
[apply(a[i,],1,which.min)],sites[i],"$Point.Forecast")))))), rbind.data.frame(eval(parse(text =
paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)
[apply(a[i,],1,which.min)],sites[i],"$Point.Forecast"))))))), lty = c(1,3), col=c(5,2))


paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))), c(rev(eval(parse(text = paste0("pred", colnames(a)
[apply(a[i,],1,which.min)],sites[i])))[ ,2]), eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))[
,3]), col = 'grey80', border = NA)

dev.off()

}else{
jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
plot(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))), lty = c(1,3), col=c(5,2))

dev.off()
}

if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
assign(paste0("dtpred",sites[i]),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
[apply(a[i,],1,which.min)],sites[i],")")))),data.frame(sites[i])))
assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)
[apply(a[i,],1,which.min)]"))))
assign(paste0("dtpred",sites[i]),eval(parse(text = paste0("dtpred",sites[i],"[,c(1,2,4,3,5,6,7,8)]"))))
} else {
assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
[apply(a[i,],1,which.min)],sites[i],"$mean)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
[apply(a[i,],1,which.min)],sites[i],"$lower)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
[apply(a[i,],1,which.min)],sites[i],"$upper)"))))),data.frame(sites[i])))
assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))

# NOTE THIS LINE BELOW DEFINES WHICH MODEL IS PICKED UP AS THE BEST PERFOMRING MODEL
assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)
[apply(a[i,],1,which.min)]"))))
}
assign(paste0("dthist",sites[i]),cbind(data.frame(eval(parse(text=paste0("ts",sites[i])))),data.frame(sites[i])))
}

# Check model performance and best performing model selected by the SCRIPT
# Next loop: Creates HISTORIC WITH NA and for the optimal one fills in dates
# dataset with forecasted values and CI

for(i in 1:numSites){
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), d = NA, .after = 2))
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), e = NA, .after = 3))
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), f = NA, .after = 4))
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), g = NA, .after = 5))
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), h = 0, .after = 6))
assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), i = NA, .after = 7))
assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), date = as.character(seq.Date(from =
as.Date(max(A_E$date)) +1, to= as.Date(max(A_E$date)) +(daysAhead) , by=1)), .after = daysAhead))
}

# Check output

# Assign column names to previous output
for(i in 1:numSites){
assign(paste0("dtpred",sites[i]),`names<-`(get(paste0("dtpred",sites[i])),
c("Attendances","low80","low95","high80","high95","Name","forecasted","model", "date")))
assign(paste0("dthist",sites[i]),`names<-`(get(paste0("dthist",sites[i])),
c("Attendances","Name","high95","high80","low80","low95","forecasted","model")))
assign(paste0("dthist",sites[i]),left_join(get(paste0("dthist",sites[i])),A_E,copy=FALSE))
assign(paste0("dthist",sites[i]), eval(parse(text=paste0("dthist",sites[i],"[,-10]"))))}

# THIS THE FINAL DATASET
final<-data.frame()
for(i in 1:numSites){
final<- rbind.data.frame(final,eval(parse(text=paste0("dthist",sites[i]))),eval(parse(text=paste0("dtpred",sites[i]))))
}
# Data cleansing anyting below 0 ( avoid negative values in the model)
final$date<- as.Date(final$date)
final$Attendances<-ifelse(final$Attendances<0,0, final$Attendances)
final$low80<-ifelse(final$low80<0,0, final$low80)
final$low95<-ifelse(final$low95<0,0, final$low95)
final$high80<-ifelse(final$high80<0,0, final$high80)
final$high95<-ifelse(final$high95<0,0, final$high95)
# Check no duplicated rows
final<-unique(final)
# Sort by date and trust name
keycol <-c("Name","date")
setorderv(final, keycol)
rownames(final)<-(1:nrow(final))
# include METRIC NAME INTO FINAL DATASET
final$METRIC <-c("Attendances")
# 10 Select just 5 days forecast
# 10.1 Create falg variable for two 5 days into the future
FINAL_actual <- final %>%
filter(forecasted == 0)
FINAL_fcast <- final %>%
filter(forecasted != 0)
# 02/01/2020 subset 5 days of forecasted values by Name
# 10.3 Merge FORECASTED values by trust together (Previous script, depreciated)
# FINAL_FCAST_ALL <-rbind.data.frame(FINAL_FCAST_LUT,FINAL_FCAST_HILL,FINAL_FCAST_BIR, FINAL_FCAST_ALDER,FINAL_FCAST_KING)
# FINAL_FCAST_ALL
# FINAL_FCAST_ALL (ATTENDANCES)
FINAL_FCAST_ALL <- FINAL_fcast %>%
group_by(Name) %>%
slice(1:5)
# 10.4 APPEND FORECAST VALUES AND ACTUAL VALUES
FINAL_ATT <- rbind.data.frame(FINAL_actual,FINAL_FCAST_ALL)
FINAL_ATT_D <- FINAL_ATT
# 10.5 sort dataset by date and Name
keycol <-c("Name","date")
setorderv(FINAL_ATT_D, keycol)
# ATTENDANCES
# OUTPUT ATTENDANCES PREDICTED VALUES
write.csv(FINAL_ATT_D, '//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/AE_PredictionNHSI_ATT.csv', row.names=T)
# Clear workspace
rm(list=ls())



# ADMISSIONS #
# Section 02-03
# SET WD
# setwd("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound")
getwd()
getwd()

# THIS WORKS FINE
Prep_A <- read_csv("RMODELINPUT FIVET.csv",
col_types = cols(
ATTENDANCES_Type_1 = col_skip(),
ADMISSIONS_Type_1 = col_double(),
TEMPERATURE = col_skip(),
TRUSTCODE = col_skip(),
DATE = col_date(format = "%Y-%m-%d")
))
names(Prep_A)
head(Prep_A)
tail(Prep_A)
str(Prep_A)
# Rename this dataset as A_E
A_E <- Prep_A
# Check min and max dates
ST_date <- min(A_E$DATE)
END_date <-max(A_E$DATE)ST_date
END_date
head(A_E)
tail(A_E)
# Check min and max dates
ST_date <- min(A_E$DATE)
END_date <-max(A_E$DATE)
ST_date
END_date
# Save workspace
# colnames(A_E)<-A_E[1,]
# A_E<-A_E[(2:nrow(A_E)),]
# setnames(A_E,"NA","date")
setnames(A_E,"TRUSTNAME", "Name")
setnames(A_E, "DATE", "date")
setnames(A_E, "ADMISSIONS_Type_1", "Admissions")
head(A_E)
tail(A_E)
# Save workspace
A_E$Admissions<-as.numeric(A_E$Admissions)
oldNames<- unique(A_E$Name)
A_E$Name<-gsub("[[:punct:]]", "", A_E$Name)
A_E$Name<-gsub(" ","_",A_E$Name)
A_E$Name<-gsub("'","",A_E$Name)
A_E$Admissions<-ifelse(A_E$Admissions == 0, NA, A_E$Admissions)
# Obtain list of sites
sites <- unique(A_E$Name)
numSites <- length(sites)
# Order the dataset
keycol <-c("Name","date")
setorderv(A_E, keycol)
# Sub missing values by
for (i in 1:nrow(A_E)) {
if(is.na(A_E$Admissions[i]) & (strcmp(A_E[i,]$Name,A_E[i-7,]$Name))){
A_E$Admissions[i] <- A_E$Admissions[i-7]
}
}
#Clean NA?s
A_E<-A_E[complete.cases(A_E), ]
#Define variable
# Forecast scenario 31 days
daysAhead<-31
results <- list()
for(i in 1:numSites){
## Generate TimeSeries and Test data
assign(paste0("ts",sites[i]), A_E[A_E$Name %in% sites[i],2])
assign(paste0("test",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[((nrow(",paste0("ts",sites[i]),")-
",daysAhead,"):nrow(",paste0("ts",sites[i]),")),]"))),frequency=7))
assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[(1:(nrow(",paste0("ts",sites[i]),")-
",daysAhead,")),]"))),frequency=7))
assign(paste0("tss2",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=7))
## We run five models per Trust
# ARIMA
assign(paste0("autoarima",sites[i]), auto.arima(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predArima",sites[i]), forecast(eval(parse(text = paste0("autoarima",sites[i]))), h = daysAhead))
assign(paste0("autoarima2",sites[i]), auto.arima(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2Arima",sites[i]), forecast(eval(parse(text = paste0("autoarima2",sites[i]))), h = daysAhead))
#plot(eval(parse(text = paste0("predArima",sites[i]))), lty = c(1,3), col=c(5,2))
# TBATS
assign(paste0("tbats",sites[i]), tbats(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predtbats",sites[i]), forecast(eval(parse(text = paste0("tbats",sites[i]))), h = daysAhead))
assign(paste0("tbats2",sites[i]), tbats(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2tbats",sites[i]), forecast(eval(parse(text = paste0("tbats2",sites[i]))), h = daysAhead))
# ETS
assign(paste0("ets",sites[i]), ets(eval(parse(text = paste0("tss",sites[i]))), model="MAM")) #Holt-Winters
assign(paste0("predets",sites[i]), forecast(eval(parse(text = paste0("ets",sites[i]))),h=daysAhead,method ='ets'))
assign(paste0("ets2",sites[i]), ets(eval(parse(text = paste0("tss2",sites[i]))), model="MAM")) #Holt-Winters
assign(paste0("pred2ets",sites[i]), forecast(eval(parse(text = paste0("ets2",sites[i]))),h=daysAhead,method ='ets'))
# ENSAMBLE
assign(paste0("predEnsemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(predArima",sites[i],")")))))+((1/3)*
(eval(parse(text = paste0("data.frame(predtbats",sites[i],")")))))+((1/3)*(eval(parse(text =
paste0("data.frame(predets",sites[i],")")))))))
assign(paste0("pred2Ensemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(pred2Arima",sites[i],")")))))+((1/3)*
(eval(parse(text = paste0("data.frame(pred2tbats",sites[i],")")))))+((1/3)*(eval(parse(text =
paste0("data.frame(pred2ets",sites[i],")")))))))# HYBRID
assign(paste0("hybrid",sites[i]), hybridModel(eval(parse(text = paste0("tss",sites[i])))))
assign(paste0("predhybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid",sites[i]))), h = daysAhead))
assign(paste0("hybrid2",sites[i]), hybridModel(eval(parse(text = paste0("tss2",sites[i])))))
assign(paste0("pred2hybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid",sites[i]))), h = daysAhead))
## Compare models
assign(paste0("Accuracy",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i]))))))
assign(paste0("Accuracy",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i]))))))
assign(paste0("Accuracy",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i]))))))
assign(paste0("Accuracy",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i]))))))
assign(paste0("MAPE",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i])))))$MAPE)
assign(paste0("MAPE",sites[i],"Ensemble") ,data.table(colMeans(abs(eval(parse(text = paste0("data.frame(test",sites[i],")")))-
(eval(parse(text = paste0("data.frame(predEnsemble",sites[i],")$Point.Forecast")))))/(eval(parse(text =
paste0("data.frame(test",sites[i],")")))))*100))
# RMSE accuracy measures
assign(paste0("RMSE",sites[i],"Arima"), data.table(accuracy(eval(parse(text = paste0("predArima",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"ETS"), data.table(accuracy(eval(parse(text = paste0("predets",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"TBATS") ,data.table(accuracy(eval(parse(text = paste0("predtbats",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"Hybrid") ,data.table(accuracy(eval(parse(text = paste0("predhybrid",sites[i])))))$RMSE)
assign(paste0("RMSE",sites[i],"Ensemble") ,data.table(colMeans(abs(eval(parse(text = paste0("data.frame(test",sites[i],")")))-
(eval(parse(text = paste0("data.frame(predEnsemble",sites[i],")$Point.Forecast")))))/(eval(parse(text =
paste0("data.frame(test",sites[i],")")))))*100))
}
# Matrix to be populated
a<-data.frame(matrix(NA, nrow = numSites, ncol = 5))
# Collect RMSE values
for(i in 1:numSites){
for(j in 1:5){
## Generate the csv's
a[i,j]<-min(eval(parse(text=data.table(ls(pattern=paste0("^RMSE",sites[i])))[j,]$V1)))
names(a)<-c("Arima", "Ensemble", "ets","hybrid","tbats")
rownames(a)<-sites
names(min(a[1,]))
colnames(a)[apply(a[j,],1,which.min)]
}
}
# Export ouptut to shared drive
write.csv(a, '//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/MAPEByModelNHSI_ADM.csv', row.names=T) # line 610
# Run from here to line 228 to produce final dataset
for(i in 1:numSites){
  if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
    jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
    graphics::plot(data.frame(cbind(data.frame(rownames(data.frame(rbind.data.frame(eval(parse(text =
                                                                                                 paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)
                                                                                                                                                    [apply(a[i,],1,which.min)],sites[i],"$Point.Forecast")))))), rbind.data.frame(eval(parse(text =
                                                                                                                                                                                                                                               paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)
                                                                                                                                                                                                                                                                                                  [apply(a[i,],1,which.min)],sites[i],"$Point.Forecast"))))))), lty = c(1,3), col=c(5,2))
    #polygon(c(rev(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))), eval(parse(text =
    paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))), c(rev(eval(parse(text = paste0("pred", colnames(a)
                                                                                                      [apply(a[i,],1,which.min)],sites[i])))[ ,2]), eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))[
                                                                                                        ,3]), col = 'grey80', border = NA)
dev.off()
  }else{
    jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
    plot(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))), lty = c(1,3), col=c(5,2))
    dev.off()
  }
  if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
    assign(paste0("dtpred",sites[i]),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
                                                                             [apply(a[i,],1,which.min)],sites[i],")")))),data.frame(sites[i])))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)
[apply(a[i,],1,which.min)]"))))
    assign(paste0("dtpred",sites[i]),eval(parse(text = paste0("dtpred",sites[i],"[,c(1,2,4,3,5,6,7,8)]"))))
  } else {
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
                                                                                   [apply(a[i,],1,which.min)],sites[i],"$mean)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
                                                                                                                                                                             [apply(a[i,],1,which.min)],sites[i],"$lower)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)
                                                                                                                                                                                                                                                                        [apply(a[i,],1,which.min)],sites[i],"$upper)"))))),data.frame(sites[i])))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)
[apply(a[i,],1,which.min)]"))))
  }
  assign(paste0("dthist",sites[i]),cbind(data.frame(eval(parse(text=paste0("ts",sites[i])))),data.frame(sites[i])))}
# Check model performance and best performing model selected by the SCRIPT
# Next loop: Creates HISTORIC WITH NA and for the optimal one fills in dates
# dataset with forecasted values and CI
for(i in 1:numSites){
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), d = NA, .after = 2))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), e = NA, .after = 3))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), f = NA, .after = 4))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), g = NA, .after = 5))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), h = 0, .after = 6))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), i = NA, .after = 7))
  assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), date = as.character(seq.Date(from =
                                                                                                                          as.Date(max(A_E$date)) +1, to= as.Date(max(A_E$date)) +(daysAhead) , by=1)), .after = daysAhead))
}
# Assign column names to previous output
for(i in 1:numSites){
  assign(paste0("dtpred",sites[i]),`names<-`(get(paste0("dtpred",sites[i])),
                                             c("Admissions","low80","low95","high80","high95","Name","forecasted","model", "date")))
  assign(paste0("dthist",sites[i]),`names<-`(get(paste0("dthist",sites[i])),
                                             c("Admissions","Name","high95","high80","low80","low95","forecasted","model")))
  assign(paste0("dthist",sites[i]),left_join(get(paste0("dthist",sites[i])),A_E,copy=FALSE))
  assign(paste0("dthist",sites[i]), eval(parse(text=paste0("dthist",sites[i],"[,-10]"))))
}
# THIS THE FINAL DATASET
final<-data.frame()
for(i in 1:numSites){
  final<- rbind.data.frame(final,eval(parse(text=paste0("dthist",sites[i]))),eval(parse(text=paste0("dtpred",sites[i]))))
}
# Data cleansing anyting below 0 ( avoid negative values in the model)
final$date<- as.Date(final$date)
final$Admissions<-ifelse(final$Admissions<0,0, final$Admissions)
final$low80<-ifelse(final$low80<0,0, final$low80)
final$low95<-ifelse(final$low95<0,0, final$low95)
final$high80<-ifelse(final$high80<0,0, final$high80)
final$high95<-ifelse(final$high95<0,0, final$high95)
# Check no duplicated rows
final<-unique(final)
# Sort by date and trust name
keycol <-c("Name","date")
setorderv(final, keycol)
rownames(final)<-(1:nrow(final))
# include METRIC NAME INTO FINAL DATASET
final$METRIC <-c("Admissions")
# 10 Select just 5 days forecast
# 10.1 Create falg variable for two 4 days into the future
FINAL_actual <- final %>%
  filter(forecasted == 0)
FINAL_fcast <- final %>%
  filter(forecasted != 0)
# FINAL_FCAST_ALL (ADMISSIONS)
FINAL_FCAST_ALL_ADM <- FINAL_fcast %>%
  group_by(Name) %>%
  slice(1:5)
# 10.3 Merge FORECASTED values by trust together (Previous script, depreciated)
# FINAL_FCAST_ALL_ADM <-rbind.data.frame(FINAL_FCAST_LUT,FINAL_FCAST_HILL,FINAL_FCAST_BIR, FINAL_FCAST_ALDER,FINAL_FCAST_KING)
# FINAL_FCAST_ALL_ADM
# 10.4 APPEND FORECAST VALUES AND ACTUAL VALUES
FINAL_ADM <- rbind.data.frame(FINAL_actual,FINAL_FCAST_ALL_ADM)
FINAL_ADM_D <- FINAL_ADM
# 10.5 sort dataset by date and Name
keycol <-c("Name","date")
setorderv(FINAL_ADM_D, keycol)
#gsub(final$Name, sites, oldNames)
# Old Script: write.csv(FINAL_ATT_D, file = "AE_PredictionNHSI_ADM.csv")
write.csv(FINAL_ADM_D, '//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/AE_PredictionNHSI_ADM.csv', row.names=T) #Line 738


# 03 PUTTING ALL RESULTS TOGETHER
# section 03-03
# PLR: PREPARE ALL OUTPUTS FOR ALTERYX
# Alteryx will load ech .csv output file produced in the above sections to run workflow comparing R and Alteryx models.
# Final output fill be saved to Tableau Server

# Llist objects in R workspace
ls()
list.files()
getwd()
setwd("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models output")
getwd()

# 1.1 Output files from
# ARIMA, # TBATS, # ETS, # ENSAMBLE, # HYBRID)# A&E-PredictionNHSI_adm.csv
  # A&E-PredictionNHSI_ATT.csv
  # 1-2 Prediction ATTENDANCES dataset
  PRED_ATT <- read.csv("AE_PredictionNHSI_ATT.csv",head=TRUE,sep=",")
  PRED_ATT <- select (PRED_ATT,-c(X))
  names(PRED_ATT)
  head(PRED_ATT)
  tail(PRED_ATT)
  # Re-name first variable prior to merging ADM with ATT
  # names(data)[3]<-"new_name"
  names(PRED_ATT)[1]<-"MValue"
  # 2-2 Prediction ADMISSIONS dataset
  PRED_ADM <- read.csv("AE_PredictionNHSI_ADM.csv",head=TRUE,sep=",")
  PRED_ADM <- select (PRED_ADM,-c(X))
  names(PRED_ADM)
  head(PRED_ADM)
  tail(PRED_ATT)
  # Re-name first variable prior to merging ADM with ATT
  # names(data)[3]<-"new_name"
  names(PRED_ADM)[1]<-"MValue"
  head(PRED_ADM)
  # JAM TOGETHER BOTH DATASETS
  PRED_ALL <-rbind.data.frame(PRED_ATT,PRED_ADM)
  dim(PRED_ALL)
  head(PRED_ALL)
  #######################################################
  write.csv(PRED_ALL,file = paste0("PRED_ALL",".csv"))
  #######################################################
  # Save workspace
  save.image("0102 PRED MODELS output.RData")
  
  # set WORKING DIRECTORY
  setwd("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models output")
  getwd()
  
  
  # 1 Import just TRUSTCODE and TRUSTNAME from main dataset
  #
  ATT.ADM.INPUT.R.MODELS <- read.csv("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/RMODELINPUT
FIVET.csv")
  View(ATT.ADM.INPUT.R.MODELS)
  head(ATT.ADM.INPUT.R.MODELS)
  names(ATT.ADM.INPUT.R.MODELS)[2] <- "Name"
  # Subset variables
  myvars <- c("TRUSTCODE", "Name")
  TNAMES <- ATT.ADM.INPUT.R.MODELS[myvars]
  names(TNAMES)
  TNAMES2 <- TNAMES
  head(TNAMES2)
  # Format TRUSTNAME to macth input dataset
  TNAMES2$Name<-gsub("[[:punct:]]", "", TNAMES2$Name)
  TNAMES2$Name<-gsub(" ","_",TNAMES2$Name)
  TNAMES2$Name<-gsub("'","",TNAMES2$Name)
  TNAMES2
  # Select distinct TRUSTCODE VALUES
  TNAMES3 <- TNAMES2 %>% distinct(TRUSTCODE, .keep_all = TRUE)
  str(TNAMES3)
  
  
  # 2 Import MAPE files
  #
  # IMPORT MAPE Attendances
  MAPEByModelNHSI_ATT <- read.csv("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/MAPEByModelNHSI_ATT.csv")
  names(MAPEByModelNHSI_ATT)
  names(MAPEByModelNHSI_ATT)[1] <- "Name"
  str(MAPEByModelNHSI_ATT)
  # IMPORT MAPE Admissions
  MAPEByModelNHSI_ADM <- read.csv("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/MAPEByModelNHSI_ADM.csv")
  names(MAPEByModelNHSI_ADM)
  names(MAPEByModelNHSI_ADM)[1] <- "Name"
  str(MAPEByModelNHSI_ADM)
  
  
  # 3 MERGE MAPE FILE TO INCLUDE TRUSTCODE IN IT.
  names(TNAMES3)
  names(MAPEByModelNHSI_ATT)
  names(MAPEByModelNHSI_ADM)
  MAPE_ATT <- merge(MAPEByModelNHSI_ATT,TNAMES3,by = "Name")
  MAPE_ADM <- merge(MAPEByModelNHSI_ADM,TNAMES3,by = "Name")
  # 4 OUTPUT MAPE VALUES WITH TRUST CODES INCLUDED
  # MAPE_ATT_TCODE.csv
  # MAPE_ADM_TCODE.csv
  #
  # \\irnhsft.local\monitor\DataAssets\AlteryxObjects\AE Prediction\InBound
  #write.csv(MAPE_ATT,file = paste0("MAPE_ATT_TCODE",".csv"))
  #write.csv(MAPE_ADM,file = paste0("MAPE_ADM_TCODE",".csv"))
  #######################################################
  #write.csv(MAPE_ATT,file = paste0("MAPE_ATT_TCODE",".csv"))
  write.csv(MAPE_ADM,file = paste0("MAPE_ADM_TCODE",".csv"))
  # output to DATA ASSETS ALTERYX OBJECTS
  write.csv(MAPE_ATT, '//irnhsft.local/monitor/DataAssets/AlteryxObjects/AE Prediction/InBound/MAPE_ATT_TCODE.csv', row.names=T)
  write.csv(MAPE_ADM, '//irnhsft.local/monitor/DataAssets/AlteryxObjects/AE Prediction/InBound/MAPE_ADM_TCODE.csv', row.names=T)
  #######################################################
  ## 5 Include TRUSTCODE IN PREDICTED VALUES
PRED_ALL <- read.csv("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound/R models
output/PRED_ALL.csv")
View(PRED_ALL)

head(PRED_ALL)

PRED_ALL <- select (PRED_ALL,-c(X))
head(PRED_ALL)
# Merge it with trust code to output final file

PRED_ALL_TCODE <- merge(PRED_ALL,TNAMES3,by = "Name")

head(PRED_ALL_TCODE)
  
  # 6 OUTPUT PREDICTED VALUES CONTAINING TRUST CODE
  # PRED_ALL_TCODE.csv
  # \\irnhsft.local\monitor\DataAssets\AlteryxObjects\AE Prediction\InBound
  #
  #######################################################
  
  write.csv(PRED_ALL_TCODE,file = paste0("PRED_ALL_TCODE",".csv"))
  
  # output to DATA ASSETS ALTERYX OBJECTS
  write.csv(PRED_ALL_TCODE, '//irnhsft.local/monitor/DataAssets/AlteryxObjects/AE Prediction/InBound/PRED_ALL_TCODE.csv',
            row.names=T)
