# Initial model to build final model from this script

# MODEL RE-RUN 08 APRIL 2019 
# Current model folder 
# Original model boilerplate start working on it

# Start time
start_time <- Sys.time()

# Install required packages
install.packages("readxl",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("partykit",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("data.table",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("tidyverse",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("forecast",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("janitor",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("forecastHybrid",repos="https://cloud.r-project.org",dependencies=TRUE)
install.packages("pracma",repos="https://cloud.r-project.org",dependencies=TRUE)

# Load required packages
library(readxl)
library(caret)
library(partykit)
library(data.table)
library(xts)
library(tidyverse)
library(forecast)
library(janitor)
library(forecastHybrid)
library(pracma)
library(readr)

# Check installed pacakges
Mypath<- .libPaths()
(.packages())

# 01 FORMAT INPUT DATASET FROM GET DATA WORKFLOW

# SET WD
setwd("//irnhsft.local/monitor/DataRequests/National AE Dashboard/Predictive/OutBound")
getwd()

# Script to be run programatically from OutBound folder: 
# Input and output files will be both saved in  OUTBOUND folder 
# PATH:  \\irnhsft.local\monitor\DataRequests\National AE Dashboard\Predictive\OutBound


# Check input file is produced and available from that directory
list.files(pattern = "^A&EPred_Dataset.csv", full.names = TRUE, ignore.case = TRUE)

# STEP 00Load and install required packages
# read_csv() function is part of the TIDYVERSE package
# install.packages("tidyverse",repos="https://cloud.r-project.org",dependencies=TRUE)
# library(tidyverse)

# Check installed packages 
(.packages())

# STEP 1: Ensure AEAttendances and Admissions are imporeted as numeric variables 
# FIle: A&EPred_Dataset.csv

TEST <- read_csv("A&EPred_Dataset.csv")
names(TEST)

# We are just interesded on these set  6 variables coming from "A&EPred_Dataset.csv" file
# 1	OrgCode                   
# 2	Orgname
# 3	AttendancesDay0
# 4	EmergencyAdmissionsA&EDay0
# 5	Temperature_4_CAT
# 6	PERIOD


Prep_A  <- read_csv("A&EPred_Dataset.csv",
                    col_types = cols(
                      AttendancesDay0 = col_double(),
                      `EmergencyAdmissionsA&EDay0` = col_double()
                    )
)

str(Prep_A)

# Now we can subset the data (PIOR TO PRODUCE OUR IMPUT DATASET AS .csv)

Prep_B <- select(Prep_A,
                 OrgCode,
                 Orgname,
                 AttendancesDay0,
                 `EmergencyAdmissionsA&EDay0`,
                 Temperature_4_CAT,
                 PERIOD)

names(Prep_B)

# 01_02 Rename variables 
Prep_C <- select(Prep_B, 
                 TRUSTCODE = OrgCode,
                 TRUSTNAME = Orgname,
                 ATTENDANCES_Type_1 = AttendancesDay0,
                 ADMISSIONS_Type_1 = `EmergencyAdmissionsA&EDay0`,
                 TEMPERATURE = Temperature_4_CAT,
                 DATE = PERIOD)

str(Prep_C)

# 02_02 subset and produce final output 
PILOT_T <- Prep_C %>%
  filter(TRUSTCODE == "RAS"|TRUSTCODE == "RC9")

names(PILOT_T)

# READ IN TODAYS DATASET
# 01 -02 this code will run model for ATTENDANCES 

#  Export final dataset as CSV. 
write.csv(PILOT_T, file = "AEPRED_NEWM.csv",row.names=TRUE)


# setup main dataset
A_E <- read_csv("AEPRED_NEWM.csv", 
                col_types = cols(
                  X1 = col_skip(),
                  ADMISSIONS_Type_1 = col_skip(), 
                  TEMPERATURE = col_skip(),
                  TRUSTCODE = col_skip(),
                  DATE = col_date(format ="%Y-%m-%d")
                  
                )
)
str(A_E)
head(A_E)
names(A_E)

# Rename variables using sensible names
names(TEST)[1]<-"TRUSTNAME"  
names(TEST)[2]<-"ATTENDANCES_Type_1"
names(TEST)[3]<-"DATE"



# Save workspace
# save.image("01 load input dataset_ATT.RData")

#
# colnames(A_E)<-A_E[1,]
# A_E<-A_E[(2:nrow(A_E)),]
# setnames(A_E,"NA","date")
setnames(A_E,"TRUSTNAME", "Name")
setnames(A_E, "DATE", "date")
setnames(A_E, "ATTENDANCES_Type_1", "Attendances")

head(A_E)
tail(A_E)

# Save workspace
# save.image("01 MODEL COMP load dataset_ATT")

#A_E$date <- excel_numeric_to_date(A_E$date)
A_E$Attendances<-as.numeric(A_E$Attendances)

# A_E$temp <- paste0(A_E$date,A_E$Name)
# 
# 
# sums <- A_E %>%
#   group_by(temp) %>%
#   dplyr::summarise(sum = sum(`Total attendances`))
# 
# sums$date <- substr(sums$temp, 1, 10)
# sums$Name <- substr(sums$temp, 11, nchar(sums$temp))
# sums$temp <- NULL
# 
# A_E<- sums
# 

#setnames(A_E,"Total attendances","Attendances")
oldNames<- unique(A_E$Name)
A_E$Name<-gsub("[[:punct:]]", "", A_E$Name)
A_E$Name<-gsub(" ","_",A_E$Name)
A_E$Name<-gsub("'","",A_E$Name)

A_E$Attendances<-ifelse(A_E$Attendances == 0, NA, A_E$Attendances)


# foo <- with(A_E,by(Attendances,Name,function(xx)sum(!is.na(xx))))
# A_E<-A_E[A_E$Name %in% names(which(foo>=16)),]
# 
# A_E$Zeros <- ifelse(A_E$Attendances <5,1,0)
# listZeros<-A_E$Name[A_E$Zeros==1]
# '%!in%' <- function(x,y)!('%in%'(x,y))
# A_E<-A_E[A_E$Name %!in% listZeros,]

# Obtain list of sites
sites <- unique(A_E$Name)
numSites <- length(sites)

# Order the dataset
keycol <-c("Name","date")
setorderv(A_E, keycol)

# Sub missing values by 
for (i in 1:nrow(A_E)) {
  
  if(is.na(A_E$Attendances[i]) & (strcmp(A_E[i,]$Name,A_E[i-7,]$Name))){
    
    A_E$Attendances[i] <- A_E$Attendances[i-7] 
    
  }
  
}

#Clean NAŽs
A_E<-A_E[complete.cases(A_E), ]

# Save workspace
# save.image("03 Clean Attendances data_compl_cases_ATT.RData")

#Define variable
# Forecast scenario 31 days
daysAhead<-31
results <- list()

for(i in 1:numSites){
  
  ## Generate TimeSeries and Test data
  assign(paste0("ts",sites[i]), A_E[A_E$Name %in% sites[i],2])
  assign(paste0("test",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[((nrow(",paste0("ts",sites[i]),")-",daysAhead,"):nrow(",paste0("ts",sites[i]),")),]"))),frequency=7))
  assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[(1:(nrow(",paste0("ts",sites[i]),")-",daysAhead,")),]"))),frequency=7))
  assign(paste0("tss2",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=7))
  
  #assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=7))
  ## We run five models per Trust
  
  #ARIMA
  assign(paste0("autoarima",sites[i]), auto.arima(eval(parse(text = paste0("tss",sites[i])))))
  assign(paste0("predArima",sites[i]), forecast(eval(parse(text = paste0("autoarima",sites[i]))), h = daysAhead))
  assign(paste0("autoarima2",sites[i]), auto.arima(eval(parse(text = paste0("tss2",sites[i])))))
  assign(paste0("pred2Arima",sites[i]), forecast(eval(parse(text = paste0("autoarima2",sites[i]))), h = daysAhead))
  #plot(eval(parse(text = paste0("predArima",sites[i]))), lty = c(1,3), col=c(5,2))
  
  #TBATS
  assign(paste0("tbats",sites[i]), tbats(eval(parse(text = paste0("tss",sites[i])))))
  assign(paste0("predtbats",sites[i]), forecast(eval(parse(text = paste0("tbats",sites[i]))), h = daysAhead))
  assign(paste0("tbats2",sites[i]), tbats(eval(parse(text = paste0("tss2",sites[i])))))
  assign(paste0("pred2tbats",sites[i]), forecast(eval(parse(text = paste0("tbats2",sites[i]))), h = daysAhead))
  #plot(eval(parse(text = paste0("predtbats",sites[i]))), lty = c(1,3), col=c(8,3))
  
  #ETS
  assign(paste0("ets",sites[i]), ets(eval(parse(text = paste0("tss",sites[i]))), model="MAM")) #Holt-Winters
  assign(paste0("predets",sites[i]), forecast(eval(parse(text = paste0("ets",sites[i]))),h=daysAhead,method ='ets'))
  assign(paste0("ets2",sites[i]), ets(eval(parse(text = paste0("tss2",sites[i]))), model="MAM")) #Holt-Winters
  assign(paste0("pred2ets",sites[i]), forecast(eval(parse(text = paste0("ets2",sites[i]))),h=daysAhead,method ='ets'))
  
  #plot(eval(parse(text = paste0("predets",sites[i]))), lty = c(1,3), col=c(3,6))
  
  #Ensemble
  assign(paste0("predEnsemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(predArima",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(predtbats",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(predets",sites[i],")")))))))
  assign(paste0("pred2Ensemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(pred2Arima",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(pred2tbats",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(pred2ets",sites[i],")")))))))
  
  # Hybrid
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
  assign(paste0("MAPE",sites[i],"Ensemble") ,data.table(colMeans(abs(eval(parse(text = paste0("data.frame(test",sites[i],")")))-(eval(parse(text = paste0("data.frame(predEnsemble",sites[i],")$Point.Forecast")))))/(eval(parse(text = paste0("data.frame(test",sites[i],")")))))*100))
  
} 

# Matrix to be populated  

a<-data.frame(matrix(NA, nrow = numSites, ncol = 5))


for(i in 1:numSites){
  
  for(j in 1:5){
    
    ## Generate the csv's
    
    a[i,j]<-min(eval(parse(text=data.table(ls(pattern=paste0("^MAPE",sites[i])))[j,]$V1)))
    
    names(a)<-c("Arima", "Ensemble", "ets","hybrid","tbats")         
    
    rownames(a)<-sites
    
    names(min(a[1,]))
    
    colnames(a)[apply(a[j,],1,which.min)]
    
  }
  
}

write.csv(a, file="ATT MAPEByModelNHSI.csv")

# Run from here to line 228 to produce final dataset

for(i in 1:numSites){
  
  if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
    
    jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
    
    graphics::plot(data.frame(cbind(data.frame(rownames(data.frame(rbind.data.frame(eval(parse(text = paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i],"$Point.Forecast")))))), rbind.data.frame(eval(parse(text = paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i],"$Point.Forecast"))))))), lty = c(1,3), col=c(5,2))
    
    #polygon(c(rev(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))), eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))), c(rev(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i])))[ ,2]), eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))[ ,3]), col = 'grey80', border = NA)
    
    dev.off()
    
  }else{
    
    jpeg(paste0(sites[i],"pred", colnames(a)[apply(a[i,],1,which.min)],".jpg"), width = 350, height = 350)
    
    plot(eval(parse(text = paste0("pred", colnames(a)[apply(a[i,],1,which.min)],sites[i]))), lty = c(1,3), col=c(5,2))
    
    dev.off()
    
  }
  
  if(colnames(a)[apply(a[i,],1,which.min)] %in% "Ensemble"){
    
    assign(paste0("dtpred",sites[i]),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)[apply(a[i,],1,which.min)],sites[i],")")))),data.frame(sites[i])))
    
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)[apply(a[i,],1,which.min)]"))))
    
    assign(paste0("dtpred",sites[i]),eval(parse(text = paste0("dtpred",sites[i],"[,c(1,2,4,3,5,6,7,8)]"))))
    
  } else {
    
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)[apply(a[i,],1,which.min)],sites[i],"$mean)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)[apply(a[i,],1,which.min)],sites[i],"$lower)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2",colnames(a)[apply(a[i,],1,which.min)],sites[i],"$upper)"))))),data.frame(sites[i])))
    
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),eval(parse(text="colnames(a)[apply(a[i,],1,which.min)]"))))
    
    #assign(eval(parse(text=paste0("names(dtpred",sites[i],"[1])"))),substr(1,4,eval(parse(text=paste0("names(dtpred",sites[i],"[1])")))))
    
  }
  
  #assign(colnames(eval(parse(text=paste0("dtpred",sites[i])))),c("mean","high80","high95","site","low80","low95","forecasted"))
  
  assign(paste0("dthist",sites[i]),cbind(data.frame(eval(parse(text=paste0("ts",sites[i])))),data.frame(sites[i])))
  
  #assign(paste0("dthist",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dthist",sites[i]))))),0))
  
}


# Next loop:  Creates HISTORIC WITH NA and for the optimal one fills in dates 
# dataset with forecasted values and CI 


for(i in 1:numSites){
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), d = NA, .after = 2))
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), e = NA, .after = 3))
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), f = NA, .after = 4))
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), g = NA, .after = 5))
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), h = 0, .after = 6))
  
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), i = NA, .after = 7))
  
  assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), date = as.character(seq.Date(from = as.Date(max(A_E$date)) +1, to= as.Date(max(A_E$date)) +(daysAhead) , by=1)), .after = daysAhead))
  
  
  
}


# Assign column names to previous output

for(i in 1:numSites){
  
  assign(paste0("dtpred",sites[i]),`names<-`(get(paste0("dtpred",sites[i])), c("Attendances","low80","low95","high80","high95","Name","forecasted","model", "date")))
  assign(paste0("dthist",sites[i]),`names<-`(get(paste0("dthist",sites[i])), c("Attendances","Name","high95","high80","low80","low95","forecasted","model")))
  assign(paste0("dthist",sites[i]),left_join(get(paste0("dthist",sites[i])),A_E,copy=FALSE))
  assign(paste0("dthist",sites[i]), eval(parse(text=paste0("dthist",sites[i],"[,-10]"))))
  
}

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



write.csv(final, file = "A&E-PredictionNHSI_ATT.csv")



#rm(list=ls())

# Save workspace containing final dataset 
save.image("01 NEW MODEL ATTENDANCES.RData")


#### ACCURACY MEASURES ATTENDANCES 

# sites <- unique(A_E$Name)
# numSites <- length(sites)
# Each accuracy measure has 7 different ACCURACY indicators
# ncols must be 9 ( 7 Accuracy measures + Trusts (1) + CTrusts (1) )
# Dynamic objects
TCodeL <- c("RC9","RC9","RC9","RC9")
TNameL <- c("LUTON","LUTON","LUTON","LUTON")
ModelsL <- c("ARIMA","ETS","HYBRID","TBATS")

# Emat <- matrix(nrow = 5, ncol = 2)
TMAT <- matrix(nrow=4,ncol=4)
for (i in 1:4)
{
  Atrusts <- paste(TCodeL[i])
  Btrusts <- paste(TNameL[i])
  CModels <- paste(ModelsL[i])
  Date
  
  TMAT[i,] <- cbind(Atrusts,Btrusts,CModels,Date)
  
  # This works   TMAT[i,] <- cbind(Ctrusts,Ntrusts,Date,Models)
  #   Emat[i,]<-cbind(Ctrusts,Ntrusts,Date,Models)
}
TMAT
# Turn it into a data.frame and rename columns
TMAT_data_frame <-  rbind.data.frame(TMAT)

colnames(TMAT_data_frame)<-c("TRUST_CODE","TRUST_NAME","MODEL","DATE")
TMAT_data_frame

# Indluce different accuracy measures model in the above code
Date <- format(Sys.Date(),"%d %B %Y")


# Dynamic objects
TCodeL <- c("RC9","RC9","RC9","RC9")
TNameL <- c("LUTON","LUTON","LUTON","LUTON")
ModelsL <- c("ARIMA","ETS","HYBRID","TBATS")

# TMAT2 <- matrix(nrow=5, ncol=2)
TMAT2 <- matrix(nrow=4,ncol=4)
for (i in 1:4)
{
  Atrusts <- paste(TCodeL[i])
  Btrusts <- paste(TNameL[i])
  CModels <- paste(ModelsL[i])
  Date
  
  TMAT2[i,] <- cbind(Atrusts,Btrusts,CModels,Date)
  
  # This works   TMAT[i,] <- cbind(Ctrusts,Ntrusts,Date,Models)
  #   Emat[i,]<-cbind(Ctrusts,Ntrusts,Date,Models)
}

TMAT2

# Include them hardcoding them
# TO ADD THE DIFFERENT ACCURACY MEASURES, ALWAYS USE rbind !!

ACM_CB <- rbind(
  AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustArima,
  AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustETS,
  AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustHybrid,
  AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustTBATS)
ACM_CB

FINAL_CBIND <- cbind(TMAT2,ACM_CB)
FINAL_CBIND

# Rename columns accordingly:
colnames(FINAL_CBIND)<-c("Trust Code","Trust Name","Model","Refresh Date",
                         "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
FINAL_CBIND
# Accuracy measures for LUTON
#                "AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustArima",
#                "AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustETS",
#                "AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustHybrid",
#                "AccuracyLuton_and_Dunstable_University_Hospital_NHS_Foundation_TrustTBATS")
# FINAL OUTPUT  MODEL ACCURACY FOR LUTON
# write.table(as.data.frame(FINAL_CBIND),"LUTON MODELS ACCURACY.txt",sep="\t")
# write.csv(FINAL_CBIND,file="LUTON MODELS ACCURACY 25MAR2019.csv")
# Include time stamp on it
# Date as. POSIXct command
now <- format(Sys.time()-4,"%d %B %Y %H %M")
now

# write.csv(FINAL_CBIND,file = paste0("LUTON MODELS ACCURACY   ",  now,".csv"))

### HILLINGDON

# Dynamic objects
TCodeLH <- c("RAS","RAS","RAS","RAS")
TNameLH <- c("HILLINGDON","HILLINGDON","HILLINGDON","HILLINGDON")
ModelsLH <- c("ARIMA","ETS","HYBRID","TBATS")

TMAT3 <- matrix(nrow=4,ncol=4)

for (i in 1:4)
{
  AtrustsH <- paste(TCodeLH[i])
  BtrustsH <- paste(TNameLH[i])
  CModelsH <- paste(ModelsLH[i])
  Date
  
  TMAT3[i,] <- cbind(AtrustsH,BtrustsH,CModelsH,Date)
  
  # This works   TMAT[i,] <- cbind(Ctrusts,Ntrusts,Date,Models)
  #   Emat[i,]<-cbind(Ctrusts,Ntrusts,Date,Models)
}
TMAT3

# Include them hardcoding them
# TO ADD THE DIFFERENT ACCURACY MEASURES, ALWAYS USE rbind !!
ACM_CBH <- rbind(
  AccuracyThe_Hillingdon_Hospitals_NHS_Foundation_TrustArima,
  AccuracyThe_Hillingdon_Hospitals_NHS_Foundation_TrustETS,
  AccuracyThe_Hillingdon_Hospitals_NHS_Foundation_TrustHybrid,
  AccuracyThe_Hillingdon_Hospitals_NHS_Foundation_TrustTBATS)

ACM_CBH
FINAL_CBINDH <- cbind(TMAT3,ACM_CBH)
FINAL_CBINDH

# Rename columns accordingly:
colnames(FINAL_CBINDH)<-c("Trust Code","Trust Name","Model","Refresh Date",
                          "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
FINAL_CBINDH
# FINAL OUTPUT  MODEL ACCURACY FOR HILLINGDON
now <- format(Sys.time(),"%d %B %Y %H %M")
now

# write.csv(FINAL_CBINDH,file = paste0("HILLINGDON MODELS ACCURACY   ",  now,".csv"))

# APPEND ACCURACY MEASURES OUTPUT INTO SINGLE FILE
# RC9 Trust 01 LUTON       [ FINAL_CBIND  ]
# RAS Trust 02 HILLINGDON  [ FINAL_CBINDH ]
# FLAG BEST PERFORMING MODEL WITH LOWEEST RMSE FOR EACH TRUST
# Delete vars: dataframe$VAR <- NULL
FINAL_CBIND$MIN_RMSE <- ifelse(FINAL_CBIND$RMSE == min(FINAL_CBIND$RMSE),1,0)
FINAL_CBINDH$MIN_RMSE <-ifelse(FINAL_CBINDH$RMSE == min(FINAL_CBINDH$RMSE),1,0)

ACC_RC9_RAS <-rbind(FINAL_CBIND,FINAL_CBINDH)
ACC_RC9_RAS

#  Turn the above code into a matrix
# We will need to loop through the number of sites
# 1 Setup the matrix to be populated
# ROWS:  Rows defined b the number of models multiplid by sites
# COLS: Defined by  [ The following set of cols we want to populate ]
# Setup number of cols by total number of parameters to be displaeyd on the final output
# ( )
# c("Trust Code","Trust Name","Model","Refresh Date", "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
# ncol = 11
colnamesFINAL_CBINDH<-c("Trust Code","Trust Name","Model","Refresh Date",
                        "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")

length(colnamesFINAL_CBINDH)
# Number of rows would be equal to TRUSTS BY MODELS
N_models <-unique(FINAL_CBIND$Model)
N_trusts <-unique(A_E$Name)
NRows <-length(N_models)*length(N_trusts)
# Number of corls is equal to length)(colnanesFINAL_CBIND)
NCols <-length(colnamesFINAL_CBINDH)

# Setup matrix to populate it with values
ACCM <-data.frame(matrix(NA, nrow = NRows, ncol = NCols))
ACCM
# NRows
NR <-length(N_models)*length(N_trusts)
NR
# Initialize 'usq'
usq <-0
A <-c(1,2,3,4,5,6,7,8)
A
length(HMR)

colnamesFINAL_CBINDH<-c("Trust Code","Trust Name","Model","Refresh Date",
                        "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")

NC <-length(colnamesFINAL_CBINDH)
NC
# Setup matrix to populate it with values
ACCM <-data.frame(matrix(NA, nrow = NRows, ncol = NCols))
ACCM

dim(ACCM)
# Matrix FINAL_CBINDH
# NR (value is 8 letter i )
# NC (value is 11 letter j )
# NR (letter i )
# NC (letter j)
usq <-0
testing <-0

# NR rows 8
# NC cols 11 [ fixed ]
# LOOP TO POPULATE THROUGH THE CODE TO OUTPUT FINAL TABLE
for (i in 1:NR) {
  for (j in 1:NC){
    ACCM[i,] <- ACC_RC9_RAS[i,]
  }
}
# This is how the code should work
ACCM <-data.frame(matrix(NA, nrow = NRows, ncol = NCols))
ACCM

# Include rownames into the code  
colnames(ACCM) <- c ("Trust Code","Trust Name","Model","Refresh Date",
                     "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
ACCM
# LOOP TO POPULATE THROUGH THE CODE TO OUTPUT FINAL TABLE
# Include features into the new matrix ( rename columns )
ACCMF<-data.frame(matrix(NA, nrow = NRows, ncol = NCols))
ACCMF

for (i in 1:NR) {
  for (j in 1:NC){
    ACCMF[i,] <- ACC_RC9_RAS[i,]
    colnames(ACCMF) <- c ("Trust Code","Trust Name","Model","Refresh Date",
                          "ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
  }
}
# Check output from previous matrix 
ACCMF
# Output final table as csv file:
write.csv(ACCMF,file = paste0("ACCURACY MEASURES ATTENDANCES",".csv"))

# Calculates and displays total model running time
end_time <- Sys.time()
end_time - start_time

# Save workspace containing final dataset  
save.image("02 ACCURACY ATTENDANCES.RData")


#  PLR NOTE: 10/04/2019
# Clear Environment prior to  running Admissions Predictive Model 
rm(list=ls())