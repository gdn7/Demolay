
#################################Libraries#############################################
library(fpp)
library(Hmisc)
library(dynlm)
library(Matrix)
library(zoo)
library(astsa)
library(forecast)
library(tseries)
library(vars)
library(urca)
library(MASS)
library(geoR)
library(grDevices)
attach(mtcars)
####################################setting up Directory###################################

getwd()
setwd("C:\\Users\\DELL\\Desktop\\R and python\\Project")

######################################Time serries data####################################

data_grouped=read.table("trasformed_data_grouped.txt",header=TRUE)
View(data_grouped)
target1=data_grouped$Members_Per_Event
target2=data_grouped$Year
y=subset(data_grouped,select=c(Members_Per_Event,TRAVEL,RIT,AWARDS,COMSERV,FUN,EDUCATION,MEMBERSHIP))
data_grouped_ts=ts(data_grouped,start=c(2012,1),frequency=12)

data_analysis=subset(data_grouped,select=-c(Year,Month,Members_Per_Event))
log_data=log(data_analysis+1)
data_analysis_ts=ts(log_data,start=c(2012,1),frequency=12)



data_analysis=log(data_analysis+1)
data_analysis_ts_TRAVEL=(data_analysis_ts[,"TRAVEL"])
data_analysis_ts_RIT=(data_analysis_ts[,"RIT"])
data_analysis_ts_AWARDS=(data_analysis_ts[,"AWARDS"])
data_analysis_ts_COMSERV=(data_analysis_ts[,"COMSERV"])
data_analysis_ts_FUN=(data_analysis_ts[,"FUN"])
data_analysis_ts_EDUCATION=(data_analysis_ts[,"EDUCATION"])
data_analysis_ts_MEMBERSHIP=(data_analysis_ts[,"MEMBERSHIP"])

#################################### Creating a matrix ##############################################
analysis_matrix=data.matrix(log_data, rownames.force = NA)

####################################### Graphics ####################################################
par(bg="gray")
plot(data_analysis_ts,main="")
title(main="DeMolay Organiation")
theme(legend.title=element_blank())
png("C:\\Users\\DELL\\Desktop\\R and python\\Project\\Plots\\DeMolay_time_series.png")


############################################ARIMA and VAR #######################################################
TRAVEL=diff(log_data$TRAVEL)
acf2(TRAVEL)
plot(TRAVEL)
auto.arima(TRAVEL)


RIT=diff(log_data$RIT)
acf2(RIT)
plot(RIT)
auto.arima(RIT)

AWARDS=diff(log_data$AWARDS)
acf2(AWARDS)
plot(AWARDS)
auto.arima(AWARDS)

COMSERV=diff(log_data$COMSERV)
acf2(COMSERV)
plot(COMSERV)
auto.arima(COMSERV)

FUN=diff(log_data$FUN)
acf2(FUN)
plot(FUN)
auto.arima(FUN)
par(opar)
par(cex=1.0,cex.main=1.5,bg="white",cex.lab=1.5)

EDUCATION=diff(EDUCATION)
acf2(EDUCATION)
plot(EDUCATION)
auto.arima(EDUCATION)

MEMBERSHIP=diff(MEMBERSHIP)
acf2(MEMBERSHIP)
plot(MEMBERSHIP)
auto.arima(MEMBERSHIP)

VARselect(log_data, lag.max = 20, type = "const" ,exogen=analysis_matrix) 
var_result=VAR(y,type = c("const", "trend", "both", "none"),p=2,exogen =analysis_matrix,ic = c("AIC", "HQ", "SC", "FPE"))

summary(var_result)
plot(var_result)
roots(var_result)
args(restrict)
var_restrict_ser=restrict(var_result,method="ser",thresh=2)
var_restrict_ser$restrictions

var_restrict_man=restrict(var_result,method="manual",thresh=2)
var_restrict_man$restrictions
###############################################################################################################################

auto.arima(Members_Per_Event$data_grouped,D=1,frequency=12,xreg=analysis_matrix)

ccf(data_analysis_ts_TRAVEL,data_analysis_ts_RIT)