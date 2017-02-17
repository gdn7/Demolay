
#################################Libraries#######################################
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
library(mtcars)
library(rattle)
library(boot)
####################################settingupDirectory###################################

getwd()
setwd("C:\\Users\\DELL\\Desktop\\R and python\\Project")
data_grouped=read.table("trasformed_data_grouped.txt",header=TRUE)
View(data_grouped)
write.table(data_grouped,"data.txt",sep=',')
########################################Descriptive Analysis#####################

desc=describe(data_grouped)
sum=summary(data_grouped)
colnames(data_grouped)
library(gclus)
dta <- (data_grouped[1:5]) # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

par(mfrow=c(3,3))
hist(data_grouped$MEMBERSHIP,main="MEMBERSHIP")
boxplot(data_grouped$MEMBERSHIP,main="MEMBERSHIP")
qqnorm(data_grouped$MEMBERSHIP,main="MEMBERSHIP")

hist(data_grouped$EDUCATION,main="EDUCATION")
boxplot(data_grouped$MEMBERSHIP,main="EDUCATION")
qqnorm(data_grouped$MEMBERSHIP,main="EDUCATION")

hist(data_grouped$FUN,main="FUN")
boxplot(data_grouped$MEMBERSHIP,main="FUN")
qqnorm(data_grouped$MEMBERSHIP,main="FUN")

hist(data_grouped$COMSERV,main="COMSERV")
boxplot(data_grouped$MEMBERSHIP,main="COMSERV")
qqnorm(data_grouped$MEMBERSHIP,main="COMSERV")


hist(data_grouped$AWARDS,main="AWARDS")
boxplot(data_grouped$MEMBERSHIP,main="AWARDS")
qqnorm(data_grouped$MEMBERSHIP,main="AWARDS")

hist(data_grouped$RIT,main ="RIT")
boxplot(data_grouped$MEMBERSHIP,main="RIT")
qqnorm(data_grouped$MEMBERSHIP,main="RIT")

hist(data_grouped$TRAVEL,main="TRAVEL")
boxplot(data_grouped$MEMBERSHIP,main="TRAVEL")
qqnorm(data_grouped$MEMBERSHIP,main="TRAVEL")

hist(data_grouped$Members_Per_Event,main="Members_Event")
boxplot(data_grouped$MEMBERSHIP,main="Members_Event")
qqnorm(data_grouped$MEMBERSHIP,main="Members_Event")
#######################################after trasformation #########################################
hist(log_data$MEMBERSHIP,main="MEMBERSHIP",xlab = "log(x+3/8)")
hist((log_data$EDUCATION)*0.61,main="EDUCATION",xlab = "log(x+3/8)")
hist(log_data$FUN,main="FUN",xlab = "log(x+3/8)")
hist(log_data$COMSERV,main="COMSERV",xlab = "log(x+3/8)")
hist(log_data$AWARDS,main="AWARDS",xlab = "log(x+3/8)")
hist((log_data$RIT)*0.404,main="RIT",xlab = "log(x+3/8)")
hist(log_data$TRAVEL,main="TRAVEL",xlab = "log(x+3/8)")
######################################Time serries data##############################

data_grouped=read.table("trasformed_data_grouped.txt",header=TRUE)
View(data_grouped)

target1=data_grouped$Members_Per_Event
target2=data_grouped$Year
y=subset(data_grouped,select=c(Members_Per_Event,TRAVEL,RIT,AWARDS,COMSERV,FUN,EDUCATION,MEMBERSHIP))
data_grouped_ts=ts(data_grouped,start=c(2012,1),frequency=12)

data_analysis=subset(data_grouped,select=-c(Year,Month,Members_Per_Event))
log_data=log(data_analysis+3/8)
View(log_data)
data_analysis_ts=ts(log_data,start=c(2012,1),frequency=12)
######################################boot strapping#######################
#                 data_grouped1=subset(data_grouped,select=-c(Month,Year))        
#                 colnames(data_grouped1)
#
#                 log_data_grouped=function(MEMBERSHIP,EDUCATION)
#                           {
#                          data1=log(data_grouped1$MEMBERSHIP,data_grouped1$EDUCATION+1)
#                           }         
#
#                 boot_data=boot(data_grouped1,log_data_grouped,R=1000,stype='w')
#                 boot.array()
#############################################################################

data_analysis=log(data_analysis+1)
data_analysis_ts_TRAVEL=(data_analysis_ts[,"TRAVEL"])
data_analysis_ts_RIT=(data_analysis_ts[,"RIT"])
data_analysis_ts_AWARDS=(data_analysis_ts[,"AWARDS"])
data_analysis_ts_COMSERV=(data_analysis_ts[,"COMSERV"])
data_analysis_ts_FUN=(data_analysis_ts[,"FUN"])
data_analysis_ts_EDUCATION=(data_analysis_ts[,"EDUCATION"])
data_analysis_ts_MEMBERSHIP=(data_analysis_ts[,"MEMBERSHIP"])

####################################Creating a matrix##############################################
analysis_matrix=data.matrix(log_data, rownames.force = NA)
colnames(analysis_matrix)
#######################################Graphics###############################################
par(bg="gray")
plot(data_analysis_ts)
title(main="DeMolay Organiation")
theme(legend.title=element_blank())


png("C:\\Users\\DELL\\Desktop\\R and python\\Project\\Plots\\DeMolay_time_series.png")


######################################################################################
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

EDUCATION=diff(log_data$EDUCATION)
acf2(EDUCATION)
plot(EDUCATION)
auto.arima(EDUCATION)

MEMBERSHIP=diff(log_data$MEMBERSHIP)
acf2(MEMBERSHIP)
plot(MEMBERSHIP)
auto.arima(MEMBERSHIP)
#####################################Augumented Dickey Fuller Test#################

adf.test(log_data$MEMBERSHIP,alternative = "stationary")
adf.test(log_data$EDUCATION,alternative = "stationary")
adf.test(log_data$FUN,alternative = "stationary")
adf.test(log_data$COMSERV,alternative = "stationary")
adf.test(log_data$AWARDS,alternative = "stationary")
adf.test(log_data$RIT,alternative = "stationary")
adf.test(log_data$TRAVEL,alternative = "stationary")


########################################################################################

                 #MODEL BUILDING -VECTOR AUTO REGRESSION#

#######################################################################################
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
plothist(var_result)
rootshist(var_result)
argshist(restrict)
var_restrict_ser=restricthist(var_result,method="ser",thresh=2)
var_restrict_ser$restrictions

var_restrict_man=restricthist(var_result,method="manual",thresh=2)
var_restrict_man$restrictions
########################################################################################

