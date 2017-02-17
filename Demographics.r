library(psych)
library(car) 
library(dummies)
library(tree)
library(Hmisc)

workingdirectory = "C:\\Users\\Jaymie\\Dropbox\\MSIS5223\\Project\\Data\\Demographics"
setwd(workingdirectory)
demo_data = read.table("C:\\Users\\Jaymie\\Dropbox\\MSIS5223\\Project\\Data\\Demographics\\Full Demographics 10_1_2016 va.csv", header=T, sep=",")
active_data = read.table("C:\\Users\\Jaymie\\Dropbox\\MSIS5223\\Project\\Data\\Demographics\\Active Demographics 10_1_2016 va.csv", header=T, sep=",")

demo_data = na.omit(demo_data)
active_data = na.omit(active_data)

demo_data$cen_Tract_GeoID <- categorical(demo_data$cen_Tract_GeoID)
active_data$cen_Tract_GeoID <- categorical(active_data$cen_Tract_GeoID)

demo_tree = tree(Tenure_Yrs~cen_Tract_GeoID, demo_data, mindev=1e-6, minsize=2)

plot(demo_tree)
text(demo_tree)

active_tree = tree(Tenure_Yrs~cen_Tract_GeoID, active_data, mindev=1e-6, minsize=2)

plot(active_tree)
text(active_tree)

tract_dummy1 = dummy(demo_data$cen_Tract_GeoID, sep = '_')
colnames(tract_dummy1)
tract_dummy1 = as.data.frame(tract_dummy1)
demo_data = data.frame(demo_data, tract_dummy1)

demo_reg1 = lm(demo_data$Tenure_Yrs ~ demo_data$cen_Tract_GeoID_6099003907 + demo_data$cen_Tract_GeoID_40027202206  + demo_data$cen_Tract_GeoID_40119010101 + demo_data$cen_Tract_GeoID_40119010102 + demo_data$cen_Tract_GeoID_40119010200 + demo_data$cen_Tract_GeoID_40119010300 + demo_data$cen_Tract_GeoID_40119010500 + demo_data$cen_Tract_GeoID_40119010600 + demo_data$cen_Tract_GeoID_40119010700 + demo_data$cen_Tract_GeoID_40119010800 + demo_data$cen_Tract_GeoID_40119010900 + demo_data$cen_Tract_GeoID_40119011000 + demo_data$cen_Tract_GeoID_40119011101 + demo_data$cen_Tract_GeoID_40119011102)
summary(demo_reg1)

demo_reg2 = lm(demo_data$Mem_Attend_Pct ~ demo_data$cen_Tract_GeoID_6099003907 + demo_data$cen_Tract_GeoID_40027202206  + demo_data$cen_Tract_GeoID_40119010101 + demo_data$cen_Tract_GeoID_40119010102 + demo_data$cen_Tract_GeoID_40119010200 + demo_data$cen_Tract_GeoID_40119010300 + demo_data$cen_Tract_GeoID_40119010500 + demo_data$cen_Tract_GeoID_40119010600 + demo_data$cen_Tract_GeoID_40119010700 + demo_data$cen_Tract_GeoID_40119010800 + demo_data$cen_Tract_GeoID_40119010900 + demo_data$cen_Tract_GeoID_40119011000 + demo_data$cen_Tract_GeoID_40119011101 + demo_data$cen_Tract_GeoID_40119011102)
summary(demo_reg2)

plot(demo_data$Mem_Attend_Pct, demo_data$Tenure_Yrs)
plot(demo_data$miles_From_Lodge, demo_data$Tenure_Yrs)
plot(demo_data$Age_Yrs, demo_data$Tenure_Yrs)

rcorr(as.matrix(demo_data)

demo_reg3 = lm(demo_data$Tenure_Yrs ~ demo_data$miles_From_Lodge+demo_data$Mem_Attend_Pct+demo_data$Age_Yrs)
summary(demo_reg3)

vif(demo_reg3)
plot(demo_reg3)

active_reg1 = lm(active_data$Tenure_Yrs ~ active_data$miles_From_Lodge+active_data$Mem_Attend_Pct+active_data$Age_Yrs)
summary(active_reg1)