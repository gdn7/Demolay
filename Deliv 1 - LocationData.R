## MSIS 5223 Analytics Programming ##
## Fritts, Robert                  ##
## semester project - location data analysis
#############################
## language: R
## date: 2016-10-7
#############################


#--------------#
#- Load stuff -#
#--------------#
install.packages("psych")
install.packages("ggplot2")
#install.packages("Hmisc") ##this overwrites functions from others, don't load unless explicitly needed
install.packages("tree")

library(foreign)
library(ggplot2)
library(psych)
#library(Hmisc) ##this overwrites functions from others, don't load unless explicitly needed
library(tree)



#-----------------#
#- declare stuff -#
#-----------------#
data_dir = "C:\\Users\\Admin\\Dropbox\\Project\\Data\\Demographics"
setwd(data_dir)



#------------#
#- do stuff -#
#------------#

# read in csv
#locdata = read.table("Full Demographics 10_1_2016 v2.csv", header=T, sep=",") ##sep="\t", quote="\"")
# error reading as a table, trying csv option
locdata = read.csv("Full Demographics 10_1_2016 v2a.csv"
			, header=TRUE
			, colClasses=c("ID"="character"
					  ,"Address" = "character"
					  ,"City" = "factor"
					  ,"State" = "factor"
					  ,"Zip" = "factor"
					  ,"cen_rowid" = "character"
					  ,"cen_State_cd" = "factor"
					  ,"cen_County_cd" = "factor"
					  ,"cen_Tract_GeoID" = "factor"
					  ,"cen_Block_GeoID" = "factor"
					  )
			)
#success, check it out
head(locdata,3)
str(locdata)
tail(locdata,3)
#first pass, see attendance as a factor instead of num btwn 0 and 1 as pct... 
#  fixed in source formatting, exported w/o % sign to v2a.csv and reloaded

## add dummy binary col for active_level 1=active, 0=not from 1=act,4=not
locdata$Active.Level.Bin = ifelse(locdata$Active.Level == 1 , 1, 0)
str(locdata) #checking results
locdata$Active.Level.Bin #check results
locdata$Active.Level #check results

## drop rows with null vals for lat/long/tract/block if necessary
##  but put these in modified df locdata2
nrow(locdata)
locdata2 = na.omit(locdata)
nrow(locdata2)
locdata2$cen_Latitude

## drop youngest row of non-unique ID#s (should only be one)
##  but put these in modified df locdata2
locdata2[(locdata2$ID_ct == 2),]
nrow(locdata2)
locdata2 = locdata2[(locdata2$ID_ct == 1) | (locdata2$Age == 16),] #keep ct=1 or age=16 if ct<>1
nrow(locdata2)
locdata2[(locdata2$ID_ct == 2),]

## make analysis frame locdf
locdf = locdata2[,c(24,14,13,12,18,19,22,23)]
head(locdf,3)
str(locdf)

locdf_num = locdf[,sapply(locdf,is.numeric)] ##numeric vars to a specific df
head(locdf_num)
str(locdf_num) ##check new frame
describe(locdf_num) 
summary(locdf_num) 




#### stop execution here, only notes and old code below



#########################################################################
############# old pca/fa/cluster/agglom code below here #################
#########################################################################
##  no need for pca/fa here, but would be interesting 
##  to examine clustering based on categoricals
#########################################################################
############# old pca/fa/cluster/agglom code below here #################
#########################################################################



## caution - scales and descriptive stats are all over the place for these
##         - if these tests are sensitive to scale than must normalize
##         - so far only know that averaging FA results requires normalization
##         - so moving forward without normalization expecting to not avg factors


#PCA & eigenvalues
PCA_newdata_num = princomp(newdata_num,cor=TRUE) #run PCA and store loadings
PCA_newdata_num  #print frame
## PCA analysis - only 2 stddevs >=1, var=stddev^2 and is the eigen vectors
##              - thus only 2 factors due to eigens >=1
plot(PCA_newdata_num, main="ECT_Data Scree Plot") #scree plot
## SCREE plot - per plot, factors level out from 4 on, thus only 3 factors.
##            - disagrees with PCA due to 3rd factor being so close to 1


#FA comparison
# build FA -------- run lines below together --------
fa_newdata_num = factanal(~NoFTE + NetPatRev + InOperExp + OutOperExp 
				  + OperRev + OperInc + AvlBeds + Compensation 
				  + MaxTerm
			,factors=3
			,rotation="varimax"
			,scores="none"
			,data=newdata_num
		     )
# build FA -------- run lines above together --------
fa_newdata_num
edit(newdata_num)
#above resulted in error... explore data set further for nulls or missing stuff
#no nulls found
#per searching around, try with fewer factors
fa_newdata_num = factanal(~NoFTE + NetPatRev + InOperExp + OutOperExp 
				  + OperRev + OperInc + AvlBeds + Compensation 
				  + MaxTerm
			,factors=2
			,rotation="varimax"
			,scores="none"
			,data=newdata_num
		     )
#so that failed... also per searching around, if your correlation matrix 
#is singular, you should specify PRIORS=MAX instead of PRIORS=SMC
fa_newdata_num = factanal(~NoFTE + NetPatRev + InOperExp + OutOperExp 
				  + OperRev + OperInc + AvlBeds + Compensation 
				  + MaxTerm
			,factors=2
			,rotation="varimax"
			,scores="none"
			,data=newdata_num
			,PRIORS=MAX
		     )
#so that failed too...
#some indications that some columns my be duplicates or perfectly correlated
# leading to this error, so checking pairs
pairs(newdata_num)
# pairs grid shows NetPatRev and OperRev to be related linearly and picking
# a few points they seem to have a very close multiplier scaling 
#run FA without NetPatRev
fa_newdata_num_woNPR = factanal(~NoFTE + InOperExp + OutOperExp 
				  + OperRev + OperInc + AvlBeds + Compensation 
				  + MaxTerm
			,factors=2
			,rotation="varimax"
			,scores="none"
			,data=newdata_num
		     )
#try again by removing column from data set to be analyzed not just from the 
#+ terms
head(newdata_num,1)
head(newdata_num[,c(1,3:9)],1)
newdata_num_woNPR = newdata_num[,c(1,3:9)]
str(newdata_num_woNPR)
fa_newdata_num_woNPR = factanal(~NoFTE + InOperExp + OutOperExp 
				  + OperRev + OperInc + AvlBeds 
				  + Compensation + MaxTerm
			,factors=2
			,rotation="varimax"
			,scores="none"
			,data=newdata_num
		     )
#still singular... checking again
pairs(newdata_num_woNPR)

#ahh, missed that OperInc is related to Exp and Rev fields... Inc=rev-exp
#trying again by taking it out
head(newdata_num,1)
head(newdata_num[,c(1,3:5,7:9)],1)
newdata_num_small = newdata_num[,c(1,3:5,7:9)]
str(newdata_num_small)
fa_newdata_num_small = factanal(~NoFTE + InOperExp + OutOperExp 
				  + OperRev + AvlBeds 
				  + Compensation + MaxTerm
			,factors=2
			,rotation="varimax"
			,scores="none"
			,data=newdata_num_small
		     )
## success!!
fa_newdata_num_small
#try w/ 3 factors
fa_newdata_num_small3 = factanal(~NoFTE + InOperExp + OutOperExp 
				  + OperRev + AvlBeds 
				  + Compensation + MaxTerm
			,factors=3
			,rotation="varimax"
			,scores="none"
			,data=newdata_num_small
		     )
## success!!
fa_newdata_num_small3
## decision - only 2 relevant factors
##          - 3rd factor not loaded with anything significant
##          - fac1: nofte,inexp,outexp,operrev,avlbeds
##          - fac2: comp,maxterm
##          - note - this was after manually identifying NetPatRev and OperInc
##                 - as related to others and removing them since they
##                 - prevented the FA from running at all

##########
## done ##

## forgot to export table of relevant variables
newdata_small = newdata_num_small[,]
str(newdata_small)
#add cols w/ categories we want
str(newdata_small[,c(8)])
str(newdata[,c('Teaching','DonorType','TypeControl')])
newdata_small[,c(8:10)] = newdata[,c('Teaching','DonorType','TypeControl')]
str(newdata_small)
write.table(newdata_small, "fritts_robert_export4python.txt", sep="\t", row.names=FALSE, quote=FALSE) #good options for import into python

### now I'm done ###
