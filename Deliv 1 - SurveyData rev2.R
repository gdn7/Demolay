## MSIS 5223 Analytics Programming ##
## Fritts, Robert                  ##
## semester project - survey data analysis
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

# !! - placeholder for potential packages
# survey analysis package - https://cran.r-project.org/web/packages/survey/index.html


#-----------------#
#- declare stuff -#
#-----------------#
data_dir = "C:\\Users\\Admin\\Dropbox\\Project\\Data\\Survey"
setwd(data_dir)



#------------#
#- do stuff -#
#------------#

# read in csv
surveydata = read.csv("Three Week Survey Results.csv"
			, header=TRUE
			, colClasses=c("Date"  = "factor"
					  ,"Age"   = "factor"
					  ,"Level" = "factor"
					  )
			)
#success, check it out
head(surveydata,3)
str(surveydata)
tail(surveydata,3)

## add dummy binary cols for q25 variants, 1=1 else 0
surveydata$Q25_Membership_ranked1.int = as.integer(ifelse(surveydata$Q25_Membership == 1 , 1, 0))
surveydata$Q25_LCC_RD_ranked1.int = as.integer(ifelse(surveydata$Q25_LCC_RD == 1 , 1, 0))
surveydata$Q25_Ritual_ranked1.int = as.integer(ifelse(surveydata$Q25_Ritual == 1 , 1, 0))
surveydata$Q25_Fun_ranked1.int = as.integer(ifelse(surveydata$Q25_Fun == 1 , 1, 0))
surveydata$Q25_Education_ranked1.int = as.integer(ifelse(surveydata$Q25_Education == 1 , 1, 0))
surveydata$Q25_Com_Service_ranked1.int = as.integer(ifelse(surveydata$Q25_Com_Service == 1 , 1, 0))
surveydata$Q25_Traveling_ranked1.int = as.integer(ifelse(surveydata$Q25_Traveling == 1 , 1, 0))

# surveydata$Q25_Membership_ranked1.fac = factor(ifelse(surveydata$Q25_Membership == 1 , 1, 0))
# surveydata$Q25_LCC_RD_ranked1.fac = factor(ifelse(surveydata$Q25_LCC_RD == 1 , 1, 0))
# surveydata$Q25_Ritual_ranked1.fac = factor(ifelse(surveydata$Q25_Ritual == 1 , 1, 0))
# surveydata$Q25_Fun_ranked1.fac = factor(ifelse(surveydata$Q25_Fun == 1 , 1, 0))
# surveydata$Q25_Education_ranked1.fac = factor(ifelse(surveydata$Q25_Education == 1 , 1, 0))
# surveydata$Q25_Com_Service_ranked1.fac = factor(ifelse(surveydata$Q25_Com_Service == 1 , 1, 0))
# surveydata$Q25_Traveling_ranked1.fac = factor(ifelse(surveydata$Q25_Traveling == 1 , 1, 0))

str(surveydata) #checking results

surveydata$Q25_Membership_ranked1.int #check results
surveydata$Q25_Membership_ranked1.fac #check results
surveydata$Q25_Membership #check results

surveydata$Q25_LCC_RD_ranked1.int #check results
surveydata$Q25_LCC_RD_ranked1.fac #check results
surveydata$Q25_LCC_RD #check results

surveydata$Q25_Ritual_ranked1.int #check results
surveydata$Q25_Ritual_ranked1.fac #check results
surveydata$Q25_Ritual #check results

surveydata$Q25_Fun_ranked1.int #check results
surveydata$Q25_Fun_ranked1.fac #check results
surveydata$Q25_Fun #check results

surveydata$Q25_Education_ranked1.int #check results
surveydata$Q25_Education_ranked1.fac #check results
surveydata$Q25_Education #check results

surveydata$Q25_Com_Service_ranked1.int #check results
surveydata$Q25_Com_Service_ranked1.fac #check results
surveydata$Q25_Com_Service #check results

surveydata$Q25_Traveling_ranked1.int #check results
surveydata$Q25_Traveling_ranked1.fac #check results
surveydata$Q25_Traveling #check results


nrow(surveydata)
describe(surveydata)
summary(surveydata)

str(surveydata)


## make frames for pca/fa, need to break set into random samples of about 50% each
split.num = round(nrow(surveydata)*.5,0)  # find num of rows to use in sample
split.num
split.num / nrow(surveydata)
range = 1:(nrow(surveydata))              # set range variable
range
surveydata.split1 = surveydata[sample(range,split.num,replace=F)
						 ,c(		#28,31,    #target vars
						    4:27,29:30,32:45)] #pred vars
head(surveydata.split1,3)
nrow(surveydata.split1)
surveydata.split2 = surveydata[sample(range,split.num,replace=F)
						 ,c(		#28,31,    #target vars
						    4:27,29:30,32:45)] #pred vars
head(surveydata.split2,3)
nrow(surveydata.split2)

str(surveydata.split1)
head(surveydata.split1,0)

#test samples for most significant columns from Q25 questions
#most significant will be the lowest overall sum or highest ranked1 sum
colSums(surveydata.split1[,c(27:40)]) #fun best, mem 2nd per lowest reg column and highest dummy
colSums(surveydata.split2[,c(27:40)]) #fun best, mem 2nd per same

#check pca/fa

surveydata.split1.q1=surveydata.split1[,c(1:26)]
surveydata.split1.q25=surveydata.split1[,c(27:40)]
surveydata.split2.q1=surveydata.split2[,c(1:26)]    #changed from split1[,c     10/11/2016
surveydata.split2.q25=surveydata.split2[,c(27:40)]  #changed from split1[,c     10/11/2016

#PCA & eigenvalues
PCA_surveydata.split1.q1 = princomp(surveydata.split1.q1,cor=TRUE) #run PCA and store loadings
PCA_surveydata.split1.q1  #print frame
## PCA analysis - 7 stddevs > 1, 2 near it (stddevs >=1, var=stddev^2 and is the eigen vectors
##              - thus possibly 7-9 columns important
plot(PCA_surveydata.split1.q1, main="Survey Data Scree Plot (Q1-Q24)") #scree plot
## SCREE plot - per plot, factors level out from 8 on, thus only 7 factors

##### **************************************************************************
##### 2016/10/11 -- adding stuff between these lines due to split1.q25 pca error
#####               per Dr. Hammer, no need to split since so few records,
#####               reworking PCA and FA for full set
##### --------------------------------------------------------------------------
nrow(surveydata.split2.q25)
nrow(surveydata.split1.q25)
describe(surveydata.split2.q25)
describe(surveydata.split1.q25)
surveydata.q1 = surveydata[,c("Q1","Q1a","Q1b","Q1c","Q1d")]
surveydata.q2_23 = surveydata[,c("Q2" ,"Q3" ,"Q4" ,"Q5" ,"Q6" ,"Q7" ,"Q8" ,"Q9"
					  ,"Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17"
					  ,"Q18","Q19","Q20","Q22","Q23")]
surveydata.q25 = surveydata[,c("Q25_Membership","Q25_Membership_ranked1.int"
					,"Q25_LCC_RD","Q25_LCC_RD_ranked1.int"
					,"Q25_Ritual","Q25_Ritual_ranked1.int"
					,"Q25_Fun","Q25_Fun_ranked1.int"
					,"Q25_Education","Q25_Education_ranked1.int"
					,"Q25_Com_Service","Q25_Com_Service_ranked1.int"
					,"Q25_Traveling","Q25_Traveling_ranked1.int"
					)]

surveydata.q1
surveydata.q2_23
surveydata.q25


pca_surveydata.q1    = princomp(surveydata.q1   ,cor=TRUE) #run PCA and store
pca_surveydata.q1     #print frame
pca_surveydata.q2_23 = princomp(na.omit(surveydata.q2_23),cor=TRUE) #run PCA and store
pca_surveydata.q2_23  #print frame
pca_surveydata.q25   = princomp(surveydata.q25  ,cor=TRUE) #run PCA and store
pca_surveydata.q25    #print frame

pairs(surveydata.q1)
# build FA -------- run lines below together --------
fa_surveydata.q1 = factanal(~Q1+Q1a+Q1b+Q1c+Q1d 
					#+Q2+Q3+Q4+Q5+Q6+Q7
				  	#+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17
				 	#+Q18+Q19+Q20+Q22+Q23
			,factors=1 
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1
## results -- pval=.00607 test of hyp that 1 factor is sufficient


# build FA -------- run lines below together --------
fa_surveydata.q2_23 = factanal(~Q2 +Q3 +Q4 +Q5 +Q6 +Q7 +Q8 +Q9
					 +Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17
				 	 +Q18+Q19+Q20+Q22+Q23
			,factors=4
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q2_23
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q2_23
## results 7fac=pval .185 (Hah!), 8 is worse and 7 already didn't have a
##		, 6 pval is .0173, 5 .001... when tried 7 2 facs didn't load any >.7
##		, so 5 seems to fit but one fac(3) doesn't load out anything >.7
##		, when try 4 facs all 4 get loaded and quite strongly... keep 4
##		, 4 pval is 5.28e-05

##

#!!!!!!!!!!!!!!!!!!!!!!!! 2016/10/11
#!!! return here, run FA on q25, then full q1-25 IVs, then full q1-25+DVs
#!!! also return to PCA and print plots for percent of variance described
#!!! and try to do same for final consolidated set
#    per Dr. Hammer, ignore dummy vars for consolidation purposes but bring
#                    into the regression
#!!!!!!!!!!!!!!!!!!!!!!!!

##### --------------------------------------------------------------------------
##### 2016/10/11 -- adding stuff between these lines due to split1.q25 pca error
#####               per Dr. Hammer, no need to split since so few records,
#####               reworking PCA and FA for full set
##### **************************************************************************


#repeat pca for q25 subset
PCA_surveydata.split1.q25 = princomp(surveydata.split1.q25,cor=TRUE) #run PCA and store loadings
PCA_surveydata.split1.q25  #print frame
## PCA analysis - 6 stddevs > 1, none near it thus possibly 6 columns important
plot(PCA_surveydata.split1.q25, main="Survey Data Scree Plot (Q25)") #scree plot
## scree plot confirms 6 in this set

#FA comparison !! use split2 for FA !!
pairs(surveydata.split2.q1)
# build FA -------- run lines below together --------
fa_surveydata.split2.q1 = factanal(
					~Q1+Q1a+Q1b+Q1c+Q1d+Q2+Q3+Q4+Q5+Q6+Q7
				  	+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17
				 	+Q18+Q19+Q20+Q22+Q23
			,factors=7
			,rotation="varimax"
			,scores="none"
			,data=surveydata.split2.q1
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.split2.q1
edit(fa_surveydata.split2.q1)

pairs(surveydata.split2.q25)
# build FA -------- run lines below together --------
fa_surveydata.split2.q25 = factanal(
					~Q25_Membership + Q25_LCC_RD + Q25_Ritual + Q25_Fun
					+ Q25_Education + Q25_Com_Service + Q25_Traveling 
					+ Q25_Membership_ranked1.int + Q25_LCC_RD_ranked1.int
					+ Q25_Ritual_ranked1.int + Q25_Fun_ranked1.int 
					+ Q25_Education_ranked1.int + Q25_Com_Service_ranked1.int
					+ Q25_Traveling_ranked1.int
			,factors=6
			,rotation="varimax"
			,scores="none"
			,data=surveydata.split2.q25
			#,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.split2.q25
edit(fa_surveydata.split2.q25)

## both FAs fail/error out due to error:
## Error in solve.default(cv) : 
##   system is computationally singular: reciprocal condition number = 2.98948e-17

## perhaps look at k-means/clustering/agglom analysis for reduction

### key takeaway - FA inconclusive, keep for further review/analysis to reduce
###              - reducing Q25 set as result of colsums data


## make analysis data frames as result of reduction
## more to come, but these for now

survey_q21_df = surveydata[,c(28,1:27,29:30,31,35,42,32,39)]
head(survey_q21_df,0)
survey_q24_df = surveydata[,c(31,1:27,28,29:30,35,42,32,39)]
head(survey_q24_df,0)


#### stop execution here, only notes and old code below



#########################################################################
############# old pca/fa/cluster/agglom code below here #################
#########################################################################
##  no need for pca/fa here, but would be interesting 
##  to examine clustering based on categoricals
#########################################################################
############# old pca/fa/cluster/agglom code below here #################
#########################################################################







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
			,PRIORS="MAX"
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
