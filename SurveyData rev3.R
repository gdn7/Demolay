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
## note -- comment out all splitting, unnecessary due to small size of data set
#split.num = round(nrow(surveydata)*.5,0)  # find num of rows to use in sample
#split.num
#split.num / nrow(surveydata)
#range = 1:(nrow(surveydata))              # set range variable
#range
#surveydata.split1 = surveydata[sample(range,split.num,replace=F)
#						 ,c(		#28,31,    #target vars
#						    4:27,29:30,32:45)] #pred vars
#head(surveydata.split1,3)
#nrow(surveydata.split1)
#surveydata.split2 = surveydata[sample(range,split.num,replace=F)
#						 ,c(		#28,31,    #target vars
#						    4:27,29:30,32:45)] #pred vars
#head(surveydata.split2,3)
#nrow(surveydata.split2)
#
#str(surveydata.split1)
#head(surveydata.split1,0)
#
##test samples for most significant columns from Q25 questions
##most significant will be the lowest overall sum or highest ranked1 sum
#colSums(surveydata.split1[,c(27:40)]) #fun best, mem 2nd per lowest reg column and highest dummy
#colSums(surveydata.split2[,c(27:40)]) #fun best, mem 2nd per same
colSums(surveydata[,c(27:45)]) 
#Q25 fun best, mem 2nd per lowest sum reg col and highest sum dummy cols 


#check pca/fa
#commenting out splits
#surveydata.split1.q1=surveydata.split1[,c(1:26)]
#surveydata.split1.q25=surveydata.split1[,c(27:40)]
#surveydata.split2.q1=surveydata.split2[,c(1:26)]    #changed from split1[,c     10/11/2016
#surveydata.split2.q25=surveydata.split2[,c(27:40)]  #changed from split1[,c     10/11/2016

#PCA & eigenvalues
#PCA_surveydata.split1.q1 = princomp(surveydata.split1.q1,cor=TRUE) #run PCA and store loadings
#PCA_surveydata.split1.q1  #print frame
## PCA analysis - 7 stddevs > 1, 2 near it (stddevs >=1, var=stddev^2 and is the eigen vectors
##              - thus possibly 7-9 columns important
#plot(PCA_surveydata.split1.q1, main="Survey Data Scree Plot (Q1-Q24)") #scree plot
## SCREE plot - per plot, factors level out from 8 on, thus only 7 factors

##### **************************************************************************
##### 2016/10/11 -- adding stuff between these lines due to split1.q25 pca error
#####               per Dr. Hammer, no need to split since so few records,
#####               reworking PCA and FA for full set
##### --------------------------------------------------------------------------
#nrow(surveydata.split2.q25)
#nrow(surveydata.split1.q25)
#describe(surveydata.split2.q25)
#describe(surveydata.split1.q25)
surveydata.q1 = surveydata[,c("Q1","Q1a","Q1b","Q1c","Q1d")]
surveydata.q2_23 = surveydata[,c("Q2" ,"Q3" ,"Q4" ,"Q5" ,"Q6" ,"Q7" ,"Q8" ,"Q9"
					  ,"Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17"
					  ,"Q18","Q19","Q20","Q22","Q23")]
surveydata.q25 = surveydata[,c("Q25_Membership" #,"Q25_Membership_ranked1.int" --ignore dummy vars for reduction 
					,"Q25_LCC_RD" 	#,"Q25_LCC_RD_ranked1.int" 	 --ignore dummy vars for reduction 
					,"Q25_Ritual" 	#,"Q25_Ritual_ranked1.int"	 --ignore dummy vars for reduction 
					,"Q25_Fun" 		#,"Q25_Fun_ranked1.int"		 --ignore dummy vars for reduction
					,"Q25_Education" 	#,"Q25_Education_ranked1.int"	 --ignore dummy vars for reduction
					,"Q25_Com_Service" #,"Q25_Com_Service_ranked1.int" --ignore dummy vars for reduction
					,"Q25_Traveling" 	#,"Q25_Traveling_ranked1.int"	 --ignore dummy vars for reduction
					)]
surveydata.q1_25 = surveydata[,c("Q1","Q1a","Q1b","Q1c","Q1d"
					,"Q2" ,"Q3" ,"Q4" ,"Q5" ,"Q6" ,"Q7" ,"Q8" ,"Q9"
					,"Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17"
					,"Q18","Q19","Q20","Q22","Q23"
					,"Q25_Membership" #,"Q25_Membership_ranked1.int" --ignore dummy vars for reduction 
					,"Q25_LCC_RD" 	#,"Q25_LCC_RD_ranked1.int" 	 --ignore dummy vars for reduction 
					,"Q25_Ritual" 	#,"Q25_Ritual_ranked1.int"	 --ignore dummy vars for reduction 
					,"Q25_Fun" 		#,"Q25_Fun_ranked1.int"		 --ignore dummy vars for reduction
					,"Q25_Education" 	#,"Q25_Education_ranked1.int"	 --ignore dummy vars for reduction
					,"Q25_Com_Service" #,"Q25_Com_Service_ranked1.int" --ignore dummy vars for reduction
					,"Q25_Traveling" 	#,"Q25_Traveling_ranked1.int"	 --ignore dummy vars for reduction
					)]

surveydata.q1     #q1 related indep vars
surveydata.q2_23  #q2-23 related indep vars
surveydata.q25    #q25 related indep vars
surveydata.q1_25  #all indep vars


pca_surveydata.q1    = princomp(surveydata.q1   ,cor=TRUE) #run PCA and store
pca_surveydata.q1     #print frame
pca_surveydata.q2_23 = princomp(na.omit(surveydata.q2_23),cor=TRUE) #run PCA and store
pca_surveydata.q2_23  #print frame
pca_surveydata.q25   = princomp(surveydata.q25  ,cor=TRUE) #run PCA and store
pca_surveydata.q25    #print frame
pca_surveydata.q1_25   = princomp(na.omit(surveydata.q1_25),cor=TRUE) #run PCA and store
pca_surveydata.q1_25    #print frame
## results
##   - q1 1 stddev >1, q2_23 7 stddev > 1, q25 4 stddev > 1
##   - q1-q25 all indep vars together show 10 components stddev >1 with 2 more at .998 and .983


pairs(surveydata.q1)
# build FA -------- run lines below together --------
fa_surveydata.q1 = factanal(~Q1+Q1a+Q1b+Q1c+Q1d 
			,factors=1 #tried 1 per PCA, tried 2 but bad pval 
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1
## results -- pval=.00607 test of hyp that 1 factor is sufficient
##         -- select Q1b as representative with a loading of .969 (q1c .930)


# build FA -------- run lines below together --------
fa_surveydata.q2_23 = factanal(~Q2 +Q3 +Q4 +Q5 +Q6 +Q7 +Q8 +Q9
					 +Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17
				 	 +Q18+Q19+Q20+Q22+Q23
			,factors=4 #tried 7 first per PCA but failed and iterated several times, notes in results
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
##         --representative vars are selected as follows
##          -fac1 Q10 (.902), fac2 Q2 (.843), fac3 Q23 (.840), fac4 Q20 (.967)

##

#!!!!!!!!!!!!!!!!!!!!!!!! 2016/10/11
#!!! return here, run FA on q25, then full q1-25 IVs, then full q1-25+DVs
#!!! also return to PCA and print plots for percent of variance described
#!!! and try to do same for final consolidated set
#    per Dr. Hammer, ignore dummy vars for consolidation purposes but bring
#                    into the regression
#!!!!!!!!!!!!!!!!!!!!!!!!


# build FA -------- run lines below together --------
fa_surveydata.q25 = factanal(~Q25_Membership 
					#+ Q25_LCC_RD #won't run w/ 'singular' error with all Q25s, 
					#  drop LCC_RD due to highest sum of ranks and lowest sum of #1 ranks
					+ Q25_Ritual 
					+ Q25_Fun 
					+ Q25_Education 
					+ Q25_Com_Service 
					+ Q25_Traveling
			,factors=2 #tried 4 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q25
## results 
##   -- factors=4 : Error in facatanal(... -- 4 factors are too many for 7 variables
##   -- factors=3,2,1 : system is computationally singular
##   -- cannot reduce with FA, will use result of colsums done previously
##   --    that reduced to "fun" and "membership" but will keep all dummy vars
##   -- second attempt found that omitting a single indep var here let the FA run
##   --    dropped LCC_RD due to highest sum (worst total ranks) and lowest count of #1 ranks (sum of dummy var)
##   --    results indicate:
##   --    4 factors too many, 3 work but 3rd doesn't get loaded and has no p-val
##   --    2 factors are sufficient with p-val 9e-12
##   --           fac1 loads with membership (-.927) and education (.997)
##   --           expected membership and fun to return but since they both ranked
##   --             very high, then I think this makes sense... one is dominant
##   --               and the second factor is dominant on opposite end hence sign
##   --               difference... 


# build FA -------- run lines below together --------
fa_surveydata.q1_25 = factanal(~Q1 + Q1a + Q1b + Q1c + Q1d
					+ Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9
					+ Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17
				 	+ Q18 + Q19 + Q20 + Q22 + Q23
					+ Q25_Membership 
					#+ Q25_LCC_RD #won't run w/ 'singular' error with all Q25s, 
					#  drop LCC_RD due to highest sum of ranks and lowest sum of #1 ranks
					+ Q25_Ritual 
					+ Q25_Fun 
					+ Q25_Education 
					+ Q25_Com_Service 
					+ Q25_Traveling 
			,factors=2 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25
## results 
##   -- 10 factors good pval ... fac 4 fails to load
##   --  9 factors good pval ... facs 4,5 fail to load
##   --  8 factors good pval ... facs 3,7 fail to load
##   --  7 factors good pval ... fac 7 fails to load
##   --  6 factors good pval ... facs 3,6 fail to load
##   --  5 factors good pval ... fac 4 fails to load
##   --  4 factors good pval ... fac 4 fails to load
##   --  3 factors good pval ... fac 3 fails to load
##   --  2 factors good pval ... all facs load finally
##          fac1: Q1b@.997, fac2: Q10@.807


## make frame for IVs and test correlation, reduction
surveydata.iv = surveydata[,c("Q21","Q24","Q1b")]
pairs(surveydata.iv)
pca_surveydata.iv   = princomp(surveydata.iv,cor=TRUE) #run PCA and store
pca_surveydata.iv    #print frame
#result, 1 factor sufficient for the IVs
# build FA -------- run lines below together --------
fa_surveydata.iv = factanal(~Q21 + Q24 + Q1b
			,factors=1 #try 2 then 1
			,rotation="varimax"
			,scores="none"
			,data=surveydata.iv 
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.iv
#q21 loads best, but the pairs data show bad correlation, treat each separate


##### --------------------------------------------------------------------------
##### 2016/10/11 -- adding stuff between these lines due to split1.q25 pca error
#####               per Dr. Hammer, no need to split since so few records,
#####               reworking PCA and FA for full set
##### **************************************************************************

## make analysis data frames as result of reductions
## decision
##  multiple reductions reasonable based on results, the following to be built
##    reduc1:  survey_df_qXXdv_2iv, where XX is the DV
##    reduc1:  2 vars - full set of IVs PCA indicates 10 factors suficient
##                    - full set of IVs FA  only loads all factors when limited to 2 factors
##                    - two factors are Q1b & Q10, caveat with only 26.5% cum. var.
survey_q21dv_2iv = surveydata[,c("Q21","Q1b","Q10")]
	head(survey_q21dv_2iv,3)
	describe(survey_q21dv_2iv)
	
survey_q24dv_2iv = surveydata[,c("Q24","Q1b","Q10")]
	head(survey_q24dv_2iv,3)
	describe(survey_q24dv_2iv)

##    reduc2:  survey_df_qXXdv_7iv, where XX is the DV
##    reduc2:  7 vars - individual PCA and FA of related questions Q1, Q2-23, Q25
##                    - indicate 7 supporting vars w/ approx 50% cum. var. each
##                    - vars are Q1b, Q10, Q2, Q23, Q20, Q25mem, Q25edu
survey_q21dv_7iv = surveydata[,c("Q21",")]


survey_q24_df = surveydata[,c(31,1:27,28,29:30,35,42,32,39)]
head(survey_q24_df,0)


#### stop execution here, only notes and old code below





#########################################################################
############# old pca/fa/cluster/agglom code below here #################
#########################################################################


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
