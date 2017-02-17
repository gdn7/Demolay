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
#install.packages("psych")
#install.packages("ggplot2")
#install.packages("Hmisc") ##this overwrites functions from others, don't load unless explicitly needed
#install.packages("tree")

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
fa_surveydata.q1
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
# build FA -------- run lines below together --------
fa_surveydata.q1 = factanal(~Q1b+Q1c+Q1 #eliminate vars <.7 and run again
			,factors=1 		 #tried 1 per PCA, tried 2 but bad pval 
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
			,factors=7,rotation="varimax",scores="none"
			,data=surveydata.q2_23,PRIORS="MAX"
		     ) #tried 7 first per PCA but failed and iterated several times, notes in results
# build FA -------- run lines above together --------
fa_surveydata.q2_23
# build FA -------- run lines below together --------
fa_surveydata.q2_23 = factanal(~Q2 +Q3 +Q4 +Q5 +Q6 +Q7 +Q8 +Q9
					 +Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17
				 	 +Q18+Q19+Q20+Q22+Q23
			,factors=6,rotation="varimax",scores="none"
			,data=surveydata.q2_23,PRIORS="MAX"
		     ) #try 6
# build FA -------- run lines above together --------
fa_surveydata.q2_23
# build FA -------- run lines below together --------
fa_surveydata.q2_23 = factanal(~Q2+Q6+Q10+Q14+Q20+Q22+Q23
			,factors=3,rotation="varimax",scores="none"
			,data=surveydata.q2_23,PRIORS="MAX"
		     ) #try 6, excl <.7 vars #alter to 3 due to only 7 vars
# build FA -------- run lines above together --------
fa_surveydata.q2_23
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
## results 7fac=pval .185 (Hah!), 8 is worse and 7 already didn't have sig
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
			,factors=10 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25

# build FA -------- run lines below together --------
#consolidated to remove vars that don't load at >=.7 at all
fa_surveydata.q1_25 = factanal(~Q1b+Q1c+Q2+Q10+Q11+Q15+Q16+Q19+Q22+Q23
					+Q25_Membership+Q25_Ritual+Q25_Education
					+Q25_Com_Service+Q25_Traveling
			,factors=10 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25

# build FA -------- run lines below together --------
#consolidated to remove vars that don't load at >=.7 at all from above (2,16,19,trav)
fa_surveydata.q1_25 = factanal(~Q1b+Q1c+Q10+Q11+Q15+Q22+Q23
					+Q25_Membership+Q25_Ritual+Q25_Education
					+Q25_Com_Service
			,factors=6 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25

# build FA -------- run lines below together --------
#consolidated to remove vars that don't load at >=.7 at all from above (11,mem,rit)
fa_surveydata.q1_25 = factanal(~Q1b+Q1c+Q10+Q15+Q22+Q23
					+Q25_Membership
					+Q25_Education
					+Q25_Com_Service
			,factors=5  #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25

# build FA -------- run lines below together --------
#consolidated to remove vars that don't load at >=.7 at all from above (11,mem,rit)
fa_surveydata.q1_25 = factanal(~Q1b+Q1c+Q10+Q15+Q22+Q23
					#+Q25_Membership
					+Q25_Education
					+Q25_Com_Service
			,factors=4  #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25


# build FA -------- run lines below together --------
#consolidated to remove vars that don't load at >=.7 at all from above (11,mem,rit)
fa_surveydata.q1_25 = factanal(~Q1b+Q1c+Q10+Q22+Q23
			,factors=2  #tried 10 first per PCA, notes in results
			,rotation="varimax",scores="none"
			,data=surveydata.q1_25,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25

## !!! stop removing vars from consideration, this is a wild goose chase
## !!! per chapter notes, refine FA by removing vars that don't load and re-run
## !!! this ends up with endless recursion until it fails to work
## !!! and pushes p-val way beyond acceptable limits...
## !!! backing up to original and refining from there

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
			,factors=10 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25
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
			,factors=7 #tried 10 first per PCA, notes in results
			,rotation="varimax"
			,scores="none"
			,data=surveydata.q1_25
			,PRIORS="MAX"
		     )
# build FA -------- run lines above together --------
fa_surveydata.q1_25
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
## decision:
##  multiple reductions reasonable based on results, the following to be built
##    reduc1:  survey_qXXdv_2iv, where XX is the DV
##    reduc1:  2 vars - full set of IVs PCA indicates 10 factors suficient
##                    - full set of IVs FA  only loads all factors when limited to 2 factors
##                    - two factors are Q1b & Q10, caveat with only 26.5% cum. var.
survey_q21dv_2iv = surveydata[,c("Q21","Q1b","Q10")]
	head(survey_q21dv_2iv,3)
	describe(survey_q21dv_2iv)
	
survey_q24dv_2iv = surveydata[,c("Q24","Q1b","Q10")]
	head(survey_q24dv_2iv,3)
	describe(survey_q24dv_2iv)

##    reduc2:  survey_qXXdv_7iv, where XX is the DV
##    reduc2:  7 vars - individual PCA and FA of related questions Q1, Q2-23, Q25
##                    - indicate 7 supporting vars w/ approx 50% cum. var. each
##                    - vars are Q1b, Q10, Q2, Q23, Q20, Q25mem, Q25edu
survey_q21dv_7iv = surveydata[,c("Q21","Q1b","Q10","Q2","Q23","Q20"
					,"Q25_Membership","Q25_Education")]
	head(survey_q21dv_7iv)
	describe(survey_q21dv_7iv)
survey_q24dv_7iv = surveydata[,c("Q24","Q1b","Q10","Q2","Q23","Q20"
					,"Q25_Membership","Q25_Education")]
	head(survey_q24dv_7iv)
	describe(survey_q24dv_7iv)

##    reduc3:  survey_qXXdv_9iv, where XX is the DV
##    reduc3:  9 vars - full set of IVs PCA indicates 10 factors sufficient
##                    - full set of IVs FA fac=10 only loads 9 vars, using these facs
##                    - vars are Q1b, Q11, Q16, (drop Q19 <.7)
##                    -          Q22, Q15, Q25com, Q25trav, Q25edu, Q25rit
survey_q21dv_9iv = surveydata[,c("Q21","Q1b","Q11","Q16","Q22","Q15"
					,"Q25_Com_Service","Q25_Traveling"
					,"Q25_Education","Q25_Ritual")]
	head(survey_q21dv_9iv)
	describe(survey_q21dv_9iv)
survey_q24dv_9iv = surveydata[,c("Q24","Q1b","Q11","Q16","Q22","Q15"
					,"Q25_Com_Service","Q25_Traveling"
					,"Q25_Education","Q25_Ritual")]
	head(survey_q24dv_9iv)
	describe(survey_q24dv_9iv)



str(surveydata)
head(surveydata,0)
head(survey_q24dv_9iv,0)

## add back in age, level and dummy vars as appropriate for models
## call these reduc3_tree and reduc3_regr with variants for targ vars q21,q24
q21reduc3_tree = survey_q21dv_9iv
q21reduc3_tree[,c(11:12)]=surveydata[,c(2:3)]
head(q21reduc3_tree,0)
str(q21reduc3_tree)

q21reduc3_regr = survey_q21dv_9iv
q21reduc3_regr[,c(11:12)]=surveydata[,c(2:3)]
q21reduc3_regr[,c(13:19)]=surveydata[,c(39:45)]
head(q21reduc3_regr,0)
str(q21reduc3_regr)

q24reduc3_tree = survey_q24dv_9iv
q24reduc3_tree[,c(11:12)]=surveydata[,c(2:3)]
head(q24reduc3_tree,0)
str(q24reduc3_tree)

q24reduc3_regr = survey_q24dv_9iv
q24reduc3_regr[,c(11:12)]=surveydata[,c(2:3)]
q24reduc3_regr[,c(13:19)]=surveydata[,c(39:45)]
head(q24reduc3_regr,0)
str(q24reduc3_regr)



## ******************************************************
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## tree modeling for Q21 and Q24 target variables
## ******************************************************
#question - should age and level be considered continuous?
#try a tree without them first to see what we get
q21tree_regr = tree(q21reduc3_tree)
q21tree_regr
plot(q21tree_regr)
text(q21tree_regr)
prune_q21tree_regr = prune.tree(q21tree_regr, best=3)
prune_q21tree_regr
plot(prune_q21tree_regr)
text(prune_q21tree_regr)

q21tree_class = tree(Q21~., q21reduc3_tree, mindev=1e-6, minsize=2)
q21tree_class
plot(q21tree_class)
text(q21tree_class)
prune_q21tree_class = prune.tree(q21tree_class, best=5)
prune_q21tree_class
plot(prune_q21tree_class)
text(prune_q21tree_class)


q24tree = tree(q24reduc3_tree)
q24tree
plot(q24tree)
text(q24tree)
prune_q24tree = prune.tree(q24tree, best=3)
prune_q24tree
plot(prune_q24tree)
text(prune_q24tree)

q24tree_class = tree(Q24~., q24reduc3_tree, mindev=1e-6, minsize=2)
q24tree_class
plot(q24tree_class)
text(q24tree_class)
prune_q24tree_class = prune.tree(q24tree_class, best=5)
prune_q24tree_class
plot(prune_q24tree_class)
text(prune_q24tree_class)

describe(surveydata[,c("Q21","Q24")])




## ******************************************************
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## regression modeling for Q21 and Q24 target variables
## ******************************************************
str(q21reduc3_regr)
str(q24reduc3_regr)
library(car)
library(dummies)
#loaded libraries necessary for regression tools 
range(q21reduc3_regr$Q21)
range(q24reduc3_regr$Q24)
levels(q21reduc3_regr$Age)
levels(q21reduc3_regr$Level)
hist(as.integer(q21reduc3_regr$Age))
hist(as.integer(q21reduc3_regr$Level))
plot(as.integer(q21reduc3_regr$Age))

#bin ages
bin_interval=c(12,13,14,15,16,18,20,21)
table(cut(as.integer(q21reduc3_regr$Age), bin_interval, right=FALSE))
as.integer(q21reduc3_regr$Age)
#binning not working for some reason, per hist, need to combine 17-19, others stand alone
age_dummy = dummy(q21reduc3_regr[,c('Age')], sep='_')
colnames(age_dummy)
colnames(age_dummy) = c('age_12','age_13','age_14','age_15','age_16','age_18','age_19','age_20')
colnames(age_dummy)
str(age_dummy)
age_dummy = as.data.frame(age_dummy)
str(age_dummy)
##!! remember that 17-19 should be combined... treat this as n'th element and drop for regr

q21reduc3_regr = data.frame(q21reduc3_regr,age_dummy)
q24reduc3_regr = data.frame(q24reduc3_regr,age_dummy)
colnames(q21reduc3_regr)
colnames(q24reduc3_regr)

level_dummy = dummy(q21reduc3_regr[,c('Level')], sep='_')
colnames(level_dummy)
colnames(level_dummy) = c('lev_1','lev_2','lev_3')
level_dummy = as.data.frame(level_dummy)
str(level_dummy)
q21reduc3_regr = data.frame(q21reduc3_regr,level_dummy)
q24reduc3_regr = data.frame(q24reduc3_regr,level_dummy)
colnames(q21reduc3_regr)
colnames(q24reduc3_regr)

#remove NAs
q21reduc3_regr = na.omit(q21reduc3_regr)
q24reduc3_regr = na.omit(q24reduc3_regr)


q21reg1 = lm(q21reduc3_regr$Q21 ~ q21reduc3_regr$Q1b + q21reduc3_regr$Q11
	  + q21reduc3_regr$Q16 #+ q21reduc3_regr$Q19
	  + q21reduc3_regr$Q22
	  + q21reduc3_regr$Q15 + q21reduc3_regr$Q25_Com_Service
	  + q21reduc3_regr$Q25_Traveling + q21reduc3_regr$Q25_Education
	  + q21reduc3_regr$Q25_Ritual
	  + q21reduc3_regr$Q25_Membership_ranked1.int
	  + q21reduc3_regr$Q25_LCC_RD_ranked1.int
	  + q21reduc3_regr$Q25_Ritual_ranked1.int
	  + q21reduc3_regr$Q25_Fun_ranked1.int
	  + q21reduc3_regr$Q25_Education_ranked1.int
	  + q21reduc3_regr$Q25_Com_Service_ranked1.int
	  + q21reduc3_regr$Q25_Traveling_ranked1.int 
	  + q21reduc3_regr$age_12 + q21reduc3_regr$age_13 + q21reduc3_regr$age_14  
	  + q21reduc3_regr$age_15 + q21reduc3_regr$age_16 + q21reduc3_regr$age_20  
	  + q21reduc3_regr$lev_1 + q21reduc3_regr$lev_2
	  )
summary(q21reg1)

#try again, drop intercept since insignificant and if all terms are 0 then 
#           response var must be zero (null)
q21reg2 = lm(q21reduc3_regr$Q21 ~ 0 + q21reduc3_regr$Q1b + q21reduc3_regr$Q11
	  + q21reduc3_regr$Q16 #+ q21reduc3_regr$Q19
	  + q21reduc3_regr$Q22
	  + q21reduc3_regr$Q15 + q21reduc3_regr$Q25_Com_Service
	  + q21reduc3_regr$Q25_Traveling + q21reduc3_regr$Q25_Education
	  + q21reduc3_regr$Q25_Ritual
	  + q21reduc3_regr$Q25_Membership_ranked1.int
	  + q21reduc3_regr$Q25_LCC_RD_ranked1.int
	  + q21reduc3_regr$Q25_Ritual_ranked1.int
	  + q21reduc3_regr$Q25_Fun_ranked1.int
	  + q21reduc3_regr$Q25_Education_ranked1.int
	  + q21reduc3_regr$Q25_Com_Service_ranked1.int
	  + q21reduc3_regr$Q25_Traveling_ranked1.int 
	  + q21reduc3_regr$age_12 + q21reduc3_regr$age_13 + q21reduc3_regr$age_14  
	  + q21reduc3_regr$age_15 + q21reduc3_regr$age_16 + q21reduc3_regr$age_20  
	  + q21reduc3_regr$lev_1 + q21reduc3_regr$lev_2
	  )
summary(q21reg2)

# still only 1-2 terms sig but huge R^2 when omit intercept

surveydata_nona = na.omit(surveydata) 
nrow(surveydata)
nrow(surveydata_nona)
nrow(q21reduc3_regr)
q21reduc3_regr$Q10 = surveydata_nona$Q10
describe(q21reduc3_regr)
colnames(surveydata)
nrow(age_dummy)
nrow(level_dummy)
nrow(surveydata)
nrow(surveydata_nona)
regdata=na.omit(data.frame(surveydata,age_dummy,level_dummy))
nrow(regdata)
colnames(regdata)
colnames(regdata[,c(32:45)])
colnames(regdata[,c(32:45)])=c('Q25mem','Q25lcc','Q25rit','Q25fun'
				      ,'Q25edu','Q25com','Q25tra'
				      ,'Q25mem_1','Q25lcc_1','Q25rit_1','Q25fun_1'
				      ,'Q25edu_1','Q25com_1','Q25tra_1')

colnames(regdata$Q25_Membership)
colnames(regdata)
describe(regdata)
colnames(regdata)
temp=regdata
colnames(temp)
colnames(temp)=c('Date','Age','Level','Q1','Q1a','Q1b','Q1c','Q1d'
		    ,'Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10'
		    ,'Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18'
		    ,'Q19','Q20','Q21','Q22','Q23','Q24'
#		    ,'Q25_Membership','Q25_LCC_RD','Q25_Ritual','Q25_Fun'
#			,'Q25_Education','Q25_Com_Service','Q25_Traveling'
#			,'Q25_Membership_ranked1.int','Q25_LCC_RD_ranked1.int'
#			,'Q25_Ritual_ranked1.int','Q25_Fun_ranked1.int'
#			,'Q25_Education_ranked1.int','Q25_Com_Service_ranked1.int'
#			,'Q25_Traveling_ranked1.int'
		    ,'Q25mem','Q25lcc','Q25rit','Q25fun'
		      ,'Q25edu','Q25com','Q25tra'
		      ,'Q25mem_1','Q25lcc_1','Q25rit_1','Q25fun_1'
		      ,'Q25edu_1','Q25com_1','Q25tra_1'
		    ,'age_12','age_13','age_14','age_15','age_16','age_18'
		    ,'age_19','age_20'
		    ,'lev_1','lev_2','lev_3'
		    )
colnames(temp)
describe(temp)

regdata=temp
head(regdata,0)
head(regdata,3)
plot(regdata$Age, regdata$Level)



q21reg3 = lm(regdata$Q21 ~ 0 + regdata$Q1b + regdata$Q11
	  + regdata$Q16 #+ regdata$Q19
	  + regdata$Q22 + regdata$Q15
	  + regdata$Q25com + regdata$Q25tra + regdata$Q25edu + regdata$Q25rit
	  + regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	  + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
 	  + regdata$Q25tra_1 
	  + regdata$age_12 + regdata$age_13 + regdata$age_14  
	  + regdata$age_15 + regdata$age_16 + regdata$age_20  
	  + regdata$lev_1 + regdata$lev_2
	  )
summary(q21reg3)

q21reg4 = lm(regdata$Q21 ~ 0 + regdata$Q22)
summary(q21reg4)
pairs(regdata[,c("Q21","Q22")])

##full regression on every variable and dummy variable
q21reg5 = lm(regdata$Q21 ~ 0 +
		  regdata$Q1 + regdata$Q1a + regdata$Q1b + regdata$Q1c + regdata$Q1d
	      + regdata$Q2 + regdata$Q3 + regdata$Q4 + regdata$Q5 + regdata$Q6
	      + regdata$Q7 + regdata$Q8 + regdata$Q9 + regdata$Q10 
		+ regdata$Q11 + regdata$Q12 + regdata$Q13 + regdata$Q14 + regdata$Q15
	      + regdata$Q16 + regdata$Q17 + regdata$Q18 + regdata$Q19 + regdata$Q20
		#+ regdata$Q21
	      + regdata$Q22 + regdata$Q23
		#+ regdata$Q24
	      + regdata$Q25mem + regdata$Q25lcc + regdata$Q25rit
	      + regdata$Q25fun + regdata$Q25edu + regdata$Q25com + regdata$Q25tra
	      + regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
		#+ regdata$age_18 + regdata$age_19 #combined final dummy var
	      + regdata$lev_1 + regdata$lev_2 # + regdata$lev_3 #final dummy var
		)
summary(q21reg5)

#Q21 7var piece parts -- tested int/noint, dummy/nodummy... keep noint,nodummy
q21reg6a = lm(regdata$Q21 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg6a) #bad fit
q21reg6b = lm(regdata$Q21 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg6b) #good fit and pval, interesting coefs (23+25dummies)
q21reg6c = lm(regdata$Q21 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		)
summary(q21reg6c) #bad fit
q21reg6d = lm(regdata$Q21 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		)
summary(q21reg6d) #good fit and pval


#Q24 7var piece parts -- tested int/noint, dummy/nodummy... keep noint,nodummy
q24reg6a = lm(regdata$Q24 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg6a)
q24reg6b = lm(regdata$Q24 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg6b)
q24reg6c = lm(regdata$Q24 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		)
summary(q24reg6c)
q24reg6d = lm(regdata$Q24 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q2 + regdata$Q10 + regdata$Q20 + regdata$Q23
		+ regdata$Q25mem + regdata$Q25edu
		)
summary(q24reg6d)



#Q21 2var reduced -- tested int/noint, dummy/nodummy... keep dummy,noint
q21reg7a = lm(regdata$Q21 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q10
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg7a) #bad pval
q21reg7b = lm(regdata$Q21 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q10
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg7b) #good pval, good fit, only dummy var coefs very interesting
q21reg7c = lm(regdata$Q21 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q10
		)
summary(q21reg7c) #decent pval, terrible fit
q21reg7d = lm(regdata$Q21 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q10
		)
summary(q21reg7d)  #good pval and fit only 1 coef (q10 self dev leader)


#Q24 2var reduced -- tested int/noint, dummy/nodummy... keep dummy,noint
q24reg7a = lm(regdata$Q24 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q10
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg7a) #bad fit
q24reg7b = lm(regdata$Q24 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q10
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg7b) #interesting coefs, good fit/pval
q24reg7c = lm(regdata$Q24 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q10
		)
summary(q24reg7c) #bad fit
q24reg7d = lm(regdata$Q24 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q10
		)
summary(q24reg7d) #good fit,pval but bad choice



#Q21 9var reduced -- tested int/noint, dummy/nodummy... keep noint/nodummy
q21reg8a = lm(regdata$Q21 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg8a) #acceptable pval, bad fit
q21reg8b = lm(regdata$Q21 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q21reg8b) #good pval, good fit, only 1 coef (q22 voice)
q21reg8c = lm(regdata$Q21 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		)
summary(q21reg8c) #decent pval bad fit
q21reg8d = lm(regdata$Q21 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		)
summary(q21reg8d)  #great pval, good fit, q11, q22 and q15 sig coefs



#Q24 9var reduced -- tested int/noint, dummy/nodummy... keep 
q24reg8a = lm(regdata$Q24 ~ #0 + #int, dummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg8a) #acceptable pval, bad fit
q24reg8b = lm(regdata$Q24 ~ 0 + #noint, dummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		+ regdata$Q25mem_1 + regdata$Q25lcc_1 + regdata$Q25rit_1
	      + regdata$Q25fun_1 + regdata$Q25edu_1 + regdata$Q25com_1
	      + regdata$Q25tra_1
	      + regdata$age_12 + regdata$age_13 + regdata$age_14
	      + regdata$age_15 + regdata$age_16 + regdata$age_20
	      + regdata$lev_1 + regdata$lev_2
		)
summary(q24reg8b) #better pval and good fit but only 2-3 coefs
q24reg8c = lm(regdata$Q24 ~ #0 + #int, nodummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		)
summary(q24reg8c) #good pval, bad fit
q24reg8d = lm(regdata$Q24 ~ 0 + #noint, nodummy
		  regdata$Q1b + regdata$Q11 + regdata$Q16 + regdata$Q22
		+ regdata$Q15 + regdata$Q25com + regdata$Q25tra
		+ regdata$Q25edu + regdata$Q25rit
		)
summary(q24reg8d)  #great pval and fit, good coefs





summary(q21reg6d)
summary(q21reg7d)
summary(q21reg8d)

summary(q21reg6b)
summary(q24reg6b)

## decision, choose 7var piece part reduction - offers best fit and addresses age/lev
#make class trees associated w/ same
colnames(regdata)
q21treedat = regdata[,c("Q21","Q1b","Q10","Q2"
			     ,"Q23","Q20","Q25mem","Q25edu")]
q24treedat = regdata[,c("Q24","Q1b","Q10","Q2"
			     ,"Q23","Q20","Q25mem","Q25edu")]



q21tree2_class = tree(Q21~., q21treedat, mindev=1e-6, minsize=2)
q21tree2_class
plot(q21tree2_class)
text(q21tree2_class)
prune_q21tree2_class = prune.tree(q21tree2_class, best=3)
prune_q21tree2_class
plot(prune_q21tree2_class)
text(prune_q21tree2_class)

q24tree2_class = tree(Q24~., q24treedat, mindev=1e-6, minsize=2)
q24tree2_class
plot(q24tree2_class)
text(q24tree2_class)
prune_q24tree2_class = prune.tree(q24tree2_class, best=3)
prune_q24tree2_class
plot(prune_q24tree2_class)
text(prune_q24tree2_class)


describe(regdata[,c("Q21","Q24")])
q21histinfo<-hist(regdata$Q21,breaks=c(0,1,2,3,4,5),main="Q21 Histogram")
q21histinfo$counts
barplot(table(regdata$Q21), main="Q21: Overall, Stillwater Chapter meets my expectations.", horiz=TRUE
		,sub=NULL, xlab="Number of Respondants" ,ylab="Score (higher is better)")
q24histinfo<-hist(regdata$Q24,breaks=c(0,1,2,3,4,5),main="Q24 Histogram")
q24histinfo$counts
barplot(table(regdata$Q24), main="Q24: DeMolay is an organization that is worthwhile to me.", horiz=TRUE
		,sub=NULL, xlab="Number of Respondants" ,ylab="Score (higher is better)")
agehistinfo<-hist(as.integer(regdata$Age),breaks=c(0,1,2,3,4,5,6,7,8),main="Age Histogram")
levhistinfo<-hist(as.integer(regdata$Level),breaks=c(0,1,2,3),main="Level Histogram")
barplot(table(regdata$Age), main="Age", horiz=FALSE
		,sub=NULL, ylab="Number of Respondants" ,xlab="Age")
barplot(table(regdata$Level), main="Member Level", horiz=FALSE
		,sub=NULL, ylab="Number of Respondants" ,xlab="Level")



describe(regdata)

summary(q21reg6d)
summary(q24reg6b)