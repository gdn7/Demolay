library(psych)
library(FactoMineR)



setwd("C:\\Users\\DELL\\Desktop\\R and python\\Project")

data=read.table("Demolay_data_formatted.csv")
write.csv(data,file="nov_15_data.txt",sep=',')

View(data)
str(data)

data1=subset(data,select=-c(ID,Members_Per_Event,Year,trav_Mileage,Month,comserv_Charitable,mem_Fundraising,fun_Misc))

write_data=write.csv(data1,file="data1")

col_names <- names(data1)
data_factor=data1
data_factor[]= lapply(data1[,col_names] , factor)

str(data_factor)

analysis_data=write.csv(data1,file="data_factor")

pca=princomp(data_factor,cor=FALSE)

write_data=write.csv(data_factor,file="factor_data")
#####################MCA######################

result=MCA(data_factor,ncp=5,graph=TRUE)

#####################PCA#######################
fa1=fa(data1,2,scores=TRUE)
biplot(pca,scale=0)

