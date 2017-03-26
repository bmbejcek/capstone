library("RSQLite")
library("eeptools")

##################
##Clustering the data, and looking at the loss ratio within the clusters
##################

#Step 1: Organize the data into smaller data frames

# #Connect to the database
con = dbConnect(drv=SQLite(),dbname="../../Data/CapstoneV1.db")
# 
# #grab all of the data
allDat = dbGetQuery(con,"Select * from All_Data")

#First derive age
allDat$DateOfBirth[allDat$DateOfBirth=="NULL"] = NA
allDat$DateOfBirth = as.Date(allDat$DateOfBirth,format="%m/%d/%Y")
allDat$DateOfBirth[is.na(allDat$DateOfBirth)] = Sys.Date()
allDat$age = age_calc(allDat$DateOfBirth,units="years")
allDat$age[allDat$age==0] = NA

#Create data frame for the continuous variables
contCols = c("Violations","Accidents","MilesToWork","age","Population","Percent0to15","Percent15to25","Percent25to40","Percent50","AvgPrice",
             "ClaimsAmount","Premium")
continousData = allDat[,contCols]
saveRDS(continousData,"../../data/continuousData.rda")



#K-means data frame
kDatFull = continousData[,-c(11,12)]
kDatFull$lgAvgPrice = log(kDatFull$AvgPrice)
kDatFull$lgPop = log(kDatFull$Population)

rowsNoNA = which(!apply(kDatFull,1,anyNA))

kDatSub = kDatFull[rowsNoNA,c(-5,-10)]

#Do K-Means
cl = kmeans(kDatSub,15,iter.max=100,nstart=50) 
#50.2% of variation is accounted for by between cluster variation













