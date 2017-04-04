#________________________________________________________________________________
#Decision Tree Analysis
library("rpart")
library("dplyr")
library("ggplot2")

allDat = readRDS("../../../data/all_data.rda")


#Further cleaning!!!
allDat$MilesToWork[allDat$MilesToWork=="NULL"] = NA
allDat$MilesToWork = as.numeric(allDat$MilesToWork)
allDat$hasClaim = as.factor(allDat$ClaimsAmount>0)
#allDat$Violations = as.factor(allDat$Violations)
allDat$Violations = as.factor(ifelse(allDat$Violations>=4,"HIGH","LOW"))
#allDat$Accidents = as.factor(allDat$Accidents)
allDat$Accidents = as.factor(ifelse(allDat$Accidents>=3,"HIGH","LOW"))

#Split data into claims and non claims
claimSet = subset(allDat,hasClaim==TRUE)
nonClaimSet = subset(allDat,hasClaim==FALSE)

#Just take the risk factors
riskFacs = c("ClaimsAmount","Violations","Accidents","MaritalStatus","Gender","MilesToWork","PrimaryVehicleUsage","hasClaim")

claimTreeSet = claimSet[,riskFacs]
nonClaimTreeSet = nonClaimSet[,riskFacs]
fullData = rbind(claimTreeSet,nonClaimTreeSet)

###Make final tree set
indices = sample(1:nrow(nonClaimTreeSet),nrow(claimTreeSet))
subNonClaim = nonClaimTreeSet[indices,]
subData = rbind(claimTreeSet,subNonClaim) #50% of data is no claim, 50% of data is claim
fullData = na.omit(fullData)
subData = na.omit(subData)


##quick exploration
for(i in 2:8) {
  #i=2
  isFac = is.factor(subData[,i])
  name = names(subData)[i]
  if (isFac) {
    dat = summarise(group_by(subData,subData[,i],hasClaim),n=n()) %>% mutate(prop=n/sum(n))
    dat = subset(dat,hasClaim=="TRUE")
    print(paste("The variable is ",name,sep=""))
    print(dat)
    ggplot(data=dat,aes(x=dat[,1],y=prop))+geom_bar(stat="identity")+ggtitle(name)
  }
  else {
    plot(subData[,i]~subData$hasClaim,main=name)
  }
  
}



###build the tree with the subData and all risk features
trmSub = rpart(hasClaim~.,subData[,c(-1)])
plot(trmSub)
text(trmSub,pretty=0)
###build the tree with all the data
trmFull = tree(hasClaim~.,fullData[,c(-1)])
plot(trmFull)
text(trmFull,pretty=0)

#######Take out Violations
noViolations = subData[,-c(2)]
###build the tree with the subData and NOT the violations
treeModV = rpart(hasClaim~.,noViolations[,c(-1)])
plot(treeModV)
text(treeModV,pretty=0)
#######Take out Accidents
###build the tree with the subData and NOT the accidents
noAccidents = subData[,-c(3)]
treeModA = rpart(hasClaim~.,noAccidents[,c(-1)])
plot(treeModA)
text(treeModA,pretty=0)


#What we learn from these overfitted models is that
#there appears to be a legimiate interaction between Violations/Accidents and MilesToWork
#AND Violations, and Accidents are our most powerful main effects
#Next we might to add other variables besides risk factors, and also prune our current trees!

