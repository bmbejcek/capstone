# allDat = read.csv("../../Data/all_data.txt")

# #First derive age
allDat$DateOfBirth[allDat$DateOfBirth=="NULL"] = NA
allDat$DateOfBirth = as.Date(allDat$DateOfBirth,format="%m/%d/%Y")
allDat$DateOfBirth[is.na(allDat$DateOfBirth)] = Sys.Date()
allDat$age = age_calc(allDat$DateOfBirth,units="years")
allDat$age[allDat$age==0] = NA

#Then make NA zero for claimsAmount
# allDat$ClaimsAmount[is.na(allDat$ClaimsAmount)] = 0
# 
# allDat = readRDS("../../Data/all_data.rda")
# 
# allDat$Losses = allDat$Premium-allDat$ClaimsAmount
# allDat$Losses = log(standardize(allDat$Losses)+1)
# allDat$Losses = log(allDat$Losses+abs(min(allDat$Losses))+1)
# 
# 
# 
# fac = ifelse(allDat$Losses>300,"BigGain",
#              ifelse(allDat$Losses>100,"ModerateGain",
#                     ifelse(allDat$Losses>0,"SmallGain",
#                           ifelse(allDat$Losses>-5000,"SmallLoss",
#                                  ifelse(allDat$Losses>-25000,"ModerateLoss",
#                                         ifelse(allDat$Losses>-95000,"HugeLoss",
#                                               ifelse(allDat$Losses>-200000,"EnormousLoss")))))))
# 
# allDat$LossCategory = fac
# losses = summarise(group_by(allDat,LossCategory),
#           n = n(),
#           TotalLosses = sum(Losses),
#           AvgLosses = mean(Losses)) %>% mutate(propTotal = n/nrow(allDat))
# 
# losses$propTotal = round(losses$propTotal,3)
# 
# levels(losses$LossCategory) = c("EnormousLoss","ModerateLoss","SmallLoss","SmallGain","ModerateGain","BigGain")
# ggplot(data=losses,aes(x=LossCategory,y=TotalLosses,label=propTotal))+geom_bar(stat="identity")+geom_text()
# 
# 
# standardize <- function(x){(x-min(x))/(max(x)-min(x))}
# 
# saveRDS(allDat,"../../Data/all_data.rda")

allDat$losses = allDat$ClaimsAmount-allDat$Premium
shiftUp = allDat$losses + abs(min(allDat$losses))+1
allDat$logShiftUp = log(shiftUp)
ggplot(data=allDat,aes(x=losses))+geom_histogram(binwidth=5)+xlim(-500,5000)
ggplot(data=allDat,aes(x=shiftUp))+geom_histogram(binwidth=5)+xlim(0,7000)
ggplot(data=allDat,aes(x=logShiftUp))+geom_histogram(binwidth=.02)

#Investigating MartialStatus, Gender interaction
ggplot(data=allDat,aes(x=losses))+geom_histogram()+facet_wrap(MaritalStatus~Gender)
mrdGenInter = summarise(group_by(allDat,MaritalStatus,Gender),n=n(),totalLosses=sum(losses),maxLosses = max(losses),avgLosses=totalLosses/n)
ggplot(data=mrdGenInter,aes(x=Gender,y=avgLosses))+geom_bar(stat="identity")+facet_wrap(~MaritalStatus) 
#######Clearly there is a main effect of Marital Status in the dataset. Divorced people have much higher losses than single people, who have higher
#######losses than married people. So they might need to charge more for divorced people. I should look at the preimum next

#Is it possible we can break down Divorced people to see if there is some segment of the divorced peopulation that has such high losses?

#Investigating MaritalStatus, Primary Vehichle Usage
mrdUseInter = summarise(group_by(allDat,MaritalStatus,PrimaryVehicleUsage),n=n(),totalLosses=sum(losses),maxLosses = max(losses),avgLosses=totalLosses/n)
ggplot(data=mrdUseInter,aes(x=PrimaryVehicleUsage,y=avgLosses))+geom_bar(stat="identity")+facet_wrap(~MaritalStatus)
######Clearly there a main effect of Primary Vehicle Usage. People who use their vehicle for lesiure cause more losses. But there is definetly an interaction.
######For married people, those who primarly drive for leisure are getting into more accidents than those who drive for work!


allDat = subset()
#Investigating MaritalStatus, Primary Vehicle Usage, Gender interaction
ThreeWayinter = summarise(group_by(allDat,MaritalStatus,PrimaryVehicleUsage,Gender),n=n(),totalLosses=sum(losses),maxLosses = max(losses),avgLosses=mean(losses))
ggplot(data=ThreeWayinter,aes(x=PrimaryVehicleUsage,y=avgLosses))+geom_bar(stat="identity")+facet_grid(Gender~MaritalStatus)



#Further cleaning!!!
allDat$MilesToWork[allDat$MilesToWork=="NULL"] = NA
allDat$MilesToWork = as.numeric(allDat$MilesToWork)

#Split data into claims and non claims
claimSet = subset(allDat,hasClaim==TRUE)
nonClaimSet = subset(allDat,hasClaim==FALSE)

#Just take the risk factors
riskFacs = c("ClaimsAmount","Violations","Accidents","MaritalStatus","Gender","MilesToWork","PrimaryVehicleUsage")

claimTreeSet = claimSet[,riskFacs]
nonClaimTreeSet = nonClaimSet[,riskFacs]

#Visualize distributions of the response
ggplot(data=claimTreeSet,aes(x=log(ClaimsAmount)))+geom_histogram()
#I don't want that one outlier point to have such a big effect, so I'm going to take the log of claimsAmounts, and work with that instead.

claimTreeSet$lgClaimsAmount = log(claimTreeSet$ClaimsAmount)

#Lets do some brief exploration before we look at a tree model
#Main Effect Exploration
ggplot(data=claimTreeSet,aes(x=as.factor(Violations),y=lgClaimsAmount))+geom_boxplot() #Violation doesn't seem to matter for severity
ggplot(data=claimTreeSet,aes(x=as.factor(Accidents),y=lgClaimsAmount))+geom_boxplot() #Accident MIGHT matter for severity [4 accidents seems bit higher]
ggplot(data=claimTreeSet,aes(x=MaritalStatus,y=lgClaimsAmount))+geom_boxplot() #Severity seems slightly higher for divorced folks
ggplot(data=claimTreeSet,aes(x=Gender,y=lgClaimsAmount))+geom_boxplot() #Gender doesn't seem to matter
ggplot(data=claimTreeSet,aes(x=MilesToWork,y=lgClaimsAmount))+geom_point() #Miles to work doesn't seem to make a difference
ggplot(data=claimTreeSet,aes(x=PrimaryVehicleUsage,y=lgClaimsAmount))+geom_boxplot() #PrimaryVehicle Usage doesn't seem to matter
#Interaction Effect Exploration
#Hypothesis: Maybe women who are divorced have more severe claims then men who are divorced
ggplot(data=claimTreeSet,aes(x=Gender,y=lgClaimsAmount))+geom_boxplot()+facet_wrap(~MaritalStatus)

regTree = tree(ClaimsAmount~.,claimTreeSet[,c(1:7)])
lgTree = tree(lgClaimsAmount~.,claimTreeSet[,c(2:8)])