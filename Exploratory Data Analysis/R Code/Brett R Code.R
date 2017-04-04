library("RSQLite")
library("dplyr")

# connect to the sqlite file
con = dbConnect(drv=RSQLite::SQLite(), dbname="../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get the large table of data
all = dbGetQuery( con,'select * from All_DATA' )
all$ClaimsAmount[is.na(all$ClaimsAmount)] <- 0
all$LossRatio <- all$ClaimsAmount/all$Premium
all$lr <- all$LossRatio < 1.0 # indicator variable if there is a claim or not

# initial plots (do not use)
# p <- ggplot(all, aes(jitter(Violations), jitter(AgencyID)))
# p + geom_point(aes(colour = factor(lr)))
# p <- ggplot(allDat, aes(age, Gender))
# p + geom_point(aes(colour = factor(lr)))
# scatter3D(all$Premium, all$AvgPrice, all$Population, colvar = all$lr, theta = 100, phi = 5)
# scatter3D(all$Percent0to15, all$Percent15to25, all$Percent25to40, colvar = all$lr, col = c(rgb(0.9,0.2,0.24,alpha=1.0), rgb(0.2,0.2,0.2,alpha=0.1)), theta = 60, phi = 45)


# visualization of Violations ~ AgencyID by % of drivers who made claim
d = data.frame(summarize(group_by(all,Violations,AgencyID,lr),n = n()) %>% mutate(freq=n/sum(n)))
d = d[d$lr == FALSE,]

# ggplot(data=d,aes(x=lr,y=freq))+geom_bar(stat="identity",aes(fill=factor(lr)))+facet_grid(factor(Violations) ~ factor(AgencyID))
# ggplot(data=d_false,aes(x=lr,y=freq))+geom_bar(stat="identity",aes(fill=factor(lr)))+facet_grid(factor(Violations) ~ factor(AgencyID))

ggplot(data=d,aes(x=factor(Violations),y=factor(AgencyID))) + geom_tile(aes(fill = freq), colour = "white") + geom_text(aes(label = round(freq, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of Violations ~ AgencyID by avg(LossRatio)
d2 = all %>% group_by(Violations,AgencyID) %>% summarize(mean=mean(LossRatio), n = n())

ggplot(data=d2,aes(x=factor(Violations),y=factor(AgencyID))) + geom_tile(aes(fill = mean), colour = "white") + geom_text(aes(label = round(mean, 3))) + scale_fill_gradient(low = "white", high = "red")

# visualization of UnderwritingAgencyName ~ AgencyID by % of drivers who made claim
d3 = data.frame(summarize(group_by(all,UnderwritingAgencyName,AgencyID,lr),n = n()) %>% mutate(freq=n/sum(n)))
d3 = d3[d3$lr == FALSE,]
ggplot(data=d3,aes(x = UnderwritingAgencyName, y=factor(AgencyID))) + geom_tile(aes(fill = freq), colour = "white") + geom_text(aes(label = round(freq, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of UnderwritingAgencyName ~ AgencyID by avg(LossRatio)
d4 = all %>% group_by(UnderwritingAgencyName,AgencyID) %>% summarize(mean=mean(LossRatio), n = n())
ggplot(data=d4,aes(x=UnderwritingAgencyName,y=factor(AgencyID))) + geom_tile(aes(fill = mean), colour = "white") + geom_text(aes(label = round(mean, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of MaritalStatus ~ Gender by % of drivers who made claim
d5 = data.frame(summarize(group_by(all,MaritalStatus,Gender,lr),n = n()) %>% mutate(freq=n/sum(n)))
d5 = d5[d5$lr == FALSE,]
ggplot(data=d5,aes(x = MaritalStatus, y=Gender)) + geom_tile(aes(fill = freq), colour = "white") + geom_text(aes(label = round(freq, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of MaritalStatus ~ Gender by avg(LossRatio)
d6 = all %>% group_by(MaritalStatus,Gender) %>% summarize(mean=mean(LossRatio), n = n())
ggplot(data=d6,aes(x=MaritalStatus,y=Gender)) + geom_tile(aes(fill = mean), colour = "white") + geom_text(aes(label = round(mean, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of VehicleModelYear ~ PrimaryVehicleUsage by % of drivers who made claim
d7 = data.frame(summarize(group_by(all,VehicleModelYear,PrimaryVehicleUsage,lr),n = n()) %>% mutate(freq=n/sum(n)))
d7 = d7[d7$lr == FALSE,]
ggplot(data=d7,aes(x=factor(VehicleModelYear),y=PrimaryVehicleUsage)) + geom_tile(aes(fill = freq), colour = "white") + geom_text(aes(label = round(freq, 3))) + scale_fill_gradient(low = "white", high = "red")

# visualization of VehicleModelYear ~ PrimaryVehicleUsage by avg(LossRatio)
d8 = all %>% group_by(VehicleModelYear,PrimaryVehicleUsage) %>% summarize(mean=mean(LossRatio), n = n())
ggplot(data=d8,aes(x=factor(VehicleModelYear),y=PrimaryVehicleUsage)) + geom_tile(aes(fill = mean), colour = "white") + geom_text(aes(label = round(mean, 3))) + scale_fill_gradient(low = "white", high = "red")


# visualization of VehicleModelYear ~ VehicleID by % of drivers who made claim
d9 = data.frame(summarize(group_by(all,VehicleModelYear,VehicleID,lr),n = n()) %>% mutate(freq=n/sum(n)))
d9 = d9[d9$lr == FALSE,]
ggplot(data=d9,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill = freq), colour = "white") + geom_text(aes(label = round(freq, 3))) + scale_fill_gradient(low = "white", high = "red")

# visualization of VehicleModelYear ~ VehicleID by avg(LossRatio)
d10 = all %>% group_by(VehicleModelYear,VehicleID) %>% summarize(mean=mean(LossRatio), n = n())
ggplot(data=d10,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill = mean), colour = "white") + geom_text(aes(label = round(mean, 3))) + scale_fill_gradient(low = "white", high = "red")


d10 = all %>% group_by(VehicleModelYear,VehicleID) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d10,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = round(lr, 2))) + scale_fill_gradient(low = "white", high = "red")


ggplot(data=d10,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill =n), colour = "white") + scale_fill_gradient(low = "black", high = "white")
ggplot(data=d10,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill =lr), colour = "white") + scale_fill_gradient(low = "white", high = "chartreuse1")


p <- ggplot(all, aes(jitter(Percent25to40), jitter(AvgPrice)))
p + geom_point(aes(colour = factor(lr)))

p <- ggplot(all, aes(jitter(Percent25to40), jitter(AvgPrice)))
p + geom_point(aes(colour = factor(lr)))

ggplot(data=d10,aes(x=factor(VehicleModelYear),y=VehicleID)) + geom_tile(aes(fill =lr), colour = "white") + scale_fill_gradient(low = "white", high = "red") + geom_tile(aes(fill =n), colour = "white")+ scale_fill_gradient(low = "black", high = "white")

ggplot(data=d10,aes(x=factor(Population15to25),y=MaritalStatus)) + geom_tile(aes(fill =lr), colour = "white") + scale_fill_gradient(low = "white", high = "red") + geom_tile(aes(fill =n), colour = "white")+ scale_fill_gradient(low = "black", high = "white")

##SEND HELP I AM DISORGANIZED
d11 = all %>% group_by(PrimaryVehicleUsage,MaritalStatus) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d11,aes(x=factor(PrimaryVehicleUsage),y=MaritalStatus)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")

##SEND HELP I AM DISORGANIZED
d12 = all %>% group_by(PrimaryVehicleUsage,age) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d12,aes(x=factor(PrimaryVehicleUsage),y=age)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")

#First derive age
all$DateOfBirth[all$DateOfBirth=="NULL"] = NA
all$DateOfBirth = as.Date(all$DateOfBirth,format="%m/%d/%Y")
all$DateOfBirth[is.na(all$DateOfBirth)] = Sys.Date()
all$age = age_calc(all$DateOfBirth,units="years")
all$age[all$age==0] = NA

#age category
all$agecat <- NA
all$agecat[all$age<=77] <- "49-77"
all$agecat[all$age<=48] <- "37-48"
all$agecat[all$age<=36] <-"28-36"
all$agecat[all$age<=27] <-"0-27"

##SEND HELP I AM DISORGANIZED
d13 = all %>% group_by(PrimaryVehicleUsage,agecat) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d13,aes(x=factor(PrimaryVehicleUsage),y=agecat)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")

d13 = all %>% group_by(Accidents,agecat) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d13,aes(x=factor(Accidents),y=agecat)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")

d13 = all %>% group_by(Violations,agecat) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d13,aes(x=factor(Violations),y=agecat)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")

d13 = all %>% group_by(Gender,agecat) %>% summarize(sumPremium=sum(Premium), sumClaims = sum(ClaimsAmount), lr = sum(ClaimsAmount)/sum(Premium), n = n())
ggplot(data=d13,aes(x=factor(Gender),y=agecat)) + geom_tile(aes(fill =lr), colour = "white") + geom_text(aes(label = paste("LR: ",round(lr, 2),"\ncount: ", n))) + scale_fill_gradient(low = "white", high = "red")
