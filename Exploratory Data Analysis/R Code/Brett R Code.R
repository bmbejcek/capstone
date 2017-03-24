library("RSQLite")
library("dplyr")


# connect to the sqlite file
con = dbConnect(drv=RSQLite::SQLite(), dbname="../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# CODE FOR JOINING SQL TABLES TOGETHER
# EVENTUALLY THIS WAS COMPLETED USING SQLITE3
# THIS IS JUST HERE FOR DOCUMENTATION ON HOW WE STARTED IT
# # get all tables
# agency = dbGetQuery( con,'select * from agency' )
# 
# claim = dbGetQuery( con,'select * from claim' ) #need to sum across driver ID
# summed_claim = dbGetQuery( con,'select DriverID, PolicyID, VehicleID, sum(Amount) as SumAmount from claim group by DriverID' )
# 
# company = dbGetQuery( con,'select * from company' )
# driver = dbGetQuery( con,'select * from driver' )
# driverveh = dbGetQuery( con,'select * from driverveh' )
# location = dbGetQuery( con,'select * from location' )
# policy = dbGetQuery( con,'select * from policy' )
# risk = dbGetQuery( con,'select * from risk' )
# vehicle = dbGetQuery( con,'select * from vehicle' )
# 
# agg1 = dbGetQuery( con,'select risk.DriverID, risk.PolicyID, risk.VehicleID, risk.Premium, s.Claims from risk left join 
#                    (select DriverID, PolicyID, VehicleID, sum(Amount) as Claims from claim group by DriverID) as s
#                    on risk.driverID = s.driverID' )


all = dbGetQuery( con,'select * from All_DATA' )
all$ClaimsAmount[is.na(all$ClaimsAmount)] <- 0
##TEST FOR GGMAP

map <- get_googlemap('california', scale = 2)

allDat$lr <- allDat$LossRatio < 1.0

p <- ggplot(all, aes(jitter(Violations), jitter(AgencyID)))
p + geom_point(aes(colour = factor(lr)))

ggplot(all,aes(x=as.factor(Violations),y=as.factor(AgencyID)))+geom_bar()+facet_grid(as.factor(Violations)~as.factor(AgencyID))

p <- ggplot(allDat, aes(age, Gender))
p + geom_point(aes(colour = factor(lr)))

scatter3D(all$Premium, all$AvgPrice, all$Population, colvar = all$lr, theta = 100, phi = 5)
scatter3D(all$Percent0to15, all$Percent15to25, all$Percent25to40, colvar = all$lr, col = c(rgb(0.9,0.2,0.24,alpha=1.0), rgb(0.2,0.2,0.2,alpha=0.1)), theta = 60, phi = 45)

d = data.frame(summarize(group_by(all,Violations,AgencyID,lr),
              n = n()) %>% mutate(freq=n/sum(n)))

t = ggplot(data=d,aes(x=lr,y=freq))+geom_bar(stat="identity",aes(fill=factor(lr)))+facet_grid(factor(Violations) ~ factor(AgencyID))
t

