library("RSQLite")
library(maps)
library(mapdata)

# connect to the sqlite file
con = dbConnect(drv=RSQLite::SQLite(), dbname="../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# CREATING SINGLE CSV WAS DONE IN SQLITE
# THE R CODE IS JUST HERE FOR DOCUMENTATION
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
all$LossRatio <- all$ClaimsAmount/all$Premium

#GET LATITUDE AND LONGITUDES
loc = dbGetQuery( con,'select * from LOCATION' )
loc <- loc[1]
loc$lat <- 0
loc$lon <- 0
for(i in 1:nrow(loc))
{
  geo <- geocode(toString(loc$ZipCode[i]))
  lon <- toString(geo[1])
  loc$lon[i] <- as.double(lon)
  lat <- toString(geo[2])
  loc$lat[i] <- as.double(lat)
  print(i)
}

all_loc <- merge(x = all, y = zipcode, by = "ZipCode", all.x = TRUE, sort = FALSE)
#write.csv(all_loc, "../../../LatLong.csv")

all_loc$aboveOne <- (all_loc$LossRatio>=1.0)

#GGMAP

CaliMap <- qmap("California", zoom = 6, maptype="toner")

CaliMap + 
  geom_point(aes(x = LNG, y = LAT),
             data = all_loc[all_loc$ClaimsAmount==0,], alpha = .05, color = "deepskyblue",
             position=position_jitter(w = 0.08, h = 0.08)) + 
  geom_point(aes(x = LNG, y = LAT),
           data = all_loc[all_loc$ClaimsAmount>0,], alpha = .05, color = "red",
           position=position_jitter(w = 0.08, h = 0.08))

CaliMap + 
  geom_point(aes(x = LNG, y = LAT),
             data = all_loc[all_loc$LossRatio < 1,], alpha = .05, color = "deepskyblue",
             position=position_jitter(w = 0.08, h = 0.08)) + 
  geom_point(aes(x = LNG, y = LAT),
             data = all_loc[all_loc$LossRatio > 1,], alpha = .1, color = "red",
             position=position_jitter(w = 0.08, h = 0.08))
library(tree)
tree1 = tree(as.factor(aboveOne) ~ Violations+Accidents+MaritalStatus+Gender+MilesToWork+PrimaryVehicleUsage+AvgPrice+UnderwritingAgencyName+Name,
                   data = all_loc)
summary(tree1)

library(randomForest)
forest1 = randomForest(as.factor(aboveOne) ~ Violations+Accidents+MaritalStatus+Gender+MilesToWork+PrimaryVehicleUsage+AvgPrice+UnderwritingAgencyName+Name,
                       data = all_loc)
print(forest1) 

model = lm(LossRatio ~ Violations+Accidents+MaritalStatus+Gender+MilesToWork+PrimaryVehicleUsage+AvgPrice+UnderwritingAgencyName+Name,
             data = all_loc[all_loc$LossRatio > 0,])
summary(model)

model2 <- glm(aboveOne ~ Violations+Accidents+MaritalStatus+Gender+MilesToWork+PrimaryVehicleUsage+AvgPrice+UnderwritingAgencyName+Name,
    data = all_loc)
