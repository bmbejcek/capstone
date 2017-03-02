

#CLEAN THE DRIVER TABLE
driver = read.table('../asCSV/driver.csv',sep=",",header=TRUE)
levels(driver$Miles.to.work) = c(levels(driver$Miles.to.work),"NULL")
levels(driver$Date.of.Birth) = c(levels(driver$Date.of.Birth),"NULL")

driver$Miles.to.work[which(driver$Miles.to.work=='Err:512')] = "NULL"
driver$Date.of.Birth[which(driver$Date.of.Birth==" ")] = "NULL"

#Grabbing Vehicle ID from the risk table and putting it into the driver table.
risk = read.table('../asCSV/risk.csv',sep=",",header=TRUE)
risk.sub = data.frame(Driver.ID = risk$Driver.ID, Vehicle.ID = risk$Vehicle.ID,Vehicle.Model.Year = risk$Vehicle.Model.Year)
driver.with.vehID = merge(driver,risk.sub,by = "Driver.ID",all.x="TRUE")
driver.with.vehID = driver.with.vehID[,-12]
driver.with.vehID$Vehicle.ID[which(is.na(driver.with.vehID$Vehicle.ID))] = "NULL"
write.csv(driver.with.vehID,"../asCSV/driverAdj.csv",row.names=FALSE)

#Create the DriverVehicle table
write.csv(risk.sub,"../asCSV/driverVehicle.csv",row.names=FALSE)



#CLEAN THE VEHICLE TABLE
vehicle = read.table('../asCSV/vehicle.csv',sep=",",header=TRUE)
vehicle$Average.Price = gsub(vehicle$Average.Price,pattern=",| |\\$",replacement="")





write.csv(vehicle,"../asCSV/vehicleAdj.csv",row.names=FALSE)

#MERGE THE LOCATION AND POPULATION DISTRIBUTION TABLES (And do some cleaning)
location = read.table('../asCSV/location.csv',sep=",",header=TRUE)
popdist = read.table('../asCSV/popdist.csv',sep=",",header=TRUE)
result=merge(location,popdist,by="ZIP")
result = result[,-7]
result$Pop015 = result$Population...0.15*10E-3
result$Pop1525 = result$Population...15.25*10E-3
result$Pop2540 = result$Population...25.40*10E-3
result$Pop50 = result$Population...50.*10E-3
result = result[,c(-7:-10)]
result[,6] = gsub(result[,6],pattern=",",replacement="")
names(result) = c("ZIP","LocationID","City","State","County","Population",names(result)[7:length(names(result))])
write.csv(result,"../asCSV/locationAdj.csv",row.names=FALSE)



#CLEAN THE RISK TABLE
risk = risk[,c(-5)] #take out the vehicle model year, which is now stored in driverVeh
write.csv(risk,"../asCSV/riskAdj.csv",row.names=FALSE)




