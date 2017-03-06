library("RSQLite")

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all tables
agency = dbGetQuery( con,'select * from agency' )

claim = dbGetQuery( con,'select * from claim' ) #need to sum across driver ID
summed_claim = dbGetQuery( con,'select DriverID, PolicyID, VehicleID, sum(Amount) as SumAmount from claim group by DriverID' )

company = dbGetQuery( con,'select * from company' )
driver = dbGetQuery( con,'select * from driver' )
driverveh = dbGetQuery( con,'select * from driverveh' )
location = dbGetQuery( con,'select * from location' )
policy = dbGetQuery( con,'select * from policy' )
risk = dbGetQuery( con,'select * from risk' )
vehicle = dbGetQuery( con,'select * from vehicle' )

# get all data table
data = dbGetQuery(con, 'select * from all_data')

# Loss ratio by column function
loss.ratio <- function(data, col.name) {
  losses <- aggregate(data$ClaimsAmount,
                      by = list(Group = data[,col.name]),
                      FUN = sum,
                      na.rm = TRUE, na.action = NULL)
  premiums <- aggregate(data$Premium,
                        by = list(Group = data[,col.name]),
                        FUN = sum,
                        na.rm = TRUE, na.action = NULL)
  result <- data.frame(Group = losses$Group,
                       Losses = losses$x,
                       Premiums = premiums$x)
  colnames(result)[1] <- col.name
  result$Loss.Ratio <- result$Losses / result$Premiums
  return(result)
}

# Loss ratio by factors

gender.loss.ratio <- loss.ratio(data, "Gender")
ms.loss.ratio <- loss.ratio(data, "MaritalStatus")
# and so on...
