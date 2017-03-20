library("RSQLite")

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all data table
data = dbGetQuery(con, 'select * from all_data')

# premium as a function of (log) average vehicle price
boxplot(Premium ~ factor(round(log(AvgPrice), 2)), data = data)
abline(h = 220, col = "red")
abline(h = 240, col = "green")
abline(h = 260, col = "purple")

# claims and premium by vehicle id
df1 <- aggregate(data.frame(ClaimsAmount = data$ClaimsAmount,
                            Premium = data$Premium),
                 by = data.frame(VehicleID = data$VehicleID),
                 FUN = sum, na.rm = TRUE, na.action = NULL)

# number of claims and number of drivers by vehicle id
count <- function(a) {
  a <- a[!is.na(a)]
  result <- length(a)
  return(result)
}
df2 <- aggregate(data.frame(DriversWithClaims = data$ClaimsAmount,
                            Premiums = data$Premium),
                 by = data.frame(VehicleID = data$VehicleID),
                 FUN = count)

# loss ratio by vehicle
plot(df1$VehicleID, df1$ClaimsAmount / df1$Premium)

# driver claims percentage by vehicle
plot(df2$VehicleID, df2$DriversWithClaims / df2$Premiums)


