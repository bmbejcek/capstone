
# --- IMPORT LIBRARIES --- #

library("RSQLite")
library("ggplot2")



# --- IMPORT DATA --- #

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all data table
data = dbGetQuery(con, 'select * from all_data')



# --- HELPER FUNCTIONS --- #

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



# --- MODIFY DATA --- #

# pricecat
data$pricecat <- factor(numeric(nrow(data)),
                        levels = c("11-22k", "23-39k", "40-80k", "81-166k"))
data$pricecat[(data$AvgPrice <= 22000)] <- "11-22k"
data$pricecat[(23000 <= data$AvgPrice) &
                    (data$AvgPrice <= 39000)] <- "23-39k"
data$pricecat[(40000 <= data$AvgPrice) &
                    (data$AvgPrice <= 80000)] <- "40-80k"
data$pricecat[(81000 <= data$AvgPrice)] <- "81-166k"

# f.Percent25to40
data$f.Percent25to40 <- factor(numeric(nrow(data)),
                               levels = c("0-25", "25-32", "32-40", "40-60"))
data$f.Percent25to40[data$Percent25to40 < 0.25] <- "0-25"
data$f.Percent25to40[(0.25 <= data$Percent25to40) &
                       (data$Percent25to40 < 0.32)] <- "25-32"
data$f.Percent25to40[(0.32 <= data$Percent25to40) &
                       (data$Percent25to40 < 0.4)] <- "32-40"
data$f.Percent25to40[0.4 <= data$Percent25to40] <- "40-60"



# --- PLOTS --- #

loss.ratio(data, "pricecat")
loss.ratio(data, "f.Percent25to40")
table(data$pricecat, data$f.Percent25to40)



