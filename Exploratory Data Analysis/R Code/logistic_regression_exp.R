library("RSQLite")

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all data table
data = dbGetQuery(con, 'select * from all_data')

# claims indicator variable
data$Claims <- as.integer(!is.na(data$ClaimsAmount))

# logistic regression
lr.model <- glm(Claims ~ Premium + factor(Violations) + factor(Accidents) +
                  MaritalStatus + Gender + MilesToWork + PrimaryVehicleUsage +
                  Population + Make + AvgPrice + factor(AgencyID) +
                  factor(CompanyID),
                data = data,
                family = binomial(link = "logit"))
summary(lr.model)

lr.model.2 <- glm(Claims ~ Premium + factor(VehicleID),
                  data = data,
                  family = binomial(link = "logit"))
summary(lr.model.2)





