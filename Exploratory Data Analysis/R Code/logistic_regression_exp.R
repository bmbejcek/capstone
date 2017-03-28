library("RSQLite")
library("eeptools")

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all data table
data = dbGetQuery(con, 'select * from all_data')

# derive age
data$DateOfBirth[data$DateOfBirth=="NULL"] = NA
data$DateOfBirth = as.Date(data$DateOfBirth,format="%m/%d/%Y")
data$DateOfBirth[is.na(data$DateOfBirth)] = Sys.Date()
data$age = age_calc(data$DateOfBirth,units="years")
data$age[data$age==0] = NA

# claims indicator variable
data$Claims <- as.integer(!is.na(data$ClaimsAmount))

# logistic regression
# lr.model <- glm(Claims ~ Premium + factor(Violations) + factor(Accidents) +
#                   MaritalStatus + Gender + MilesToWork + PrimaryVehicleUsage +
#                   Population + Make + AvgPrice + factor(AgencyID) +
#                   factor(CompanyID),
#                 data = data,
#                 family = binomial(link = "logit"))
# summary(lr.model)
# 
# lr.model.2 <- glm(Claims ~ Premium + factor(VehicleID),
#                   data = data,
#                   family = binomial(link = "logit"))
# summary(lr.model.2)

lr.model.full <- glm(Claims ~ Premium + Violations + Accidents +
                       MaritalStatus + Gender + MilesToWork +
                       PrimaryVehicleUsage + age + County + Population +
                       Percent0to15 + Percent15to25 + AvgPrice +
                       UnderwritingAgencyName + Name,
                     data = data,
                     family = binomial(link = "logit"))
summary(lr.model.full)

lr.model.3 <- glm(Claims ~ Premium + Violations + Accidents +
                    MaritalStatus + Gender + MilesToWork +
                    PrimaryVehicleUsage + age * age + Population +
                    Percent0to15 + Percent15to25 + Percent25to40 +
                    AvgPrice + Gender * MaritalStatus + Gender * age +
                    Violations * Accidents * MaritalStatus +
                    PrimaryVehicleUsage * Gender,
                  data = data,
                  family = binomial(link = "logit"))
summary(lr.model.3)

# setup regression covariates
data.reg <- data[,c("Claims", "Premium", "Violations", "Accidents",
                    "MaritalStatus", "Gender", "MilesToWork",
                    "PrimaryVehicleUsage", "County", "Population",
                    "Percent0to15", "Percent15to25", "Percent25to40",
                    "AvgPrice", "UnderwritingAgencyName", "Name")]
data.reg <- data.reg[!apply(data.reg, 1, anyNA),]


# forward and backward selections
library(MASS)
null <- glm(Claims ~ Premium,
            data = data.reg,
            family = binomial(link = "logit"))
full <- glm(Claims ~ Premium + (Violations + Accidents +
              MaritalStatus + Gender + MilesToWork +
              PrimaryVehicleUsage + Population +
              Percent0to15 + Percent15to25 + Percent25to40 +
              AvgPrice)^2,
            data = data.reg,
            family = binomial(link = "logit"))

full.2 <- glm(Claims ~ Premium + (Violations + Accidents +
                MaritalStatus + Gender + MilesToWork +
                PrimaryVehicleUsage)^2,
              data = data.reg,
              family = binomial(link = "logit"))

# AIC forwards
stepAIC(null, scope = list(lower = null, upper = full),
        direction = "backward", k = 2)

# AIC backwards
stepAIC(full, direction = "backward", k = 2)



