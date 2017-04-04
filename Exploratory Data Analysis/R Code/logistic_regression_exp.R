
# --- IMPORT LIBRARIES --- #

library("RSQLite")
library("eeptools")
library("MASS")



# --- IMPORT DATA --- #

# connect to the sqlite file
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
            "Exploratory Data Analysis/R Code", sep = ""))
con = dbConnect(drv = RSQLite::SQLite(), dbname = "../../data/CapstoneV1.db")

# get a list of all tables
alltables = dbListTables(con)

# get all data table
data = dbGetQuery(con, 'select * from all_data')



# --- MODIFY DATA --- #

# derive age
data$DateOfBirth[data$DateOfBirth=="NULL"] = NA
data$DateOfBirth = as.Date(data$DateOfBirth,format="%m/%d/%Y")
data$DateOfBirth[is.na(data$DateOfBirth)] = Sys.Date()
data$age = age_calc(data$DateOfBirth,units="years")
data$age[data$age==0] = NA

# claims indicator variable
data$Claims <- as.integer(!is.na(data$ClaimsAmount))

# no claims set to zero
data[is.na(data$ClaimsAmount), "ClaimsAmount"] <- 0

# setup regression covariates
data.reg <- data[,c("Claims", "Premium", "Violations", "Accidents",
                    "MaritalStatus", "Gender", "MilesToWork",
                    "PrimaryVehicleUsage", "County", "Population",
                    "Percent0to15", "Percent15to25", "Percent25to40",
                    "AvgPrice", "UnderwritingAgencyName", "Name",
                    "age")]

data.reg$age[is.na(data.reg$age)] <- 0
data.reg$agecat <- factor(numeric(nrow(data.reg)),
                          levels = c("1-27", "28-36", "37-48", "49-77", "NA"))
data.reg$agecat[data.reg$age == 0] <- "NA"
data.reg$agecat[(1 <= data.reg$age) & (data.reg$age < 28)] <- "1-27"
data.reg$agecat[(28 <= data.reg$age) & (data.reg$age < 37)] <- "28-36"
data.reg$agecat[(37 <= data.reg$age) & (data.reg$age < 49)] <- "37-48"
data.reg$agecat[(49 <= data.reg$age) & (data.reg$age <= 77)] <- "49-77"

data.reg$pricecat <- factor(numeric(nrow(data.reg)),
                            levels = c("11-22k", "23-39k", "40-80k", "81-166k"))
data.reg$pricecat[(data.reg$AvgPrice <= 22000)] <- "11-22k"
data.reg$pricecat[(23000 <= data.reg$AvgPrice) &
                    (data.reg$AvgPrice <= 39000)] <- "23-39k"
data.reg$pricecat[(40000 <= data.reg$AvgPrice) &
                    (data.reg$AvgPrice <= 80000)] <- "40-80k"
data.reg$pricecat[(81000 <= data.reg$AvgPrice)] <- "81-166k"
    
data.reg <- data.reg[!apply(data.reg, 1, anyNA),]

data.reg$f.Violations <- factor(data.reg$Violations)
data.reg$f.Accidents <- factor(data.reg$Accidents)



# --- MANUAL SELECTION --- #

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



# --- STEPWISE SELECTION --- #

# (1) null and full models
model.null <- glm(Claims ~ Premium,
                  data = data.reg,
                  family = binomial(link = "logit"))
model.full <- glm(Claims ~ Premium + (Violations + Accidents +
                    MaritalStatus + Gender + MilesToWork +
                    PrimaryVehicleUsage + Population +
                    Percent0to15 + Percent15to25 + Percent25to40 +
                    AvgPrice)^2,
                  data = data.reg,
                  family = binomial(link = "logit"))

# (1) AIC backwards
stepAIC(model.full, direction = "backward", k = 2)

# (1) result
model.10 <- glm(Claims ~ Premium + Violations + Accidents +
                  MaritalStatus + PrimaryVehicleUsage + Population +
                  Percent0to15 + Percent15to25 + Percent25to40 +
                  AvgPrice + Violations:Percent25to40 + Accidents:Population +
                  Accidents:Percent0to15 + MaritalStatus:Percent15to25 +
                  Percent25to40:AvgPrice,
                family = binomial(link = "logit"),
                data = data.reg)
summary(model.10)

# (2) initial model
init_mod <- glm(Claims ~ .,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "AvgPrice",
                  "UnderwritingAgencyName", "Name", "County")],
                family = binomial(link = "logit"))

# (2) AIC both
stepAIC(init_mod, scope = . ~ .^2, direction = "both")

# (2) result -- forgot binomial on first attempt
model.20 <- glm(Claims ~ Premium + f.Accidents + PrimaryVehicleUsage + 
                  Percent25to40 + AvgPrice + Premium:f.Accidents +
                  Percent25to40:AvgPrice,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "AvgPrice",
                  "UnderwritingAgencyName", "Name", "County")])
summary(model.20)

# (3) initial model
init_mod <- glm(Claims ~ .,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "AvgPrice",
                  "UnderwritingAgencyName", "Name", "County",
                  "agecat")],
                family = binomial(link = "logit"))

# (3) AIC both
stepAIC(init_mod, scope = . ~ .^2, direction = "both")

# (3) result
model.30 <- glm(Claims ~ Premium + PrimaryVehicleUsage + Percent25to40 + 
                  AvgPrice + Percent25to40:AvgPrice,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "AvgPrice",
                  "UnderwritingAgencyName", "Name", "County",
                  "agecat")],
                family = binomial(link = "logit"))
summary(model.30)

# (4) initial model
init_mod <- glm(Claims ~ .,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "pricecat",
                  "agecat")],
                family = binomial(link = "logit"))

# (4) AIC both
stepAIC(init_mod, scope = . ~ .^2, direction = "both")

# (4) result
model.40 <- glm(Claims ~ Premium + PrimaryVehicleUsage,
                data = data.reg[,c("Claims", "Premium", "f.Violations",
                  "f.Accidents", "MaritalStatus", "Gender", "MilesToWork",
                  "PrimaryVehicleUsage", "Population", "Percent0to15",
                  "Percent15to25", "Percent25to40", "pricecat",
                  "agecat")],
                family = binomial(link = "logit"))


