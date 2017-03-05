# Import libraries
options(java.parameters = "-Xmx4g")
library(XLConnect)

# Set workbook location
setwd(paste("/Users/iowner/OneDrive/School/Spring17/Capstone/capstone/",
      "Exploratory Data Analysis/Data", sep = ""))
wb <- loadWorkbook("insurance.xlsx")

# Risk Fact
col.types.1 <- c("character", "character", "character", "character", "numeric",
               "character", "numeric")
risk <- readWorksheet(wb, sheet = 3, startRow = 1, startCol = 1,
                           endRow = 35713, endCol = 7, header = TRUE,
                           colTypes = col.types.1)
risk$Location.ID <- trimws(risk$Location.ID)

# Driver Dimension
col.types.2 <- c("character", "character", "character", "numeric", "numeric",
               "character", "character", "numeric", "character", "character")
driver <- readWorksheet(wb, sheet = 4, startRow = 1, startCol = 1,
                        endRow = 43653, endCol = 10, header = TRUE,
                        colTypes = col.types.2)
driver$Gender <- as.factor(driver$Gender)
driver$Marital.Status <- as.factor(driver$Marital.Status)
driver$Primary.Vehicle.Use <- as.factor(driver$Primary.Vehicle.Use)

# Claims Fact
col.types.3 <- c("character", "character", "character", "character", "numeric",
                 "character")
claims <- readWorksheet(wb, sheet = 5, startRow = 1, startCol = 1,
                        endRow = 1355, endCol = 6, header = TRUE,
                        colTypes = col.types.3)

# Policy Dimension
col.types.4 <- c("character", "character", "character", "character",
                 "character", "character")
policy <- readWorksheet(wb, sheet = 6, startRow = 1, startCol = 1,
                        endRow = 30001, endCol = 6, header = TRUE,
                        colTypes = col.types.4)

# Population Distribution
col.types.5 <- c("character", "numeric", "numeric", "numeric", "numeric",
                 "numeric")
population <- readWorksheet(wb, sheet = 7, startRow = 1, startCol = 1,
                            endRow = 601, endCol = 6, header = TRUE,
                            colTypes = col.types.5)

# Location Dimension
col.types.6 <- c("character", "character", "character", "character",
                 "character", "numeric")
location <- readWorksheet(wb, sheet = 8, startRow = 1, startCol = 1,
                          endRow = 601, endCol = 6, header = TRUE,
                          colTypes = col.types.6)

# Vehicles Dimension
col.types.7 <- c("character", "character", "character", "character")
vehicles <- readWorksheet(wb, sheet = 9, startRow = 1, startCol = 1,
                          endRow = 88, endCol = 4, header = TRUE,
                          colTypes = col.types.7)

# Agency Dimension
col.types.8 <- c("character", "character", "character", "character")
agency <- readWorksheet(wb, sheet = 10, startRow = 1, startCol = 1,
                        endRow = 21, endCol = 4, header = TRUE,
                        colTypes = col.types.8)

# Company Dimension
col.types.9 <- c("character", "character", "character", "character")
company <- readWorksheet(wb, sheet = 11, startRow = 1, startCol = 1,
                         endRow = 8, endCol = 4, header = TRUE,
                         colTypes = col.types.9)

# Merge claims and gender (test)
clm <- claims[1:5, c("Claim.ID", "Claimant.Id", "Claim.Amount")]
colnames(clm) <- c("Claim.ID", "Driver.ID", "Claim.Amount")
drv <- driver[,c("Driver.ID", "Gender")]
clm.gender <- merge(clm, drv, by = "Driver.ID", all.x = TRUE)

# Merge claims and gender
claims.sub <- claims[,c("Claim.ID", "Claimant.Id", "Claim.Amount")]
colnames(claims.sub) <- c("Claim.ID", "Driver.ID", "Claim.Amount")
driver.sub <- driver[,c("Driver.ID", "Gender")]
claims.gender <- merge(claims.sub, driver.sub, by = "Driver.ID", all.x = TRUE)

# Gender losses
m.losses <- sum(claims.gender$Claim.Amount[claims.gender$Gender == "M"])
f.losses <- sum(claims.gender$Claim.Amount[claims.gender$Gender == "F"])

# Merge risk and gender
risk.sub <- risk[,c("Record.ID", "Driver.ID", "Premium")]
driver.sub <- driver[,c("Driver.ID", "Gender")]
risk.gender <- merge(risk.sub, driver.sub, by = "Driver.ID", all.x = TRUE)

# Gender premiums
m.premiums <- sum(risk.gender$Premium[risk.gender$Gender == "M"])
f.premiums <- sum(risk.gender$Premium[risk.gender$Gender == "F"])

# Gender loss ratios
m.loss.ratio <- m.losses / m.premiums
f.loss.ratio <- f.losses / f.premiums

# Merge claims and marital status
claims.sub <- claims[,c("Claim.ID", "Claimant.Id", "Claim.Amount")]
colnames(claims.sub) <- c("Claim.ID", "Driver.ID", "Claim.Amount")
driver.ms <- driver[,c("Driver.ID", "Marital.Status")]
claims.ms <- merge(claims.sub, driver.ms, by = "Driver.ID", all.x = TRUE)

# Marital status losses
ms.d.losses <- sum(claims.ms$Claim.Amount[claims.ms$Marital.Status == "D"])
ms.m.losses <- sum(claims.ms$Claim.Amount[claims.ms$Marital.Status == "M"])
ms.s.losses <- sum(claims.ms$Claim.Amount[claims.ms$Marital.Status == "S"])

# Merge risk and marital status
risk.sub <- risk[,c("Record.ID", "Driver.ID", "Premium")]
driver.ms <- driver[,c("Driver.ID", "Marital.Status")]
risk.ms <- merge(risk.sub, driver.ms, by = "Driver.ID", all.x = TRUE)

# Marital status premiums
ms.d.premiums <- sum(risk.ms$Premium[risk.ms$Marital.Status == "D"])
ms.m.premiums <- sum(risk.ms$Premium[risk.ms$Marital.Status == "M"])
ms.s.premiums <- sum(risk.ms$Premium[risk.ms$Marital.Status == "S"])

# Marital status loss ratio
ms.d.loss.ratio <- ms.d.losses / ms.d.premiums
ms.m.loss.ratio <- ms.m.losses / ms.m.premiums
ms.s.loss.ratio <- ms.s.losses / ms.s.premiums

# Loss ratio by factor function
factor.loss.ratio <- function(claims, driver, col.name) {
  claims.sub <- claims[c("Claim.ID", "Claimant.Id", "Claim.Amount")]
  colnames(claims.sub) <- c("Claim.ID", "Driver.ID", "Claim.Amount")
  driver.sub <- driver[,c("Driver.ID", col.name)]
  claims.col <- merge(claims.sub, driver.sub, by = "Driver.ID", all.x = TRUE)
  
  risk.sub <- risk[,c("Record.ID", "Driver.ID", "Premium")]
  risk.col <- merge(risk.sub, driver.sub, by = "Driver.ID", all.x = TRUE)
  
  losses <- aggregate(claims.col$Claim.Amount,
                      by = list(Group = claims.col[,col.name]), FUN = sum)
  premiums <- aggregate(risk.col$Premium,
                        by = list(Group = risk.col[,col.name]), FUN = sum)
  
  result <- data.frame(Group = losses$Group, Losses = losses$x,
                       Premiums = premiums$x)
  result$Loss.Ratio <- result$Losses / result$Premiums
  return(result)
}

# Loss ratio by factors
gender.loss.ratio <- factor.loss.ratio(claims, driver, "Gender")
ms.loss.ratio <- factor.loss.ratio(claims, driver, "Marital.Status")
primary.loss.ratio <- factor.loss.ratio(claims, driver, "Primary.Vehicle.Use")

driver$Number.of.Accidents <- as.factor(driver$Number.of.Accidents)
driver$Number.of.Violations <- as.factor(driver$Number.of.Violations)

acc.loss.ratio <- factor.loss.ratio(claims, driver, "Number.of.Accidents")
vio.loss.ratio <- factor.loss.ratio(claims, driver, "Number.of.Violations")



