library("RSQLite")

# connect to the sqlite file
con = dbConnect(drv=RSQLite::SQLite(), dbname="../../data/CapstoneV1.db")

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

agg1 = dbGetQuery( con,'select risk.DriverID, risk.PolicyID, risk.VehicleID, risk.Premium, s.Claims from risk left join 
                   (select DriverID, PolicyID, VehicleID, sum(Amount) as Claims from claim group by DriverID) as s
                   on risk.driverID = s.driverID' )
