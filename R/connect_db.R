## Requirements

if (!require('devtools')) install.packages('devtools')
if (!require('remotes')) install.packages('remotes')
if (!require("DBI")) install.packages("DBI")
remotes::install_github("r-dbi/RPostgres")

library(RPostgres)

# Set environmental variables
dsn_database = "nmRanalysis"
dsn_hostname = "localhost"
dsn_port = "5432"
dsn_uid = "developer"
dsn_pwd = "developer"

load(file = 'data/bmse_associations.rda')
test <- subset(bmse_associations[1,])
test <- cbind(id=1,test)
test <- as.data.frame(test)

# Connect to Database
tryCatch({
  #drv <- RPostgres::Postgres()
  print("Connecting to Database…")
  connec <- dbConnect(RPostgres::Postgres(),
                      dbname = dsn_database,
                      host=dsn_hostname,
                      port=dsn_port,
                      user=dsn_uid,
                      password=dsn_pwd)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})


# Query a TABLE
df <- dbGetQuery(connec, "SELECT * FROM bmse_associations")

# Create a new table
dbCreateTable(connec, "bmse_associations", bmse_associations)

#Append to a existing table
dbAppendTable(conn = connec,
              name = SQL("bmse_associations"),
              value = bmse_associations,
              copy = NULL,
              row.names = NULL)


#Insert row by row
##### NOT NECESSARY #####
for(i in seq(nrow(bmse_associations))) {
  UPDATE_LINE <- sprintf("INSERT INTO bmse_associations (id, entryID, CASno, Field_strength, Solute, Solvent, Reference, pH, Temperature, Concentration) VALUES ('%i','%s','%s','%i','%s','%s','%s','%s','%i','%s');",
                         i, bmse_associations$Entry_ID[i], bmse_associations$CASno[i], bmse_associations$Field_strength[i],
                         bmse_associations$Solute[i], bmse_associations$Solvent[i], bmse_associations$Reference[i],
                         bmse_associations$pH[i], bmse_associations$Temperature[i], bmse_associations$Concentration[i])
  print(UPDATE_LINE)
  #D_MASTER_UPDATE <- dbGetQuery(conn = connec, statement = UPDATE_LINE)
}

dbGetQuery(conn = connec, statement = "INSERT INTO bmse_associations (id, entryID, CASno, Field_strength, Solute, Solvent, Reference, pH, Temperature, Concentration) VALUES ('10','bmse000005','53624-78-5','500','AMP','D2O','DSS','7.4','298','100mM');")



#############################################################################


# function for updating from user specified table in app UI

#dbCreateTable(connec, "profiling_parameters", User_defined_params)

dbAppendTable(conn = connec,
              name = SQL("profiling_parameters"),
              value = User_defined_params,
              copy = NULL,
              row.names = NULL)

dbDisconnect(connec)
