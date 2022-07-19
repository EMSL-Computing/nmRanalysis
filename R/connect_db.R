install.packages('RPostgreSQL')
library(RPostgreSQL)

dsn_database = "nmRanalysis"
dsn_hostname = "localhost"
dsn_port = "5432"
dsn_uid = "prym311"
dsn_pwd = "dbtest1"

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  connec <- dbConnect(drv,
                      dbname = dsn_database,
                      host = dsn_hostname,
                      port = dsn_port,
                      user = dsn_uid,
                      password = dsn_pwd)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})
