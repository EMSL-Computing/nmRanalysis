#' Connect the Reference Database to the app session.
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#'
#' @details This main function establishes a connection to a PostgreSQL database containing reference an user defined data
#' upon application launch. The connection must be established before any referencing can take place.
#'
#' @import shiny
#' @import RPostgres
#'
connect_db <- function(){
    # Set environmental variables

    dsn_database = "nmRanalysis"
    dsn_port = "5432"
    dsn_hostname = "localhost"
    dsn_uid = "developer"
    dsn_pwd = "developer"

    #dsn_database = Sys.getenv(c("DATABASE_NAME"))
    #dsn_hostname = Sys.getenv(c("DATABASE_HOST"))
    #dsn_port = Sys.getenv(c("DATABASE_PORT"))
    #dsn_uid = Sys.getenv(c("DATABASE_USER"))
    #dsn_pwd = Sys.getenv(c("DATABASE_PASSWORD"))

    # Connect to Database
    tryCatch({
      #drv <- RPostgres::Postgres()
      print("Connecting to Database…")
      connec <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = dsn_database,
                          host=dsn_hostname,
                          port=dsn_port,
                          user=dsn_uid,
                          password=dsn_pwd)
      print("Database Connected!")
    },
    error=function(cond) {
      print("Unable to connect to Database.")
      print("Here's the original error message:")
      print(cond)
    })
    return(connec)
}


#' Create a table in the Reference Database.
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#' @param db_connection The established connection from connect_db()
#' @param table_name The name of a table you wish to create.
#' @param df_object The data frame object you wish to structure the table as
#'
#' @details This function creates a table in the database based on an existing data frame. The columns of
#' the data frame will become the fields in the table created on the database.
#'
#' @import shiny
#' @import RPostgres
#'
create_new_table <- function(db_connection, table_name, df_object){
  DBI::dbCreateTable(db_connection, toString(table_name), df_object)
}


#' Update a table in the Reference Database
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#' @param db_connection The established connection from connect_db()
#' @param table_name The name of a table you wish to append to.
#' @param df_object The data frame object to append to an existing table. Must contain the same columns as
#' the table being appended to.
#'
#' @details This function appends to a table in the database a specified existing data frame. The columns of
#' the data frame must be sure to match the fields in the table on the database.
#'
#' @import shiny
#' @import RPostgres
#'
append_table <- function(db_connection, table_name, df_object){
  DBI::dbAppendTable(conn = db_connection,
                name = DBI::SQL(toString(table_name)),
                value = df_object,
                copy = NULL,
                row.names = NULL)
}

#' Query a table in the Reference Database
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#' @param db_connection The established connection from connect_db()
#' @param table_name The name of a table you wish to query to.
#'
#' @details This function a table in the database and returns a data frame object
#'
#' @import shiny
#' @import RPostgres
#'
query_table <- function(db_connection, table_name){
  name <- deparse(substitute(table_name))
  SQLstring <- paste("SELECT * FROM ", name)
  df <- DBI::dbGetQuery(db_connection, SQLstring)
  return(df)
}

## Requirements DBI and RPostgres
# Test variables
load(file = 'data/bmse_associations.rda')
test <- subset(bmse_associations[1,])
test <- cbind(id=1,test)
test <- as.data.frame(test)
user.name <- paste0("user name is ",Sys.getenv(c("SHINYPROXY_USERNAME")))
