setGeneric(
  name = "sendGoAlongPairsToDB",
  def = function(dataframe,dataSourceInfo,tablename)
  {
    loadPackages()
    standardGeneric("sendGoAlongPairsToDB")
  }
)

setMethod(
  f = "sendGoAlongPairsToDB",
  signature = c("list","DataSourceInfo","character"),
  definition = function(dataframe,dataSourceInfo,tablename)
  {
    require("RPostgreSQL")


    partnerframe <- data.frame(dataframe)

    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dataSourceInfo@db,
                     host = dataSourceInfo@host, port = dataSourceInfo@port,
                     user = dataSourceInfo@user, password = dataSourceInfo@password)
    on.exit(dbDisconnect(con))

    if(!dbExistsTable(con, tablename)){
      sql_command <- paste("CREATE TABLE", tablename,"
                           (
                           begintime timestamp with time zone, endtime timestamp with time zone,id1 text, id2 text
                           )
                           WITH (
                           OIDS=FALSE
                           );
                           ")
# sends the command and creates the table
dbGetQuery(con, sql_command)

}

dbWriteTable(con, tablename,
value = partnerframe, append = TRUE, row.names = FALSE)

##dbDisconnect(con)
  }
)


setMethod(
  f = "sendGoAlongPairsToDB",
  signature = c("list","PostgreSQLConnection","character"),
  definition = function(dataframe,dataSourceInfo,tablename)
  {
    require("RPostgreSQL")


    partnerframe <- data.frame(dataframe)


    con <- dataSourceInfo

    if(!dbExistsTable(con, tablename)){
      sql_command <- paste("CREATE TABLE", tablename,"
                           (
                           begintime timestamp with time zone, endtime timestamp with time zone,id1 text, id2 text
                           )
                           WITH (
                           OIDS=FALSE
                           );
                           ")
# sends the command and creates the table
dbGetQuery(con, sql_command)

}

dbWriteTable(con, tablename,
value = partnerframe, append = TRUE, row.names = FALSE)

##dbDisconnect(con)
  }
)
