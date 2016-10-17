setGeneric(
  name = "sendPartnerPairsToDB",
  def = function(plist,dataSourceInfo,tablename)
  {
    loadPackages()
    standardGeneric("sendPartnerPairsToDB")
  }
)

setMethod(
  f = "sendPartnerPairsToDB",
  signature = c("list","DataSourceInfo","character"),
  definition = function(plist,dataSourceInfo,tablename)
  {
    require("RPostgreSQL")

    partner1<-list()
    partner2<-list()
    for(i in 1:length(plist)){
      partner1<-c(plist[[i]][1])
      partner2<-c(plist[[i]][2])
    }
    partnerList<-(cbind(p1=partner1,p2=partner2))
partnerframe <- data.frame(partnerList)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dataSourceInfo@db,
host = dataSourceInfo@host, port = dataSourceInfo@port,
user = dataSourceInfo@user, password = dataSourceInfo@password)
if(!dbExistsTable(con, tablename)){
  sql_command <- paste("CREATE TABLE", tablename,"
(
partner1 numeric, partner2 numeric
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


  }
)
