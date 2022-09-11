library(dplyr)
library(DBI)
library(RPostgreSQL)


# Read csv files

inter_player <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/international_box_player_season.csv") %>% as.data.frame()
nba_player <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/nba_box_player_season.csv") %>% as.data.frame()
player_lst <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/player.csv")%>% as.data.frame()


# Set up my PostgreSQL Credentials

kapp_database <- "kings_application_db"
kapp_hostname <- "kings-application-db.cjvfe0oplbqc.us-west-1.rds.amazonaws.com"
kapp_port <- 5432
kapp_uid <- "chen"
kapp_pwd <- "iwnzw"

# Establish R and PostgreSQL Connection

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  connec <- dbConnect(drv, 
                      dbname = kapp_database,
                      host = kapp_hostname, 
                      port = kapp_port,
                      user = kapp_uid, 
                      password = kapp_pwd)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})

# Check all the schema

schema <- dbGetQuery(connec, "SELECT schema_name FROM information_schema.schemata")

# Import data from csv files

dbWriteTable(connec, c("chen", "playerinfo"), player_lst, row.names = FALSE, append = TRUE)
dbWriteTable(connec, c("chen", "euroleagues"), inter_player, row.names = FALSE, append = TRUE)
dbWriteTable(connec, c("chen", "nba"), nba_player, row.names = FALSE, append = TRUE)

# df1 <- dbGetQuery(connec, "SELECT * FROM playerinfo")

dbDisconnect(connec)


