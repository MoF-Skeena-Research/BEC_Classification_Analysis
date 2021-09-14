library(RPostgres)
library(data.table)
library(RPostgreSQL)

load("inputs/VegDat_Raw.RData")
vegData <- as.data.table(vegData)
setnames(vegData,c("plotno","species","lifeform","cover"))

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", host = "138.197.168.220",password = "postgres", port = 5432, dbname = "bec_master") ### for local use
dbWriteTable(con,"veg",vegData,row.names = F)

env <- fread("inputs/BECMaster19_Env.txt")
for (j in names(env)) set(env, i = grep("^$|^ $", env[[j]]), j = j, value = NA)
numNA <- env[, lapply(.SD, function(x) sum(is.na(x)))]
numNA <- as.matrix(numNA)
numNA <- numNA/nrow(env)
toRemove <- colnames(numNA)[numNA > 0.5]
env[,(toRemove) := NULL]
env <- env[!is.na(latitude) & !is.na(longitude),]

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

setnames(env,dbSafeNames(names(env)))
library(sf)
env[,longitude := abs(longitude) * -1]
envSf <- st_as_sf(env, coords = c("longitude","latitude"), crs = 4326, agr = "constant")
colnames(envSf)[43] <- "geom"
st_geometry(envSf) <- "geom"
st_write(envSf,con,"env")
