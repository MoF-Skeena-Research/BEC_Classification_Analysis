install.packages("spatialEco")

library(pacman)
pacman::p_load(raster, tidyverse, spatialEco)

wd = "D:/2. PhD/SpatialAnalyses/Predictors_NS"
setwd(wd)

localDir = "D:/2. PhD/SpatialAnalyses/Predictors_NS"
data.directory = file.path(localDir, "data")

#########################################################
# Create heat load raster from DEM
#########################################################

# uses spatialEco package 

# Bring in provincial DEM

dem <- raster(file.path(data.directory, "NS_DEM_20m_NAD83_UTMz20_CSRS.tif")) # this is the 20 m provincial DEM 
plot(dem)

heat.load <- hli(dem)
plot(heat.load, main="Heat Load Index") 

compareRaster(heat.load, dem) # compare rasters = TRUE = all good!

writeRaster(heat.load,(file.path(data.directory,"hli.tif")),overwrite=TRUE) # save


###################################################################################
# Create heat load raster from field collected measures of slope, aspect, latitude
###################################################################################

# uses formula
# code for formula is in heatload.r from this website:
# https://www.davidzeleny.net/anadat-r/doku.php/en:customized_functions:heatload

# create headload function with code as above
heatload <- function (aspect, slope, latitude, method = 'heatload', units = 'degrees', equation = 1)
{
  if (units == 'degrees')   # convert degrees to radians
  {
    aspect <- aspect/180*pi
    slope <- slope/180*pi
    aspect[slope == 0] <- 0
    latitude <- latitude/180*pi
  }  
  A <- if (method == 'heatload') abs (pi - abs (aspect - (5*pi/4))) else pi - abs (aspect-pi)
  S <- slope
  L <- if (length (latitude) == 1) rep (latitude, length (A)) else latitude
  if (equation == 1) res <- exp (-1.467 +1.582*cos(L)*cos(S) -1.500*cos(A)*sin(S)*sin(L) -0.262*sin(L)*sin(S) +0.607*sin(A)*sin(S))
  if (equation == 2) res <- exp (-1.236 +1.350*cos(L)*cos(S) -1.376*cos(A)*sin(S)*sin(L) -0.331*sin(L)*sin(S) +0.375*sin(A)*sin(S))
  if (equation == 3) res <-      +0.339 +0.808*cos(L)*cos(S)                             -0.196*sin(L)*sin(S)                       - 0.482*cos(A)*sin(S)
  return (res)
}

# see website for example using sample data from Czech republic
