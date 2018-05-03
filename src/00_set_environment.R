# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/mekbib_vegtrans/"
} else {
  filepath_base = "/media/permanent/active/mekbib_vegtrans/"
}

path_data = paste0(filepath_base, "data/")
path_rdata = paste0(filepath_base, "rdata/")
path_temp = paste0(path_data, "temp/")


# Set libraries ----------------------------------------------------------------
library(betapart)
library(ggplot2)
library(readxl)
library(reshape2)
library(vegan)

library(biodivTools) # devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(doParallel)
library(grid)
library(gridExtra)
library(gpm)
library(lavaan)
library(rgeos)

library(mapview)
library(metTools)  # devtools::install_github("environmentalinformatics-marburg/metTools")
library(raster)
library(rgdal)
library(satellite)
library(satelliteTools)  # devtools::install_github("environmentalinformatics-marburg/satelliteTools")
library(semPlot)
library(sp)

# Other settings ---------------------------------------------------------------
rasterOptions(tmpdir = path_temp)

# saga_cmd = "C:/OSGeo4W64/apps/saga/saga_cmd.exe "
# initOTB("C:/OSGeo4W64/bin/")
# initOTB("C:/OSGeo4W64/OTB-5.8.0-win64/OTB-5.8.0-win64/bin/")


