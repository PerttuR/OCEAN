# LIBRARYS ####
library(readxl)
library(raster)
library(stringr)
library(dplyr)
library(purrr)
library(tidyverse)
library(devtools)
library(tidyverse)
library(mapdata)
#library(maptools) #downloaded from: https://cran.r-project.org/src/contrib/Archive/maptools/
library(PBSmapping)
library(vmstools)
library(sf)
library(data.table)
library(terra)
library(mapview)
library(Matrix)
library(doBy)
library(mixtools)
library(tidyr)
library(glue)
library(gt)
library(progressr)
library(geosphere)
library(ggplot2)
#install.packages("sfdSAR", repos = "https://ices-tools-prod.r-universe.dev")
library(sfdSAR)
library(icesVocab)
library(generics)
#install.packages("icesConnect")
library(icesConnect)
#install.packages("sqldf")
#install.packages("icesVMS", repos = 'https://ices-tools-prod.r-universe.dev')
library(icesVMS)
#install.packages("icesSharePoint") This is downloaded from: https://ices-tools-prod.r-universe.dev/icesSharePoint
library(icesSharePoint)
library(units)
#library(tcltk) # R says: Is BASE package already
library(lubridate)
library(here)
library(operators) #breaks pipes, is this needed?
library(magrittr) #unbreak pipes :)
library(rnaturalearth)
# library(rnaturalearthhires)
library(rnaturalearthdata)
library(purrr)
library(maps)
library(csquares)

source("prog/run.R")

# PATHS ####
path <- paste0(getwd(), "/") # Working directory
codePath  <- paste0(path, "prog/")   # Location to store R scripts
outPath   <- paste0(path, paste0("out/"))   # Location to store the results
dataPath  <- paste0(path, "orig/")      # Location to store tacsat (VMS) and eflalo (logbook) data


# MAKE DATA ####

runScript(paste0(codePath, "02_import_data.R"))

runScript(paste0(codePath, "03_prepare_data.R"))

# MAKE MAPS ####

runScript(paste0(codePath, "04_make_maps.R"))


