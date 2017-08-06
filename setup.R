# setup.R
# make sure any needed packages are loaded
# Jim Lutz  "Fri Jun 23 08:36:31 2017"

# had to change permissions for library
# /usr/lib/R$ sudo -R chmod 777 library

# added & changed permissions for
# /home/jiml/R/packages 
# see: http://www.r-bloggers.com/installing-r-packages/

# clean up leftovers before starting
# clear all the objects except fn_script
l_obj=ls(all=TRUE)
l_obj = c(l_obj, "l_obj") # be sure to include l_obj
rm(list = l_obj[l_obj != "fn_script"])
# clear the plots
if(!is.null(dev.list())){
  dev.off(dev.list()["RStudioGD"])
}
# clear history
cat("", file = "nohistory")
loadhistory("nohistory")
# clear the console
cat("\014")


# only works if have internet access
update.packages(checkBuilt=TRUE)


sessionInfo() 
  # R version 3.4.1 (2017-06-30)
  # Platform: i686-pc-linux-gnu (32-bit)
  # Running under: Ubuntu 16.04.2 LTS


# work with tidyverse
# http://tidyverse.org/
# needed libxml2-dev installed
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

# work with stringr
if(!require(stringr)){install.packages("stringr")}
library(stringr)


# work with data.tables
#https://github.com/Rdatatable/data.table/wiki
#https://www.datacamp.com/courses/data-analysis-the-data-table-way
if(!require(data.table)){install.packages("data.table")}
library(data.table)

# change the default background for ggplot2 to white, not gray
theme_set( theme_bw() )

# generic plot scaling methods
if(!require(scales)){install.packages("scales")}
library(scales)


