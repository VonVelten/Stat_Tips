# ---------------------------------------- #
# Filename: useMulticore.R
# Object: parallel processing for project VAGABOND
# Project: VAGABOND
# R version: 3.5.1
# Authors: S. Poggi
# Date: December 2018
# Last modified: 
# ------------------------------------- #


useMulticore <- function(){
  library(doParallel)
  nc=detectCores(all.tests=FALSE, logical=TRUE) #optional: detect the number of cores
  use_nc=4
  print(paste0("number of cores for computation: ",use_nc,"/",nc))
  cl <- makePSOCKcluster(use_nc)
  registerDoParallel(cl)
  return(cl)
}