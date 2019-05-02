# load packages ---------------------------------------------------------------
library(my.utils)
library(data.table)
library(tidyverse)
library(pbapply)
library(parallel)
library(pbmcapply)
library(foreach)
library(doParallel)
library(R.oo)

# settings -------------------------------------------------------------------
path2temp_results <- "./temp_results"
