# load packages ---------------------------------------------------------------
install_myutils <- readline("Should my.utils be installed from scratch? 
                            Type Y or N. May take a while... \n")
if(install_myutils %in% c("Y", "y", "yes", "Yes")) {
    devtools::install_github("simschul/my.utils", upgrade = "never")
    .rs.restartR()    
}
rm(install_myutils)
library(my.utils)
library(data.table)
library(tidyverse)
library(pbapply)
library(parallel)
library(pbmcapply)
library(foreach)
library(doParallel)
library(R.oo)
library(bit64)
library(raster)
library(sf)
library(RhpcBLASctl)


library(viridis)
library(ggthemes)
library(gganimate)
library(gifski)
library(directlabels)
library(ggrepel)
library(alluvial)
# settings -------------------------------------------------------------------
# 1. Paths ---------------------------------------------------------------------
path2project <- "/home/simon/Documents/PhD_PROSET"
path2rproj <- file.path(path2project, "code", "R", "SDA", "SDA")
path2data <- file.path(path2project, "data")
path2exiobase <- file.path(path2project, "data", "EXIOBASE3")
path2exiobase36 <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.6/constant_prices"

path2plot <- file.path(path2rproj, "plots")
path2temp_results <- "./temp_results"

options("datatable.print.class" = TRUE)
RhpcBLASctl::blas_set_num_threads(7)
# Meta data --------------------------------------------------------------------
EB3_metadata <- readRDS(file.path(path2exiobase, "EB3_metadata.RData"))
EB3_concordance <- readRDS(file.path(path2exiobase, "EB3_concordance.RData"))
EB3_midpoints <- readRDS(file.path(path2exiobase, "EB3_midpoints.RData"))


final_demand_categories <- data.table("long" = c("Final consumption expenditure by households"                                         
                                                 , "Final consumption expenditure by non-profit organisations serving households (NPISH)"
                                                 , "Final consumption expenditure by government"                                         
                                                 , "Gross fixed capital formation"                                                       
                                                 , "Changes in inventories"                                                              
                                                 , "Changes in valuables"                                                                
                                                 , "Exports: Total (fob)"), 
                                      "short" = c("Households", 
                                                  "NPISH", 
                                                  "Government", 
                                                  "GFCF", 
                                                  "Inventories", 
                                                  "Valuables", 
                                                  "Exports"))



colnames_A_mat <- readRDS(file = file.path(path2data, "colnames_A_mat.RData"))
colnames_y <- readRDS(file.path(path2data, "colnames_y.RData"))
countries_class <- fread(file.path(path2exiobase, "EB3_countries_classification.csv"))


stressor_names <- fread(file.path(path2data, "stressor_names.csv"))

load(file.path(path2data, "Industry_classification.RData"))

Industry_classes[Class1 == "Manufacturing" & Country == "Germany"]

# settings ---------------------------------------------------------------------
years <- 1995:2016
# id_sector <- colnames_A_mat[country == "Germany" & 
#                              grepl("Manufacture of motor vehicles", industry)]$id
#ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id

# ids_stressor <- stressor_names[grepl("CO2", stressor)]$id # EB3 ixi
EB3_metadata$stressorsV3.6[grepl(combine_words_regexp(c("CO2")), 
                                                 stressor_name, perl = TRUE)]
# ids_stressor <- c(716, 1104)
ids_stressor <- c(1,70,71,405,415,416)
EB3_metadata$colnames200[country_name == "Germany" & 
                                        grepl("Motor vehicles", product200_name)]$id
id_sector <- 1123

colors <- RColorBrewer::brewer.pal(12, "Set3")[-2] # colors for plots