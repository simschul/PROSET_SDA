#' This script prepares the output from the Structural Path Analysis for the 
#' analysis. 
#' The cleaned data is safed in the folder 'temp_results'. 
#'  
#' @author Simon Schulte
#' Date: 2019-06-21 12:10:44
#' 
#' Content:
#'  


############################################################################## # 
##### settings #################################################################
############################################################################## # 
# _a) Load external settings and functions -------------------------------------
source("./settings.R")
source("./functions.R")


list.dirs(path2temp_results)
path2model_results <- file.path(path2temp_results, 
                                "2019-09-11 17:22:46_1.70.71.405.415.416_1123_SPA")
############################################################################## #
###### 1. Load and prepare data ############################################################
############################################################################## #

files <- list.files(path = file.path(path2model_results), 
                    pattern = "^sector1123_.*?.csv" , full.names = TRUE)
data <- lapply(files, fread) %>% 
  setNames(years %>% as.character) %>% 
  rbindlist(idcol = "year") %>% 
  .[, year := as.numeric(year)]

n_layers <- data[, max(order)]

# split datatable into 2: one with info on paths, one with values
cols <- paste0("dim", n_layers:1)
pathID <- do.call(paste, c(data[, ..cols], sep = "-")) %>% 
  gsub("NA-", "", .)
data[, "pathID" := pathID]


data_list <- list("paths" = data[, c("pathID", "order", paste0("dim", 1:n_layers), 
                                     "supply_chain", "emitter", 
                                     paste0("dim", 1:n_layers, "_country"), 
                                     paste0("dim", 1:n_layers, "_industry")), 
                                 with = FALSE] %>% unique, 
                  "values" = data[, c("pathID","rank", "year", "order", "value[t]")])




# 2. Save to disk --------------------------------------------------------------
saveRDS(data_list, file = file.path(path2model_results, "SPAdata2analyse.RData"))




# The End ----------------------------------------------------------------------



