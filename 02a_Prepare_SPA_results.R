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
Rcpp::sourceCpp("SPA_recursive.cpp")

years      <- 1995:2016
n_layers  <- 8
list.dirs(file.path(path2temp_results))
path2model_results <- file.path(path2temp_results, 
                                "2019-09-11 17:22:46_1.70.71.405.415.416_1123_SPA")
# _b) Set year, sector ....-----------------------------------------------------

# ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
colnames_A_mat <- copy(EB3_metadata$colnames200[, c("id","country_name", 
                                                    "product200_name")])
setnames(colnames_A_mat, c("id", "country", "industry"))
for(year in years) {
  ############################################################################## # 
  ##### 1. load data #############################################################
  ############################################################################## # 
  
  data <- fread(file.path(path2model_results, 
                          paste0("SPAsector", id_sector, "_", year, "_RAW.txt")))
  data <- tstrsplit(data$paths, " ") %>% as.data.table %>%
    setnames(paste0("dim", 1:length(names(.)))) %>% 
    .[, lapply(.SD, as.numeric)] %>% 
    .[, lapply(.SD, function(x)(ifelse(x == "nan", NA, x)))] %>% 
    cbind(., data) %>% 
    .[, paths := NULL] %>% 
    .[, "rank" := frankv(value, order = -1, ties.method = "random")] %>% 
    setorder(., rank) %>% 
    .[, "cum_value" := cumsum(value)] %>%  # cumulative sums
    .[] %>% 
    merge(., get_emitting_sector(.)[, c("rank", "emitter")], by = "rank")
  
  
  
  # check if for double countings
  data[emitter == id_sector]
  data[order > 3]
  
  
  data[, "supply_chain" := ""]
  #max_layer <- data[, max(order)]
  for(i in 1:n_layers) {
    data <- merge(data, colnames_A_mat, 
                  by.x = paste0("dim", i), by.y = "id", 
                  all.x = TRUE)
    # add column with one string for each supply chain
    data[, supply_chain := ifelse(!is.na(country), 
                                  paste0(supply_chain, ifelse(i > 1, " < ", ""), 
                                         country, " - ", industry), 
                                  supply_chain)]
    setnames(data, c("country", "industry"), 
             c(paste0("dim", i, "_country"), paste0("dim", i, "_industry")))
  }
  
  setorder(data, rank)
  
  
  ############################################################################## # 
  ##### 2. Write to disk #############################################################
  ############################################################################## # 
  
  setcolorder(data, c("rank", "order", "value", "supply_chain"))
  data[, value := value / 1E3] # to t
  setnames(data, "value", "value[t]")
  data[, `value[t]` := format(`value[t]`, scientific = TRUE)]
  
  fwrite(data, file.path(path2model_results, paste0("sector", 
                                                    id_sector,"_", 
                                                    year, ".csv")), 
         sep = "\t")
  
  cat(year, "")  
  
}




# THE END ---------------------------------------------------------------------
