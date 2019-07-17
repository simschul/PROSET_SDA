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
Rcpp::sourceCpp("SPA_functions.cpp")


# _b) Set year, sector ....-----------------------------------------------------
year      <- 2011
n_layers  <- 6 # current max. = 6

# ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id



############################################################################## # 
##### 1. load data #############################################################
############################################################################## # 

data <- fread(file.path(paste0("SPAsector", id_sector, "_", year, "_RAW.txt")))
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



data[, "supply_chain" := ""]
for(i in 1:n_layers) {
  data <- merge(data, colnames_A_mat, by.x = paste0("dim", i), by.y = "id", all.x = TRUE)
  # add column with one string for each supply chain
  data[, supply_chain := ifelse(!is.na(country), 
                                paste0(supply_chain, ifelse(i > 1, " < ", ""), country, " - ", industry), 
                                supply_chain)]
  setnames(data, c("country", "industry"), c(paste0("dim", i, "_country"), paste0("dim", i, "_industry")))
}

setorder(data, rank)


############################################################################## # 
##### 2. Write to disk #############################################################
############################################################################## # 

setcolorder(data, c("rank", "order", "value", "supply_chain"))
data[, value := value / 1E3] # to t
setnames(data, "value", "value[t]")
data[, `value[t]` := format(`value[t]`, scientific = TRUE)]

fwrite(data, file.path(path2temp_results, paste0("sector", id_sector,"_", year, ".csv")), sep = "\t")





# THE END ---------------------------------------------------------------------
