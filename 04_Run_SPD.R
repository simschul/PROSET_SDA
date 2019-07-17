#' 
#' Run SPD with most important paths 
#'  
#' @author Simon Schulte
#' Date: 2019-07-12 12:56:57
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 
 

############################################################################## # 
##### settings #################################################################
############################################################################## # 
# _a) Load external settings and functions -------------------------------------
source("./settings.R")
source("./functions.R")
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id



############################################################################## # 
##### 1. Load data #############################################################
############################################################################## # 
data_list <- readRDS(file = file.path(path2temp_results, "SPAdata2analyse.RData"))
n_layers <- data_list$paths[, max(order)]
years <- data_list$values[, unique(year)]

# _a) Extract largest paths (absolute + change) --------------------------------
data_list$values[, "value_dif" := diff(`value[t]`), by = pathID]



indices_list <- vector("list", length = n_layers)

for(i in 1:n_layers) {
  indices_list[[i]] <- sapply(data_list$values[order == i & rank < 200]$pathID %>% as.list, 
                              function(x) {
                                path_extract(x)
                              }) %>% t
  
}

# Bind one col with 1's as first col (=> Row of S matrix, only one stressor), 
# and one with 1's as last col (=> only one FD category)

indices_list <- lapply(indices_list, function(x) {
  cbind(1, x, 1)
})
indices_list[[1]] <- NULL


# _b) EB3 for two years --------------------------------------------------------
table <- create_named_list(years)
for(i in years) {
  S <- fread(file.path(path2exiobase, paste0("S_", i, ".csv"))) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(path2exiobase, paste0("L_", i, ".csv"))) %>% as.matrix
  A <- fread(file.path(path2exiobase, paste0("A_", i, ".csv"))) %>% as.matrix
  Y <- fread(file.path(path2exiobase, paste0("Y_", i, ".csv")), select = ids_fd) %>%
    as.matrix %>% rowSum
  x <- calculate_x(Y = Y, L = L)
  
  table[[as.character(i)]] <- list("S" = S, "A" = A, "x" = x)
}

# 2. Run Structural Path Decomposition algorithm --------------------------------

system.time(test <- SPD(table[[1]], table[[2]], indices = indices_list))

lapply(test, colSums)



# THE END ---------------------------------------------------------------------
