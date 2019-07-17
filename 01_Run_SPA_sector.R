#' This script runs a Structural Path Analysis for a given year. 
#' The output is safed at the 'temp_results' subfolder. 
#' 
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
year      <- 1995
n_layers  <- 6 # current max. = 6

# ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id



############################################################################## # 
##### 1. load data #############################################################
############################################################################## # 

# _a) EB3 ---------------------------------
S <- fread(file.path(path2exiobase, paste0("S_", year, ".csv"))) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L <- fread(file.path(path2exiobase, paste0("L_", year, ".csv"))) %>% as.matrix
A <- fread(file.path(path2exiobase, paste0("A_", year, ".csv"))) %>% as.matrix
Y <- fread(file.path(path2exiobase, paste0("Y_", year, ".csv")), select = ids_fd) %>%
  as.matrix %>% rowSums

total_emmissions <- emission_calculator(list(S, L, Y)) %>% as.numeric
x <- calculate_x(L = L, Y = Y) %>% as.numeric

# _b) Choose tolerance thresholds -----------------------------------------------

colrow_sums <- calc_colrow_sums(S = S, A = A, Y = x, L = L, n_layers)
tol_subtree <- 2E10
tol <- 1E6#1E6
apply(colrow_sums$col.sums, 1, function(x) {
  x[which(x > tol_subtree)] %>% length
}) 


############################################################################## # 
##### 2. Run SPA_sector algorithm #############################################################
############################################################################## # 

system.time({
  resid <- spa_sector(S %>% as.numeric, 
                      A, L, x,  sector = id_sector, 
                      n = n_layers, tol = tol, tol_subtree = tol_subtree, 
                      tol_row = tol_subtree,
                      row_sums = colrow_sums$row.sums, 
                      col_sums = colrow_sums$col.sums, 
                      file = file.path(path2temp_results, 
                                       paste0("SPAsector2", id_sector, "_", year, "_RAW.txt")))
})




############################################################################## # 
##### 3. For-Loop through all years #############################################################
############################################################################## # 
# years <- 1995:2011

path2model_results <- (file.path(path2temp_results, paste0(Sys.time(), "_SPA")))
dir.create(path2model_results)

for(iyear in years) {
  cat(iyear, "\n")
  S <- fread(file.path(path2exiobase, paste0("S_", iyear, ".csv"))) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(path2exiobase, paste0("L_", iyear, ".csv"))) %>% as.matrix
  A <- fread(file.path(path2exiobase, paste0("A_", iyear, ".csv"))) %>% as.matrix
  Y <- fread(file.path(path2exiobase, paste0("Y_", iyear, ".csv")), select = ids_fd) %>%
    as.matrix %>% rowSums
  
  total_emmissions <- emission_calculator(list(S, L, Y)) %>% as.numeric
  x <- calculate_x(L = L, Y = Y) %>% as.numeric
  

  colrow_sums <- calc_colrow_sums(S = S, A = A, Y = x, L = L, n_layers)
  tol_subtree <- 3E10
  tol <- 1E5#1E6
  apply(colrow_sums$col.sums, 1, function(x) {
    x[which(x > tol_subtree)] %>% length
  }) 
  
  
  
  system.time({
    resid <- spa_sector(S %>% as.numeric, 
                        A, L, x,  sector = id_sector, 
                        n = n_layers, tol = tol, tol_subtree = tol_subtree, 
                        tol_row = tol_subtree,
                        row_sums = colrow_sums$row.sums, 
                        col_sums = colrow_sums$col.sums, 
                        file = file.path(path2model_results, 
                                         paste0("SPAsector", id_sector, "_", iyear, "_RAW.txt")))
  })
  
  cat("\n End: ", Sys.time(), "\n")
}




# THE END ---------------------------------------------------------------------
