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
Rcpp::sourceCpp("SPA_recursive.cpp")


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
F_total <- S %*% L
yz <- rep(0, length(x))

# _b) Choose tolerance thresholds -----------------------------------------------
tol_subtree <- 2E9
tol2write <- 1E6

############################################################################## # 
##### 2. Run SPA_sector algorithm #############################################################
############################################################################## # 
system.time(sectorSPA(sector = id_sector, n = 9, x = x[id_sector], 
                      S = S, A = A, F_total = F_total, 
                      tolSubtree = tol_subtree, 
                      tolWrite = tol2write))

system.time(sectorSPA2(sector = id_sector, n = 3, Y = Y, 
                      S = S, A = A, F_total = F_total, 
                      tolSubtree = tol_subtree, 
                      tolWrite = tol2write, 
                      file = "example2.txt"))

# system.time(spa_sector_test(S = S, A = A, L = L, x = x, F_total = F_total, n = 6, 
#                             tol = tol, tol_subtree = tol_subtree, sector = id_sector, 
#                             file = "example_sector_test.txt", progress = TRUE))


# begin test 
data1 <- fread("example.txt") %>% setorderv(cols = "value", order = -1L)
data2 <- fread("example_sector_test.txt") %>% setorderv("value", order = -1L)
all.equal(data1[, c("value", "order")], data2[, c("value", "order")])


############################################################################## # 
##### 3. For-Loop through all years #############################################################
############################################################################## # 
years <- 1995:2011

path2model_results <- (file.path(path2temp_results, paste0(Sys.time(), "_SPA")))
dir.create(path2model_results)
file.copy(from = c("./settings.R", "./functions.R", 
                   "SPA_functions.cpp", "SPA_recursive.cpp", 
                   "01_Run_SPA_sector.R"), 
          to = path2model_results)
tol_subtree <- 2E8
tol2write <- 1E5
n_layers <- 9
for(iyear in years) {
  cat(iyear, "\n")
  
  S <- fread(file.path(path2exiobase, paste0("S_", iyear, ".csv"))) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(path2exiobase, paste0("L_", iyear, ".csv"))) %>% as.matrix
  A <- fread(file.path(path2exiobase, paste0("A_", iyear, ".csv"))) %>% as.matrix
  Y <- fread(file.path(path2exiobase, paste0("Y_", iyear, ".csv")), select = ids_fd) %>%
    as.matrix %>% rowSums
  
  x <- calculate_x(L = L, Y = Y) %>% as.numeric
  F_total <- S %*% L
  
  
  
  
  system.time(sectorSPA(sector = id_sector, n = n_layers, x = x[id_sector], 
                        S = S, A = A, F_total = F_total, 
                        tolSubtree = tol_subtree, 
                        tolWrite = tol2write, 
                        file = file.path(path2model_results, 
                                         paste0("SPAsector", id_sector, "_", iyear, "_RAW.txt"))))
  
 
  cat("\n End: ", Sys.time(), "\n")
}

# 4. With  pxp200 EB3 version 3.6 -------------------



path2model_results <- (file.path(path2temp_results, 
                                 paste0(Sys.time(), 
                                        "_",
                                        paste(ids_stressor, 
                                              collapse = "."), 
                                        "_",
                                        id_sector,
                                        "_SPA")))
dir.create(path2model_results)
file.copy(from = c("./settings.R", "./functions.R", 
                   "SPA_functions.cpp", "SPA_recursive.cpp", 
                   "01_Run_SPA_sector.R"), 
          to = path2model_results)

# find a reasonable tolerance threshold
test <- calc_footprint_sector(L, S, Y, id_sector, TRUE)
1E5 / test$indirect # 0.0016

tol_subtree <- 2E8  #0.0016 * test$indirect # 2E8
tol2write <- 8E-07 * test$indirect  # 1E5
n_layers <- 8

# begin test
dt <- data.table("S" = S %>% as.numeric, 
                 EB3_metadata$colnames200)
setorderv(dt, "S", order = -1L)
dt[S > 0.001 * sum(S)] %>% 
  ggplot(data = ., aes(x = id, y = S, col = country_name)) + 
  geom_point()
plot(S %>% as.numeric)
# end
iyear <- 2016
for(iyear in years) {
  cat(iyear, "\n")
  ipath <- file.path(path2exiobase, "V3.6", "constant_prices",
                     paste0("IOT_", iyear, "_pxp"))
  S <- fread(file.path(ipath, "S.txt")) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(ipath, "L.txt")) %>% as.matrix
  A <- fread(file.path(ipath, "A.txt")) %>% as.matrix
  Y <- fread(file.path(ipath, "Y.txt"), select = ids_fd) %>% 
    as.matrix %>% rowSums
  
  x <- calculate_x(L = L, Y = Y) %>% as.numeric
  
  
  F_total <- S %*% L
  
  system.time(sectorSPA(sector = id_sector, n = n_layers, x = x[id_sector], 
                        S = S, A = A, F_total = F_total, 
                        tolSubtree = tol_subtree, 
                        tolWrite = tol2write, 
                        file = file.path(path2model_results, 
                                         paste0("SPAsector", id_sector, "_", 
                                                iyear, "_RAW.txt"))))
  
  
  cat("\n End: ", Sys.time(), "\n")
}


totalpxp <- S %*% L %*% Y

S2 <- fread(file.path(path2exiobase, paste0("S_", iyear, ".csv"))) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L2 <- fread(file.path(path2exiobase, paste0("L_", iyear, ".csv"))) %>% as.matrix
Y2 <- fread(file.path(path2exiobase, paste0("Y_", iyear, ".csv")), select = ids_fd) %>%
  as.matrix %>% rowSums
totalixi <- S %*% L %*% Y
Y2[906] / Y[id_sector]
dim(L)
plot(L[, id_sector] %>% log, L2[, 906] %>% log)
abline(b = 1, a = 0, col = "red")
sum(Y2) / sum(Y)

fp_p<- calc_footprint_sector(L, S, Y, id_sector, detailed = F)
fp_i <- calc_footprint_sector(L2, S2, Y2, 906, detailed = F)


lapply(1:length(fp_p), function(x) {
  (fp_p[[x]] - fp_i[[x]]) / fp_i[[x]]
})


# THE END ---------------------------------------------------------------------
