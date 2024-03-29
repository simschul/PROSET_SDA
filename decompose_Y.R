#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-03-11 16:39:48
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(my.utils)

############################################################################## # 
##### settings #################################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))

# add second column to final demand
io_table$year0$Y <- matrix(c(io_table$year0$Y, 4,7,4,2), ncol = 2, nrow = 4)
io_table$year1$Y <- matrix(c(io_table$year1$Y, 6,7,5,4), ncol = 2, nrow = 4)

F_mat <- io_table$year0$Y # entire Final demand matrix
f_vec <- apply(F_mat, 1, sum) # FD by industry
y_vec <- apply(F_mat, 2, sum) # FD by category

f_tot <- sum(f_vec) # Total FD
d_vec <- y_vec / f_tot # proportion of each category
B_mat <- F_mat %*% diag(1 / y_vec)





decompose_final_demand(F_mat)

apply(B_mat, 2, sum)
# THE END ---------------------------------------------------------------------
