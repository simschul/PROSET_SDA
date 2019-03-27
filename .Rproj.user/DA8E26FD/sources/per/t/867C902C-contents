#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-03-19 12:05:01
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)

############################################################################## # 
##### settings #################################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))

A <- io_table$year0$A
L <- io_table$year0$L

A <- matrix(rnorm(8000*8000), 8000, 8000)
B <- matrix(rnorm(8000*8000), 8000, 8000)

system.time(mat_mult(A, B))
system.time(C <- A %*% B)
system.time(C2 <- Rfast::mat.mult(A, B))
Rfast::all_equals(C, C2)
C[1:10, 1:10]
C2[1:10, 1:10]
dif <- C - C2
summary(dif %>% )
library(Rfast)

# 1. Structural Path Analysis --------------------------------------------------
n_layers <- 5

leontief_series_expansion <- function(A_mat, n) {
  list <- vector(mode = "list", length = n)
  list[[1]] <- diag(1, nrow = nrow(A_mat), ncol = ncol(A_mat))
  for(i in 2:n) {
    list[[i]] <- list[[i-1]] %*% A_mat
  }
  return(list)
}

leontief_series_expansion(A, 8)


# THE END ---------------------------------------------------------------------
