#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-03-01 10:53:42
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
path2temp_results <- "./temp_results"


############################################################################## # 
##### load data #############################################################
############################################################################## # 
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))


# 1. Structural decomposition analysis according to dietzenbacher 1998 ---------
d_S <- io_table$year1$S - io_table$year0$S 
d_L <- io_table$year1$L - io_table$year0$L 
d_Y <- io_table$year1$Y - io_table$year0$Y 


d_B <- 0.5 * (d_S %*% io_table$year0$L %*% io_table$year0$Y + 
                d_S %*% io_table$year1$L %*% io_table$year1$Y) + 
  0.5 * (io_table$year0$S %*% d_L %*% io_table$year1$Y + 
           io_table$year1$S %*% d_L %*% io_table$year0$Y) + 
  0.5 * (io_table$year0$S %*% io_table$year0$L %*% d_Y +
           io_table$year1$S %*% io_table$year1$L %*% d_Y)

d_B <- 0.5 * (IO_calculator(d_S, io_table$year0$L, io_table$year0$Y) + 
                IO_calculator(d_S, io_table$year1$L, io_table$year1$Y)) + 
  0.5 * (IO_calculator(io_table$year0$S, d_L, io_table$year1$Y) + 
           IO_calculator(io_table$year1$S, d_L, io_table$year0$Y)) + 
  0.5 * (IO_calculator(io_table$year0$S, io_table$year0$L, d_Y) +
           IO_calculator(io_table$year1$S, io_table$year1$L, d_Y))


log_mean <- function(x, y) {
  return((x - y) / (log(x) - log(y)))
}

emission_calculator <- function(list, return = c("total", "detailed")) {
  if(is.matrix(list[[3]])) list[[3]] <- as.numeric(list[[3]])
  x <- as.numeric(list[[2]] %*% list[[3]])
  if("total" %in% return) B <- list[[1]] %*% x
  else if(return == "detailed") B <- list[[1]] %*% diag(x)
  return(B)
}

sda_fun <- function(year0, year1, fun, 
                    type = "AMDI", 
                    return = c("total", "detailed"), 
                    version = "forloop") {
  #if(type != "polar_average") stop("Only method 'polar average' implemented so far")
  if(length(year0) != length(year1)) stop("Both lists need to be of same legnth")
  if(!all.equal(names(year0), names(year1))) stop("both lists need same elements in same order")
  
  n <- length(year0)
  # calculate difference for each argument between year 0 and year 1
  delta <- create_named_list(names(year0))
  for(i in 1:n) {
    delta[[i]] <- year1[[i]] - year0[[i]]
  }
  
  decomp <- create_named_list(names(year0))
  
  if(type == "AMDI") {
    temp0 <- create_named_list(names(year0))
    temp1 <- create_named_list(names(year0))
    for(i in 1:n) {
      # all components on the left hand side from year 0, right: year 1
      temp0[1:i] <- year0[1:i]
      temp0[i:n] <- year1[i:n]
      temp0[[i]] <- delta[[i]]
      # the other way round
      temp1[1:i] <- year1[1:i]
      temp1[i:n] <- year0[i:n]
      temp1[[i]] <- delta[[i]]
      # take mean of the two
      decomp[[i]] <- 0.5 * (fun(temp0) + fun(temp1)) 
    }
  } else if(type == "LMDI1") {
    # indices
    n_em <- dim(year0[[1]])[1]
    n_ix <- dim(year0[[1]])[2]
    n_iy <- dim(year0[[2]])[2]
    n_fd <- dim(year0[[3]])[2]
    
    
    vars <- expand.grid("n_em" = 1:n_em, 
                        "n_ix" = 1:n_ix, 
                        "n_iy" = 1:n_iy, 
                        "n_fd" = 1:n_fd)
    
    if(version == "forloop") {
      for(i in 1:n) {
        #mat <- matrix(ncol = n_industriesy, nrow = n_industriesx)
        mat <- array(dim = c(n_ix, n_iy, n_em, n_fd))
        for(m in 1:n_ix) {
          for(n in 1:n_iy) {
            for(o in 1:n_em) {
              for(p in 1:n_fd) {
                if(i == 1) logx <- log(year1[[1]][o,n] / year0[[1]][o,n]) # S
                if(i == 2) logx <- log(year1[[2]][m,n] / year0[[2]][m,n]) # L
                if(i == 3) logx <- log(year1[[3]][m,p] / year0[[3]][m,p]) # Y
                mat[m,n,o,p] <- log_mean(x = year1[[1]][o,n] * year1[[2]][m,n] * year1[[3]][m,p],
                                         y = year0[[1]][o,n] * year0[[2]][m,n] * year0[[3]][m,p]) *
                  logx
                # mat[m,n] <- log_mean(x = year1[[1]][o,n] * year1[[2]][m,n] * year1[[3]][m,p], 
                #                          y = year0[[1]][o,n] * year0[[2]][m,n] * year0[[3]][m,p]) * 
                #   logx    
                
              }
            }
            
          }
          
        }
        decomp[[i]] <- mat 
        
      }
      
    }
    else if(version == "faster") {
      for(i in 1:n) {
        #mat <- matrix(ncol = n_industriesy, nrow = n_industriesx)
        mat <- array(dim = c(n_ix, n_iy, n_em, n_fd))
        for(j in 1:nrow(vars)) {
          m <- vars[j, "n_ix"]
          n <- vars[j, "n_iy"]
          o <- vars[j, "n_em"]
          p <- vars[j, "n_fd"]
          if(i == 1) logx <- log(year1[[1]][o,n] / year0[[1]][o,n]) # S
          if(i == 2) logx <- log(year1[[2]][m,n] / year0[[2]][m,n]) # L
          if(i == 3) logx <- log(year1[[3]][m,p] / year0[[3]][m,p]) # Y
          mat[m,n,o,p] <- log_mean(x = year1[[1]][o,n] * year1[[2]][m,n] * year1[[3]][m,p],
                                   y = year0[[1]][o,n] * year0[[2]][m,n] * year0[[3]][m,p]) *
            logx
        }
        decomp[[i]] <- mat 
        
      }  
    }
    
  }
  if("total" %in% return) {
    decomp <- lapply(decomp, function(x) x %>% unlist %>% sum)
  } 
  return(decomp)
}

x1 <- io_table$year0[c("S", "L", "Y")]
x2 <- io_table$year1[c("S", "L", "Y")]
x1$Y <- matrix(c(x1$Y, 4,7,4,2), ncol = 2, nrow = 4)
x2$Y <- matrix(c(x2$Y, 6,7,5,4), ncol = 2, nrow = 4)

test <- sda_fun(x1, x2, function(x) emission_calculator(x, return = "detailed"), return = "detailed")
test2 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1")
test3 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1", return = "detailed")
test4 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1", return = "detailed")

system.time(sda_fun(x1, x2, emission_calculator, type = "LMDI1", return = "detailed"))


lapply(test4, function(x) x %>% unlist %>% sum)
test3$S %>% apply(., 2, sum)
test3$L %>% apply(., 2, sum)
test3$Y %>% apply(., 1, sum)

test4$S %>% apply(., c(2,3), sum)
test4$L %>% apply(., 2, sum)
test4$Y %>% apply(., c(1,4), sum)




x2$Y - x1$Y

test
test2

lapply(test2, sum) %>% unlist %>% sum
lapply(test, sum) %>% unlist %>% sum
b2 - b1



b1 <- emission_calculator(x1) %>% sum
b2 <- emission_calculator(x2) %>% sum
b1 <- emission_calculator(x1, "detailed")
b2 <- emission_calculator(x2, "detailed")

x1$S[1,2] * sum(x1$L[2,] * x1$Y) 



b0 <- IO_calculator(L = mrio1$L, S = mrio1$S, Y = mrio1$Y)

b1 <- IO_calculator(L = mrio2$L, S = mrio2$S, Y = mrio2$Y)

b1 - b0




# THE END ---------------------------------------------------------------------
