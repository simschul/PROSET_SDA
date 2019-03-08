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

sda_fun <- function(year0, year1, fun, type = "AMDI") {
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
    
    l <- log_mean(emission_calculator(year1, "detailed"), emission_calculator(year0, "detailed")) 
    print(l)
    # delta S
    temp <- 0
    for(m in 1:4) {
      for(n in 1:4) {
        temp <- temp + (year1[[1]][1,n] * year1[[1]][m,n]) * log(year0[[1]][1,n])
      }
    }
    decomp[[1]] <- temp
    
    # delta L
    temp <- 0
    for(m in 1:4) {
      for(n in 1:4) {
        temp <- temp + l[1,n] * log(year0[[2]][m,n])
      }
    }
    decomp[[2]] <- temp
    
    # delta Y
    temp <- 0
    for(m in 1:4) {
      for(n in 1:4) {
        temp <- temp + l[1,n] * log(year0[[3]][m,1])
      }
    }
    decomp[[3]] <- temp
    
    # 
    # for(i in 1:n) {
    #   y0 <- as.numeric(year0[[i]])
    #   y1 <- as.numeric(year1[[i]])
    #   dy <- y1 - y0
    #   temp <- 0
    #   
    #   for(j in 1:length(y0)){
    #     temp <- temp + log(y1[j] / y0[j])
    #     #temp <- temp +  (fun(year1) - fun(year0) /  log()) * log(y1[j] / y0[j])
    #   }
    #   decomp[[i]] <- temp * l
    #   
    #   # decomp[[i]] <- as.numeric(log_mean(fun(year1), fun(year0))) * (log(year1[[i]] / year0[[i]]))
    # 
    #   }
  }
  return(decomp)
}
x1$Y <- matrix(x1$Y, ncol = 1, nrow = 4)
x2$Y <- matrix(x2$Y, ncol = 1, nrow = 4)

test <- sda_fun(x1, x2, emission_calculator)
test2 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1")




lapply(test2, sum) %>% unlist %>% sum
b2 - b1

emission_calculator <- function(list, return = c("total", "detailed")) {
  if(is.matrix(list[[3]])) list[[3]] <- as.numeric(list[[3]])
  x <- as.numeric(list[[2]] %*% list[[3]])
  if("total" %in% return) B <- list[[1]] %*% x
  else if(return == "detailed") B <- list[[1]] %*% diag(x)
  return(B)
}
  
x1 <- io_table$year0[c("S", "L", "Y")]
x2 <- io_table$year1[c("S", "L", "Y")]

b1 <- emission_calculator(x1) %>% sum
b2 <- emission_calculator(x2) %>% sum
b1 <- emission_calculator(x1, "detailed")
b2 <- emission_calculator(x2, "detailed")

x1$S[1,2] * sum(x1$L[2,] * x1$Y) 



b0 <- IO_calculator(L = mrio1$L, S = mrio1$S, Y = mrio1$Y)

b1 <- IO_calculator(L = mrio2$L, S = mrio2$S, Y = mrio2$Y)

b1 - b0




# THE END ---------------------------------------------------------------------
