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


############################################################################## # 
##### load data #############################################################
############################################################################## # 
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))


# 1. Structural decomposition analysis according to dietzenbacher 1998 ---------

# _a) functions ----------------------------------------------------------------


sda_fun <- function(year0, year1, fun, 
                    type = "AMDI", 
                    return = c("total", "detailed"), 
                    version = "forloop") {
  
  if(length(year0) != length(year1)) stop("Both lists need to be of same legnth")
  if(!all.equal(names(year0), names(year1))) stop("both lists need same elements in same order")
  
  n <- length(year0)
  # calculate difference for each argument between year 0 and year 1
  delta <- lapply(1:n, function(x) {
    return(year1[[x]] - year0[[x]])
  })
  
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
    n_ix <- dim(year0[[2]])[1]
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



# _b) Applying SDA function ---------------------------------------------------
x1 <- io_table$year0[c("S", "L", "Y")]
x2 <- io_table$year1[c("S", "L", "Y")]
x1$Y <- matrix(c(x1$Y, 4,7,4,2), ncol = 2, nrow = 4)
x2$Y <- matrix(c(x2$Y, 6,7,5,4), ncol = 2, nrow = 4)

test <- sda_fun(x1, x2, function(x) emission_calculator(x, return = "detailed"), return = "detailed")
test2 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1")
test3 <- sda_fun(x1, x2, emission_calculator, type = "LMDI1", return = "detailed")



lapply(test, function(x) x %>% unlist %>% sum)
test3$S %>% apply(., 2, sum)
test3$L %>% apply(., 2, sum)
test3$Y %>% apply(., c(1,4), sum)

test4$S %>% apply(., c(2,3), sum)
test4$L %>% apply(., 2, sum)
test4$Y %>% apply(., c(1,4), sum)


########################################################################## #
#### 2. Digging deeper into FINAL DEMAND #####################################
########################################################################## #

# _a) Functions -----------------------------------------------------------------
sda_fun <- function(year0, year1, fun, 
                    type = "AMDI", 
                    return = c("detailed", "total"), 
                    version = "forloop") {
  
  if(length(year0) != length(year1)) stop("Both lists need to be of same legnth")
  if(!all.equal(names(year0), names(year1))) stop("both lists need same elements in same order")
  n <- length(year0)
  decomp <- create_named_list(names(year0))
  
  if(type == "AMDI") {
    # calculate difference for each argument between year 0 and year 1
    delta <- lapply(1:n, function(x) {
      return(year1[[x]] - year0[[x]])
    })
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
    n_ix <- dim(year0[[2]])[1]
    n_iy <- dim(year0[[2]])[2]
    n_fd <- dim(year0[[4]])[2]
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
                if(i == 3) logx <- log(year1[[3]] / year0[[3]]) # f
                if(i == 4) logx <- log(year1[[4]][m,p] / year0[[4]][m,p]) # B
                if(i == 5) logx <- log(year1[[5]][p] / year0[[5]][p]) # d
                
                mat[m,n,o,p] <- log_mean(x = year1[[1]][o,n] * year1[[2]][m,n] 
                                         * year1[[3]] * year1[[4]][m,p] 
                                         * year1[[5]][p],
                                         y = year0[[1]][o,n] * year0[[2]][m,n] * year0[[3]]
                                         * year1[[4]][m,p] 
                                         * year1[[5]][p]) *
                  logx
                
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
  if(return == "total") {
    decomp <- lapply(decomp, function(x) x %>% unlist %>% sum)
  } 
  return(decomp)
}


# _b) Applying function --------------------------------------------------------

x1 <- c(io_table$year0[c("S", "L")], cbind(io_table$year0$Y, c(5,3,1,0)) %>% decompose_final_demand)
x2 <- c(io_table$year1[c("S", "L")], cbind(io_table$year1$Y, c(6,3,4,2)) %>% decompose_final_demand)

result_simple <- sda_fun(x1, x2, fun = emission_calculator2, return = "total")
result <- sda_fun(x1, x2, type = "LMDI1", return = "total")
result_detailed <- sda_fun(x1, x2, type = "LMDI1", return = "detailed")
result_agg <- sda_fun(x1, x2, type = "LMDI1", return = "aggregated")

apply(result_detailed$S, c(2,3), sum)
apply(result_detailed$L, c(1,2), sum)
apply(result_detailed$total, c(1), sum)
apply(result_detailed$B_mat, c(1,4), sum)
apply(result_detailed$d_vec, c(2, 4), sum)

# comparing with calculated emissions 
e0 <- emission_calculator(io_table$year0[c("S", "L", "Y")])
e1 <- emission_calculator(io_table$year1[c("S", "L", "Y")])

e1 - e0

result %>% unlist %>% sum


emission_calculator2(x1)
emission_calculator2(x2)





# 3. Structural Path Decomposition - An easy examply---------------------------------------------

spd <- function(year0, year1, 
                n.layers, cut = NULL, ...) {
  # begin test
  # year0 <- io_table$year0[c("S", "L", "Y", "A")]
  #  year1 <- io_table$year1[c("S", "L", "Y", "A")]
  #year0 <- x1
  #year1 <- x2
  # n.layers <- 3

  # end test
  if(length(year0) != length(year1)) stop("Both lists need to be of same length")
  if(!all.equal(names(year0), names(year1))) stop("both lists need same elements in same order")
  
  
  n.comp <- length(year0)
  decomp <- vector("list", length = n.layers)
  # extract all but L and A matrices
  x0 <- year0[!(names(year0) %in% c("L", "A"))]
  x1 <- year1[!(names(year1) %in% c("L", "A"))]
  
  for(ilayer in 1:n.layers) {
    decomp[[ilayer]] <- .SDA.lmdi(x0, x1, aggregate = FALSE, ...)
   # decomp[[ilayer]]$B[which(decomp[[ilayer]]$B < 0.01)] <- NA
    x0 <- append(x0, list(year0[["A"]]), ilayer)
    x1 <- append(x1, list(year1[["A"]]), ilayer)
    names(x0)[ilayer + 1] <- paste0("A", ilayer)
    names(x1)[ilayer + 1] <- paste0("A", ilayer)
  } # ilayer

  decomp <- lapply(decomp, function(y) {
    lapply(y, function(x) {
      res <- as.data.table(x)
      if(!is.null(cut)) res <- res[abs(value) > cut]
      return(res)
    }) %>% rbindlist(idcol = "differential")  
  }) %>% rbindlist(., idcol = "order", fill = TRUE) 
  
  decomp[, "rank" := frankv(abs(value), na.last = TRUE, order = -1)]
  setorder(decomp, rank)
  setcolorder(decomp, c("rank", "value", "order", "differential"))
  return(decomp[])
}

x1 <- c(io_table$year0[c("S", "L", "A")], cbind(io_table$year0$Y, c(5,3,1,0)) %>% decompose_final_demand)
x2 <- c(io_table$year1[c("S", "L", "A")], cbind(io_table$year1$Y, c(6,3,4,2)) %>% decompose_final_demand)
delta_IO(x1, x2)


test <- spd(x1, x2, 4, cut = abs(as.numeric(0.0001 * delta_e)), zero.handling = FALSE)
test[differential == "f"]


system.time(test2 <- spd(x1, x2, 8))



lapply(test2, function(x) x$value %>% sum) %>% unlist %>% sum

test <- decompose_final_demand(cbind(Y, c(5,3,1,0)))
e0 <- IO_calculator(S = io_table$year0$S, L = io_table$year0$L, 
              Y = io_table$year0$Y, detailed = FALSE)
e1 <- IO_calculator(S = io_table$year1$S, L = io_table$year1$L, 
                    Y = io_table$year1$Y, detailed = FALSE)
delta_e <- e1 - e0


test <- .SDA.lmdi(year0[c("S", "L", "Y")], year1[c("S", "L", "Y")], aggregate = FALSE) 

test$L %>% as.data.table

test %>% unlist %>% sum
delta_IO(IO_calculator(year0$S, year0$L, year0$Y), IO_calculator(year1$S, year1$L, year1$Y)) %>% 
  unlist %>% sum

lapply(res, function(x) lapply(x, sum))




delta_IO(x1, x2)
.SDA.lmdi(x1, x2)




# THE END ---------------------------------------------------------------------
