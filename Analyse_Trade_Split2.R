#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-10-31 16:55:52
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(truncnorm)
library(gtools)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source("./settings.R")
source("./functions.R")

path2sut <- file.path(path2exiobase, "MRSUT_2011")
############################################################################## # 
##### load data #############################################################
############################################################################## # 
sut <- readRDS(file.path(path2sut, "SUT.RData"))

dim(sut$U) # product X industry (which products are used in which industries?)
U <- array(sut$U, dim = c(200,49,163,49), 
           dimnames = list(EB3_metadata$pxp200$product200_code, 
                           EB3_metadata$regions$country_code2,
                           EB3_metadata$pxp163$product163_code, 
                           EB3_metadata$regions$country_code2))
dim(U)
# 200 products from 49 region used in 163 industries in 49 regions
rm(sut)
gc()

# 1. Method 1 ------------------------------------------------------------------ 
product <- 104
country <- 6

create_random_import_shares <- function(U, product, country) {
  U_subset <- U[product,,,country]
  U_subset[country,] <- 0
  
  origin_vec <- U_subset %>% rowSums # imports by country of origin
  dest_vec <- U_subset %>% colSums # imports by sector of destination
  
  # create new matrix to store import shares
  new_imp_shares <- U_subset %>% replace(., values = 0)
  randomize_flows2 <- function(origin_vec, dest_vec) {
    originprop_vec <- origin_vec / sum(origin_vec)
    destprop_vec <- dest_vec / sum(dest_vec)
    flows_left <- origin_vec
    resid <- dest_vec %>% replace(., values = 0)
    #i <- 2
    for (i in 1:length(dest_vec)) {
      # draw random numbers
      rand <- rtruncnorm(length(origin_vec),
                         a = 0,
                         mean = originprop_vec, 
                         sd = originprop_vec / 2)
      rand[is.nan(rand)] <- 0
      rand <- rand/ sum(rand)
      new_orig <- rand * dest_vec[i]
      # check how much is left
      temp <- flows_left - new_orig
      # if not enough left, take what is left
      new_orig[temp < 0] <- flows_left[temp < 0]
      # redistribute the rest
      rest <- temp[temp < 0] %>% sum %>% abs
      resid[i] <- rest
      
      new_imp_shares[,i] <- new_orig
      # update what is left
      flows_left <- flows_left - new_orig
      #cat(i, "")
    }
    #  return(resid)
    #}
    sum(resid)
    resid_dest <- resid
    resid_orig <- (origin_vec - (new_imp_shares %>% rowSums()))
    sum(resid_dest)
    sum(resid_orig)
    
    new_imp_shares1 <- new_imp_shares
    new_imp_shares <- U_subset %>% replace(., values = 0)
    
    rest2 <- randomize_flows2(resid_orig, resid_dest)
    sum(rest2)
    new_imp_shares
  }
  
  sum(resid) # sum of what is left
  new_imp_shares %>% colSums # imports allocated to sectors
  new_imp_shares %>% rowSums # ...by exporting country
  
  (dest_vec - (new_imp_shares %>% colSums())) %>% plot # flows left by importing sector
  (origin_vec - (new_imp_shares %>% rowSums())) %>% summary # flows left by exporting country 
  
  (dest_vec - (new_imp_shares %>% colSums())) %>% sum # sum of flows left by importing sector
  (origin_vec - (new_imp_shares %>% rowSums())) %>% sum # sum of flows left by exporting country 
  
  sum(resid) / sum(new_imp_shares) # what proportion is left?
  
  # redistribute the rest
  
  
  
  
  
  # 2. randomize flows function -------------
  
  x <- origin_vec
  y <- dest_vec
  randomize_flows <- function(x, y) {
    xp <- x / sum(x)
    yp <- y / sum(y)
    flows_left <- x
    resid <- y %>% replace(., values = 0)
    new_imp_shares <- matrix(0, nrow = length(x), 
                             ncol = length(y), 
                             dimnames = list(names(x), 
                                             names(y)))
    #i <- 2
    for (i in 1:length(y)) {
      # draw random numbers
      rand <- rtruncnorm(length(x),
                         a = 0,
                         mean = xp, 
                         sd = xp / 2)
      rand[is.nan(rand)] <- 0
      rand <- rand/ sum(rand)
      y_new <- rand * y[i]
      # check how much is left
      temp <- flows_left - y_new
      # if not enough left, take what is left
      y_new[temp < 0] <- flows_left[temp < 0]
      # redistribute the rest
      rest <- temp[temp < 0] %>% sum %>% abs
      resid[i] <- rest
      
      new_imp_shares[,i] <- y_new
      # update what is left
      flows_left <- flows_left - y_new
      #cat(i, "")
    }
    return(list(imp_shares = new_imp_shares,
                residuals = resid))
  }
  
  N <- 100
  list <- vector("list", N)
  for(i in 1:N) {
    result <- matrix(0, nrow = length(x), 
                     ncol = length(y), 
                     dimnames = list(names(x), 
                                     names(y)))
    
    x <- origin_vec
    y <- dest_vec
    
    repeat{
      z <- randomize_flows(x,y)
      x <- x - (z$imp_shares %>% rowSums)
      y <- z$residuals
      result <- result + z$imp_shares
      if(sum(z$residuals) < 1E4) {
        break
      }
    }
    list[[i]] <- result
  }
  
  
  z$residuals
  sum(result)
  sum(origin_vec)
  sum(dest_vec)
  
  list[[1]]
  
  lapply(list, function(x) x[,2]) %>% 
    as.data.table %>% 
    melt %>% 
    ggplot(aes(x = value))
  
  # THE END ---------------------------------------------------------------------
  