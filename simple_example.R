#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-02-28 12:54:02
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
##### functions ############################################################
############################################################################## #

source("./functions.R")

############################################################################## # 
##### settings #################################################################
############################################################################## # 
path2temp_results <- "./temp_results"

n_countries <- 2
n_sectors <- 2
n_dim <- n_countries * n_sectors
n_fdcats <- 1
n_emissions <- 2

# meta data
names_countries <- c("Germany", "RoW")
names_industries <- c("Ag", "Ma")

years <- c(2000, 2010)

set.seed(123)
############################################################################## # 
##### Create simple EE-MRIO table #############################################################
############################################################################## # 
# IO table for year 0
mrio1 <- list()
mrio1[["Z"]] <- matrix(c(1,5,2,4,
                  4,2,1,3,
                  6,4,3,1,
                  5,3,4,2), 
                ncol = n_dim, nrow = n_dim, byrow = TRUE)
mrio1[["Y"]] <- matrix(c(9,5,4,3), ncol = 1)
mrio1[["va"]] <- c(5,1,8,7)

mrio1[["E"]] <- sample(x = 10:500, size = n_dim)
  # matrix(sample(x = 10:500, size = n_emissions * n_dim), 
  #               nrow = n_emissions, ncol = n_dim)

mrio1 <- c(mrio1, IO_creator(Z = mrio1$Z, Y = mrio1$Y, va = mrio1$va, E = mrio1$E))



# IO table for year 1
mrio2 <- list()
mrio2[["Z"]] <- matrix(c(1,4,1,2, 
                         3,3,1,2, 
                         8,5,4,2,
                         6,5,4,4), 
                       n_dim, n_dim, byrow = TRUE)
mrio2[["Y"]] <- matrix(c(11, 9, 4, 6), ncol = 1)

x <- apply(mrio2$Z, 1, sum) + apply(mrio2$Y, 1, sum)
mrio2[["va"]] <- x - apply(mrio2$Z, 2, sum) 

apply(mrio2$Z, 2, sum) + mrio2$va

curve(dnorm(x, mean = -0.3, sd = 0.15), xlim = c(-2, 2))
mrio2[["E"]] <- mrio1$E + (mrio1$E * rnorm(n_dim, mean = -0.3, sd = 0.15))
mrio2 <- c(mrio2, IO_creator(Z = mrio2$Z, Y = mrio2$Y, va = mrio2$va, E = mrio2$E))

# save io tables
IO_example <- list("year0" = mrio1, 
                   "year1" = mrio2)
saveRDS(IO_example, file = file.path(path2temp_results, "IO_example.RData"))


# 2. Calculate Emission -------------------------------------------------------
IO_calculator(A = mrio1$A, S = mrio1$S, Y = mrio1$Y)

IO_calculator(A = mrio2$A, S = mrio2$S, Y = mrio2$Y)


# 3. run 
table <- readRDS(file = file.path(path2temp_results, "IO_example.RData"))
S <- table$year0$S
L <- table$year0$L
A <- table$year0$A
Y <- table$year0$Y
x <- calculate_x(Y = Y, L = L)
F_total <- S %*% L
F_total2 <- S0 %*% L

yz <- c(0,2,0,0)
F_total2 %*% yz
as.numeric(F_total2) %*% diag(yz)
test <- (S %*% diag((L %*% yz) %>% as.numeric))
sum(test) - 82
S0 <- S
S0[2] <- 0
test2 <- (S0 %*% diag((L %*% yz) %>% as.numeric))

sum(test2)

system.time(test <- sectorSPA(sector = 2, n = 30, x = x[2], 
                      S = S %>% as.numeric, A = A, 
                      F_total = F_total2 %>% as.numeric, 
                      tolSubtree = 20, 
                      tolWrite = 0.1))
res <- fread("example.txt")
resid <- fread("resid.txt")
sum1 <- sum(res$value)
sum2 <- sum(resid$V1)
sum1 + sum2
max(resid$V1)
res[,max(value)]
calc_footprint_sector(L, S, Y, 2, detailed = FALSE)
# THE END ---------------------------------------------------------------------
