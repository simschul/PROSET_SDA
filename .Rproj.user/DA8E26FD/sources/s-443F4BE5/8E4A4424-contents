#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-04-17 09:46:10
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
Y <- io_table$year0$Y
S <- io_table$year0$S
S <- rbind(S, runif(4, 10, 50))



calc_footprint_sector(L_mat = L, S_mat = S, y_vec = Y, detailed =  TRUE)
# path analyis

SPA_footprint_sector(8, L, A, Y, S)


res <- SPA_footprint_sector(1, 8, L, A, Y, S)
value_cum <- res

for(i in 2:length(res)) {
  value_cum[[i]] <- value_cum[[i-1]] + value_cum[[i]]
}

# convert to tidy data.table
result2dt <- function(list) {
  n_em <- nrow(list[[1]])
  res <- lapply(list, function(x) data.table("em_factor" = 1:n_em, "value" = x %>% as.numeric)) %>% 
    rbindlist(., idcol = "layer")
  return(res)
}
data <- result2dt(value_cum)

# plot 
ggplot(data, aes(x = layer, y = value, col = as.factor(em_factor))) + 
  geom_point()


# THE END ---------------------------------------------------------------------
