#' 
#' Run SPD with most important paths 
#'  
#' @author Simon Schulte
#' Date: 2019-07-12 12:56:57
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 


############################################################################## # 
##### settings #################################################################
############################################################################## # 
# _a) Load external settings and functions -------------------------------------
source("./settings.R")
source("./functions.R")
Rcpp::sourceCpp('SPD_functions.cpp')

ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
colnames_y[id %in% ids_fd, "newid" := 1:.N]

ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id

path2model_results <- file.path(path2temp_results, "2019-07-23 17:44:53_SPA") 

threshold <- 100
############################################################################## # 
##### 1. Load data #############################################################
############################################################################## # 
# _a) EB3 for two years --------------------------------------------------------
years <- c(1995, 2011)
table <- create_named_list(years)
y_as_vec <- TRUE
y_agg <- FALSE




for(i in years) {
  S <- fread(file.path(path2exiobase, paste0("S_", i, ".csv"))) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>%
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(path2exiobase, paste0("L_", i, ".csv"))) %>% as.matrix
  A <- fread(file.path(path2exiobase, paste0("A_", i, ".csv"))) %>% as.matrix
  Y <- fread(file.path(path2exiobase, paste0("Y_", i, ".csv")), select = ids_fd) %>%
    as.matrix
  
  if (y_as_vec) {
    Y <- Y %>% as.matrix %>% rowSums
    x <- calculate_x(Y = Y, L = L)
    table[[as.character(i)]] <- list("S" = S, "A" = A, "x" = x)
  } else {
    if (y_agg) {
      Y_dom <- Y[, colnames_y[country == "DE"]$newid %>% na.exclude]
      Y_exp <- Y[, colnames_y[country != "DE"]$newid %>% na.exclude] %>% 
        rowSums
      Y <- cbind(Y_dom, Y_exp)
    }
    table[[as.character(i)]] <- list("S" = S, "A" = A, "L" = L, "Y" = Y)
  } 
  cat(i, "")
}

# test <- lapply(table, function(x) x[["Y"]] %>% as.matrix %>% decompose_final_demand)
# for(i in 1:length(years)){
#   table[[i]]$Y <- NULL
#   #table[[i]]$x <- NULL
#   table[[i]] <- (c(table[[i]], test[[i]]))
# }
# names(table$`1995`)

# _b) SPA data ----------------------------------------------------------------
data_list <- readRDS(file = file.path(path2model_results, "SPAdata2analyse.RData"))
n_layers <- data_list$paths[, max(order)]
years <- data_list$values[, unique(year)]

# 3. Extract largest paths (absolute + change) --------------------------------
data_list$values[, "value_dif" := diff(`value[t]`), by = .(pathID, year)]

data_list[["means"]] <- data_list$values[, mean(`value[t]`), by = .(pathID)] %>% 
  setnames(., "V1", "mean_value") %>% 
  .[, "rank" := frankv(mean_value, order = -1L)] %>% 
  .[, "order" := sapply(pathID, function(x) {
    x %>% path_extract %>% length
  })]
data_list[["means"]] <- data_list$values[, sd(`value[t]`) / mean(`value[t]`), by = .(pathID)] %>% 
  setnames(., "V1", "cv") %>% 
  merge(data_list$means, ., by = "pathID", all.x = TRUE)


indices_list <- vector("list", length = n_layers)
for(i in 1:n_layers) {
  indices_list[[i]] <- sapply(data_list$means[order == i & rank < threshold]$pathID %>%
                                as.list, 
                              function(x) {
                                path_extract(x)
                              }) %>% t 
}
#indices_list <- lapply(indices_list, function(x) if(dim(x)[1] <= 1) return(NULL) else return(x))
indices_list[[5]] <- NULL
indices_list[[1]] <- NULL


# Bind one col with 1's as first col (=> Row of S matrix, only one stressor), 
# and one with 1's as last col (=> only one FD category)
indices_list <- lapply(indices_list, function(i) {
  cbind(1, i, 1) %>% 
    as.matrix
})



# indices_list <- lapply(indices_list, function(i) {
#   x <- lapply(1:nrow(i), function(ii) {
#     expand.grid(c(i[ii,] %>% as.list,
#                   list(1:nrow(table$`1995`$B), 
#                        1:nrow(table$`1995`$d), 
#                        1:nrow(table$`1995`$f)))) 
#     
#   } ) %>% rbindlist
#   cbind(1, x) %>% 
#     as.matrix
# })

# begin test
# indices_list <- lapply(indices_list, function(i) {
#   apply(i, c(1,2), function(x) {
#     ifelse(x > 4, x %% 4 + 1, x)
#   })
#   
# })
# indices_list <- lapply(indices_list, function(x) {
#   x[1:10,]
# })

# end test

# table$`1995`$L %>% ncol
# table$`1995`$B %>% dim %>% .[2]
# table$`1995`$d %>% dim %>% .[2]
# table$`1995`$B %>% dim
# indices_list[[2]][1,] %>% as.list


# 2. Run Structural Path Decomposition algorithm --------------------------------

system.time(test <- SPD(table[[1]], table[[length(table)]], indices = indices_list))



# 3. prepare data ----------------
test <- lapply(test, function(x) {
  dt <- x %>% 
    as.data.table 
  names <- c("dS", paste0("dA", 1:(ncol(dt)-2)), "dx")
  setnames(dt, names)
  return(dt)
})


for (i in 1:length(test)) {
  id_vec <- apply(indices_list[[i]], 1, function(x) {
    paste0(x, sep = "", collapse = "-")
  })
  test[[i]][, "id" := id_vec]
}





test <- lapply(test, function(x) {
  melt(x, id.vars = "id", variable.name = "differential")
}) %>% 
  rbindlist(idcol = "order")
test[id == "1-911-879-906-906-1", sum(value)]


data <- test[, sum(value),by = differential]
ggplot(data, aes(x = differential, 
                 y = V1)) + 
  geom_bar(stat = "identity")

test[, id := as.factor(id)]
test[, "total_change" := sum(value), by = id]

# add information for paths

test[, "id" := substr(id, 3, nchar(id %>% as.character)-2)]
data_list

test <- merge(test, data_list$means[, -c("order")], 
      by.x = "id", by.y = "pathID", all.x = TRUE)
test <- merge(test, data_list$paths[, c("pathID", "emitter")] %>% unique, 
      by.x = "id", by.y = "pathID", all.x = TRUE)

test$differential <- factor(test$differential, 
                            levels = c("dS", "dA1","dA2", "dA3", "dx"))
# add relative changes
test[, "rel_value" := value / abs(total_change)]

# plot ---------------------

ggplot(test[emitter == 911], aes(x = differential, 
                             y = value, 
                             group = id,
                             col = id), alpha = 0.3) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.5, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T)

ggplot(test[emitter %in% ids], aes(x = differential, 
                                 y = rel_value, 
                                 group = id,
                                 col = id), alpha = 0.3) + 
  geom_boxplot(aes(group = NULL, col = NULL)) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.7, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T) + 
  theme_bw()

test[, sum(rel_value), by = id]

colnames_A_mat[id == 888]
ids <- colnames_A_mat[grepl("*coastal water*", industry)]$id
ids <- colnames_A_mat[grepl(combine_words_regexp(c("coal", "electricity")), 
                            industry, perl = TRUE)]$id

# THE END ---------------------------------------------------------------------
