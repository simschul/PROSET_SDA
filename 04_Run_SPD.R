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

colnames_y[id %in% ids_fd, "newid" := 1:.N]

list.dirs(path2temp_results)
path2model_results <- file.path(path2temp_results, 
                                "2019-09-11 17:22:46_1.70.71.405.415.416_1123_SPA") 

threshold <- 100
############################################################################## # 
##### 1. Load data #############################################################
############################################################################## # 
# _a) EB3 for two years --------------------------------------------------------
years <- c(1995, 2016)
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

# spd_results <- lapply(table, function(x) x[["Y"]] %>% as.matrix %>% decompose_final_demand)
# for(i in 1:length(years)){
#   table[[i]]$Y <- NULL
#   #table[[i]]$x <- NULL
#   table[[i]] <- (c(table[[i]], spd_results[[i]]))
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

# begin spd_results
# indices_list <- lapply(indices_list, function(i) {
#   apply(i, c(1,2), function(x) {
#     ifelse(x > 4, x %% 4 + 1, x)
#   })
#   
# })
# indices_list <- lapply(indices_list, function(x) {
#   x[1:10,]
# })

# end spd_results

# table$`1995`$L %>% ncol
# table$`1995`$B %>% dim %>% .[2]
# table$`1995`$d %>% dim %>% .[2]
# table$`1995`$B %>% dim
# indices_list[[2]][1,] %>% as.list


# 2. Run Structural Path Decomposition algorithm --------------------------------

system.time(spd_results <- SPD(table[[1]], table[[length(table)]], indices = indices_list))



# 3. prepare data ----------------
spd_results <- lapply(spd_results, function(x) {
  dt <- x %>% 
    as.data.table 
  names <- c("dS", paste0("dA", 1:(ncol(dt)-2)), "dx")
  setnames(dt, names)
  return(dt)
})


for (i in 1:length(spd_results)) {
  id_vec <- apply(indices_list[[i]], 1, function(x) {
    paste0(x, sep = "", collapse = "-")
  })
  spd_results[[i]][, "id" := id_vec]
}





spd_results <- lapply(spd_results, function(x) {
  melt(x, id.vars = "id", variable.name = "differential")
}) %>% 
  rbindlist(idcol = "order")
spd_results[id == "1-911-879-906-906-1", sum(value)]


data <- spd_results[, sum(value),by = differential]
ggplot(data, aes(x = differential, 
                 y = V1)) + 
  geom_bar(stat = "identity")

spd_results[, id := as.factor(id)]
spd_results[, "total_change" := sum(value), by = id]

# add information for paths

spd_results[, "id" := substr(id, 3, nchar(id %>% as.character)-2)]
data_list

spd_results <- merge(spd_results, data_list$means[, -c("order")], 
      by.x = "id", by.y = "pathID", all.x = TRUE)
spd_results <- merge(spd_results, data_list$paths[, c("pathID", "emitter")] %>% unique, 
      by.x = "id", by.y = "pathID", all.x = TRUE)

spd_results$differential <- factor(spd_results$differential, 
                            levels = c("dS", "dA1","dA2", "dA3", "dx"))
# add relative changes
spd_results[, "rel_value" := value / abs(total_change)]

# plot ---------------------

ggplot(spd_results[emitter == 911], aes(x = differential, 
                             y = value, 
                             group = id,
                             col = id), alpha = 0.3) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.5, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T)

ggplot(spd_results[emitter %in% ids], aes(x = differential, 
                                 y = rel_value, 
                                 group = id,
                                 col = id), alpha = 0.3) + 
  geom_boxplot(aes(group = NULL, col = NULL)) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.7, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T) + 
  theme_bw()

spd_results[, sum(rel_value), by = id]

colnames_A_mat[id == 888]
ids <- colnames_A_mat[grepl("*coastal water*", industry)]$id
ids <- colnames_A_mat[grepl(combine_words_regexp(c("coal", "electricity")), 
                            industry, perl = TRUE)]$id




# _______________________________________####
# with pxp200 ------------------------------------------------------------------

############################################################################## # 
##### settings #################################################################
############################################################################## # 
# _a) Load external settings and functions -------------------------------------
source("./settings.R")
source("./functions.R")
Rcpp::sourceCpp('SPD_functions.cpp')

colnames_y[id %in% ids_fd, "newid" := 1:.N]

list.dirs(path2temp_results)
path2model_results <- file.path(path2temp_results, 
                                "2019-09-11 17:22:46_1.70.71.405.415.416_1123_SPA") 

threshold <- 100
############################################################################## # 
##### 1. Load data #############################################################
############################################################################## # 
# _a) EB3 for two years --------------------------------------------------------
years <- c(1995, 2016)
table <- create_named_list(years)
y_as_vec <- TRUE
y_agg <- FALSE




for(i in years) {
  IOtable <- readRDS(file.path(path2exiobase36, 
                               paste0("IOT_", i, "_pxp"),
                               "EB3_table_SALY.RData"))
  IOtable$S <- IOtable$S %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>%
    matrix(nrow = 1)# only CO2 emissions
  IOtable$L <- as.matrix(IOtable$L)
  #IOtable$A <- as.matrix(IOtable$A)
  IOtable$Y <- IOtable$Y %>% as.matrix %>% .[,ids_fd] %>% rowSums %>% 
    as.matrix(ncol = 1)
  table[[as.character(i)]] <- list("S" = IOtable$S, 
                                   "L" = IOtable$L, 
                                   "Y" = IOtable$Y)
  # 
  # if (y_as_vec) {
  #   IOtable$Y <- IOtable$Y %>% rowSums
  #   IOtable$x <- calculate_x(Y = IOtable$Y, L = IOtable$L)
  #   table[[as.character(i)]] <- list("S" = IOtable$S, 
  #                                    "A" = IOtable$A, 
  #                                    "x" = IOtable$x)
  # } else {
  #   if (y_agg) {
  #     Y_dom <- Y[, colnames_y[country == "DE"]$newid %>% na.exclude]
  #     Y_exp <- Y[, colnames_y[country != "DE"]$newid %>% na.exclude] %>% 
  #       rowSums
  #     Y <- cbind(Y_dom, Y_exp)
  #   }
  #   table[[as.character(i)]] <- list("S" = S, "A" = A, "L" = L, "Y" = Y)
  # } 
  # rm(IOtable)
  # gc()
  cat(i, "")
}

# spd_results <- lapply(table, function(x) x[["Y"]] %>% as.matrix %>% decompose_final_demand)
# for(i in 1:length(years)){
#   table[[i]]$Y <- NULL
#   #table[[i]]$x <- NULL
#   table[[i]] <- (c(table[[i]], spd_results[[i]]))
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

# begin spd_results
# indices_list <- lapply(indices_list, function(i) {
#   apply(i, c(1,2), function(x) {
#     ifelse(x > 4, x %% 4 + 1, x)
#   })
#   
# })
# indices_list <- lapply(indices_list, function(x) {
#   x[1:10,]
# })

# end spd_results

# table$`1995`$L %>% ncol
# table$`1995`$B %>% dim %>% .[2]
# table$`1995`$d %>% dim %>% .[2]
# table$`1995`$B %>% dim
# indices_list[[2]][1,] %>% as.list


# 2. Run Structural Path Decomposition algorithm --------------------------------

system.time(spd_results <- SPD(table[[1]], table[[length(table)]], 
                        indices = indices_list))



# 3. prepare data ----------------
spd_results <- lapply(spd_results, function(x) {
  dt <- x %>% 
    as.data.table 
  names <- c("dS", paste0("dA", 1:(ncol(dt)-2)), "dx")
  setnames(dt, names)
  return(dt)
})


for (i in 1:length(spd_results)) {
  id_vec <- apply(indices_list[[i]], 1, function(x) {
    paste0(x, sep = "", collapse = "-")
  })
  spd_results[[i]][, "id" := id_vec]
}





spd_results <- lapply(spd_results, function(x) {
  melt(x, id.vars = "id", variable.name = "differential")
}) %>% 
  rbindlist(idcol = "order")
spd_results[id == "1-911-879-906-906-1", sum(value)]


data <- spd_results[, sum(value),by = differential]
ggplot(data, aes(x = differential, 
                 y = V1)) + 
  geom_bar(stat = "identity")

spd_results[, id := as.factor(id)]
spd_results[, "total_change" := sum(value), by = id]

# add information for paths

spd_results[, "id" := substr(id, 3, nchar(id %>% as.character)-2)]
data_list

spd_results <- merge(spd_results, data_list$means[, -c("order")], 
              by.x = "id", by.y = "pathID", all.x = TRUE)
spd_results <- merge(spd_results, data_list$paths[, c("pathID", "emitter")] %>% unique, 
              by.x = "id", by.y = "pathID", all.x = TRUE)

spd_results$differential <- factor(spd_results$differential, 
                            levels = c("dS", "dA1","dA2", "dA3", "dx"))
# add relative changes
spd_results[, "rel_value" := value / abs(total_change)]


spd_results[rank < 10]
data_list$paths[pathID == "1021-1128-1123"]

spd_results2print <- merge(spd_results[, c("id", "rank", "order", "differential", 
                      "value", "rel_value")], 
      data_list$paths[, c("pathID", "supply_chain")], 
      by.x = "id", by.y = "pathID", all = FALSE)

spd_results2print <-  unique(spd_results2print)
spd_results2print[, lapply(.SD, round, digits = 2), 
                  .SDcols = c("value", "rel_value")]
spd_results2print[, value := round(value, 0)]
spd_results2print[, rel_value := round(rel_value, 2)]

saveRDS(spd_results2print, file.path(path2temp_results, 
                                     "spd_results2print_CO2.RData"))

DT::datatable(spd_results2print)
# plot ---------------------

ggplot(spd_results[emitter == 1128], aes(x = differential, 
                                        y = value, 
                                        group = id,
                                        col = id), alpha = 0.3) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.5, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T)

ggplot(spd_results[emitter %in% ids], aes(x = differential, 
                                          y = rel_value, 
                                          group = id,
                                          col = id), alpha = 0.3) + 
  geom_boxplot(aes(group = NULL, col = NULL)) + 
  geom_point(aes(size = mean_value), 
             alpha = 0.7, show.legend = T) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.3, show.legend = FALSE) +
  geom_line(linetype = "dotted", alpha = 0.3, show.legend = T) + 
  theme_bw()

spd_results[, sum(rel_value), by = id]

colnames_A_mat[id == 888]
ids <- colnames_A_mat[grepl("*coastal water*", industry)]$id
ids <- colnames_A_mat[grepl(combine_words_regexp(c("coal", "electricity")), 
                            industry, perl = TRUE)]$id

# 4. for total ----------------------------------------
sda <- .SDA.lmdi(table$`1995`, table$`2016`, 
                 parallel = TRUE, n.cores = 6)


# THE END ---------------------------------------------------------------------
