#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-06-21 12:10:44
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
source("./settings.R")
source("./functions.R")
Rcpp::sourceCpp("SPA_functions.cpp")

path2exiobase <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3"

year <- 2011
n_layers <- 6 # current max. = 6

get_emitting_sector <- function(dt) {
  dt <- dt[, c("rank", eval(paste0("dim", 1:n_layers))), with = FALSE]
  temp <- melt(dt, id.vars = "rank") %>% 
    .[, variable := substring(variable, 4) %>% as.numeric]
  
  temp <- temp[!is.nan(value), max(variable, na.rm = TRUE), by = rank] %>%
    setnames(., "V1", "variable") %>% 
    merge(., temp, by = c("rank", "variable")) %>% 
    merge(dt, ., by = "rank")
  setnames(temp, c("variable", "value"), c("order", "emitter"))
  return(temp[])
}


############################################################################## # 
##### 1. load data #############################################################
############################################################################## # 
ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id

ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id

# a) EB3 ---------------------------------

S <- fread(file.path(path2exiobase, paste0("S_", year, ".csv"))) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L <- fread(file.path(path2exiobase, paste0("L_", year, ".csv"))) %>% as.matrix
A <- fread(file.path(path2exiobase, paste0("A_", year, ".csv"))) %>% as.matrix
Y <- fread(file.path(path2exiobase, paste0("Y_", year, ".csv")), select = ids_fd) %>%
  as.matrix %>% rowSums

# reference: calc total emissions: 
total_emmissions <- emission_calculator(list(S, L, Y)) %>% as.numeric



############################################################################## # 
##### 2. run SPA algorithm #############################################################
############################################################################## # 
# _a) settings -----------------------------

system.time(colrow_sums <- calc_colrow_sums(S = S, A = A, Y = Y, L = L, n_layers))
tol_subtree <- 90#2E9
tol <- 0#1E6
apply(colrow_sums$col.sums, 1, function(x) {
  x[which(x > tol_subtree)] %>% length
}) 
#colrow_sums$col.sums[which(colrow_sums$col.sums < tol_subtree)] <- NA
# colrow_sums$col.sums %>% as.data.table %>% 
#   .[, lapply(.SD, function(x) ifelse(x < tol_subtree, NA, x))] %>% 
#   .[, lapply(.SD, min, na.rm = TRUE)] %>% 
#   sum(na.rm = TRUE) / (colrow_sums$total$col.sums %>% sum)

# _b) run function ----------------------------------------------------
system.time({
  resid <- spa_rcpp(S %>% as.numeric, 
                    A, L, Y %>% as.numeric, 
                    n = 5, tol = tol, tol_subtree = tol_subtree, tol_row = tol_subtree,
                    row_sums = colrow_sums$row.sums, col_sums = colrow_sums$col.sums, 
                    file = paste0("Germany_SPA_", year, ".txt"))
})



############################################################################## # 
##### 3. prepare results #############################################################
############################################################################## # 

# _a) read results and prepare -----------------------------------------------

test <- fread(paste0("Germany_SPA_", year, ".txt"))

test <- tstrsplit(test$paths, " ") %>% as.data.table %>%
  setnames(paste0("dim", 1:length(names(.)))) %>% 
  .[, lapply(.SD, as.numeric)] %>% 
  .[, lapply(.SD, function(x)(ifelse(x == "nan", NA, x)))] %>% 
  cbind(., test) %>% 
  .[, paths := NULL] %>% 
  .[, "rank" := frankv(value, order = -1)] %>% 
  setorder(., rank) %>% 
  .[, "cum_value" := cumsum(value)] # cumulative sums



test[, "cum_share" := cum_value / total_emmissions]

# plot(test$cum_share)
# hist(test$value %>% log)


# get industry names
test[, "supply_chain" := ""]

for(i in 1:n_layers) {
  test <- merge(test, colnames_A_mat, by.x = paste0("dim", i), by.y = "id", all.x = TRUE)
  # add column with one string for each supply chain
  test[, supply_chain := ifelse(!is.na(country), 
                                paste0(supply_chain, ifelse(i > 1, " < ", ""), country, " - ", industry), 
                                supply_chain)]
  setnames(test, c("country", "industry"), c(paste0("dim", i, "_country"), paste0("dim", i, "_industry")))
}

setorder(test, rank)




# _b) write to disk ----------------------------------------------------------

setcolorder(test, c("rank", "order", "value", "supply_chain"))
test[, value := value / 1E3] # to t
setnames(test, "value", "value[t]")
test[, `value[t]` := format(`value[t]`, scientific = TRUE)]
fwrite(test, file.path(path2temp_results, paste0("Germany", year, ".csv")), sep = "\t")


############################################################################## # 
##### 4. analyse results #############################################################
############################################################################## # 

# _a) read data ---------------------------------------------------------------
data <- list()
data[["1995"]] <- fread(file.path(path2temp_results, "Germany1995.csv"))
data[["2011"]] <- fread(file.path(path2temp_results, "Germany2011.csv"))

# _b) compare 1995 to 2011 ---------------------------------------------------
data[["dif"]] <- merge(data$`1995`[, c("rank", "order", "value[t]", "supply_chain")], 
                       data$`2011`[, c("rank", "value[t]", "supply_chain")], 
                       by = "supply_chain", suffixes = c("_1995", "_2011"))
data$dif[, "value_dif" := `value[t]_2011` - `value[t]_1995`]
data$dif[, "value_reldif" := (`value[t]_2011` - `value[t]_1995`) / `value[t]_1995`]
data$dif[, "value_absreldif" := abs(value_reldif)]


data$dif[, "rank_dif" := rank_1995 - rank_2011]
data$dif[, "value_absdif" := abs(value_dif)]


# which paths have largest absolute increase in GHG? 
setorderv(data$dif, "value_dif", order = -1)
setcolorder(data$dif, c("rank_1995", "rank_2011", "order", "value_dif", "supply_chain"))
fwrite(data$dif, file.path(path2temp_results, "germany_largest_increase.csv"))

# largest absolute changes
setorderv(data$dif, "value_absdif", order = -1)
fwrite(data$dif, file.path(path2temp_results, "germany_largest_absdif.csv"), sep = "\t")

# largest relative changes
setorderv(data$dif, "value_absreldif", order = -1)
setcolorder(data$dif, c("order", "value_reldif", "value_absdif", "supply_chain"))
fwrite(data$dif, file.path(path2temp_results, "germany_largest_reldif.csv"), sep = "\t")



# largest jump in ranking
setorderv(data$dif, "rank_dif", order = -1)
fwrite(data$dif, file.path(path2temp_results, "germany_largest_rankjump.csv"), sep = "\t")


# 5. Run SPA_sector function ---------------------------------------------------
x <- calculate_x(L = L, Y = Y) %>% as.numeric

colrow_sums <- calc_colrow_sums(S = S, A = A, Y = x, L = L, n_layers)

tol_subtree <- 3E10
tol <- 1E5#1E6
apply(colrow_sums$col.sums, 1, function(x) {
  x[which(x > tol_subtree)] %>% length
}) 
#colrow_sums$col.sums[which(colrow_sums$col.sums < tol_subtree)] <- NA
# colrow_sums$col.sums %>% as.data.table %>% 
#   .[, lapply(.SD, function(x) ifelse(x < tol_subtree, NA, x))] %>% 
#   .[, lapply(.SD, min, na.rm = TRUE)] %>% 
#   sum(na.rm = TRUE) / (colrow_sums$total$col.sums %>% sum)

# _b) run function ----------------------------------------------------
system.time({
  resid <- spa_sector(S %>% as.numeric, 
                      A, L, x,  sector = id_sector, 
                      n = 5, tol = tol, tol_subtree = tol_subtree, 
                      tol_row = tol_subtree,
                      row_sums = colrow_sums$row.sums, 
                      col_sums = colrow_sums$col.sums, 
                      file = paste0("SPAsector", id_sector, "_", year, "_RAW.txt"))
})

#_c) read data -------------------
data <- fread(file.path(paste0("SPAsector", id_sector, "_", year, "_RAW.txt")))
data <- tstrsplit(data$paths, " ") %>% as.data.table %>%
  setnames(paste0("dim", 1:length(names(.)))) %>% 
  .[, lapply(.SD, as.numeric)] %>% 
  .[, lapply(.SD, function(x)(ifelse(x == "nan", NA, x)))] %>% 
  cbind(., data) %>% 
  .[, paths := NULL] %>% 
  .[, "rank" := frankv(value, order = -1, ties.method = "random")] %>% 
  setorder(., rank) %>% 
  .[, "cum_value" := cumsum(value)] %>%  # cumulative sums
  .[] %>% 
  merge(., get_emitting_sector(.)[, c("rank", "emitter")], by = "rank")


# check if for double countings
data[emitter == id_sector]


data[, "supply_chain" := ""]
for(i in 1:n_layers) {
  data <- merge(data, colnames_A_mat, by.x = paste0("dim", i), by.y = "id", all.x = TRUE)
  # add column with one string for each supply chain
  data[, supply_chain := ifelse(!is.na(country), 
                                paste0(supply_chain, ifelse(i > 1, " < ", ""), country, " - ", industry), 
                                supply_chain)]
  setnames(data, c("country", "industry"), c(paste0("dim", i, "_country"), paste0("dim", i, "_industry")))
}

setorder(data, rank)




# _b) write to disk ----------------------------------------------------------

setcolorder(data, c("rank", "order", "value", "supply_chain"))
data[, value := value / 1E3] # to t
setnames(data, "value", "value[t]")
data[, `value[t]` := format(`value[t]`, scientific = TRUE)]
#data[, `value[t]`:= as.numeric(`value[t]`)]
fwrite(data, file.path(path2temp_results, paste0("sector", id_sector,"_", year, ".csv")), sep = "\t")

# _c) analyse data ---------------------------------------------------------
years <- c(1995, 2011)

files <- list.files(path = file.path(path2temp_results), 
                    pattern = "sector906_", full.names = TRUE)
data <- lapply(files, fread) %>% 
  setNames(years %>% as.character) %>% 
  rbindlist(idcol = "year") %>% 
  .[, year := as.numeric(year)]


# split datatable into 2: one with info on paths, one with values
cols <- paste0("dim", n_layers:1)
pathID <- do.call(paste, c(data[, ..cols], sep = "-")) %>% 
  gsub("NA-", "", .)
data[, "pathID" := pathID]


data_list <- list("paths" = data[, c("pathID", "order", paste0("dim", 1:n_layers), 
                                     "supply_chain", "emitter", 
                                     paste0("dim", 1:n_layers, "_country"), 
                                     paste0("dim", 1:n_layers, "_industry")), 
                                 with = FALSE] %>% unique, 
                  "values" = data[, c("pathID","rank", "year", "order", "value[t]")])



# end test

dt <- data[, c("rank", "emitter", "value[t]", "order", "supply_chain")]

dt <- na.omit(dt)

dt <- merge(dt, colnames_A_mat, by.x = "emitter", by.y = "id", all.x = TRUE)

setnames(dt, "value[t]", "value")



# __i. Plots -------------------------------------------------------

# by country and industry where co2 is emitted

ggplot(dt[rank < 100], 
       aes(y = industry, x = country, 
           size = value, 
           color = value %>% log)) + 
  geom_point(alpha = 0.6) + 
  scale_color_viridis(name = "log GHG-emissions [log t]") + 
  scale_size(labels = scales::scientific, name = "GHG-emissions [t]") + 
  theme_few() +
  geom_jitter(width = .3, height = .3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(yintercept = seq(1.5, length(unique(dt$industry))-0.5, 1), col = "grey") +
  geom_vline(xintercept = seq(1.5, length(unique(dt$country))-0.5, 1), col = "grey") +
  facet_wrap(~year)
# labs(title = 'Year: {frame_time}') +
# transition_time(year) +
# ease_aes('linear')



# plot changes in supply chains 

dt2 <- dt[, c("year", "rank", "value", "order", "supply_chain")]
supply_chains <- dt2[year == 2011 & rank < 101]$supply_chain

i_paths <- dt2[supply_chain %in% supply_chains & year == 1995]$supply_chain

ggplot(dt2[supply_chain %in% i_paths], 
       aes(x = rank, 
           y = value %>% log, 
           size = value, 
           col = supply_chain)) + 
  geom_point(alpha = 0.6) + 
  theme(legend.position = "none") + 
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')


dt_dif <- dt[, diff(value), by = supply_chain]
dt_dif <- merge(dt_dif, dt[, diff(rank), by = supply_chain], by = "supply_chain")
setnames(dt_dif, c("supply_chain", "value_dif", "rank_dif"))

ggplot(dt_dif, aes(x = rank_dif, 
                   y = value_dif, 
                   #size = value_dif, 
                   color = value_dif)) + 
  geom_point()




# raster
setnames(dt, c("x", "y", "value"))
raster <- rasterFromXYZ(dt)
plot(raster)




data[order == 2 & dim2_country != "Germany", dim2_country]
data$rank %>% length
data$rank %>% summary





# _d) Extract largest paths (absolute + change) --------------------------------
data_list$values[, "value_dif" := diff(`value[t]`), by = pathID]

#TODO

indices_list <- vector("list", length = n_layers)

for(i in 1:n_layers) {
  indices_list[[i]] <- sapply(data_list$values[order == i & rank < 200]$pathID %>% as.list, 
                              function(x) {
                                path_extract(x)
                              }) %>% t
  
}

indices_list[[2]]

indices_list <- lapply(indices_list, function(x) {
  cbind(1, x, 1)
})
indices_list[[1]] <- NULL

# load data
year0 <- 1995
S0 <- fread(file.path(path2exiobase, paste0("S_", year0, ".csv"))) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L0 <- fread(file.path(path2exiobase, paste0("L_", year0, ".csv"))) %>% as.matrix
A0 <- fread(file.path(path2exiobase, paste0("A_", year0, ".csv"))) %>% as.matrix
Y0 <- fread(file.path(path2exiobase, paste0("Y_", year0, ".csv")), select = ids_fd) %>%
  as.matrix %>% rowSums
x0 <- calculate_x(Y = Y0, L = L0)

table0 <- list("S" = S0, "A" = A0, "x" = x0)

year1 <- 2011
S1 <- fread(file.path(path2exiobase, paste0("S_", year1, ".csv"))) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L1 <- fread(file.path(path2exiobase, paste0("L_", year1, ".csv"))) %>% as.matrix
A1 <- fread(file.path(path2exiobase, paste0("A_", year1, ".csv"))) %>% as.matrix
Y1 <- fread(file.path(path2exiobase, paste0("Y_", year1, ".csv")), select = ids_fd) %>%
  as.matrix %>% rowSums
x1 <- calculate_x(Y = Y1, L = L1)
table1 <- list("S" = S1, "A" = A1, "x" = x1)


system.time(test <- SPD(table0, table1, indices = indices_list))

lapply(test, colSums)






data$`1995`[, paste0("dim", 1:6), with = FALSE]
data$`2011`[, paste0("dim", 1:6), with = FALSE]


for(i in 1:length(data)) {
  data[[i]] <- data[[i]][, c("rank", paste0("dim", 1:6), "order", "value[t]", "supply_chain"), with = FALSE]
  setnames(data[[i]], "value[t]", "value")
}

dt <- merge(data$`1995`, data$`2011`, 
            by = c("supply_chain", paste0("dim", 1:6), "order"), 
            all = TRUE, suffixes = c(1995, 2011)) 
dt[, "value_dif" :=  value2011 - value1995]
setorder(dt, value2011)

median_dif <- dt$value_dif %>% median(na.rm = TRUE)

dt[rank1995 < 100 & is.na(rank2011)]$rank1995 %>% hist
dt[rank2011 < 100 & is.na(rank1995)]$rank2011 %>% hist

data$`2011`[value == 1439500] 

# junk --------------------------------------------------------------------------

l_series <- leontief_series_expansion(A, 6)
fp_series <- lapply(l_series, function(x) S %*% x %*% Y) 

fp_series_sum <- fp_series %>% unlist %>% sum
fp_series_dif <- total - fp_series_sum

test[, sum(value)] + resid + fp_series_dif


subtree_total <- colrow_sums$col.sums %>% as.data.table %>% 
  .[, lapply(.SD, function(x) ifelse(x < tol_subtree, NA, x))] %>% 
  .[, lapply(.SD, min, na.rm = TRUE)] %>%
  as.numeric %>%
  .[which(. != -Inf)] %>% 
  sum(na.rm = TRUE)

#subtree_total + 
test[, sum(value)] + resid + 87.0195+75.56018+57.07821+60.0779
test[, sum(value)] + resid +105.79625+ 82.42564 +86.82803+ 87.0195
test[order == 5]  
total

colrow_sums$col.sums %>% rowSums()
((S %*% diag(Y %>% as.numeric)) + colrow_sums$col.sums[1,]) %>% sum
875+320

test[, "rank" := frankv(V2, order = -1)]
setorder(test, rank)

test[rank > 100 & rank <200]
test[dim2 == 911]
colnames_A_mat[country == "Germany" & substr(industry, 1, 6) == "Produc"]
colnames_A_mat[id == 835]





# THE END ---------------------------------------------------------------------
