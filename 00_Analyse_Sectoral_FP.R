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
source("./settings.R")
source("./functions.R")

path2exiobase <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3"
years <- 1995:2011


ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id

############################################################################## # 
##### load data #############################################################
############################################################################## # 

# 1) Calculate Sectoral footprints in a loop ---------------------------------
data <- colnames_A_mat
for(year in years) {
  cat(year, "")
  # 1. load data
  S <- fread(file.path(path2exiobase, paste0("S_", year, ".csv"))) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(path2exiobase, paste0("L_", year, ".csv"))) %>% as.matrix
  Y <- fread(file.path(path2exiobase, paste0("Y_", year, ".csv")), select = ids_fd) %>%
    as.matrix %>% rowSums
  x <- calculate_x(Y = Y, L = L)
  
  # 2. calculate emisions
  direct_em <- S[1, id_sector] * x[id_sector] # Si * xi
  # set diagonals to zero to avoid double counting
  L2 <- L
  diag(L2) <- 0
  indirect_em <- S %*% diag(L2[,id_sector] * x[id_sector]) %>% as.numeric # S * L'i * xi
  indirect_em[id_sector] <- direct_em
  
  # 3. bind together
  temp <- data.table("id" = 1:7987, "value" = indirect_em %>% as.numeric)
  data <- merge(data, temp, by = "id")
  setnames(data, "value", year %>% as.character)
}

# 2) Prepare data --------------------------------------------------------------

data <- melt(data, id.vars = c("id", "country", "industry"), variable.name = "year")
data[, "rank" := frankv(value, order = -1L), by = year]

data[, year := year %>% as.character %>% as.integer]




# _b) write to disk -----------------------------------------------------------------
fwrite(data, file.path(path2temp_results, 
                       paste0("sectoralCarbonFP", id_sector, ".csv") ))

# select important industries and countries: 
i_ind <- data[, mean(value), by = .(industry)][V1 > 0.005 * sum(V1)]$industry
i_count <- data[, mean(value), by = .(country)][V1 > 0.005 * sum(V1)]$country


# 3) Plots ---------------------------------------------------------------------
# _a) Animated gif -------------------------------------------------------------
p <- ggplot(data[industry %in% i_ind & country %in% i_count], 
       aes(x = country, y = industry, size = value, col = value %>% log)) + 
  geom_point() +
  scale_size_continuous(range = c(0.1, 12)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis() + 
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

animate(p, height = 800, width = 1200, fps = 3)
anim_save(filename = file.path(path2plot, "sectoralfp_706_3fps.gif"))



# _________________----
# with pxp200 ---------

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source("./settings.R")
source("./functions.R")




############################################################################## # 
##### load data #############################################################
############################################################################## # 

# 1) Calculate Sectoral footprints in a loop ---------------------------------
data <- EB3_metadata$colnames200[, c("id", "country_name", "product200_name")]
year <- 2016
for(year in years) {
  cat(year, "")
  # 1. load data
  ipath <- file.path(path2exiobase36, paste0("IOT_", year, "_pxp"))
  S <- fread(file.path(ipath, "S.txt")) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  L <- fread(file.path(ipath, "L.txt"), select = id_sector) %>% 
    unlist
  Y <- fread(file.path(ipath, "Y.txt"), select = ids_fd) %>%
    as.matrix %>% rowSums
  x <- fread(file.path(ipath, "x.txt")) %>% unlist
  
  # 2. calculate emisions
  direct_em <- S[1, id_sector] * x[id_sector] # Si * xi
  # set diagonals to zero to avoid double counting
  L[id_sector] <- 0
  indirect_em <- S %*% diag(L * x[id_sector]) %>% as.numeric # S * L'i * xi
  indirect_em[id_sector] <- direct_em
  
  # 3. bind together
  temp <- data.table("id" = 1:9800, "value" = indirect_em %>% as.numeric)
  data <- merge(data, temp, by = "id")
  setnames(data, "value", year %>% as.character)
}

34792.29 # kt
35 #Mt
((S %>% as.numeric) * x)[7433]
3.627379 #kt
2.266116
151000 / 3628379000

S %*% x
7722124 #kt


# 322 kt iron ore extraction
# 2) Prepare data --------------------------------------------------------------

data <- melt(data, id.vars = c("id", "country_name", "product200_name"), 
             variable.name = "year")
data[, "rank" := frankv(value, order = -1L), by = year]

data[, year := year %>% as.character %>% as.integer]




# _b) write to disk -----------------------------------------------------------------
fwrite(data, file.path(path2temp_results, 
                       paste0("sectoralFP", 
                              paste(ids_stressor, collapse = "."),  
                                    "_pxp200_", id_sector, ".csv") ))

setnames(data, c("country_name", "product200_name"), 
         c("country", "industry"))
# select important industries and countries: 
i_ind <- data[, mean(value), by = .(industry)][V1 > 0.005 * sum(V1)]$industry
i_count <- data[, mean(value), by = .(country)][V1 > 0.005 * sum(V1)]$country


# 3) Plots ---------------------------------------------------------------------
# _a) Animated gif -------------------------------------------------------------
p <- ggplot(data[industry %in% i_ind & country %in% i_count], 
            aes(x = country, y = industry, size = value, col = value %>% log)) + 
  geom_point() +
  scale_size_continuous(range = c(0.1, 12)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis() + 
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

animate(p, height = 800, width = 1200, fps = 3)
anim_save(filename = file.path(path2plot, paste0("sectoralfp_pxp200_", 
                                                 paste(id_sector, collapse = "."), 
                                                 "_3fps.gif")))





# _b) stack area plot ---------------------------------------------------------
data <- fread(file.path(path2temp_results, "sectoralCarbonFP_pxp200_1123.csv"))
data[, "order" := "indirect"]
data[id == id_sector, "order" := "direct"]
data[, value := value / 1E3] # to tonnes
data[, sum(value), by = .(order, year)] %>% 
  ggplot(aes(x = year, y = V1)) + 
  geom_area(aes(fill = order)) + 
  scale_fill_viridis_d() + 
  ylab("CO2 emissions [t]")


# _________________----
# Layer by Layer ---------

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source("./settings.R")
source("./functions.R")

path2exiobase <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.6/constant_prices"
years <- 1995:2016


ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
#ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
ids_stressor <- EB3_metadata$stressorsV3.6[grepl(combine_words_regexp(c("Iron", "Extraction")), 
                                                 stressor_name, perl = TRUE)]$id
id_sector <- EB3_metadata$colnames200[country_name == "Germany" & 
                                        grepl("Motor vehicles", product200_name)]$id
n_layers <- 9
############################################################################## # 
##### load data #############################################################
############################################################################## # 

# 1) Calculate Sectoral footprints in a loop ---------------------------------
data <- EB3_metadata$colnames200[, c("id", "country_name", "product200_name")]
year <- 1995
list <- vector("list", length(years))
count <- 1
for(year in years) {
  cat(year, "")
  # 1. load data
  ipath <- file.path(path2exiobase36, paste0("IOT_", year, "_pxp"))
  S <- fread(file.path(ipath, "S.txt")) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  x <- fread(file.path(ipath, "x.txt")) %>% unlist
  A <- fread(file.path(ipath, "A.txt")) %>% as.matrix
  Anew <- A
  em_list <- vector("list", n_layers)
  em_list[[1]] <- S[1,id_sector] * x[id_sector]
  S[1,id_sector] <- 0
  for (ilayer in 2:n_layers) {
    Anew <- Anew %*% A
    
    em_list[[ilayer]] <- S %*% diag(Anew[,id_sector] * 
                                      x[id_sector]) %>% 
      as.numeric # S * L'i * xi
    
  }
  temp <- lapply(em_list, function(x) {
    data.table("id" = 1:9800, "value" = x %>% as.numeric) %>% 
      .[value > 0] %>% 
      merge(data, ., by = "id")
  }) %>% 
    rbindlist(idcol = "layer")
  temp[, "year" := year]
  list[[count]] <- temp
  count <- count + 1
  cat(year, "")
}

# 2) Prepare data --------------------------------------------------------------

data <- rbindlist(list)
# add total emissions
total <- fread(file.path(path2temp_results, 
                         paste0("sectoralFP", 
                                paste(ids_stressor, collapse = "."),  
                                "_pxp200_", id_sector, ".csv") ))
total <- total[value > 0]
setnames(total, "value", "total")
data <- merge(data, total[, c("id", "year", "total", "rank")], 
      by = c("id", "year"), all.x = TRUE)

data[, "value_prop" := value / total]
data[, "value_cum" := cumsum(value), by = .(id, year)]
data[, "sum_by_year" := sum(total), by = .(year, layer)]
data_agg <- data[rank > 8, lapply(.SD, sum), 
     .SDcols = "value_cum", by = .(year, layer)]
data_agg[, "country_name" := "RoW"]
data_agg <- rbindlist(list(data_agg, data[rank <= 8, colnames(data_agg), with = FALSE]), 
          use.names = TRUE)


ggplot(data, aes(x = layer, y = value_cum)) + 
  geom_area(aes(fill = country_name), 
            position = "stack") + 
  geom_line(aes(y = sum_by_year), col = "red", 
            linetype = "dotted") +
  facet_wrap(~year)

ggplot(data, aes(x = layer, y = value_cum)) + 
  geom_area(aes(fill = country_name), 
            position = "fill") + 
  facet_wrap(~year)

ggplot(data_agg, aes(x = layer, y = value_cum)) + 
  geom_area(aes(fill = country_name), 
            position = "stack") + 
  facet_wrap(~year) + 
  scale_fill_viridis_d()



# _b) write to disk -----------------------------------------------------------------
fwrite(data, file.path(path2temp_results, 
                       paste0("sectoralFP_byLayer_", 
                              paste(ids_stressor, collapse = "."),  
                              "_pxp200_", id_sector, ".csv") ))







# junk -----

year <- 2016          
ipath <- file.path(path2exiobase, paste0("IOT_", year, "_pxp"))
S <- fread(file.path(ipath, "S.txt")) %>%
  .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
  matrix(nrow = 1)# only CO2 emissions
L <- fread(file.path(ipath, "L.txt")) %>% as.matrix
ids_fd <- colnames_y[country == "CN" & 
                       fd_category %in% c("Households", "NPISH", 
                                          "Government", "GFCF")]$id
# ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", "Government", "GFCF")]$id
Y <- fread(file.path(ipath, "Y.txt"), select = ids_fd) %>%
  as.matrix %>% rowSums

x <- calculate_x(L = L, Y = Y)

fp <- S %*% diag(x %>% as.numeric)
# fp <- S %*% x
sum(fp) / 1E12
EB3_metadata$extensions[8:28,]
sum(fp / 1E12)
pop <- 1.4E9
fp/pop/1E3


EB3_metadata$regions[country_name == "China"]


fp_dt <- data.table("emissions_Mt" = fp %>% as.numeric / 1E9)
fp_dt[, "id" := 1:.N]
fp_dt <- merge(fp_dt, EB3_metadata$colnames200, 
      by = "id")
#fp_dt[, "emissions_Mt" := fp %>% as.numeric / 1E9]
ggplot(fp_dt[, sum(emissions_Mt, na.rm = TRUE), by = product200_name] %>% 
         .[V1 > 0.002 * sum(V1)], 
       aes(x = product200_name, y = V1)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_flip()



# THE END ---------------------------------------------------------------------
