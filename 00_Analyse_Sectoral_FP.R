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
years <- 2016


ids_fd <- colnames_y[country == "DE" & fd_category %in% c("Households","NPISH", 
                                                          "Government", "GFCF")]$id
ids_fd <- colnames_y[fd_category %in% c("Households", "NPISH", 
                                        "Government", "GFCF")]$id
ids_stressor <- stressor_names[grepl("CO2", stressor)]$id
id_manuf <- Industry_classes[Class1 == "Manufacturing"]$Industry_index %>% 
  unique
ids_EU28 <- countries_class[WEO2017_short == "EU"]$Symbol1
id_sector <- EB3_metadata$colnames163[country_code1 %in% ids_EU28 &
                                        product163_id %in% id_manuf]$id

years <- 2016
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
ids_stressor <- EB3_metadata$stressorsV3.6[grepl("CO2", stressor_name)]$id
id_manuf <- EB3_metadata$pxp200[sector7 == "Manufacturing"]$id
ids_EU28 <- EB3_metadata$regions[WEO2017_short == "EU"]$id
id_sector <- EB3_metadata$colnames163[region_id %in% ids_EU28 &
                                        product163_id %in% id_manuf]$id

years <- 2016

# 1) Calculate Sectoral footprints in a loop ---------------------------------
data <- EB3_metadata$colnames200[, c("id", "country_name", "product200_name")]
#year <- 2016
for(year in years) {
  cat(year, "")
  # 1. load data
  ipath <- file.path(path2exiobase36, paste0("IOT_", year, "_pxp"))
  S <- fread(file.path(ipath, "S.txt")) %>%
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1)# only CO2 emissions
  # L <- fread(file.path(ipath, "L.txt"), select = id_sector) %>% 
  #   unlist
  L <- fread(file.path(ipath, "L.txt"), select = id_sector) %>% 
    as.matrix
  
  Y <- fread(file.path(ipath, "Y.txt"), select = ids_fd) %>%
    as.matrix %>% rowSums
  x <- fread(file.path(ipath, "x.txt")) %>% unlist
  
  # 2. calculate emisions
  direct_em <- S[1, id_sector] * x[id_sector] # Si * xi
  # set diagonals to zero to avoid double counting
  indirect_em <- lapply(1:length(id_sector), function(i) {
    temp <- L[, i]
    temp[id_sector[i]] <- 0
    indirect_em <- S %*% diag(temp * x[id_sector[i]]) %>% 
      as.numeric # S * L'i * xi
  })
  
  # L[id_sector] <- 0
  # dim(L)
  # indirect_em <- S %*% diag(L * x[id_sector]) %>% as.numeric # S * L'i * xi
  # indirect_em[id_sector] <- direct_em
  
  length(indirect_em[[1]])
  
  
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
# with pxp200 for several sectors ---------

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source("./settings.R")
source("./functions.R")




############################################################################## # 
##### load data #############################################################
############################################################################## # 
ids_stressor <- EB3_metadata$stressorsV3.6[grepl("CO2", stressor_name)]$id
ids_stressor <- EB3_metadata$stressorsV3.6[grepl("CO2|CH4|", stressor_name)]$id

#ids_stressor <- 1
id_manuf <- EB3_metadata$pxp200[sector7 == "Manufacturing"]$id
ids_EU28 <- EB3_metadata$regions[WEO2017_short == "EU"]$id
id_sector <- EB3_metadata$colnames163[region_id %in% ids_EU28 &
                                        product163_id %in% id_manuf]$id
id_sector2 <- EB3_metadata$colnames163[region_id %in% ids_EU28]$id

years <- year <- 2016

# 1) Calculate Sectoral footprints in a loop ---------------------------------
data <- EB3_metadata$colnames200[, c("id", "country_name", "product200_name")]
path2exiobase36 <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.6"
for(year in years) {
  cat(year, "")
  # 1. load data
  ipath <- file.path(path2exiobase36, paste0("IOT_", year, "_pxp"))
  E <- read_EB3_S(file.path(ipath, "satellite", "F.txt"))
  dim(E)
  dim(EB3_midpoints$matrix)
  EB3_metadata$stressorsV3.6
  %>% 
    .[ids_stressor,] %>% as.matrix %>% colSums(na.rm = TRUE) %>% 
    matrix(nrow = 1) # only CO2 emissions
  sum(E) / 1E12 # in Billion Tonnes
  L <- fread(file.path(ipath, "L.txt")) %>% 
    as.matrix
  Y <- read_EB3_Y(file.path(ipath, "Y.txt")) %>%
    .[,ids_fd] %>% 
    rowSums
  #x <- fread(file.path(ipath, "x.txt")) %>% unlist
  # 2. calculate emisions
  x <- L %*% Y
  x <- as.numeric(x)
  xhat <- diag(1/x)
  xhat[is.infinite(xhat)] <- 0
  S <- E %*% xhat
  
  # EU manufacturing emissions
  direct_em <- S[1, id_sector] * x[id_sector] # Si * xi
  sum(direct_em) / 1E9 # mil t
  # EU total emissons
  direct_em2 <- S[1, id_sector2] * x[id_sector2] # Si * xi
  sum(direct_em2) / 1E12 # should be: 3.5 Mt CO2eq
  
  
  tot <- S %*% x %>% as.numeric
  tot / 1E9 # should be: 4400 Mt CO2e
  sum(direct_em) / 1E12 # should be: 877 Mt CO2eq
  # set diagonals to zero to avoid double counting
  indirect_em <- lapply(1:length(id_sector), function(i) {
    temp <- L[, i]
    temp[id_sector[i]] <- 0
    indirect_em <- S %*% diag(temp * x[id_sector[i]]) %>% 
      as.numeric # S * L'i * xi
  })
  
  for(i in 1:length(id_sector)) {
    indirect_em[[i]][id_sector[i]] <- direct_em[i]
  }
  indirect_em_mat <- do.call(cbind, indirect_em)
  dim(indirect_em_mat)
  # how much is from elect. related emissions: 
  ids_elec <- EB3_metadata$ixi[grepl("Elect", sector7)]$id
  ids_elec <- EB3_metadata$colnames163[product163_id %in% ids_elec]$id
  
  b_elec <- indirect_em_mat[ids_elec,] %>% sum
  b_total <- indirect_em_mat %>% sum
  b_direct <- sum(direct_em)
  
  b_elec / b_total
  b_elec / b_direct
  b_direct / 1E9 # kt
  b_elec / 1E6
  # 877000 kt industry ghg emissions europe
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
