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
data[, year := as.integer(year)]

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



# THE END ---------------------------------------------------------------------
