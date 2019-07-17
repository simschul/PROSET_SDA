#' This script prepares the output from the Structural Path Analysis for the 
#' analysis. 
#' The cleaned data is safed in the folder 'temp_results'. 
#'  
#' @author Simon Schulte
#' Date: 2019-06-21 12:10:44
#' 
#' Content:
#'  


############################################################################## # 
##### settings #################################################################
############################################################################## # 
# _a) Load external settings and functions -------------------------------------
source("./settings.R")
source("./functions.R")
Rcpp::sourceCpp("SPA_functions.cpp")


id_sector <- colnames_A_mat[country == "Germany" & grepl("Manufacture of motor vehicles", industry)]$id
years <- c(1995, 2011)



############################################################################## #
###### 1. Load and prepare data ############################################################
############################################################################## #

files <- list.files(path = file.path(path2temp_results), 
                    pattern = "sector906_", full.names = TRUE)
data <- lapply(files, fread) %>% 
  setNames(years %>% as.character) %>% 
  rbindlist(idcol = "year") %>% 
  .[, year := as.numeric(year)]



# 2. Analysise direct emitters -------------------------------------------------

dt <- data[, c("rank", "emitter", "value[t]", "order", "supply_chain")]

dt <- na.omit(dt)

dt <- merge(dt, colnames_A_mat, by.x = "emitter", by.y = "id", all.x = TRUE)

setnames(dt, "value[t]", "value")



# _a) Plots -------------------------------------------------------

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














