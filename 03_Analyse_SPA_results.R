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
years <- 1995:2011




############################################################################## #
###### 1. Load and prepare data ############################################################
############################################################################## #

files <- list.files(path = file.path(path2model_results), 
                    pattern = "^sector906_.*?.csv" , full.names = TRUE)
data <- lapply(files, fread) %>% 
  setNames(years %>% as.character) %>% 
  rbindlist(idcol = "year") %>% 
  .[, year := as.numeric(year)]



# 2. Analysise direct emitters -------------------------------------------------

dt <- data[, c("year", "rank", "emitter", "value[t]", "order", "supply_chain")]

dt <- na.omit(dt)

dt <- merge(dt, colnames_A_mat, by.x = "emitter", by.y = "id", all.x = TRUE)

setnames(dt, "value[t]", "value")




# _a) Plots -------------------------------------------------------

# by country and industry where co2 is emitted

ggplot(dt[rank < 10], 
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





############################################################################## #
###### 3. Plot SPA results ############################################################
############################################################################## #

# _a) load data ----------------------------------------------------------------
data_list <- readRDS(file = file.path(path2model_results, "SPAdata2analyse.RData"))
setnames(data_list$values, "value[t]", "value_t")

IDpaths <- data_list$values[, mean(rank, na.rm = TRUE), by = pathID][V1 < 100]$pathID

data_list$values[, "label" := ifelse(year == max(year), as.character(pathID), 
                                     NA_character_)]
# direct emissions
fp_data <-  fread(file.path(path2temp_results, 
                            paste0("sectoralCarbonFP", id_sector, ".csv") ))
fp_data[, id := as.character(id)]


# _b) ggplot -------------------------------------------------------------------

ggplot(data_list$values[pathID %in% IDpaths], 
       aes(x = year, y = value_t, col = pathID)) + 
  geom_point(aes(size = value_t)) + 
  geom_line(linetype = "dashed") + 
  theme(legend.position = "none") +
  expand_limits(x = c(1995, 2015)) +
  geom_label_repel(aes(label = label),
                   size = 3.5,
                   nudge_x = 10,
                   na.rm = TRUE)

data_list$paths[pathID == "938-906"]




# _c) animated gif ----------------------------------------


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


# _d) networkD3 ----------------------------------------------------------------
library(networkD3)
# __i. settings -----------------------------------------------
iyear <- 2007
irank <- 40
ipaths <- data_list$values[, mean(value_t), by = .(pathID)][V1 > 0.005 * sum(V1)]$pathID 

# __ii. data preparation --------------------------------------
data <- data_list$values[year == iyear & rank < irank]
max_order <- data[, max(order)]

# aggregate all small flows
temp <- data_list$values[rank > irank & order <= max_order, 
                         sum(value_t), 
                         by = .(order, year)]
temp[, "pathID" := lapply(temp$order, function(x) {
  x <- c(rep("9999-", x)) 
  x[length(x)] <- id_sector
  paste0(x, collapse = "")
})]
setnames(temp, "V1", "value_t")
temp[, pathID := as.character(pathID)]
data <- rbindlist(list(data, temp[year == iyear]), use.names = TRUE, fill = TRUE)

# ___define links ------------------------------------
links2 <- lapply(data$pathID, function(x) {
  x <- x %>% path_extract %>% as.character %>% 
    paste0( length(.):1, "-", .)
  if(length(x) < max_order) {
    # for graphical reasons: add "zero-flows" to all paths that are shorter than the longest one
    xnew <- vector(mode = "character", length = max_order-length(x)) 
    for(i in 1:length(xnew)) {
      xnew[i] <- paste0(max_order-i+1, "-0") 
    }
    x <- append(x, xnew, after = 0)  
  }
  n <- ifelse(length(x) == 2, 2, length(x)-1)
  res <- matrix(0, nrow = n, ncol = 2) %>% as.data.frame
  for(i in 1:n) {
    res[i,] <- x[c(i,i+1)]
  }
  return(res)
}) %>% 
  setNames(data$pathID) %>% 
  rbindlist(idcol = "pathID") %>% 
  na.omit %>% 
  # add direct emission flows
  rbind(., data.table("pathID" = id_sector %>% as.character, 
                      "V1" = paste0("1-", id_sector),
                      "V2"= paste0("1-", id_sector))) %>% 
  merge(., data[, c("pathID", "value_t")], by = "pathID")

setnames(links2, c("V1", "V2", "value_t"), c("source", "target", "value"))  
setcolorder(links2, c("source", "target", "value", "pathID"))

links2[, "zero_path" := sapply(links2$source, function(x){
  x <- x %>% path_extract 
  ifelse(x[2] == 0, "yes", "no")
})] 
links2[zero_path == "yes", value := 0.001]


# ___define nodes --------------------------------------------------
nodes2 <- data.table("id" = links2[, c("source", "target")] %>% 
                       unlist %>% 
                       unique %>% 
                       as.character())
nodes2[, "industry" := lapply(nodes2$id, function(x) {
  x %>% path_extract %>% .[2]
}) %>% as.character]
nodes2 <- merge(nodes2, fp_data[year == iyear, c("id", "value")], 
                by.x = "industry", by.y = "id",
                all.x = TRUE)
setnames(nodes2, "value", "total")
nodes2[, "no" := 0:(.N-1)]
nodes2[, "layer" := substr(id, 1,1) %>% as.factor]
nodes2[, "group" := ifelse(industry == 0, "void", "nonvoid")] #as.character(layer)

# adjust links
links2 <- merge(links2, nodes2[, c("id", "no")], 
                by.x = "source", by.y = "id", all.x = TRUE)
setnames(links2, c("source", "no"), c("sourceID", "source"))
links2 <- merge(links2, nodes2[, c("id", "no")], 
                by.x = "target", by.y = "id", all.x = TRUE)
setnames(links2, c("target", "no"), c("targetID", "target"))
links2[, "group" := sapply(links2$pathID, 
                           function(x) path_extract(x)[1] %>% as.factor)]
links2[zero_path == "yes", group := 0 %>% as.factor]


# ___ color settings -----------------------------------------------
my_color <- 'd3.scaleOrdinal().range([ "#3182bd", "white",
,"#6baed6"
,"#9ecae1"
, "#c6dbef"
, "#e6550d"
, "#fd8d3c"
, "#fdae6b"
, "#fdd0a2"
, "#31a354"
, "#74c476"
, "#a1d99b"
, "#c7e9c0"
, "#756bb1"
, "#9e9ac8"
, "#bcbddc"
, "#dadaeb"
, "#636363"
, "#969696"
, "#bdbdbd"
, "#d9d9d9"
])' # TODO extend the colors (still less colours than flows)

#my_color <- JS("d3.scaleOrdinal(d3.schemeCategory20c);")
nodes2[industry == 0, id := NA]

# __iii) plot -------------------------------------------------------- 
network <- sankeyNetwork(Links = links2, Nodes = nodes2,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "id",
                         fontSize= 12, nodeWidth = 30, 
                         LinkGroup = "group",
                         NodeGroup = "group", colourScale = my_color)
network
saveNetwork(network, 
            file = file.path(path2plot, 
                             paste0("sankey", id_sector, "_", iyear, ".html")))




# network plot first trys ------------------
IDpaths <- data_list$values[, mean(rank, na.rm = TRUE), by = pathID][V1 < 100]$pathID
ids <- path_extract(IDpaths) %>% unique 
icountries <- colnames_A_mat[id %in% ids]$country %>% unique
iindustries <- colnames_A_mat[id %in% ids]$industry %>% unique

data <- colnames_A_mat[id %in% ids]
ggplot(data, aes(x = country, y = industry, col = id)) + 
  geom_point()

dcast(data, id + industry ~ country)

mat <- matrix(0, 
              nrow = length(iindustries), 
              ncol = length(icountries))
colnames(mat) <- icountries
rownames(mat) <- iindustries

for(i in 1:nrow(mat)) {
  for(j in 1:ncol(mat)) {
    id <- data[industry == iindustries[i] & country == icountries[j]]$id
    mat[i,j] <- ifelse(is.numeric(id), id, NA) 
  }
}


# with alluvial package --------------------
devtools::install_github("erblast/easyalluvial")
suppressPackageStartupMessages( require(easyalluvial) )
suppressPackageStartupMessages( require(tidyverse) )
library(ggplot2)

data_wide = as_tibble(mtcars)
categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')

data_wide = data_wide %>%
  mutate_at( vars(categoricals), as.factor ) %>%
  mutate( car_id = row_number() )

knitr::kable( head(data_wide) )


alluvial_wide(data_wide
              , bins = 5 # Default
              , bin_labels = c('LL','ML','M','MH','HH') # Default
              , fill_by = 'all_flows'
)

alluvial_wide( select(data_wide, mpg, cyl, vs, am), fill_by = 'first_variable' )

alluvial_wide( select(data_wide, mpg, cyl, vs, am)
               , fill_by = 'first_variable'
               , order_levels = c('8','6','4') )



# riverplot --------------------------------------

edges2 <- lapply(data$pathID, path_extract) %>%
  lapply(., function(x) x[1:2] %>% as.character) %>% 
  as.data.frame(col.names = NA) %>% 
  as.matrix %>%
  t %>% 
  `rownames<-`(NULL) %>% 
  as.data.table
edges2[, "Value" := sample(10:100, .N)]
setnames(edges2, c("V1", "V2"), c("N1", "N2"))  
edges2[, "direction" := rep("A", .N)]
edges2 <- rbind(edges2, list("906", "906", 10, "A"))
edges2 <- rbind(edges2, list("911", "906", 20, "A"))

edges2[, "col" := 1:.N]
edges2$edgecol = "col"


nodes2 <- data.table("ID" = edges2[, c("N1", "N2")] %>% unlist %>% unique %>% as.character())

pos <- expand.grid("x" = 1:4, "y" = 1:3)[1:nrow(nodes2),]
nodes2 <- cbind(nodes2, pos)



river <- makeRiver( nodes2 %>% as.data.frame, edges2 %>% as.data.frame)
river$styles$`906->906`$col <- "red"
style <- list( edgestyle= "straight", nodestyle= "invisible" )
# plot the generated object
riverplot(river, lty= 0)


library(riverplot)
data(minard)
nodes <- minard$nodes
edges <- minard$edges
names(nodes) <- c( "ID", "x", "y" )
names(edges) <- c( "N1", "N2", "Value", "direction" )
# color the edges by troop movement direction
edges$col <- c( "#e5cbaa", "black" )[ factor( edges$direction ) ]
# color edges by their color rather than by gradient between the nodes
edges$edgecol <- "col"

# generate the riverplot object
river <- makeRiver( nodes, edges )
style <- list( edgestyle= "straight", nodestyle= "invisible" )
# plot the generated object
riverplot(river, lty= 1, default_style = style )
# Add cities
with( minard$cities, points( Longitude, Latitude, pch= 19 ) )
with( minard$cities, text( Longitude, Latitude, Name, adj= c( 0, 0 ) ) )








# Source: https://logfc.wordpress.com/2014/02/27/riverplot/
library(riverplot)
data(minard)
nodes <- minard$nodes
edges <- minard$edges
names(nodes) <- c( "ID", "x", "y" )
names(edges) <- c( "N1", "N2", "Value", "direction" )
# color the edges by troop movement direction
edges$col <- c( "#e5cbaa", "black" )[ factor( edges$direction ) ]
# color edges by their color rather than by gradient between the nodes
edges$edgecol <- "col"

# generate the riverplot object
river <- makeRiver( nodes, edges )
style <- list( edgestyle= "straight", nodestyle= "invisible" )
# plot the generated object
riverplot(river, lty= 1, default_style = style )
# Add cities
with( minard$cities, points( Longitude, Latitude, pch= 19 ) )
with( minard$cities, text( Longitude, Latitude, Name, adj= c( 0, 0 ) ) )




library( riverplot )
data( minard )
nodes <- minard$nodes
edges <- minard$edges
colnames( nodes ) <- c( "ID", "x", "y" )
colnames( edges ) <- c( "N1", "N2", "Value", "direction" )

# color the edges by troop movement direction
edges$col <- c( "#e5cbaa", "black" )[ factor( edges$direction ) ]
# color edges by their color rather than by gradient between the nodes
edges$edgecol <- "col"

# generate the riverplot object
river <- makeRiver( nodes, edges )

style <- list( edgestyle= "straight", nodestyle= "invisible" )
# plot the generated object
plot( river, lty= 1, default.style= style )
# Add cities
with( minard$cities, points( Longitude, Latitude, pch= 19 ) )
with( minard$cities, text( Longitude, Latitude, Name, adj= c( 0, 0 ) ) )


# igraph -------------------------
library(igraph)

fp_data <-  fread(file.path(path2temp_results, 
                            paste0("sectoralCarbonFP", id_sector, ".csv") ))
fp_data[, id := as.character(id)]


iyear = 1995

ipaths <- data_list$values[, mean(value_t), by = .(pathID)][V1 > 0.001 * sum(V1)]$pathID 
ids <- ipaths %>% 
  path_extract %>% unique
icountries <- colnames_A_mat[id %in% ids]$country %>% unique %>% sort
iindustries <- colnames_A_mat[id %in% ids]$industry %>% unique %>% sort

posdt <- copy(colnames_A_mat[id %in% ids])
posdt[, "id" := as.character(id)]
posdt[, "x" := as.factor(country) %>% as.numeric]
posdt[, "y" := as.factor(industry) %>% as.numeric]


for(iorder in data_list$values[year == iyear & order > 1]$order %>% unique %>% sort) {
  data <- data_list$values[year == 1995 & pathID %in% ipaths & order == iorder]
  if(nrow(data) > 0) {
    cat ("plot \t")
    links2 <- lapply(data$pathID, function(x) {
      x <- x %>% path_extract
      n <- ifelse(length(x) == 2, 2, length(x)-1)
      res <- matrix(0, nrow = n, ncol = 2) %>% as.data.frame
      for(i in 1:n) {
        res[i,] <- x[c(i,i+1)]
      }
      return(res)
    }) %>% 
      setNames(data$pathID) %>% 
      rbindlist(idcol = "pathID") %>% 
      na.omit
    
    links2 <- merge(links2, data[, c("pathID", "value_t")], by = "pathID")
    setnames(links2, c("V1", "V2", "value_t"), c("from", "to", "weight"))  
    setcolorder(links2, c("from", "to", "weight", "pathID"))
    
    nodes2 <- data.table("id" = links2[, c("from", "to")] %>% 
                           unlist %>% 
                           unique %>% 
                           as.character())
    nodes2 <- merge(nodes2, fp_data[year == iyear, c("id", "value")], all.x = TRUE)
    setnames(nodes2, "value", "total")
    nodes2 <- rbind(nodes2, data.table("id" = paste0("foo", 1:4), "total" = rep(0, 4)))
    
    # create net object
    net <- graph.data.frame(links2, nodes2, directed=T)
    
    
    V(net)$size <- (V(net)$total) / 1E9
    # Setting them to NA will render no labels:
    V(net)$label <- NA
    
    # Set edge width based on weight:#
    E(net)$width <-(E(net)$weight/ (sum(E(net)$weight))) * 20
    
    #change arrow size and edge color:
    E(net)$arrow.size <- 0
    
    
    pos <- merge(nodes2[!(id %in% paste0("foo", 1:4)), "id"], posdt[, c("id", "x", "y")], 
                 by = "id", all.x = TRUE, sort = FALSE)[, c("x", "y")] %>% 
      as.matrix
    print(pos)
    pos <- rbind(pos, c(1,1))
    pos <- rbind(pos, c(1,posdt$y %>% max))
    pos <- rbind(pos, c(posdt$x %>% max,posdt$y %>% max))
    pos <- rbind(pos, c(posdt$x %>% max, 1))
    
    
    plot(net, layout = pos, edge.curved = 0.2, 
         add = ifelse(iorder == 2, FALSE, TRUE), 
         edge.color = iorder)
    
  }
  cat(iorder, "")
}
axis(1, at = seq(-1, 1, length.out = length(icountries)), 
     labels = icountries, 
     las = 2, cex.axis = 0.4)
axis(2, at = seq(-1, 1, length.out = length(iindustries)), 
     labels = iindustries, 
     las = 2, cex.axis = 0.4)








# _____junk_____ -------------------

# igraph 
nodes <- fread("/home/simon/Documents/PhD_PROSET/data/toy_data/Polnet2015/Data/Dataset1-Media-Example-NODES.csv")
links <- fread("/home/simon/Documents/PhD_PROSET/data/toy_data/Polnet2015/Data/Dataset1-Media-Example-EDGES.csv")
links <-aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

net <- graph.data.frame(links, nodes, directed=T)
net <- igraph::simplify(net,remove.multiple =F, remove.loops =T)
plot(net)

# Generate colors base on media type:
colrs <-c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <-degree(net, mode="all")

V(net)$size <- deg*3
V(net)$size <-V(net)$audience.size*0.6
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:#
E(net)$width <-E(net)$weight/6


#change arrow size and edge color:
E(net)$arrow.size <- 0.8
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net, layout = l)


l <- layout.grid(net)




