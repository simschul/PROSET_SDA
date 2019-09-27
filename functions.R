# functions 
path_extract <- function(string){  
  unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string))) %>% 
    as.numeric
}

get_emitting_sector <- function(dt) {
  n <- dt[, max(order, na.rm = TRUE)]
  dt <- dt[, c("rank", eval(paste0("dim", 1:n))), with = FALSE]
  temp <- melt(dt, id.vars = "rank") %>% 
    .[, variable := substring(variable, 4) %>% as.numeric]
  
  temp <- temp[!is.nan(value), max(variable, na.rm = TRUE), by = rank] %>%
    setnames(., "V1", "variable") %>% 
    merge(., temp, by = c("rank", "variable")) %>% 
    merge(dt, ., by = "rank")
  setnames(temp, c("variable", "value"), c("order", "emitter"))
  return(temp[])
}

get_node_pos <- function(node, n.industries = 163) {
  x <- (node %/% n.industries) + 1
  y <- node %% n.industries
  return(c("x" = x, "y" = y))
}


combine_words_regexp <- function(words) {
  regexp <- ""
  for(i in 1:length(words)) {
    regexp <- paste0(regexp, "(?=.*", words[i], ")")
  }
  return(regexp)
}


sankeySPA <- function(data.list, iyear, irank, show.agg.flows = FALSE) {
  # data.list <- data_list
  # iyear <- 2007
  # irank <- 50
  
  # __i. settings -----------------------------------------------
  ipaths <- data.list$values[, mean(value_t), 
                             by = .(pathID)][V1 > 0.005 * sum(V1)]$pathID 
  
  # __ii. data preparation --------------------------------------
  data <- data.list$values[year == iyear & rank < irank]
  max_order <- data[, max(order)]
  
  if (show.agg.flows) {
    # aggregate all small flows
    temp <- data.list$values[rank > irank & order <= max_order, 
                             sum(value_t, na.rm = TRUE), 
                             by = .(order, year)]
    temp[, "pathID" := lapply(temp$order, function(x) {
      x <- c(rep("9999-", x)) 
      x[length(x)] <- id_sector
      paste0(x, collapse = "")
    })]
    setnames(temp, "V1", "value_t")
    temp[, pathID := as.character(pathID)]
    data <- rbindlist(list(data, temp[year == iyear]), use.names = TRUE, fill = TRUE)  
  }
  
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
  # nodes2 <- merge(nodes2, fp_data[year == iyear, c("id", "value")], 
  #                 by.x = "industry", by.y = "id",
  #                 all.x = TRUE)
  # setnames(nodes2, "value", "total")
  #setnames(nodes2, "value")
  nodes2[, "no" := 0:(.N-1)]
  nodes2[, "layer" := substr(id, 1,1) %>% as.factor]
  nodes2[, "group" := ifelse(industry == 0, "void", "nonvoid")] #as.character(layer)
  nodes2[group == "nonvoid", group := as.character(industry)]
  nodes2[, group := as.factor(group)]
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
  setorderv(links2, "group", order = -1L)
  #setorderv(nodes2, "group", order = -1L)
  
  my_color <- paste0('d3.scaleOrdinal().range(["white","grey","',
                     paste0(substr(viridis(links2$group %>%
                                             unique %>% 
                                             length -2),
                                   1, 7), 
                            collapse = '", "'), 
                     '", "white", "grey","', 
                     paste0(substr(viridis(nodes2$group %>%
                                             unique %>% 
                                             length -2),
                                   1, 7), 
                            collapse = '", "'),
                     '"])')
  
  # change labels of nodes 
  nodes2$id <- lapply(nodes2$id, function(x) {
    x <- path_extract(x)
    temp <- EB3_metadata$colnames200[id == x[2]]
    return(paste0(x[1], "-", temp$country_code2, temp$product200_code))
  }) %>% 
    unlist %>% as.data.table
  
  nodes2[industry == 0, id := NA]
  
  # __iii) plot -------------------------------------------------------- 
  network <- sankeyNetwork(Links = links2, Nodes = nodes2,
                           Source = "source", Target = "target",
                           Value = "value", NodeID = "id",
                           fontSize= 11, nodeWidth = 30, 
                           LinkGroup = "group",
                           NodeGroup = "group", 
                           colourScale = my_color)
  network
  return(network)
}


#sankeySPA(data_list, 1995, 60, T)


# 1. Basic IO functions --------------------------------------------------------
read_EB3_A <- function(path) {
  data.table::fread(path, skip = 3, drop = c(1,2)) %>% 
    as.matrix  
}

calculate_x <- function(Z, Y, va, L) {
  if(!is.null(dim(Y))) Y <- apply(Y, 1, sum) # if Y is matrix
  if(missing(L)) {
    # check if mass balanced
    if(!all.equal(apply(Z, 1, sum) + Y, apply(Z, 2, sum) + va)) {
      stop("IO system is not mass balanced !!")
    }
    # calculate output
    x <- apply(Z, 1, sum) + Y  
  } else {
    x <- L %*% Y
  }
  return(x)
}


calculate_A <- function(Z, x) {
  # calculate A-matrix
  x_hat <- diag(1/x)
  A <- Z %*% x_hat
  return(A)
}
calculate_L <- function(A) {
  # calculate Leontief inverse
  I_mat <- diag(rep(1, nrow(A)))
  L <- solve(I_mat - A)
  return(L)
}
calculate_S <- function(E, x) {
  # calculate Stressor matrix
  S <- E %*% diag(1/x)
  return(S)
}

IO_creator <- function(Z, Y, va, E) {
  x <- calculate_x(Z, Y, va)
  A <- calculate_A(Z, x)
  S <- calculate_S(E, x)
  L <- calculate_L(A)
  return(list("A" = A, "L" = L, "S" = S))
}

IO_calculator <- function(S, L, Y, B, d, f, detailed = TRUE) {
  if(missing(Y)) Y <- (B %*% d) * as.numeric(f)
  x <- as.numeric(L %*% Y)
  if(detailed) B <- S %*% diag(x)
  else B <- S %*% x
  return(B)
}

create_random_IOtable <- function(n.industries, n.emissions, n.fdcats, A = FALSE) {
  x0 <- list("S" = matrix(runif(n.industries*n.emissions), n.emissions, n.industries), 
             "L" = matrix(runif(n.industries^2), n.industries, n.industries), 
             "Y" = matrix(runif(n.industries * n.fdcats), n.industries, n.fdcats))
  if(A) x0[["A"]] <- matrix(runif(n.industries^2), n.industries, n.industries)
  return(x0)
}

leontief_series_expansion <- function(A_mat, n) {
  list <- vector(mode = "list", length = n)
  list[[1]] <- diag(1, nrow = nrow(A_mat), ncol = ncol(A_mat))
  for(i in 2:n) {
    list[[i]] <- list[[i-1]] %*% A_mat
  }
  return(list)
}





calc_colrow_sums <- function(S, A, Y, L, n) {
  if(dim(S)[1] > 1) stop("S needs to be either vector or matrix with nrow == 1 
                         (currently only implemented for one stressor)")
  if(!is.null(dim(Y))) Y <- rowSums(Y)
  cat("|", rep("_", n), "|\n", sep = "")
  cat("|*", sep = "")
  total_em <- diag(S %>% as.numeric) %*% (L %*% diag(Y))
  total_row.sums <-   total_em %>% rowSums
  total_col.sums <-  total_em %>% colSums
  
  A_new <- diag(1, nrow = nrow(A), ncol = ncol(A))
  list <- create_named_list(c("row.sums", "col.sums"))
  #list[["total"]] <- data.table("row.sums" = total_row.sums, "col.sums" = total_col.sums)
  list[["row.sums"]] <- list[["col.sums"]] <- matrix(ncol = ncol(A), nrow = n)
  list$row.sums[1,] <- total_row.sums
  list$col.sums[1,] <- total_col.sums
  for(i in 2:n) {
    cat("*", sep = "")
    tmp <-  diag(S %>% as.numeric) %*% (A_new %*% diag(Y))
    total_row.sums <- total_row.sums - rowSums(tmp)
    total_col.sums <- total_col.sums - colSums(tmp)
    
    list$row.sums[i,] <- total_row.sums
    list$col.sums[i,] <- total_col.sums
    
    # list[[i]][["row.sums"]] <- rowSums(tmp)
    # list[[i]][["col.sums"]] <- colSums(tmp)
    if(i < n) A_new <- A_new %*% A
  }
  rm(tmp, i, A_new)
  gc()
  cat("|")
  return(list)
}



calc_colrow_cumsums <- function(S, A, Y, n) {
  if(dim(S)[1] > 1) stop("S needs to be either vector or matrix with nrow == 1 
                         (currently only implemented for one stressor)")
  if(!is.null(dim(Y))) Y <- rowSums(Y)
  A_new <- diag(1, nrow = nrow(A), ncol = ncol(A))
  list <- vector(mode = "list", length = n)
  for(i in 1:n) {
    tmp <-  diag(S %>% as.numeric) %*% (A_new %*% diag(Y))
    if(i > 1) {
      list[[i]][["row.sums"]] <- rowSums(tmp) + list[[i-1]][["row.sums"]]
      list[[i]][["col.sums"]] <- colSums(tmp) + list[[i-1]][["col.sums"]]
    } else {
      list[[i]][["row.sums"]] <- rowSums(tmp)
      list[[i]][["col.sums"]] <- colSums(tmp)  
    }
    if(i < n) A_new <- A_new %*% A
  }
  rm(tmp, i, A_new)
  gc()
  return(list)
}



# io.table <- io_table$year0[c("S", "A", "Y")]
# io.table <- create_random_IOtable(500, 1, 1)
# names(io.table)[2] <- "A"
# 
# spa <- function(io.table, n.layers) {
#   A_mat <- io.table[["A"]]
#   io.table[["A"]] <- NULL
#   gc()
#   paths_list <- vector("list", n.layers)
#   
#   ilayer <- 1
#   for(ilayer in 1:n.layers) {
#     dims <- get_indices(io.table)
#     cols <- paste0("Var", 1:length(dims)) 
#     paths_list[[ilayer]] <- lapply(dims, function(x) 1:x) %>% 
#       expand.grid %>%
#       as.data.table 
#     
#     paths_list[[ilayer]][, "indices" := paste0(Var1, Var2, Var3)]
#     paths_list[[ilayer]][, (cols) := NULL] 
#     
#     # option 1
#     system.time(paths_list[[ilayer]][, c("value") := extract_selected(io.table, 
#                                                                       c(Var1, Var2, Var3, Var4)) %>% 
#                                        prod, 
#          by = 1:nrow(paths_list[[ilayer]]), with = TRUE])
#     # option 2
#     system.time(paths_list[[ilayer]][, c("value") := extract_selected_1L(io.table, 
#                                                                       indices) %>% 
#                                        prod, 
#                                      by = 1:nrow(paths_list[[ilayer]])])
#     
#     
#     # option 3
#     system.time(test <- io.table$S %*% io.table$A %*% diag(io.table$Y %>% as.numeric))
#     test %>% sum
#     paths_list[[2]][, sum(value)]
#     
#     crossprod(io.table$S, io.table$A)
#     
#     # option 4
# 
#     
#     # -----
#     gc()
#     io.table <- append(io.table, list(io_table$year0[["A"]]), ilayer)
#     names(io.table)[ilayer + 1] <- paste0("A", ilayer)
#   } # ilayer
#   return(paths_list)
# }
# 
# 
# io.table <- io_table$year0[c("S", "A", "Y")]
# spa <- function(io.table, n.layers) {
#   A_mat <- io.table[["A"]]
#   io.table[["A"]] <- NULL
#   gc()
#   paths_list <- vector("list", n.layers)
#   
#   fp_fun <- function(indices, IO.list) {
#     y0 <- extract_selected(IO.list, indices) %>% prod  
#     return(y0)
#   }
#   #ilayer <- 1
#   for(ilayer in 1:n.layers) {
#     dims <- get_indices(io.table)
#     vars <- lapply(dims, function(x) 1:x) %>% 
#       expand.grid 
#     vars <- lapply(1:nrow(vars), function(x) vars[x,] %>% as.numeric)
#     tmp <- lapply(X = vars,  FUN = function(x) {
#       fp_fun(indices = x, IO.list = io.table)
#     })
#     paths_list[[ilayer]] <- array(dim = dims)
#     for(i in 1:length(tmp)) {
#       paths_list[[ilayer]][matrix(vars[[i]], ncol = length(vars[[i]]))] <- tmp[[i]]
#     }
#     rm(tmp, i)
#     gc()
#     io.table <- append(io.table, list(io_table$year0[["A"]]), ilayer)
#     names(io.table)[ilayer + 1] <- paste0("A", ilayer)
#   } # ilayer
#   return(paths_list)
# }
# 
# 
# io.table <- io_table$year0[c("S", "A", "Y")]
# spa.original <- function(io.table, n.layers) {
#   A_mat <- io.table[["A"]]
#   io.table[["A"]] <- NULL
#   gc()
#   paths_list <- vector("list", n.layers)
#   
#   fp_fun <- function(indices, IO.list) {
#     y0 <- extract_selected(IO.list, indices) %>% prod  
#     return(y0)
#   }
#   ilayer <- 1
#   for(ilayer in 1:n.layers) {
#     dims <- get_indices(io.table)
#     vars <- lapply(dims, function(x) 1:x) %>% 
#       expand.grid 
#     vars <- lapply(1:nrow(vars), function(x) vars[x,] %>% as.numeric)
#     tmp <- lapply(X = vars,  FUN = function(x) {
#       fp_fun(indices = x, IO.list = io.table)
#     })
#     paths_list[[ilayer]] <- array(dim = dims)
#     for(i in 1:length(tmp)) {
#       paths_list[[ilayer]][matrix(vars[[i]], ncol = length(vars[[i]]))] <- tmp[[i]]
#     }
#     rm(tmp, i)
#     gc()
#     io.table <- append(io.table, list(io_table$year0[["A"]]), ilayer)
#     names(io.table)[ilayer + 1] <- paste0("A", ilayer)
#   } # ilayer
#   return(paths_list)
# }
# 
# 
# x1 <- create_random_IOtable(500, 1, 1, A = TRUE)
# 
# system.time(test <- spa(io.table = x1, n.layers = 4))
# object.size(test)
# 
# 
# 
# data <- lapply(test, function(x) {
#   apply(x, c(2,3), sum) %>% as.numeric
# }) %>% as.data.table %>% t %>% 
#   as.data.table %>% 
#   .[, "layer" := 1:nrow(.)] %>% 
#   melt(id.vars = "layer", variable.name = "industry")
# 
# ggplot(data, aes(x = layer, y = value, col = industry)) + 
#   geom_line()
# 
# 
# 


# 2. Sectoral Footprint Functions ----------------------------------------------
.calc.sector.fp.direct <- function(S_mat, L_mat, y_vec, index) {
  if(missing(index)) {
    fp <- IO_calculator(S_mat, L_mat, y_vec) 
  } else {
    # only for 1 sector
    fp <- S_mat[,index] %*% (L_mat[index,] %*% y_vec)
  }
  return(fp)
}

.calc.sector.fp.indirect <- function(S_mat, L_mat, x, index) {
  diag(L_mat) <- 0 # all diagonal entries (e.g. input of cars into car industry) are already considered in the direct footprint calculations
  if(missing(index)) {
    fp <- S_mat %*% L_mat %*% diag(as.numeric(x))
  } else {
    fp <- S_mat %*% L_mat[,index] %*% x[index]  
  }
  return(fp)
}



#' Title
#'
#' @param L_mat 
#' @param S_mat 
#' @param y_vec 
#' @param index the index of the sector footprints are to be calculated. If missing results for ALL sectors are returned (higher computational expenses)
#' @param detailed shall footprints be returned split up by direct + indirect emissions?
#'
#' @return
#' @export
#'
#' @examples
calc_footprint_sector <- function(L_mat, S_mat, y_vec, index,  
                                  detailed = FALSE) {
  direct <- .calc.sector.fp.direct(S_mat = S_mat, L_mat = L_mat, 
                                   y_vec = y_vec, index = index)
  x <- calculate_x(Y = Y, L = L)
  indirect <- .calc.sector.fp.indirect(S_mat = S_mat, L_mat = L_mat, 
                                       x = x, index = index)
  if(detailed) {
    fp <- list("direct" = direct, "indirect" = indirect)
  } else {
    fp <- direct + indirect  
  }
  return(fp)
}


#' Title
#'
#' @param n number of layers. recommendation >= 8 
#' @param L_mat 
#' @param A_mat 
#' @param y_vec 
#' @param S_mat 
#' @param index see ?calc_footprint_sector
#'
#' @return
#' @export
#'
#' @examples
SPA_footprint_sector <- function(n = 8, L_mat, A_mat, y_vec, S_mat, index) {
  L_series <- leontief_series_expansion(A_mat, n)
  fp <- vector(mode = "list", length = n)
  fp[[1]] <- .calc.sector.fp.direct(index = index, S_mat = S_mat, 
                                    L_mat = L_mat, y_vec = y_vec)
  
  if(missing(index)) {
    # total output
    x <- calculate_x(L = L_mat, Y = y_vec) %>% as.numeric
    for(i in 2:n) {
      fp[[i]] <- S_mat %*% L_series[[i]] %*% diag(x) 
    }
  } else {
    # output of sector i
    x <- L_mat[index,] %*% y_vec
    for(i in 2:n) {
      fp[[i]] <- S_mat %*% L_series[[i]][,index] %*% x
    }  
  }
  return(fp)
}


# 3. SPD - structural path decomposition --------------------------------------

contribution_path <- function(path, S, A, Y) {
  len <- length(path)
  Y <- as.vector(Y)
  node.z <- Y[path[1]]
  if(len > 1) {
    for(i in 2:len) {
      node.z <- node.z * A[path[i], path[i-1]]
    }  
  }
  return(as.numeric(S[path[len]] * node.z))
}


log_mean <- function(x, y) {
  return((x - y) / (log(x) - log(y)))
}

emission_calculator <- function(list, return = c("total", "detailed")) {
  if(is.matrix(list[[3]])) list[[3]] <- apply(list[[3]], 1, sum) %>% as.numeric
  x <- as.numeric(list[[2]] %*% list[[3]])
  if("total" %in% return) B <- list[[1]] %*% x
  else if(return == "detailed") B <- list[[1]] %*% diag(x)
  return(B)
}

emission_calculator2 <- function(list, return = c("total", "detailed")) {
  Y <- list[[3]] * (list[[4]] %*% list[[5]])
  B <- emission_calculator(list = c(list[[1]], list[[2]], Y))
  return(B)
}

#FD_matrix <- matrix(runif(12), 4,3)
decompose_final_demand <- function(FD_matrix) {
  # TODO: all components should be of type matrix
  f_vec <- apply(FD_matrix, 1, sum) # FD by industry
  y_vec <- apply(FD_matrix, 2, sum) # FD by category
  f_tot <- matrix(sum(f_vec), 1,1) # Total FD
  d_vec <- matrix(y_vec / as.numeric(f_tot)) # proportion of each category
  B_mat <- FD_matrix %*% diag(1 / y_vec)
  # return(list("B_mat" = B_mat, 
  #             "d_vec" = d_vec, 
  #            "total" = f_tot))
  return(list("B" = B_mat, 
              "d" = d_vec, 
              "f" = f_tot))
}

get_indices <- function(list) {
  # get dimensions of each component
  indices <- lapply(list, function(x) {
    ind <- dim(x)
    if(is.null(ind)) ind <- c(1,1)
    return(ind)
  })
  # select 1st dimension of each + last dimension of last component
  indices <- c(indices %>% lapply(., "[", 1) %>% unlist, indices[[length(indices)]][2])
  return(indices %>% as.numeric)
}

# used in SDA function. extracts values indicates by 'indices' from all components of 'list'
extract_selected <- function(list, indices) {
  lapply(1:length(list), function(x) {
    list[[x]][indices[x], indices[x+1]]
  }) %>% unlist
}


extract_selected_1L <- function(list, indices) {
  indices <- as.numeric(strsplit(as.character(indices),"")[[1]])
  extract_selected(list, indices)
}


delta_IO <- function(x0, x1) {
  lapply(1:length(x0), function(x) {
    return(x1[[x]] - x0[[x]])
  }) %>% setNames(names(x0))
}


mat2dt <- function(mat, exclude.zeros = FALSE, exclude.na = FALSE) {
  #mat <- matrix(runif(12), 4, 3)
  dt <- mat %>% 
    as.data.table 
  dt <- suppressWarnings(melt(dt, variable.name = "col")) 
  dt[, "col" := substring(col, 2) %>% as.numeric] 
  dt[, "row" := 1:nrow(mat)]
  if(exclude.zeros) dt <- dt[value != 0]
  if(exclude.na) dt <- dt[!is.na(value)]
  setcolorder(dt, neworder = c("row", "col", "value"))
  return(dt[])
}

.SDA.lmdi <- function(year0, year1, 
                      zero.handling = FALSE, aggregate = TRUE, 
                      parallel = FALSE, n.cores) {
  
  # year0 <- io_table$year0[c("S", "L", "Y")]
  # year1 <- io_table$year1[c("S", "L", "Y")]
  # zero.handling <- FALSE
  # aggregate <- TRUE
  # parallel <- FALSE
  
  n.comp <- length(year0)
  indices <- get_indices(year0)
  vars <- lapply(indices, function(x) 1:x) %>% 
    expand.grid %>% 
    as.matrix
  decomp <- create_named_list(names(year0)) %>%
    lapply(., function(x) array(dim = indices))
  if(!parallel) {
    for(j in 1:nrow(vars)) {
      inds <- vars[j,] %>% as.numeric # combinations of coefficients
      y0 <- extract_selected(year0, inds) %>% prod
      y1 <- extract_selected(year1, inds) %>% prod
      
      if(zero.handling & (y0 == 0 | y1 == 0)) {
        # zero-value handling (as suggested in Wood & Lenzen 2006)
        # TODO: check again if it really does what it is supposed to
        if(y0 == 0 & y1 == 0) {
          res <- 0
        } else if (y0 == 0) {
          for(i in 1:n.comp) {
            x0 <- year0[[i]][inds[i], inds[i+1]]
            if(x0 == 0) {
              res <- y1
            } else {
              res <- 0
            }
            decomp[[i]][matrix(inds, ncol = length(inds))] <- res
          }
        } else if (y1 == 0) {
          for(i in 1:n.comp) {
            x1 <- year1[[i]][inds[i], inds[i+1]]
            if(x1 == 0) {
              res <- -y0
            } else {
              res <- 0
            }
            decomp[[i]][matrix(inds, ncol = length(inds))] <- res
          }
        }
        
      } else {
        # neither y0, y1, x0, x1 are zero OR zero.handling == FALSE
        y_log_mean <- log_mean(x = y1, 
                               y = y0)
        for(i in 1:n.comp) {
          x_log <- log(year1[[i]][inds[i], inds[i+1]] / year0[[i]][inds[i], inds[i+1]])
          decomp[[i]][matrix(inds, ncol = length(inds))] <- y_log_mean * x_log
        }  
      }
    }
    
  } else {
    vars <- lapply(1:nrow(vars), function(x) vars[x,] %>% as.numeric)
    .lmdi.fun <- function(j) {
      inds <- j
      y0 <- extract_selected(year0, inds) %>% prod
      y1 <- extract_selected(year1, inds) %>% prod
      y_log_mean <- log_mean(x = y1, 
                             y = y0)
      list <- create_named_list(names(year0)) 
      
      
      for(i in 1:n.comp) {
        x_log <- log(year1[[i]][inds[i], inds[i+1]] / year0[[i]][inds[i], inds[i+1]])
        list[[i]] <- y_log_mean * x_log
      }
      return(list)
    }
    
    tmp <- lapply(X = vars,  FUN = .lmdi.fun)
    #tmp <- pbmclapply(mc.cores = n.cores, X = vars,  FUN = .lmdi.fun)
    # reshape to array
    for(i in 1:length(tmp)) {
      for(ii in 1:length(decomp)) {
        decomp[[ii]][matrix(vars[[i]], ncol = length(vars[[i]]))] <- tmp[[i]][[ii]]  
      }
    }
    rm(tmp)
    gc()
  }
  if(aggregate) {
    decomp <- lapply(1:n.comp, function(x) {
      decomp[[x]] %>% apply(., c(x, x+1), sum)
    }) %>% setNames(names(year0))
  }
  return(decomp)
}

#.SDA.lmdi(list("S"= S0,"Y"= x0), list("S"= S1,"Y" = x1))
# n_ind <- 300
# n_em <- 1
# n_fd <- 1
# 
# x0 <- create_random_IOtable(n_ind, n_em, n_fd)
# x1 <- create_random_IOtable(n_ind, n_em, n_fd)
# 
# system.time(res1 <- .SDA.lmdi(x0, x1, parallel = FALSE))
# system.time(res2 <- .SDA.lmdi(x0, x1, parallel = TRUE, n.cores = 1))
# 
# all.equal(res1, res2)
# ref


# begin test
# test1 <- lapply(vars, FUN = function(j) {
#   #inds <- vars[j,] %>% as.numeric # combinations of coefficients
#   inds <- j
#   y0 <- extract_selected(year0, inds) %>% prod
#   y1 <- extract_selected(year1, inds) %>% prod
#   
#   
#     y_log_mean <- log_mean(x = y1, 
#                            y = y0)
#     decomp <- create_named_list(names(year0)) 
#     for(i in 1:n.comp) {
#       x_log <- log(year1[[i]][inds[i], inds[i+1]] / year0[[i]][inds[i], inds[i+1]])
#       decomp[[i]] <- y_log_mean * x_log
#     }
#     return(decomp)
# })
# 
# 
# 
# 
# decomp2 <- create_named_list(names(year0)) %>%
#   lapply(., function(x) array(dim = indices))
# 
# test1[[1]][[2]] # goes to vars[[1]]
# 
# vars <- lapply(1:nrow(vars), function(x) vars[x,] %>% as.numeric)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# all.equal(decomp, decomp2)
# ref
# 
# 
# 
# vars[[1]]
# result <- create_named_list(names(year0)) %>%
#   lapply(., function(x) array(dim = indices))
# 
# 
#     #[matrix(inds, ncol = length(inds))]
# 
# cl <- makeForkCluster(7)
# doParallel::registerDoParallel(cl)
# 
# acomb <- function(...) abind::abind(..., along = 4)
# guad <- foreach(r=1:3, .combine='acomb', .multicombine=TRUE) %dopar% {
#   x <- matrix(rnorm(16), 2)  # compute x somehow
#   x  # return x as the task result
# }
# dim(guad)
# 
# decomp <- array(dim = c(indices))
# vars <- lapply(c(indices), function(x) 1:x) %>% 
#   expand.grid %>%
#   as.matrix
# vars <- lapply(1:nrow(vars), function(x) vars[x,] %>% as.numeric)
# 
# foreach::foreach(j = 1:length(vars)) %dopar% {
#   #inds <- vars[j,] %>% as.numeric # combinations of coefficients
#   inds <- vars[[j]]
#   y0 <- extract_selected(year0, inds) %>% prod
#   y1 <- extract_selected(year1, inds) %>% prod
#   # neither y0, y1, x0, x1 are zero OR zero.handling == FALSE
#   y_log_mean <- log_mean(x = y1, 
#                          y = y0)
#   for(i in 1:n.comp) {
#     x_log <- log(year1[[i]][inds[i], inds[i+1]] / year0[[i]][inds[i], inds[i+1]])
#     decomp[[i]][matrix(inds, ncol = length(inds))] <- y_log_mean * x_log
#   }  
#   
# }
# stopCluster(cl)  
# 


#' Title
#'
#' @param mat 
#' @param threshold 
#' @param maxpoints 
#' @param cex 
#' @param attributes 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 

IOvisualize <- function(mat, threshold, maxpoints = 1E4, 
                        cex = "absolut", attributes, 
                        colnames = NA, rownames = NA,  ...) {
  # TODO: include color scales to visulize < and > 0
  if (maxpoints > (ncol(mat) * nrow(mat))) {
    min_threshold <- 0
  } else {
    suppressWarnings(min_threshold <- mat %>%
                       abs %>% 
                       fsort(., decreasing = TRUE, na.last = TRUE) %>%
                       .[maxpoints])  
  }
  if (missing(threshold)) {
    threshold <- min_threshold
  } 
  if (min_threshold > threshold) {
    warning(paste0("maxpoints = ", maxpoints, " reached: ", 
                   "threshold taken to ", min_threshold))
    threshold <- min_threshold
  }
  
  mat[mat < threshold & mat > -threshold] <- NA
  
  res <- mat %>% as.sparse.matrix 
  if (!missing(attributes)) {
    if (sum(c("row", "col") %in% colnames(attributes)) != 2) {
      warning("attributes needs to have both arguments col and row!")
    }
    res <- merge(res, attributes[, -"col"], by = "row", 
                 suffixes = c(".row", ".col")) %>% 
      merge(., attributes[, -"row"], by = "col", 
            suffixes = c(".row", ".col"))
  }
  
  if (missing(colnames)) colnames <- colnames(mat)
  if (missing(rownames)) rownames <- rownames(mat)
  colnames <- data.table(col = 1:length(colnames), 
                         col.names = colnames)
  rownames <- data.table(row = 1:length(rownames), 
                         row.names = rownames)
  res <- merge(res, colnames, by = "col") %>% 
    merge(., rownames, by = "row")
  res <- res %>% 
    .[, row := -row] %>% 
    st_as_sf(coords = c("col", "row"), 
             remove = FALSE) 
  res$row <- -res$row
  if (cex == "absolut") {
    res[["abs_value"]] <- abs(res$value)
    cex <- "abs_value"
  } else if (cex == "increasing") {
    cex <- "value"
  } else if (cex == "decreasing") {
    res[["dec_value"]] <- -(res$value)
    cex <- "dec_value"
  }
  mapview::mapview(res, alpha = 0.3, lwd = 0, cex = cex, 
          color = (viridis), zcol = "value", ...)
}

#' Title
#'
#' @param mat 
#'
#' @return
#' @export
#'
#' @examples

as.sparse.matrix <- function(mat) {
  mat <- data.table::as.data.table(mat)
  colnames(mat) <- paste0(1:ncol(mat))
  mat <- mat[, row := 1:.N] 
  mat <- data.table::melt(mat, id.vars = "row", na.rm = TRUE, 
                     variable.name = "col") %>% 
    .[, col := col %>% as.integer] %>% 
    .[]
  return(mat)
}

#' Title
#'
#' @param xy 
#'
#' @return
#' @export
#'
#' @examples
point2polygon <- function (xy) {
  x <- c(xy[1]-1, xy[1])
  y <- c(xy[2]-1, xy[2])
  poly <- matrix(c(x[1],y[1],
                   x[1],y[2],
                   x[2],y[2],
                   x[2],y[1],
                   x[1],y[1]), 
                 nrow = 5, byrow = TRUE)
  return(st_polygon(list(poly)))
}

# End --------------------------------------------------------------------------