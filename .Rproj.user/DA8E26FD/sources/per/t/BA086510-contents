# functions 

# 1. Basic IO functions --------------------------------------------------------
calculate_x <- function(Z, Y, va, L) {
  if(missing(L)) {
    # check if mass balanced
    if(!all.equal(apply(Z, 1, sum) + apply(Y, 1, sum), apply(Z, 2, sum) + va)) {
      stop("IO system is not mass balanced !!")
    }
    # calculate output
    x <- apply(Z, 1, sum) + apply(Y, 1, sum)  
  } else {
    x <- L %*% apply(Y, 1, sum)
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

IO_calculator <- function(S, L, Y) {
  # calculate emissions
  x <- as.numeric(L %*% Y)
  B <- S %*% diag(x)
  return(B)
}


leontief_series_expansion <- function(A_mat, n) {
  list <- vector(mode = "list", length = n)
  list[[1]] <- diag(1, nrow = nrow(A_mat), ncol = ncol(A_mat))
  for(i in 2:n) {
    list[[i]] <- list[[i-1]] %*% A_mat
  }
  return(list)
}


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

decompose_final_demand <- function(FD_matrix) {
  # TODO: all components should be of type matrix
  f_vec <- apply(FD_matrix, 1, sum) # FD by industry
  y_vec <- apply(FD_matrix, 2, sum) # FD by category
  f_tot <- sum(f_vec) # Total FD
  d_vec <- y_vec / f_tot # proportion of each category
  B_mat <- FD_matrix %*% diag(1 / y_vec)
  return(list("total" = f_tot, 
              "B_mat" = B_mat, 
              "d_vec" = d_vec))
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


.SDA.lmdi <- function(year0, year1, zero.handling = FALSE) {
  n.comp <- length(year0)
  indices <- get_indices(year0)
  vars <- lapply(indices, function(x) 1:x) %>% 
    expand.grid
  decomp <- create_named_list(names(year0)) %>%
    lapply(., function(x) array(dim = indices))
  
  
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
      # neither y0, y1, x0, x1 are zero
      y_log_mean <- log_mean(x = y1, 
                             y = y0)
      for(i in 1:n.comp) {
        x_log <- log(year1[[i]][inds[i], inds[i+1]] / year0[[i]][inds[i], inds[i+1]])
        decomp[[i]][matrix(inds, ncol = length(inds))] <- y_log_mean * x_log
      }  
    }
  }
  
  decomp <- lapply(1:n.comp, function(x) {
    decomp[[x]] %>% apply(., c(x, x+1), sum)
  }) %>% setNames(names(year0))
  return(decomp)
}









# End --------------------------------------------------------------------------