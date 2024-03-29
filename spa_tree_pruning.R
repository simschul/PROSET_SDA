source("./settings.R")
source("./functions.R")


# load data --------------------------------------------------------------------
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))

S <- io_table$year0$S
L <- io_table$year0$L
A <- io_table$year0$A
Y <- io_table$year0$Y
x <- calculate_x(Y = Y, L= L)
F_total <- S %*% L

spa_sector_test(S = S, A = A, L = L, x = x, F_total = F_total, n = 6, 
                tol = 0.1, tol_subtree = 20, sector = 2)

fread("example.txt")


test2 <- calc_colrow_sums(S, A, Y, L, 10)

vec1 <- 1:4
vec2 <- 2:5
vec1 %*% vec2
res <- 0
for(i in 1:4) {
  res <- res + (vec1[i] * vec2[i])
}


#total_row_sum <- diag(S %>% as.numeric) %*% 

xmat <-   (L %*% diag(Y %>% as.numeric))
`*`(S) #%>% sum
%>% rowSums %>% 
  
  diag(S %>% as.numeric) %*% xmat %>% row
xmat[2,] * S[2]


L[,1] * Y[1,] 

step1 <- diag(S %>% as.numeric) %*%  diag(Y %>% as.numeric)
step2 <- diag(S %>% as.numeric) %*% (A %*% diag(Y %>% as.numeric))
step3 <- A %*% step2
step4 <- A %*% step3

S %*% diag((A %*% Y) %>% as.numeric)
S %*% ((A %*% diag(Y %>% as.numeric)))

diag(S %>% as.numeric) %*% ((A %*% A) %*% diag(Y %>% as.numeric)) #%>% sum


S %*% A %*% A %*% Y %>% sum

S %*% diag(A[1,] %>% as.numeric)



L_series <- leontief_series_expansion(A, 9)
fp_total <- diag(S %>% as.numeric) %*% (L %*% diag(Y %>% as.numeric)) 

test[, "dim1" := as.numeric(substr(path,1,1))]
S%*%L%*%Y %>% sum

fp_series <- lapply(L_series, function(x) {
  diag(S %>% as.numeric) %*% (x %*% diag(as.numeric(Y)))  
})

fp_part <- Reduce("+", fp_series)

fp_dif <- (fp_total - fp_part)/fp_total


# test recursive rcpp -------------
spa_recurs(S = S, A = A, x = 0, path = vector(length = n_layers), n_ind = 4, 
           index = 1, tol = 0, L_max = n_layers, Layer = 1, emissions = 0)


# test sf --------------------------------------------------------

library(sf)
A_raster <- raster(array(dim = c(4,4,4)))

install.packages("sf")

test2 <- test[1:10] 
test2[, "x" := V1[[1]] ]

test <- data.table("x" = 1:10, "y" = 2:11, "z" = 3:12, "z2" = 4:13, "value" = runif(10))


test %>% st_as_sf(coords = c("x", "y", "z"))



lapply(test, unlist) %>% 
  rbindlist
object.size(test)/1E6




# test tensorr -------------------------------------

library(tensorr)

ncol <- 10

A <- matrix(((runif(ncol*ncol))), ncol, ncol)
A[sample(c(T, F), ncol * ncol / 2, replace = TRUE)] <- 0
B <- matrix(((runif(ncol*ncol))), ncol, ncol)
B[sample(c(T, F), ncol * ncol / 2, replace = TRUE)] <- 0

dt1 <- as_dtensor(A)
sp1 <- as_sptensor(dt1)

dt2 <- as_dtensor(B)
sp2 <- as_sptensor(dt2)

unfold(sp1, 2)

innerprod(sp1, sp1) %>% sum


str(sp2)
sp2@dims



# functions --------------------------------------------------------------------

# Step 1 -----------------------------------------------------------------------
# calculate total emission
#F_total <- diag(S %>% as.numeric) %*% L
F_total <- S %*% L



# Create Tree-Object ------------------------------------------
# following: https://www.r-project.org/nosvn/conferences/DSC-2003/Drafts/Bengtsson.pdf


# 1. Define constructor --------------------------------------------------------

setConstructorS3("Node", function(path = 0, value = 0, 
                                  childs = list(), contribution_subtree = 0) {
  extend(Object(), "Node", 
         .path = path, 
         .value = value, 
         .childs = childs, 
         .contribution_subtree = contribution_subtree)
})

# 2. Define methods ------------------------------------------------------------


setMethodS3("addChild", "Node", function(this, child, ...) {  
  if(!("Node" %in% class(child))) 
    throw("Child must be of type Node")
  len <- length(this$.childs)
  this$.childs[[len+1]] <- child
})

setMethodS3("addNullChild", "Node", function(this, ...) {  
  len <- length(this$.childs)
  this$.childs[[len+1]] <- 0
})

isNullChild <- function(child) {  
  if("Node" %in% class(child)) return(FALSE)
  if(child == 0) return(TRUE)
}

setMethodS3("hasChilds", "Node", function(this, ...) {  
  len <- length(this$.childs)
  ifelse(len < 1, return(FALSE), return(TRUE))
})


setMethodS3("contributionNode", "Node", function(this, S, A, y, ...) {  
  len <- length(this$.path)
  y <- as.vector(y)
  node.z <- y[this$.path[1]]
  if(len > 1) {
    for(i in 2:len) {
      node.z <- node.z * A[this$.path[i], this$.path[i-1]]
    }  
  }
  return(S[this$.path[len]] * node.z)
})

setMethodS3("setValue", "Node", function(this, S, A, y, ...) {
  len <- length(this$.path)
  y <- as.vector(y)
  node.z <- y[this$.path[1]]
  if(len > 1) {
    for(i in 2:len) {
      node.z <- node.z * A[this$.path[i], this$.path[i-1]]
    }  
  }
  this$.value <- S[this$.path[len]] * node.z  
  
})

test_node <- Node()
test_node$.path <- c(1,2,3)
# setValue(test_node, S, A, Y)
# test_node$.value

setMethodS3("contributionSubtree", "Node", function(this, S, A, y, F_total) {
  Z <- this$.path
  len <- length(Z)
  node.z <- y[Z[1]]
  if(len > 1) {
    for(i in 2:len) {
      node.z <- node.z * A[Z[i], Z[i-1]]
    }  
  }
  return(F_total[Z[len]] * node.z)
})
contributionSubtree(test_node, S, A, Y, F_total)
# contribution_subtree(S, A, Y, F_total, c(1,2,3))

setMethodS3("setBothValues", "Node", function(this, S, A, y, F_total, ...) {
  len <- length(this$.path)
  y <- as.vector(y)
  node.value <- cont.subtree <- y[this$.path[1]]
  if(len > 1) {
    for(i in 2:len) {
      node.value <- node.value * A[this$.path[i], this$.path[i-1]]
      cont.subtree <- cont.subtree * A[this$.path[i], this$.path[i-1]]
    }  
  }
  this$.value <- S[this$.path[len]] * node.value  # set value
  this$.contribution_subtree <- F_total[this$.path[len]] * cont.subtree # set contribution of subtree
})


setMethodS3("getNumberOfNodes", "Node", function(this, ...) {
  nodes <- 1
  if(length(this$.childs) > 0) {
    for(i in 1:length(this$.childs)) {
      if(!isNullChild(this$.childs[[i]])) { 
        nodes <- nodes + getNumberOfNodes(this$.childs[[i]])
      }
    }  
  } 
  return(nodes)
})





# 3. Construct the Tree --------------------------------------------------------

construct_tree <- function(S, A, y, F_total, L.max, tol, tree) {
  if(length(tree$.path) == L.max) {
    #cat("L.max reached \n")
    # construct the final node
    setValue(tree, S, A, y)
    for(i in 1:length(y)) {
      addNullChild(tree)
    } # i
  } else {
    # add a new sub-tree
    setValue(tree, S, A, y)
    for(i in 1:length(y)) {
      child <- Node(path = c(tree$.path, i))
      setBothValues(child, S, A, y, F_total)
      if(child$.contribution_subtree < tol) {
        # sub-tree is negligible
        #cat("Tolerance threshold reached \n")
        addNullChild(child)
        addChild(tree, child)
      } else {
        # recursively build a new sub-tree
        addChild(tree, construct_tree(S = S, A = A, y = y, 
                                      F_total = F_total, L.max = L.max, tol = tol, 
                                      tree = child))
      }
    }
  }
  return(tree)
}

# with new contribution subtree function 
construct_tree <- function(S, A, y, F_total, L.max, tol, tree) {
  if(length(tree$.path) == L.max) {
    #cat("L.max reached \n")
    # construct the final node
    setValue(tree, S, A, y)
    for(i in 1:length(y)) {
      addNullChild(tree)
    } # i
  } else {
    # add a new sub-tree
    setValue(tree, S, A, y)
    for(i in 1:length(y)) {
      child <- Node(path = c(tree$.path, i))
      setValues(child, S, A, y)
      crit <- test[[length(child$.path)]]$row.sums[i]
      if(crit < tol) {
        # sub-tree is negligible
        #cat("Tolerance threshold reached \n")
        addNullChild(child)
        addChild(tree, child)
      } else {
        # recursively build a new sub-tree
        addChild(tree, construct_tree(S = S, A = A, y = y, 
                                      F_total = F_total, L.max = L.max, tol = tol, 
                                      tree = child))
      }
    }
  }
  return(tree)
}


# with lapply instead of foor loop

construct_tree <- function(S, A, y, F_total, L.max, tol, tree) {
  if(length(tree$.path) == L.max) {
    # construct the final node
    setValue(tree, S, A, y)
    lapply(1:length(y), function(x) addNullChild(tree))
    # for(i in 1:length(y)) {
    #   addNullChild(tree)
    # } # i
  } else {
    # add a new sub-tree
    setValue(tree, S, A, y)
    
    .add.subtree <- function(x) {
      child <- Node(path = c(tree$.path, x))
      setBothValues(child, S, A, y, F_total)
      if(child$.contribution_subtree < tol) {
        # sub-tree is negligible
        #cat("Tolerance threshold reached \n")
        addNullChild(child)
        addChild(tree, child)
      } else {
        # recursively build a new sub-tree
        addChild(tree, construct_tree(S = S, A = A, y = y, 
                                      F_total = F_total, L.max = L.max, tol = tol, 
                                      tree = child))
      }
      
    }
    
    lapply(1:length(y), FUN = .add.subtree)
    
  }
  return(tree)
}


# test_tree <- Node(path = NULL)
# test_tree$.path
# test <- construct_tree(S = S, A = A, y = Y, F_total = F_total,
#                        L.max = 4, tol = 50, tree = test_tree)

# test$.childs
# test$.childs[[3]]$.childs[[1]]$.childs[[1]]$.childs[[1]] 
# 

# tensor test ----------------------------------------------------------------

library(tensorA)

?einstein.tensor
A <- to.tensor(1:20, c(U=2,V=2,W=5))
B <- to.tensor(1:30,list(U=c("a","b","c"),V=c("B1","B2"),W=1:5))

S <- io_table$year0$S
L <- io_table$year0$L
A <- io_table$year0$A
Y <- io_table$year0$Y

A <- to.tensor(A %>% as.numeric, c(i = 7987, j = 7987))
B <- to.tensor(A %>% as.numeric, c(j = 7987, k = 7987))
system.time(result1 <- mul.tensor(X = A, Y = B, by = "j"))



B[1:10, 1:10]


ncol <- 7987
A <- to.tensor(runif(ncol * ncol), c(i=ncol,j=ncol))
B <- to.tensor(runif(ncol * ncol), c(j=ncol,k=ncol))

result1 %>% sum
system.time(A %*% B)
matmult

# for loop multi
list <- list(S, L, Y)
indices <- get_indices(list)
vars <- lapply(indices, function(x) 1:x) %>% 
  expand.grid %>% 
  as.matrix
result <- array(dim = indices)

for(j in 1:nrow(vars)) {
  cat(j, "")
  inds <- vars[j,] %>% as.numeric # combinations of coefficients
  y0 <- extract_selected(list, inds) %>% prod
  result[matrix(inds, ncol = length(inds))] <- y0
}

result1 %>% sum
result %>% sum  

apply(result, c(2,3), sum) - fp_total



# 4. Read the tree -------------------------------------------------------------

# sink(file.path(path2temp_results, "paths.txt"))
# cat("path \t value \n")
read_tree <- function(tree) {
  if(!hasChilds(tree)) return(NULL)
  for(i in 1:length(tree$.childs)) {
    if(!isNullChild(tree$.childs[[i]])) { 
      cat(tree$.childs[[i]]$.path %>% paste(collapse = "/"), "\t")
      cat(tree$.childs[[i]]$.value, "\n")
      read_tree(tree$.childs[[i]])
    }
  }
}
# read_tree(test)
# sink()
# 
# fread(file.path(path2temp_results, "paths.txt"))

# 5. All together in one function ----------------------------------------------

spa <- function(S, A, L, y, L.max, tol, file, write2disk = TRUE, returnDT = FALSE) {
  system.time(F.total <- S %*% L) %>% print
  print("F_total \n")
  
  system.time(tree <- construct_tree(S = S, A = A, y = Y, F_total = F.total,
                                     L.max = L.max, tol = tol, tree = Node(path = NULL))) %>% 
    print
  print("construct_tree \n")
  system.time({
    sink(file)
    cat("path \t value \n")
    read_tree(tree)
    sink()  
  }) %>% print
  print("write file \n")
  
  rm(tree)
  gc()
  system.time({
    if(returnDT) return(fread(file))
  }) %>% print
}


test <- spa(S = S, A = A, y = Y, L = L, L.max = 4, 
            tol = 1, file = file.path(path2temp_results, "test.txt"), returnDT = TRUE)

test[, sum(value)/F_total %>% sum]

test[, "rank" := frankv(value, order = -1, ties.method = "max")]
setorder(test, rank)
# end -------------------------------------------









