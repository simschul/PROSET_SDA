source("./settings.R")
#source("./functions.R")


# load data --------------------------------------------------------------------
io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))

S <- io_table$year0$S
L <- io_table$year0$L
A <- io_table$year0$A
Y <- io_table$year0$Y


# functions --------------------------------------------------------------------

# Step 1 -----------------------------------------------------------------------
# calculate total emission
F_total <- IO_calculator(S = S, L = L, Y = Y)



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

# test_node <- Node()
# test_node$.path <- c(1,2,3)
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
# contributionSubtree(test_node, S, A, Y, F_total)
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

# test_tree <- Node(path = NULL)
# test_tree$.path
# test <- construct_tree(S = S, A = A, y = Y, F_total = F_total,
#                        L.max = 4, tol = 50, tree = test_tree)

# test$.childs
# test$.childs[[3]]$.childs[[1]]$.childs[[1]]$.childs[[1]] 
# 



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
  F.total <- IO_calculator(S = S, L = L, Y = Y)
  tree <- construct_tree(S = S, A = A, y = Y, F_total = F.total,
                         L.max = L.max, tol = tol, tree = Node(path = NULL))
  sink(file)
  cat("path \t value \n")
  read_tree(tree)
  sink()
  if(returnDT) return(fread(file))
}


system.time(test <- spa(S = S, A = A, y = Y, L = L, L.max = 9, 
    tol = 0.3, file = file.path(path2temp_results, "test.txt"), returnDT = TRUE))

test[, sum(value)/F_total %>% sum]

test[, "rank" := frankv(value, order = -1, ties.method = "max")]
setorder(test, rank)
# end -------------------------------------------









