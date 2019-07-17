vec1 <- c(0.00001,4,3)
vec2 <- c(3,3,6)

prod1 <- prod_rcpp(vec1)
prod2 <- prod_rcpp(vec2)
prod2 - prod1


SDA_lmdi(vec1, vec2) %>% sum

io_table <- readRDS(file.path(path2temp_results, "IO_example.RData"))

table0 <- io_table$year0[c("S", "A", "Y")]
table1 <- io_table$year1[c("S", "A", "Y")]
indices <- list(matrix(c(1,1,4,1, 
                         1,3,2,1, 
                         1,4,4,1), ncol = 4, byrow = TRUE), 
                matrix(c(1,3,2,1,1, 
                         1,3,2,3,1, 
                         1,4,4,2,1), ncol = 5, byrow = TRUE), 
                matrix(c(1,1,4,1,1,1,
                         1,4,4,2,3,1), ncol = 6, byrow = TRUE)
)
indices <- lapply(indices, function(x) x - 1)

indices <- list(expand.grid(list(c(1), 
                                 1:4, 
                                 1:4, 
                                 1)) %>% as.matrix, 
                expand.grid(list(c(1), 
                                 1:4, 
                                 1:4,
                                 1:4,
                                 1)) %>% as.matrix, 
                expand.grid(list(c(1), 
                                 1:4, 
                                 1:4,
                                 1:4, 
                                 1:4,
                                 1)) %>% as.matrix
)

test <- SPD(table0 = table0, table1 = table1, indices)
test[[2]] %>% colSums() %>% sum
b0 <- emission_calculator(table0)
b1 <- emission_calculator(table1)
b1 - b0


(S1 %*% A1 %*% A1 %*% Y1) - (S0 %*% A0 %*% A0 %*% Y0)




