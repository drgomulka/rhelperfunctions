
givens_by_row <- function(X, j) {

  stopifnot(nrow(X) ==2)
  c <- X[1,j]
  s <- X[2,j]
d  <- hypot(c,s) 
Grotation <-  matrix( c(c, -s, s, c) , 2,2 )/d
Grotation  %*% X 
 
}
