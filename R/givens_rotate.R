
givens_single_rotation <- function(X, idcol) {
  stopifnot(is.matrix(X))
  stopifnot("idcol must be not larger than the number of columns of X" = ncol(X) >= idcol)
  stopifnot(nrow(X) == 2)
  
  c <- X[1, idcol]
  s <- X[2, idcol]
  d <- sqrt(c^2 + s^2)
  Grotation <-  matrix(c(c, -s, s, c), 2, 2 )/d
  X_returned <- Grotation %*% X 
  #X_returned[2,idcol] <- 0
  X_returned
}
