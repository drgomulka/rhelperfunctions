
givens_single_rotation <- function(X, idcol) {
  stopifnot(is.matrix(X))
  stopifnot("idcol must be not larger than the number of columns of X" = ncol(X) >= idcol)
  stopifnot(nrow(X) == 2)
  
  c <- X[1, idcol]
  s <- X[2, idcol]
  #d <- sqrt(c^2 + s^2)
  d <- norm( c(c, s), type="2")
  Grotation <-  matrix(c(c, -s, s, c), 2, 2 )/d
  X_returned <- Grotation %*% X 
  #X_returned[2,idcol] <- 0
  X_returned
}


givens_x_to_r <-  function(X) {
  stopifnot(is.matrix(X))
  
  Rout <- X
  for (idcol in 1:ncol(X) )  {
    for (idrow in nrow(X):(idcol+1)) { 
      temp <- givens_single_rotation(Rout[c(idcol,idrow),],  idcol)  
      Rout[c(idcol,idrow),]  <- temp
      }
  }
  Rout[1:ncol(Rout),]
}


givens_rotate_down <- function(X, idcol) {
  stopifnot(is.matrix(X))
  stopifnot("idcol must be not larger than the number of columns of X" = ncol(X) >= idcol)
  stopifnot(nrow(X) == 2)
  
  c <- X[2, idcol]
  s <- X[1, idcol]
  #d <- sqrt(c^2 + s^2)
  d <- norm( c(c, s), type="2")
  Grotation <- matrix(c(c, s, -s, c), 2, 2 )/d
  X_returned <- Grotation %*% X 
  #X_returned[1,idcol] <- 0
  X_returned
}

swap_adjacent <- function(X, idcol) {
  stopifnot(is.matrix(X))
  stopifnot(nrow(X) < idcol)
  stopifnot(1 > idcol)
  
  Rout <- X
  if (idcol == 1) return(Rout)
  col_range <- row_range  <- c((idcol-1), idcol)
  
  temp <- givens_rotate_down(Rout[row_range,],  idcol)  
  Rout[row_range,]  <- temp
  # flip col and row order
  Rout[row_range,]  <- Rout[rev(row_range),]
  Rout[,col_range]  <- Rout[,rev(col_range)]
  Rout
}

vmove_from_to <- function(X, from_col, to_col ) {
  Rout <- X

  if (from_col == to_col) return(Rout)
  if (from_col < to_col) {
    swap_range <- (from_col + 1):to_col 
  } else {
    swap_range <- from_col:(to_col + 1)
  }
  
  for (idcol in  swap_range) {
    Rout <- swap_adjacent(Rout, idcol)
  }
  
  Rout
}
