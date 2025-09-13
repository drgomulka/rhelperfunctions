
givens_single_rotation <- function(X, idcol) {
  stopifnot(is.matrix(X))
  stopifnot("idcol must be not larger than the number of columns of X" = ncol(X) >= idcol)
  stopifnot(nrow(X) == 2)
  stopifnot(1 <= idcol)
  
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
  stopifnot(ncol(X) >= idcol)
  stopifnot(1 <= idcol)
  
  Rout <- X
  if (idcol == 1) return(Rout)
  col_range <- row_range  <- c((idcol-1), idcol)
  
  temp <- givens_rotate_down(Rout[row_range,],  idcol)  
  Rout[row_range,]  <- temp
  # flip col and row order
  Rout[row_range,]  <- Rout[rev(row_range),]
  Rout[,col_range]  <- Rout[,rev(col_range)]
  colnames(Rout[col_range])  <- colnames(Rout[rev(col_range)])
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

reorder_ry <- function(pos1,  listv, vorder )  {

  #npx      <- ncol(X)
  npv       <- length(vorder)
  size_list <- length(listv)
  nextv     <- pos1
  
  # Work through tail ofVORDER finding variables which are in LISTV.
  for (i in pos1:npv) {

    # skip over vorder positions that are not on list
    if (!vorder[i] %in% listv)      next 

    # dont vmove when (i == nextv), but vmove when i larger than nextv
    if (i > nextv)  { 
      #X <- vmove_from_to(X = X, from_col = i, to_col = nextv) 
      temp1  <- c(1:(nextv-1), i, setdiff(nextv:npv, i))
      vorder <- vorder[temp1]
    }  
    
    # advance nextv by 1, because the spot is taken 
    nextv  <- nextv + 1 
    
    # exit function earlier when list used up
    if (nextv >= pos1 + size_list)  return(vorder)  
  }

  # list had more variables than were found in 
  if (nextv < pos1 + size_list) stop(paste("moved",nextv - pos1, "variables, list had",  size_list  )  )

  stop("this stop should never be triggered")
  # return(vorder)
}
