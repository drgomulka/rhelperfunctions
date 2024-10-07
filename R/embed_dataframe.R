embed_dataframe_loop <- function(df, dimension = 1) {
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  if ((dimension < 1) || (dimension >  nrow(df))) {
    stop("Invalid embedding dimension")
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  new_number_of_rows <- n_rows - dimension + 1L
  new_number_of_cols <- dimension * n_cols

  df_out <- data.frame(matrix(0, new_number_of_rows, new_number_of_cols))

  for (i in seq_len(n_cols)) {
    source_column_i <- as.vector(df[, i])
    
    for (j in  seq_len(dimension)-1 ) {    
      col_number <- i + n_cols*j
      range_of_rows <- (dimension-j):(n_rows-j) 
      shortened_column <- source_column_i[range_of_rows]
      df_out[, col_number] <- shortened_column
    }
  }

  new_colnames <- colnames(df)
  for (i in 1:(dimension-1)) {
    temp <- paste(colnames(df), ".l", i, sep = "")
    new_colnames <- c(new_colnames, temp)
  }
  colnames(df_out) <- new_colnames
  
  return(df_out)
}

embed_dataframe <- function(df, dimension = 1) {
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  if ((dimension < 1) || (dimension >  nrow(df))) {
    stop("Invalid embedding dimension")
  }
 
  tail_range <- dimension:nrow(df) 
  new_df <- df[tail_range,]
  
  for (i in seq_len(dimension-1) ) {    
    lag_window_range <- tail_range  - i #  (dimension-i):(nrow(df)-i  )
    lag_window_chunk  <- df[lag_window_range,]  
    colnames(lag_window_chunk) <- paste(colnames(lag_window_chunk), ".l", i, sep = "")
    new_df <- cbind(new_df, lag_window_chunk)
  }
  return(new_df)
}


#tests

#dff <-  data.frame(x = 1:10,y = 11:20,  z = 21:30)
#dff
#embed_dataframe(dff, dimension = 4)
#embed_dataframe(dff, dimension = 4)  == embed (as.matrix(dff), dimension =4) 
#testthat::expect_equal(embed_dataframe(dff, dimension = 4),  embed (as.matrix(dff), dimension =4) )

#embed_df_testobject <- as.matrix(embed_dataframe(dff, dimension = 4))
#colnames(embed_df_testobject) <- NULL
#attr(embed_df_testobject,"dimnames" ) <- NULL
#testthat::expect_equal(embed_df_testobject,  embed (as.matrix(dff), dimension =4) )


#embed_dataframe_chunk(dff, dimension=4)
