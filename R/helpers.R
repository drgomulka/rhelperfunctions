
reduce_by_sum <- function(x)  {  purrr::reduce (x , `+`)  }



#' Title repair_fable() repairs tables which lost attributes
#'
#' @param df  fable object to repair
#' @param varname  character string correct value
#'
#' @returns repaired table
#'
#' @examples  repair_fable(df_1, "somevariablename")
repair_fable <- function(df, varname) {
  require(fabletools)
  temp <- df[[varname]]
  dimnames(temp) <- varname
  df[[varname]]  <- temp
  fabletools::as_fable(df, response = !!varname, distribution = !!varname)
}
