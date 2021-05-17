#' Functions to generate lagged and logged variables
#'
#' These functions transforms input data to natural logarithm form needed for a diminishing returns media analysis (logger)
#' or lagged variables. It is recommended to generated logged forms first and then apply the lagger() function over the netire dataset.
#' @param x whatever data you put into it. works with standard DFs from HexView.
#' very general, may throw NAs. typically these do not pose a problem for further modelling.
#' Function will be updated in the near future.
#' @keywords log transform
#' @export
#' @examples lagger(mtcars)
#' logger()
## MAKE LAGS
lagger <- function (x, lag = 1) {
  y <- x %>%
    map_df(., ~dplyr::lag(.x, lag, default = 0))
  names(y) <- paste0("lag", as.character(lag), "_", names(x))
  return(y)
}
