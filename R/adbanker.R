#' A function for adbanking a single vector.
#'
#' This function will compute a range of carry-over depot effects for testing in a linear MMM model.
#' @param data the data.frame that will be used in the adbank transformation ("is adbanked"). Make sure to use proper and correct variable names!
#' @keywords compute ADBANKs for a data.frame
#' @export
#' @examples
#' adbanker(mtcars[,c("wt", "qsec")])

## FUNCTION for ADBANKING a vector
adbanker <- function(data, adbank_values = adbanks_default) {
  ## loop over data and bind together
  result <- data %>%
    map2(.x = ., .y = names(.),
         ~adbank_base(data_vec = .x, var_name = .y)) %>%
    bind_cols(.)

  return(result)
}

