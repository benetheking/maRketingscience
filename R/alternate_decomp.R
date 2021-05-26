#' Computes alternative and somewhat simpler version of the KPI decomposition
#'
#' This function decomps a model in a very basic and intuitive manner: The coefficients are simply multiplied with the respective data column, and shares are computed on the sum of these columns. Negative effects are computed in the same step.
#' @param model Name of the model object that will be decomped. Defaults to model1.
#' @keywords alternate, straightforward model KPI decomposition.
#' @export
#' @examples
#' alternate_decomp()
alternate_decomp <- function(model = model1) {
  ## Alternative Decomp needs only data and coefficients
  dats <- blabla$model[,-1]
  coefs <- blabla$coefficients

  ## simply multiply Coefs and data columns
  resu <- map2_df(.x=dats, .y=coefs, ~sum(.x*.y))
  model_predict <- sum(resu)

  ## compose return object
  decomp_shares <- paste0(round(resu/model_predict*100, 2), "%")
  names(decomp_shares) <- names(coefs)

  return(decomp_shares)
}
