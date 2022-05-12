#' A function for adbanking a single vector.
#'
#' This function will compute a range of carry-over depot effects for testing in a linear MMM model.
#' @param ... the formula for base R's lm() function.
#' @param data data containing the variables in the formula. Defaults to full_data.
#' @keywords estimate MMM model
#' @export
#' @examples
#' mdl()

## WRAPPER FUNCTION for MMM
mdl <- function(..., data=full_data) {
  ## just calls the adbank function on a DF.
  mdl_obj <- lm(..., data=data)

  ## get summary and print object
  summ <- tidy(mdl_obj) %>%
    mutate(sig. = case_when(abs(p.value) <= 0.001 ~ "***",
                            abs(p.value) <= 0.01  ~ "**",
                            abs(p.value) <= 0.05  ~ "*",
                            abs(p.value) <= 0.1   ~ ".",
                            TRUE ~ "-"))
  summ2 <- model_stats(mdl_obj)

  ## return plot, summary and the model object for downstream use
  maRketingscience::model_painter(mdl_obj)
  print(summ)
  print(summ2)
  return(mdl_obj)
}


mdl
