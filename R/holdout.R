#' A function for computing a holdout test on the model file
#'
#' This function allows you to easily calculate a time-series holdout test.
#' @param model Input data. Just name the model. Defaults to "model1".
#' @keywords preview quick sales decomposition pie chart.
#' @export
#' @examples preview_torte(my_default_model)
#' preview_torte()
## COMPUTE HOLDOUT TEST
holdout <- function(model = model1) {
  ## find length and compute test-train
  n_rows <- model %>%
    `$`(., model) %>%
    nrow()
  cutoff <- round(5/6*n_rows, 0)

  ## separate datasets - could probably do this with a caret function later...
  hodl_data <- model %>%
    `$`(., model) %>%
    `[`(1:cutoff,)
  hodl_pred <- model %>%
    `$`(., model) %>%
    `[`((cutoff+1):n_rows,)

  hodl_av <- hodl_data[, 1]
  pred_av <- hodl_pred[, 1]

  ## compute and predict
  hodl_modl <- lm(av~., data=hodl_data)
  predicter <- predict(hodl_modl)
  predict_hodl <- predict(hodl_modl, newdata=hodl_pred)

  ## render output:
  print(paste0("model.R2 is: ", round(100*cor(hodl_av, predicter)^2, 1), "%", " --- ",
               "hodlout.period.R2 is: ", round(100*cor(pred_av, predict_hodl)^2, 1), "%" ) )
  return(predict_hodl)
}

