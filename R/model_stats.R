#' Return actual R² for any model type, e.g. classic LMs as well as GAMs or Boosted Tree models.
#'
#' This function returns a calculation of the R² of your model based on the squared correlation between actual and predicted values. This is much more stable than the classic lm() function of S and R, which fails already when you delete the intercept.
#' @param model Input data. Just name the model you want to have the R² calculated for. Defaults to maRketingscience's "model1".
#' @keywords display R² of your MMM model
#' @export
#' @examples model_stats(model)
#' model_stats()
model_stats <- function(model = model1) {
  ## compute R2
  eins <- predict(model)
  zwei <- model$model[,1]
  korri <- cor(eins, zwei)^2

  ## compute DW test statistic
  dw_stat <- car::durbinWatsonTest(model) %>%
    `[[`(2)
  print(paste0("model.R2 is: ", round(korri*100, 1), "%", " --- ",
               "DW test statistic is: ", round(dw_stat, 2)))

}
