#' A function for rapid sales decomposition
#'
#' This function allows you to quickly view the sales decomposition for your model.
#' NOTE: This function was orignally named preview() and was written entirely by Johannes Ritter, Senior Consultant at Annalect Hamburg
#' @param fit Input data. Just name the model you want to inspect
#' @keywords preview quick sales decomposition
#' @export
#' @examples preview(my_default_model) ## use the object assignment operator ' <- ' to include the dummy into your dataset
#' preview()

# Preview Sales Decomposition
preview <- function(fit = out) {
  require(magrittr)
  # Matrix mit Produkten: Koeffizient * Differenz zum Referenzwert
  z <- matrix(0,ncol = length(coef(fit)),nrow = nrow(fit$model))
  model <- fit$model[-1]
  ifelse(names(coef(fit))[1] =='(Intercept)',intercept <- T,intercept <- F)
  if(intercept) model <- cbind(intercept=1,model)
  for (i in 1:length(model)) {
    ifelse(coef(fit)[i]>0,
           {z[,i]=coef(fit)[i]*(model[, i]-min(model[, i]))},
           {z[,i]=coef(fit)[i]*(model[, i]-max(model[, i]))})
  }
  colnames(z) <- names(model)

  # Baseline
  basevalue <- vector(length = length(fit$coefficients))
  for (i in 1:length(coef(fit))) {
    ifelse(coef(fit)[i] > 0,
           basevalue[i] <- min(model[,i]),
           basevalue[i] <- max(model[,i]))
  }
  baseline <- basevalue %*% coef(fit)

  if(baseline>0) {
    z <- cbind.data.frame(z,"baseline"=baseline)
  } else{
    z <- (z/rowSums(z))*fitted(fit)
    z <- as.data.frame(z)
  }
  s <- colSums(z)
  if(intercept) s <- s[-1]
  t <- scales::percent(s/sum(s))
  names(t) <- names(s)
  print(t)
  return(s)
}
