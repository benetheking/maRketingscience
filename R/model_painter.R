#' A function for painting all these happy little observations and predictions found in your MMM model
#'
#' This function allows you to brush those time series without regrets
#' @param modell the model you have created. Dependent variable in thicc blue columns, model predictions a solid red line.
#' @keywords paint all the happy little observations from your dependent variable and the model
#' @export
#' @examples model_painter(modell = sales_model) ## this will paint a nice observed vs. predicted time series plot
#' model_painter()

## PAINT HAPPY LITTLE OBSERVATIONS
model_painter <- function(modell = model1) {
  modell %>%
    `$`(model) %>%
    `[`(1) %>%
    ts.plot(col="DarkBlue", lwd=2, type="h", ## this gives us those nice thicc column bois
            ylim=c(0, max(av)+2*sd(av)))     ## sort of want an autoscale wiggle room towards the top of the chart

  lines(predict(modell), col="Red", lwd=3, lty=1)
}
