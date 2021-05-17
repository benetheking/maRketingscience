#' A function for rapid creation of an outlier catch dummy variable
#'
#' This function allows you to quickly create a dummy variable for controlling outliers
#' @param modell Input data. Just name the model you want to create a dummy for
#' @param name any character string. will be concatenated with a .csv suffix, so you don't have to put it there
#' @keywords write away a fast SD
#' @export
#' @examples write_quick_sd(my_default_model, name="model_SD_01") ## will be written into the defined directory
#' write_quick_sd()

#### write rapid SD values
write_quick_sd <- function(modell=hans, name="quick_sd") {

  b1 <- maRketingscience::preview(modell)

  b2 <- b1 %>%
    data.frame(xxx=.)

  b3 <- b2 %>%
    mutate(namers = row.names(.)) %>%
    select(2, 1)

  write_csv2(b2, paste0(name, ".csv"))

}
