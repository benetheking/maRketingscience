#' A function for collecting and exporting data for further excel makro-based processing
#'
#' This function allows you to express your love of cats.
#' @param modell the model you want to export data for. .lm files contain a reference to the original data frame whis i used here.
#' @keywords export to excel
#' @export
#' @examples
#' rausschreiber()

## RAUSSCHREIBEN
rausschreiber <- function(modell=hans) {
  muss_raus <- modell$model %>%
    names() %>%
    #  `[`(-1) %>%
    gsub(pattern="log[[:punct:]]|05|10|20|30|40|50|60|70|80|90", replacement="", x=.) %>%
    gsub(pattern="[[:punct:]] 1[[:punct:]]", replacement="", x=.) %>%
    str_trim()

  raus <- daten %>%
    select(!!muss_raus)

  return(raus)
}
