#' A function for collecting and exporting data for further excel makro-based processing
#'
#' This function allows you to express your love of cats.
#' @param daten the model you want to export data for. .lm files contain a reference to the original data frame whis i used here.
#' @param modell the model you have
#' @param cutoff cut off the results set at this COR value. Accepts the range of [-1:1]. Use this for shrinking down your results set. Defaults to 0.1, as CORs below this value are usually insignificant in your average weekly MMM model. Try higher values at the beginning of your modelling.
#' @param exclude accepts strings for any VARs that you do not want in your results set. Only accepts a single string, may work with "xx|yy|zz" expressions but untested. Plan to expand to accept lists.
#' @keywords export to excel
#' @export
#' @examples
#' rang_korrelierer()


rang_korrelierer <- function(daten, modell, cutoff=0.1, exclude="none") {
  muss_raus <- modell$model %>%
    names() %>%
    gsub(pattern="log[[:punct:]]|05|10|20|30|40|50|60|70|80|90", replacement="", x=.) %>%
    gsub(pattern="[[:punct:]] 1[[:punct:]]", replacement="", x=.) %>%
    str_trim()

  raus_damit <- paste0("^(", paste(muss_raus[-1], collapse="|"), "|",
                       paste0("log_", muss_raus[-1], collapse="|"), ")")
  if(exclude == "none") {
    daten %>%
      select(-av,
             # -Date,
             -matches(raus_damit)) %>% cor(modell$residuals, .) %>%
      t() %>% as.data.frame() %>% mutate(namen = row.names(.)) %>% arrange((V1)) %>%
      filter(V1 > cutoff) %>%
      filter( !is.na(V1) ) %>%
      arrange(V1) %>%
      return(.)
    # %>% tail(30)
    # arrange(desc(V1))
  } else {         ## ELSE-Bedingung für Ausschluß von Variablen
    daten %>%
      select(-av,
             # -Date,
             -matches(raus_damit),
             -contains(exclude)) %>% cor(modell$residuals, .) %>%
      t() %>% as.data.frame() %>% mutate(namen = row.names(.)) %>% arrange((V1)) %>%
      filter(V1 > cutoff) %>%
      filter( !is.na(V1) ) %>%
      arrange(V1) %>%
      return(.)
  }
}
