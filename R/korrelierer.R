#' A function for finding correlated VARs by name
#'
#' This function searches for correlations towards the residuals and ranks them by strength..
#' @param daten the data that you wish to find CORed VARs in. Will be searched for any VAR containing the variable string
#' @param modell name the model that your trying to find CORs for (that's where the FUN get the RESIDs) - defaults to model1.
#' @param variable name the VAR you wish to find CORs for. defaults to "TV"
#' @keywords find CORs by NAME
#' @export
#' @examples
#' korrelierer()
korrelierer <- function(daten, modell = model1, variable="TV") {
  ## Finde raus, was schon im Modell ist, und delete es
  muss_raus <- modell$model %>%
    names() %>%
    gsub(pattern="log[[:punct:]]|05|10|20|30|40|50|60|70|80|90", replacement="", x=.) %>%
    gsub(pattern="[[:punct:]] 1[[:punct:]]", replacement="", x=.) %>%
    str_trim()

  raus_damit <- paste0(
    "^(", paste(muss_raus[-1], collapse="|"), "|", paste0("log_", muss_raus[-1], collapse="|"), ")")

  ## KORRELATIONEN
  daten %>%
    select(-av, #-Date,
           -matches(raus_damit)) %>%
    cor(modell$residuals, .) %>%
    t() %>%
    as.data.frame() %>%
    mutate(namen = row.names(.)) %>%
    arrange((V1)) %>%
    filter( grepl(as.character(variable), namen) ) %>%
    filter( !is.na(V1) ) %>%
    arrange(V1) #%>% tail(30)
  # arrange(desc(V1))
}
