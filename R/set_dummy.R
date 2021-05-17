#' A function for rapid creation of an outlier catch dummy variable
#'
#' This function allows you to quickly create a dummy variable for controlling outliers
#' @param modell Input data. Just name the model you want to create a dummy for
#' @keywords create dummy variable
#' @export
#' @examples set_dummy(my_default_model) ## use the object assignment operator ' <- ' to include the dummy into your dataset
#' set_dummy()

## DUMMY MUSS DU REINBALLERN
set_dummy <- function(modell) {
  calc <- modell %>%
    `$`(residuals) %>% sd()

  dummel <- modell %>%
    `$`(residuals) %>%
    map_dbl(., ~ifelse(. < -calc, -1,
                       ifelse(. > calc, 1, 0)))
  #  ## das sind die daten:
  #  dati <- hans %>%
  #    `$`(call) %>%
  #    `$`(data) %>%
  #    eval(.)

  ## schreibe Dummy in die Daten rein
  #  dummel -> dati$dummel

  #  return(dati$dummel)
  #}

  return(dummel)
}


