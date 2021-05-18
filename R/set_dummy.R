#' A function for rapid creation of an outlier catch dummy variable
#'
#' This function allows you to quickly create a dummy variable for controlling outliers
#' @param model Input data. It's just name the model that you want the dummy for.
#' @param sd Controls the cut-off value for the dummy variable that is being generated. Can be a non-integer like 1.5. Defaults to 1.
#' @keywords create dummy variable
#' @export
#' @examples set_dummy(model1) ## use the object assignment operator ' <- ' to include the dummy into your dataset
#' set_dummy()

## DUMMY MUSS DU REINBALLERN
set_dummy <- function(model = model1, sd = 1) {
  calc <- model %>%
    `$`(residuals) %>% sd()

  dummel <- model %>%
    `$`(residuals) %>%
    map_dbl(., ~ifelse(. < -(sd*calc), -1,
                       ifelse(. > (sd*calc), 1, 0)))
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


