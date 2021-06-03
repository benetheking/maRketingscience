#' Utilities for the maRketingscience package
#'
#' This function allows you to quickly create a dummy variable for controlling outliers
#' @keywords utilities for maRketingscience
#' @export
#' @examples
#' write_quick_sd()

## remove adbanks and log
remove_clutter <- function(input) {
  i2 <- input %>%
    str_remove_all(., "log_|lag_|05|10|15|20|30|40|50|60|70|80|90")
  return(i2)
}

#### Adbanking helpers start here
## define default adbanks for function calls
adbanks_default <- c(5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90)

## core function for adbanking of a Dataframe
adbank_base <- function(data_vec, var_name = "var.name", adbank_values = adbanks_default) {

  adbanks <- adbank_values

  ## define results matrix to be filled:
  step <- matrix(0, nrow = length(data_vec), ncol=length(adbanks)) %>%
    as.data.frame(.)
  resu <- cbind(data_vec, step)
  names(resu) <- c(var_name, paste0(var_name, adbanks))

  ## fill the first line:
  for (i in 2:ncol(resu)) {
    resu[1, i] <- resu[1, 1]*(adbanks[(i-1)]/100)
  }
  ## fill up the remaining cells:
  for (j in 2:ncol(resu)) {
    for (k in 2:nrow(resu)) {
      resu[k, j] <- resu[k, 1]*(adbanks[(j-1)]/100) + resu[(k-1), j]*(1-(adbanks[(j-1)]/100))
    }
  }
  return(resu)
}
