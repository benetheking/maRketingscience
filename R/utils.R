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
    str_remove_all(., "log_|lag_|05|10|20|30|40|50|60|70|80|90")
  return(i2)
}
