#' A function for adbanking a single vector.
#'
#' This function will compute a range of carry-over depot effects for testing in a linear MMM model.
#' @param data_vec the vector that will be adbanked. Can iterate over any number of variables in a dataframe using purr::map.
#' @keywords compute ADBANKs for a vector
#' @export
#' @examples
#' adbankr()

## FUNCTION for ADBANKING a vector
adbankr <- function(data_vec) {
  ## define adbanks - this should be changeable in future versions
  adbanks <- c(5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90)

  ## define results matrix to be filled:
  step1 <- matrix(0, nrow = length(tv_grp), ncol=length(adbanks))
  step2 <- as.data.frame(step1)
  resu <- cbind(tv_grp, step2)
  names(resu)[2:length(names(resu))] <- paste0(names(resu)[1], adbanks)

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

adbankr_df <- function(data) {
  ## just calls the adbank function on a DF.
  new <- data %>%
    map(., ~adbankr(.x))

  return(new)
}
