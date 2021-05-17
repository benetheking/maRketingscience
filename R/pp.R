#' A function for fast printing of everything
#'
#' This function allows you a quick printout of all that's relevant to you
#' @param data Input data. Intended use for this function is in a tidyverse pipe, so you shouldn't have to specify this argument, usually
#' @param how_much How many lines you want printed. Helpful for when those 10 lines in a default tibble aren't enough....
#' @keywords print more than the default
#' @export
#' @examples pp(15) ## this will print 15 lines instead of the tidyverse standard of 10
#' pp()

## Print print print...
pp <- function(data, how_much = Inf){
  data %>% print(n = how_much)
}
