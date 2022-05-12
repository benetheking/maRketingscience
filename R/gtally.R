#' A function for fast grouping and tallying of variables
#'
#' This function allows you a quick printout of all that's relevant to you
#' @param data Input data. Intended use for this function is in a tidyverse pipe, so you shouldn't have to specify this argument, usually
#' @param group which variable to group and tally by
#' @keywords print more than the default
#' @export
#' @examples pp(15) ## this will print 15 lines instead of the tidyverse standard of 10
#' pp()

## group by tally print...
gtally <- function(data, group){
  data %>%
    group_by(group) %>%
    tally(.) %>%
    maRketingscience::pp(.)
}
