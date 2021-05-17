#' Quick draw of the media flightings used in the model
#'
#' This function allows for a quick checking of plausibility towards the media channels by plotting them.
#' @param modell the model you have. Model predictions will be a solid red line.
#' @param startdatum first observation as date. The function will find the time period covered by the model by itself. Only accepts week right now.
#' @keywords paint media flightings
#' @export
#' @examples plot_model_media(modell = lm_object, startdatum = "2018-01-01") ##
#' plot_model_media()

##########  FUNKTION MEDIA_FROM_MODEL
plot_model_media <- function(modell = hans, startdatum = "2016-04-01"){
  get_media <- modell %>%
    `$`(., coefficients) %>%
    names(.) %>%
    as.data.frame(x=.) %>%
    select(., namen=`.`) %>%
    mutate(media_vars = map_chr(.x=namen,
                                ~case_when(
                                  str_sub(string=., -2, -1) %in% c(05, 10, 20, 30, 40, 50, 60, 70, 80, 90) ~ "media",
                                  TRUE ~ "nonmedia") ) ) %>%
    filter(media_vars != "nonmedia") %>%
    select(namen)

  ## remove ABs and such - könnte man noch verknüpfen!
  raus <- get_media %>%
    transmute(namen = str_remove(namen, "05|10|20|30|40|50|60|70|80|90")) %>%
    transmute(namen = str_remove(namen, "log[:punct:]")) %>%
    filter(namen != "KW")

  get_data <- modell %>%
    `$`(call) %>%
    `$`(data) %>%
    eval() %>%
    select(!!raus$namen)

  ## zeitindex:
  wie_lange <- nrow(get_data)
  blabla <- tsibble(Datum = yearweek(as.Date(startdatum) ) + 0:(wie_lange-1) )
  b2 <- bind_cols(blabla, get_data)

  ## FINALLY - plotting!
  b2 %>%
    gather(key=Media, value=Spends) %>%
    ggplot(aes(x=Datum, y=Spends, col=Media)) + geom_line() + facet_wrap(~Media, scales = "free")
}
