#' A function for displaying the pie chart aggregation of the sales decomposition
#'
#' This function allows you to quickly view the sales decomposition for your model.
#' @param modell Input data. Just name the model you want to inspect
#' @param coler color palette. Accepts a vector of length k+1, with k being the number of variables in the model. Defaults to the rainbow color sequence.
#' @param rad Controls the size of the pie. Defaults to slighty above 1.
#' @param font_size Size of the text in the chart. Defaults to 0.9.
#' @keywords preview quick sales decomposition pie chart.
#' @export
#' @examples preview_torte(my_default_model)
#' preview_torte()
preview_torte <- function(modell, coler = "def", rad = 1.01, font_size = 0.9) {
  ## importiere und previewe Modell
  blubb <- preview(modell)

  ## setze Farben
  colli <- if(coler == "def") {rainbow(length(blubb))} else {coler}

  ##
  b2 <- blubb %>%
    as.data.frame() %>%
    set_names("anteile") %>%
    mutate(namers = remove_shit(row.names(.)),
           pcts = round(anteile/sum(anteile)*(1e2), 1) ) %>%
    separate(., col = namers, into="namers", sep="_", extra="drop") %>%
    mutate(real_labs = paste0(namers, "\n", pcts, "%")) %>%
    arrange(desc(anteile))

  ## hier noch eine IDEE - Prozentzahlen unter 1 oder 2 oder X Prozent werden einfach nicht angezeigt!

  ## mache Chart
  pie(b2$anteile,
      b2$real_labs,
      main="Sales Decomposition Preview",
      col = colli,
      border = T,
      init.angle = 90, clockwise = T, radius = rad, cex = font_size)
}
