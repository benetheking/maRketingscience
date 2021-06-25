#' A function for finetuning the effects of media carry-over
#'
#' This function allows you a quick printout of all relevant effects for a media group
#' @param model Input model. Defaults to model1.
#' @param var_name Part of the variable name, such as TV_GRP. Can be used like a wildcard.
#' @param data The dataset to search for variables. Defaults to full_data.
#' @keywords print more than the default
#' @export
#' @examples finetune(my_model, "radio_spend", my_data) ## will search for all variables that contain the string "radio_spend"
#' pp()

## return variable alternatives
finetune <- function(model = model1, var_name, data = full_data) {
  ## get all relevant vars
  selection <- data %>%
    select(matches(var_name))

  ## get vars for formula - is var string already contained in model?
  ind_vars <- model$coefficients %>%
    names() %>% data.frame(namers = .) %>%
    filter(!grepl(pattern = var_name, x = namers)) %>% c()

  form_vars <- paste(ind_vars$namers, collapse=" + ")

  ## build formula, then models
  formulas <- selection %>%
    names() %>%
    map(.x=., ~paste0("av ~ ", form_vars, " + ", .x) %>%
          as.formula())

  mods <- formulas %>%
    map_df(., ~lm(.x, data=full_data) %>%
             tidy() %>%
             select(1, 2, 5) %>%
             filter(grepl(var_name, term))) %>%
    arrange(desc(estimate))

  reto <- mods %>% pp(20)
  return(reto)
}

