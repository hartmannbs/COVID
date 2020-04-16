explore_data_from_labelled <-
function(dt, var_list = NULL) {
  dt <- data.table::as.data.table(dt)
  if (is.null(var_list) == FALSE) { #subset to variable list
    dt <- dt[, var_list, with=FALSE]
  } else {
    dt <- dt
  }
  dt <- label_to_factor(dt) # convert from spss format to burro useable
  burro::explore_data(dt) # launch shiny app
}
