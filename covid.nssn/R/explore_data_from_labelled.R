explore_data_from_labelled <-
function(data, var_list = NULL) {
  data <- data.table::as.data.table(dt)
  if (is.null(var_list) == FALSE) { #subset to variable list
    data <- data[, var_list, with=FALSE]
  } else {
    data <- data
  }
  data <- label_to_factor(data) # convert from spss format to burro useable
  burro::explore_data(data) # launch shiny app
}
