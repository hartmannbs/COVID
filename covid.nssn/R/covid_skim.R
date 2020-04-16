covid_skim <-
function(data, var_list = NULL) {
  data <- data.table::as.data.table(data)
  if (is.null(var_list) == FALSE) { # if variable list specify subset data
    data <- data[, var_list, with=FALSE]
  } else { data <- data } # if not ignore
  data <- dtplyr::lazy_dt(data)
  skimr::skim(data)
}
