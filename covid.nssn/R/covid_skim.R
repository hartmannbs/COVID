covid_skim <-
function(dt, var_list = NULL) {
  dt<-data.table::as.data.table(dt)
  if (is.null(var_list) == FALSE) { # if variable list specify subset data
    dt <- dt[, var_list, with=FALSE]
  } else { dt <- dt } # if not ignore
  dt <- dtplyr::lazy_dt(dt)
  skimr::skim(dt)
}
