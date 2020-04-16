label_to_factor <-
function(data, var_list = NULL) {
  data<-data.table::as.data.table(data)
  if (is.null(var_list) == FALSE) { # if variable list specify subset data
    data <- data[, var_list, with=FALSE]
  } else { # if not ignore
    data <- data
  }
  varClass <- sapply(data, class) #identifying classes
  varClass <- sapply(varClass, function(x) {
    x[1]
  })
  characterVars_haven <- names(varClass[varClass %in% c("haven_labelled")]) #selecing those that are haven_labelled
  #these are also not continuous
  temp <- as.data.table(purrr::map(data[,..characterVars_haven], labelled::to_character)) ## convert to character using attribute names
  temp <- as.data.table(purrr::map(temp[,..characterVars_haven], labelled::to_factor)) ## convert to factor
  factor_levels <- sapply(temp, function(x) {length(unique(x))})
  non_binary <- names(factor_levels>2)
  characterVars_ordered <-characterVars_haven[characterVars_haven %in% non_binary]
  #temp2 <- as.data.table(purrr::map(temp[,..characterVars_ordered], ordered))
  #suppressWarnings(temp[,names(temp2)] <- temp2)
  temp3 <- data[, !characterVars_haven, with=FALSE]
  new_data <- cbind(temp, temp3) #merge back with the continuous variables
  return(new_data)
}
