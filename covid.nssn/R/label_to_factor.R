label_to_factor <-
function(data, var_list = NULL, cat_vars = NULL) {
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
  
  level_num <- sapply(temp, unique)
  level_num <- sapply(level_num, length) 
  ordered <- names(level_num[level_num>3])
  '%ni%' <- Negate('%in%')
  characterVars_ordered <- characterVars_haven[characterVars_haven %in% ordered]
  characterVars_ordered <- characterVars_ordered[characterVars_ordered %ni% cat_vars]
  temp2 <- as.data.table(purrr::map(temp[,..characterVars_ordered], as.ordered))
  suppressWarnings(temp[,names(temp2)] <- temp2)
  
  temp3 <- data[, !characterVars_haven, with=FALSE]
  new_data <- cbind(temp, temp3) #merge back with the continuous variables
  return(new_data)
}
