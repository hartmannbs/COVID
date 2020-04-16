covid_table <-
function(group_var, ind_var, data,
                         row = FALSE, column = TRUE, total = FALSE) {
  #group_var is first argument and doesn't work well if continuous
  #column is set as TRUE so you can see percent of group_var in each ind_var
  ddpcr::quiet(attach(data))
  if (class(ind_var) == "numeric") { # ind_var if continous to show means
    suppressWarnings(descr::compmeans(ind_var, group_var, plot = FALSE))
  } else { # if ind_var is categorical show tabulation
    suppressWarnings(descr::CrossTable(group_var,ind_var,
                                       prop.r = row,
                                       prop.c = column,
                                       prop.t = total,
                                       prop.chisq = FALSE,
                                       dnn = c(deparse(substitute(group_var)),
                                               deparse(substitute(ind_var)))))
  }
}
