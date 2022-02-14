# Exportable function:  measurement.rate

# Calculates the proportion of unique subjects remaining and no longer under observation at the specified time in follow-up.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# time.point:  A (numeric) variable specifying the point at which the measurement rate should be calculated.

# grouping.variables:  A (character) vector of variable names from dat.  The resulting counts will be grouped accordingly.


measurement.rate <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", time.point = 0, grouping.variables = NULL){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  all.variable.names <- unique(names(dat))
  
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }
 
  grouping.variables <- unique(grouping.variables[grouping.variables %in% all.variable.names])
  if(length(grouping.variables) == 0){
    grouping.variables <- NULL
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
    
  obs.count <- dat[get(t1.name) <= time.point & get(t2.name) > time.point, .(time = time.point, observed = length(unique(get(id.name)))), by = eval(grouping.variables)]
  
  full.count = dat[, .(total.subjects = length(unique(id))), by = grouping.variables]
  
  if(is.null(grouping.variables)){
    res <- data.table(obs.count, full.count)
  }
  if(!is.null(grouping.variables)){
    res <- merge(x = obs.count, y = full.count, by = grouping.variables)
  }
  
  res[, eval("rate.observed") := observed / total.subjects]
  res[, eval("rate.not.observed") := 1 - observed / total.subjects]
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res[])
}

