# Internal Function:  do not export.

# x:  A logical or binary numeric (0/1) vector.

# type:  The type of counting to perform.  If "distinct", then only new occurrences (separated by zeros) are counted.  Distinct counts handle events such as hospitalizations that span multiple records.  Otherwise the total number of records with a value of TRUE or 1 are counted.

# na.rm:  A (logical) variable indicating whether missing values should be removed when calculating the utilization.

internal.count.events <- function(x, type = "overall", na.rm = T){
  
  if(type == "distinct"){
    y <- c(0, x[1:(length(x)-1)])
    
    the.count <- sum(x == 1 & (x != y), na.rm = na.rm)
  }
  if(type != "distinct"){
    the.count = sum(x == 1, na.rm = na.rm)
  }
  return(the.count)
}



# Exportable Function:  count.events

# Creates a count of the number of events that occurred from a panel data structure.

# dat:  a data.frame structured as a panel data set.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the utilization of these variables in the interval [begin, end).  Values of outcome.names will be disregarded if they are not in names(dat) or are not binary variables (1/0 or TRUE/FALSE).

# grouping.variables:  A (character) vector of variable names from dat.  The resulting counts will be grouped accordingly.

# type:  The type of counting to perform.  If "distinct", then only new occurrences (separated by zeros) are counted.  Distinct counts handle events such as hospitalizations that span multiple records.  Otherwise the total number of records (type = "overall", the default) with a value of TRUE or 1 are counted.

# na.rm:  A (logical) variable indicating whether missing values should be removed when calculating the utilization.

count.events <- function(dat, outcome.names, grouping.variables = NULL, type = "overall", na.rm = T){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  all.variable.names <- unique(names(dat))
  
  
  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    stop("outcome.names must include variable names from names(dat).")
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
  
  res <- dat[, lapply(X = .SD, FUN = "internal.count.events", type = type, na.rm = na.rm), .SDcols = outcome.names, keyby = grouping.variables]
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res)
}