# Exportable function:  unusual.duration

# Identifies any events that occur for an unusually long duration in panel data.

# dat:  a data.frame structured as a panel data set.

# outcome.name:  A (character) variable name from dat.  This variable should be binary.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# max.length:  A (numeric) variable specifying the maximum allowed amount of time for a record that includes the specified event.


unusual.duration <- function(dat, outcome.name, max.length, t1.name = "t1", t2.name = "t2"){

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
  if(outcome.name %in% all.variable.names == FALSE){
    stop("Error:  outcome.name must be a variable name from names(dat).")
  }
 
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
    
  res <- dat[get(outcome.name) == TRUE & get(t2.name) - get(t1.name) > max.length,]
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res[])
}