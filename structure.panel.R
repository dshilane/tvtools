# Exportable function:  structure.panel

# Sorts the panel data by id and time period.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

structure.panel <- function(dat, id.name = "id", t1.name = "t1"){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  all.variable.names <- unique(names(dat))
  
  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }

  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  res <- setorderv(x = dat, cols = c(id.name, t1.name))
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res)
}


