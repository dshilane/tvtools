# Exportable function:  followup.time

# Computes the end point of observation (the maximum ending time, e.g. death, loss of follow-up, end of study, etc.) for each patient.)

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# followup.name:  The (character) name of a variable to display with the total/maximum follow-up time for each subject.

# calculate.as:  A (character) value specifying whether the follow-up time should be calculated as a) the maximum observed time for each subject (calculate.as = "max") or b) the total observed time for each subject, e.g. sum(t2-t1) across all rows of the data (calculate.as = "total", the default value).  Note that when a subject's records begin at time 0 and do not include gaps or duplicated intervals, then setting calculate.as = "max" will return the same result as calculate.as = "total".

# append.to.data:  A (logical) value specifying whether the calculated follow-up time should be appended as a new column to the data.frame dat (append.to.data = TRUE).  If not (append.to.data = FALSE), then a separate data.frame object is return with identifiers and follow-up times.


followup.time <- function(dat, id.name = "id", t1.name = "t1", t2.name="t2", followup.name = "followup.time", calculate.as = "total", append.to.data = FALSE){
  
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  value.max <- "max"
  value.total <- "total"
  
  all.variable.names <- unique(names(dat))
  
  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
  }
  if(calculate.as == value.total){
    if((t1.name %in% all.variable.names) == FALSE){
      stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
    }
  }
  
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat) that represents the end of a time interval.")
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  if(calculate.as != value.max){
    calculate.as <- value.total
  }
  
  if(append.to.data == TRUE){
    if(calculate.as == value.max){
      tab <- dat[, eval(followup.name) := max(get(t2.name)), by = id.name]
    }
    if(calculate.as == value.total){
      tab <- dat[, eval(followup.name) := sum(get(t2.name) - get(t1.name), na.rm = T), by = id.name]
    }
  }
  if(append.to.data == FALSE){
    if(calculate.as == value.max){
      tab <- dat[, .(V1 = max(get(t2.name))), by = id.name]
      setnames(x = tab, old = "V1", new = followup.name)
    }
    if(calculate.as == value.total){
      tab <- dat[, .(V1 = sum(get(t2.name) - get(t1.name), na.rm = T)), by = id.name]
      setnames(x = tab, old = "V1", new = followup.name)
    }
  }

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = tab)
  }
  
  return(tab)
}
