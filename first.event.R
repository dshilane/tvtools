# Internal function, not exported
calc.event.time <- function(outcome, times, time.function = "min", na.rm = T){
  w <- which(outcome == 1)
  if(length(w) > 0){
    res <- unlist(lapply(X = list(times[w]), FUN = time.function, na.rm = na.rm))
    if(is.numeric(times)){
      res <- as.numeric(res)
    }
    if(is.integer(times)){
      res <- as.integer(res)
    }
  }
  if(length(w) == 0){
    if(is.numeric(times)){
      res <- NA_real_
    }
    if(is.integer(times)){
      res <- NA_integer_
    }
  }
  return(res)
}



# Exportable function:  event.time

# Calculates the time to an event in a panel data structure.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the first time (see t1.name) that each outcome takes the value 1 for each unique id (see id.name).

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# FUN:  The (character) function to apply.  Selecting "min" calculates the time of the first event, and "max" is the time of the last event.  Other functions (e.g. "mean" or "median") may be applied.  These calculations will be applied to the vector of times for which events occur.

# append.to.data:  A (logical) value specifying whether the calculated follow-up time should be appended as a new column to the data.frame dat (append.to.data = TRUE).  If not (append.to.data = FALSE), then a separate data.frame object is return with identifiers and follow-up times.

# event.name:  A (character) value specifying the column name for the event time when appended to the data.

event.time <- function(dat, id.name, outcome.names, t1.name, time.function = "min", append.to.table = FALSE, event.name = "first.event"){
  
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  all.variable.names <- unique(names(dat))
  
  if(!is.null(id.name)){
    if((id.name %in% all.variable.names) == FALSE){
      stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
    }
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  outcome.names <- outcome.names[outcome.names %in% names(dat)]
  
  if(length(outcome.names) == 0){
    stop("Error: outcome.names must be variable names in names(dat).")
  }
  
  res <- dat[, lapply(X = .SD, FUN = "calc.event.time", times = get(t1.name), time.function = time.function), .SDcols = outcome.names, by = id.name]
  
  setnames(x = res, old = outcome.names, new = sprintf("%s.%s", outcome.names, event.name))
  
  if(append.to.table == TRUE){
    res <- merge(x = dat, y = res, by = "id", all.x = TRUE)
  }
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res)
}



# Exportable function:  first.event

# Calculates the time to the first event in a panel data structure.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the first time (see t1.name) that each outcome takes the value 1 for each unique id (see id.name).

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# FUN:  The (character) function to apply.  Selecting "min" calculates the time of the first event, and "max" is the time of the last event.  Other functions (e.g. "mean" or "median") may be applied.  These calculations will be applied to the vector of times for which events occur.

# append.to.data:  A (logical) value specifying whether the calculated follow-up time should be appended as a new column to the data.frame dat (append.to.data = TRUE).  If not (append.to.data = FALSE), then a separate data.frame object is return with identifiers and follow-up times.

# event.name:  A (character) value specifying the column name for the first event time when appended to the data.

first.event <- function(dat, id.name, outcome.names, t1.name, append.to.table = FALSE, event.name = "first.event"){
  res <- event.time(dat = dat, id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, time.function = "min", append.to.table = append.to.table, event.name = event.name)
  
  return(res)
}

# Exportable function:  last.event

# Calculates the time to an event in a panel data structure.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the first time (see t1.name) that each outcome takes the value 1 for each unique id (see id.name).

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# FUN:  The (character) function to apply.  Selecting "min" calculates the time of the first event, and "max" is the time of the last event.  Other functions (e.g. "mean" or "median") may be applied.  These calculations will be applied to the vector of times for which events occur.

# append.to.data:  A (logical) value specifying whether the calculated follow-up time should be appended as a new column to the data.frame dat (append.to.data = TRUE).  If not (append.to.data = FALSE), then a separate data.frame object is return with identifiers and follow-up times.

# event.name:  A (character) value specifying the column name for the last event time when appended to the data.

last.event <- function(dat, id.name, outcome.names, t1.name, append.to.table = FALSE, event.name = "last.event"){
  res <- event.time(dat = dat, id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, time.function = "max", append.to.table = append.to.table, event.name = event.name)
  
  return(res)
}
