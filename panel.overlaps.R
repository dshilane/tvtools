# internal function, do not export

id.panel.overlaps.one.patient <- function(patient.dat, id.name, t1.name, t2.name, row.index.name){
  require(data.table)
  setDT(patient.dat)
  
  beginning.times <- patient.dat[, get(t1.name)]
  ending.times <- patient.dat[, get(t2.name)]
  
  overlapping.results <- patient.dat[, sum((get(t1.name) > beginning.times & get(t1.name) < ending.times) | (get(t2.name) < beginning.times & get(t2.name) > ending.times)) > 0, by = row.index.name][, sum(get("V1")) > 0]
  
  return(overlapping.results)
}

# Exportable function

# Performs a quality check to determine whether any records reflect overlapping periods of observation.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  


panel.overlaps <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2"){
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
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  setorderv(x = dat, cols = c(id.name, t1.name), order = 1)
  
  dat[, eval("record.index") := 1:.N, by = id.name]
  
  ids.with.overlaps <- dat[, id.panel.overlaps.one.patient(patient.dat = .SD, id.name = id.name, t1.name = t1.name, t2.name = t2.name, row.index.name = "record.index"), by = id.name]
  
  setnames(x = ids.with.overlaps, old = c("V1"), new = c("overlapping_panels"))
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = ids.with.overlaps)
  }
  
  return(ids.with.overlaps)
}


