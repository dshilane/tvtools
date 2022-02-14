# Exportable function:  cross.sectional.data

# Creates a cross-sectional cohort of data from a panel data structure at the specified time point.  Outcome variables are specified as the amount of time after this point that an event occurred.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# time.point: The (numeric) time to create the cross-sectional split.  Any subject (as specified by unique id in id.name) will have a single record drawn from this selected time point if the subject was observed at that time.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the first time (see t1.name) that each outcome takes the value 1 for each unique id (see id.name).  This time will be relative to the selected reference point (see time).  Values of outcome.names not in names(dat) will be disregarded.

# relative.followup:  A (logical) value indicating whether the outcomes should be calculated in absolute time (relative.followup = FALSE) or relative to the reference value of time (relative.followup = TRUE).  Outcomes prior to the reference value of time will be discarded when relative.followup = TRUE.

cross.sectional.data <- function(dat, time.point = 0, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = NULL, relative.followup = F){
  
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
  
  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    outcome.names <- NULL
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  w <- which(dat[,get(t1.name)] <= time.point & dat[,get(t2.name)] > time.point)
  
  baseline <- dat[w, .SD, .SDcols = names(dat)[!(names(dat) %in% outcome.names)]]
  
  res <- baseline[, .SD, .SDcols = names(baseline)[!(names(baseline) %in% c(t1.name, t2.name))]]
  res[, eval("cross.sectional.time") := time.point]
  
  if(!is.null(outcome.names)){
    
    first.ones <- first.event(dat = dat[get(t1.name) >= time.point,], id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, append.to.table = FALSE) 
    
    if(relative.followup == T){
      # Subtract cross-sectional time as baseline to produce relative time.
      first.ones[, (sprintf("%s.first.event", outcome.names)) := lapply(X = .SD, FUN = function(x, y){return(x - y)}, y = time.point), .SDcols = sprintf("%s.first.event", outcome.names)]
    }
    
    folls <- followup.time(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, append.to.data = F)
    
    if(relative.followup == T){
      # Subtract cross-sectional time as baseline to produce relative time.
      folls[, eval("followup.time") := get("followup.time") - time.point]
    }
    event.time <- merge(x = first.ones, y = folls[, .SD, .SDcols = c(id.name, "followup.time")], by = id.name)
    
    res <- merge(x = baseline, y = event.time, by = id.name, all.x = TRUE)
    
    res[, eval("cross.sectional.time") := time.point]
    
    res <- res[, .SD, .SDcols = names(res)[!(names(res) %in% c(t1.name, t2.name))]]
  }
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(res)
  }
    
  return(res[])
}
