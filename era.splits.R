# Exportable Function:  era.splits

# Restructures  a panel data structure by separating the records based upon eras (periods of observation).  Rows of data that overlap multiple eras are separated.  Newly created records are appended in additional rows to the data set.  If a row encapsulates 0-2 years, and we wish to split it into the first and second years, the original row will be converted to a time interval of 0-1, and a row for 1-2 will be appended to the end of the data set.


# dat:  a data.frame structured as a panel data set.

# cut.points:  A numeric vector of time points.  Each value defines the end of one era and the beginning of the next.  Setting cut.points = c(10, 20) will signal that records might be split into intervals of [min(x), 10), [10, 20), and [20, max(x)).  So a row of data with t1 = 0 and t2 = 30 would be divided into 3 rows, with t1 = c(0,10,20) and t2 = c(10,20,30).

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  


# This implementation is ready for testing.
era.splits <- function(dat, cut.points, id.name = "id", t1.name = "t1", t2.name = "t2"){
  
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
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }

  cut.points <- sort(unique(cut.points[cut.points <= dat[,max(get(t2.name))] & cut.points >= dat[, min(get(t1.name))]]))
  
  res <- dat
  
  res[, eval(t1.name) := as.numeric(get(t1.name))]
  res[, eval(t2.name) := as.numeric(get(t2.name))]
  
  for(cutoff in cut.points){
    w <- which(res[,get(t1.name)] < cutoff & res[,get(t2.name)] > cutoff)
    
    if(length(w)>0){
      dd <- res[w,]
      dd[, eval(t1.name) := cutoff]
      res[w, eval(t2.name) := cutoff]
      
      res <- rbindlist(l = list(res, dd), fill = T)
    }
  }
  
  setorderv(x = res, cols = c(id.name, t1.name))
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res)
}
