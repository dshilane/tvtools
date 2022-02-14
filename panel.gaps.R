## Internal function, do not export

## Assumes that the data are all for a single patient (same id) and sorted in increasing order of t1.name
identify.panel.gaps.one.patient <- function(patient.dat, t1.name, t2.name, first.value = 0, expected.gap.between = 0){
  require(data.table)
  setDT(patient.dat)
  
  gap.first.row <- (patient.dat[1, get(t1.name) > first.value])
  n <- patient.dat[, .N]
  
  if(n == 1){
    res <- gap.first.row
  }
  if(n > 1){
    t2.values <- patient.dat[1:(n-1), get(t2.name)]
    gaps.other.rows <- patient.dat[2:n, get(t1.name) > t2.values + expected.gap.between]
    res <- c(gap.first.row, gaps.other.rows)
  }
  return(res)
}

# Exportable function:  panel.gaps

# Determines whether each record was preceded by a gap (missing period) of observation.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# gap.name:  a (character) value for the name of the variable specifying whether a gap is observed before this record.

# first.value: The (numeric) expected beginning time of the observation period. 

# expected.gap.between:  The (numeric) amount of time expected between the end of one record and the next; zero by default.

panel.gaps <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", gap.name = "gap_before", first.value = 0, expected.gap.between = 0){
  
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
  
  dat[, eval(gap.name) := identify.panel.gaps.one.patient(patient.dat = .SD, t1.name = t1.name, t2.name = t2.name, first.value = first.value, expected.gap.between = expected.gap.between), by = id.name]
  
  if(format.dt == FALSE){
    setDF(x = dat)
  }
  
  
  return(dat[])
}


# Exportable function:  first.panel.gap

# Determines the time point of the first gap (missing period) of observation for each unique subject.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# gap.name:  a (character) value for the name of the variable specifying whether a gap is observed before this record.

# first.value: The (numeric) expected beginning time of the observation period. 

# expected.gap.between:  The (numeric) amount of time expected between the end of one record and the next; zero by default.

# append.to.table:  The (logical) value specifying whether the calculated quantities should be appended (TRUE) as a new variable to the existing data or not (FALSE, default).

first.panel.gap <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", gap.name = "gap_before", first.value = 0, expected.gap.between = 0, append.to.table = F){
  
  the.gaps <- panel.gaps(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, gap.name = gap.name, first.value = first.value, expected.gap.between = expected.gap.between)
  
  res <- first.event(dat = the.gaps, id.name = id.name, t1.name = t1.name, outcome.names = gap.name, append.to.table = append.to.table)
  
  return(res)
}

# Exportable function:  last.panel.gap

# Determines the time point of the last gap (missing period) of observation for each unique subject.

# dat:  a data.frame structured as a panel data set.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# gap.name:  a (character) value for the name of the variable specifying whether a gap is observed before this record.

# first.value: The (numeric) expected beginning time of the observation period. 

# expected.gap.between:  The (numeric) amount of time expected between the end of one record and the next; zero by default.

# append.to.table:  The (logical) value specifying whether the calculated quantities should be appended (TRUE) as a new variable to the existing data or not (FALSE, default).


last.panel.gap <- function(dat, id.name, t1.name, t2.name, gap.name = "gap_before", first.value = 0, expected.gap.between = 0, append.to.table = F){
  
  the.gaps <- panel.gaps(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, gap.name = gap.name, first.value = first.value, expected.gap.between = expected.gap.between)
  
  res <- last.event(dat = the.gaps, id.name = id.name, t1.name = t1.name, outcome.names = gap.name, append.to.table = append.to.table)
  
  return(res)
}
