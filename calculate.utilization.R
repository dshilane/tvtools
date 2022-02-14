# Internal function.  Do not export.

# Calculates utilization for one variable for a single patient.

one.record.calculate.utilization <- function(x, t1, t2, begin = 0, end = 365, type = "rate", na.rm = TRUE){
  w <- which(t1 < end & t2 > begin)
  
  tot <- sum((pmin(end, t2[w]) - pmax(begin, t1[w])) * x[w], na.rm = na.rm)
  
  if(type == "total"){
    res <- tot
  }
  if(type != "total"){
    res <- tot / sum((pmin(end, t2[w]) - pmax(begin, t1[w])), na.rm = na.rm)
  }
  return(res)
}


# Exportable function:  calculate.utilization

# Calculates the amount or proportion of time that a condition (e.g. use of a medication) is met for each subject over a period of observation.

# dat:  a data.frame structured as a panel data set.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the utilization of these variables in the interval [begin, end).  Values of outcome.names will be disregarded if they are not in names(dat) or are not binary variables (1/0 or TRUE/FALSE).

# begin: The (numeric) beginning time at which utilization begins to be counted.

# end:  The (numeric) beginning time at which utilization begins to be counted.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# type:  A (character) variable specifying what kind of utilization to calculate.  When type == "rate" (the default), the proportion of days in [begin, end) with the variables (outcome.names) set to 1 is calculated.  When type %in% c("total", "count"), then the number of days in [begin, end) with the variables (outcome.names) set to 1 is calculated.  All other values of type default to type = "rate".

# full.followup:  A (logical) variable indicating whether all of the subjects (full.followup = FALSE, the default value) should be included or only those with fully observed records in [begin, end) should be included (full.followup = TRUE).  When full.followup = FALSE, the calculated counts (type %in% c("total", "count")) are based on the partially observed record.

# na.rm:  A (logical) variable indicating whether missing values should be removed when calculating the utilization.

calculate.utilization <- function(dat, outcome.names, begin, end, id.name = "id", t1.name = "t1", t2.name = "t2", type = "rate", full.followup = FALSE, na.rm = TRUE){
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
  
  if(type %in% c("total", "count")){
    type <- "total"
  }
  if(type != "total"){
    type <- "rate"
  }
  
  binary.outcomes.tab <- dat[, .(variable = outcome.names, qualifies = lapply(X = .SD, FUN = function(x){return(is.logical(x) | (is.numeric(x) & mean(sort(unique(x[!is.na(x)])) == c(0,1)) == 1))})), .SDcols = outcome.names]
  
  outcome.names <- binary.outcomes.tab[qualifies == TRUE, variable]
  
  if(full.followup == TRUE){
    qualifying.ids <- dat[, .(qualifies = one.record.calculate.utilization(x = rep.int(x = 1, times = length(get(t1.name))), t1 = get(t1.name), t2 = get(t2.name), begin = begin, end = end, type = "total") == end - begin), by = id.name][qualifies == TRUE, unique(get(id.name))]
  }
  if(full.followup == FALSE){
    qualifying.ids <- dat[, unique(get(id.name))]
  }
  
  tab <- dat[get(id.name) %in% qualifying.ids, lapply(X = .SD, FUN = "one.record.calculate.utilization", t1 = get(t1.name), t2 = get(t2.name), begin = begin, end = end, type = type), .SDcols = outcome.names, by = id.name]
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = tab)
  }
  
  return(tab)
}
