# Exportable function:  crude.rates

# Calculates the rate of events relative to the amount of follow-up time based on panel data.

# dat:  a data.frame structured as a panel data set.

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the utilization of these variables in the interval [begin, end).  Values of outcome.names will be disregarded if they are not in names(dat) or are not binary variables (1/0 or TRUE/FALSE).

# cut.points:  A numeric vector of time points.  Each value defines the end of one era and the beginning of the next.  Setting cut.points = c(10, 20) will signal that records might be split into intervals of [min(x), 10), [10, 20), and [20, max(x)).  So a row of data with t1 = 0 and t2 = 30 would be divided into 3 rows, with t1 = c(0,10,20) and t2 = c(10,20,30).

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  


# grouping.variables:  A (character) vector of variable names from dat.  The resulting counts will be grouped accordingly.

# type:  The type of counting to perform.  If "distinct", then only new occurrences (separated by zeros) are counted.  Distinct counts handle events such as hospitalizations that span multiple records.  Otherwise the total number of records (type = "overall", the default) with a value of TRUE or 1 are counted.

# na.rm:  A (logical) variable indicating whether missing values should be removed when calculating the utilization.

# era.name:  A (character) name for a column showing the time period in the resulting table.

crude.rates <- function(dat, outcome.names, cut.points = NULL, time.multiplier = 1, id.name = "id", t1.name = "t1", t2.name = "t2", grouping.variables = NULL, type = "overall", na.rm = T, era.name = "period"){
  
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
  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    outcome.names <- NULL
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
  
  cut.points <- as.numeric(sort(unique(cut.points[cut.points <= dat[,max(get(t2.name))] & cut.points >= dat[, min(get(t1.name))]])))
  
  
  dd <- era.splits(dat = dat, cut.points = cut.points, id.name = id.name, t1.name = t1.name, t2.name = t2.name)
  
  num.cut.points <- length(cut.points)
  if(num.cut.points == 0){
    dd[, eval(era.name) := "All Follow-Up"]
  }
  if(num.cut.points > 0){
    for(i in 1:(num.cut.points + 1)){
      if(i == 1){
        dd[get(t2.name) <= cut.points[i], eval(era.name) := sprintf("Before %s", cut.points[i])]
      }
      if(i > 1 & i < num.cut.points + 1){
        dd[get(t1.name) >= cut.points[i-1] & get(t2.name) <= cut.points[i], eval(era.name) := sprintf("[%s, %s)", cut.points[i-1], cut.points[i])]
      }
      if(i == num.cut.points + 1){
        dd[get(t1.name) >= cut.points[i-1], eval(era.name) := sprintf("On or After %s", cut.points[i-1])]
      }
    }
  }
  
  
  the.counts <- count.events(dat = dd, outcome.names = outcome.names, grouping.variables = c(grouping.variables, era.name), type = type, na.rm = na.rm)
  
  observed <- dd[, .(observation.time = sum(get(t2.name) - get(t1.name))), by = c(grouping.variables, era.name)]
  
  res <- merge(x = observed, y = the.counts, by = c(grouping.variables, era.name))
  
  for(i in 1:length(outcome.names)){
    res[, eval(sprintf("%s.rate", outcome.names[i])) := get(outcome.names[i]) * time.multiplier / observation.time]
  }
  
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  
  return(res[])
}
