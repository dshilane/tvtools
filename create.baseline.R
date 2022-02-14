# Exportable function:  create.baseline

# Creates a baseline cohort of data at the initial time (0).  If other times are considered, use cross.sectional.data() to specify a time point.  Outcome variables are specified as the amount of time from baseline that an event occurred.

# id.name:  The (character) name of an identifying variable within the data.frame dat.  Values from this variable are used to track subjects across multiple rows of data.

# t1.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t1.name should represent the left endpoints t1.

# t2.name:  The (character) name of a time-tracking variable within the data.frame dat.  Since panel data sets include intervals of time [t1, t2), the selected variable for t2.name should represent the right endpoints t2.  

# outcome.names:  A (character) vector of variable names from dat.  These variables should be binary.  The function will calculate the first time (see t1.name) that each outcome takes the value 1 for each unique id (see id.name).

create.baseline <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = NULL){
    return(cross.sectional.data(dat = dat, time.point = 0, id.name = id.name, t1.name = t1.name, t2.name = t2.name, outcome.names = outcome.names))
}
