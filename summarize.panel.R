summarize.panel <- function(dat, id.name, t1.name, t2.name, grouping.variables = NULL){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  
  all.variable.names <- names(dat)
  if(!is.null(id.name)){
    if((id.name %in% all.variable.names) == FALSE){
      stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
    }
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat) that represents the end of a time interval.")
  }
  
  require(data.table)
  format.dt <- is.data.table(x = dat)
  
  if(format.dt == FALSE){
    setDT(x = dat)
  }
  
  res <- dat[, .(total.records = .N, unique.ids = length(unique(get(id.name))), mean.records.per.id = .N / length(unique(get(id.name))), total.followup = sum(get(t2.name) - get(t1.name)), max.followup = max(get(t2.name))), keyby = grouping.variables]
 
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  return(res)
}