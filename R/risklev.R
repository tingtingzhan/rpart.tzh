

risklev <- function(object, ...) {
  
  # use `$model`, i.e., after removing missingness from original data
  
  if (!inherits(object, what = 'rpart')) stop('input must be rpart')
  
  leafID <- (object$frame$var == '<leaf>')
  leafRisk <- object$frame$yval[leafID]
  leafLab <- labels(object, pretty = FALSE)[leafID] # ?rpart:::labels.rpart
  # order as in the leaves, from left to right
  
  whr <- object$where # which `leafID` goes every (non-missing) observation
  # stopifnot(setequal(which(leafID), whr)) 
   
  y0 <- object$frame$yval[whr]
  risk <- factor(y0) # levels in ascending order
  attr(risk, which = 'levels') <- sprintf(fmt = '%s (%.2g)', leafLab, leafRisk)[order(leafRisk)]
  return(risk)
  
}







