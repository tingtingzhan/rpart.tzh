

#' @title My favorite customization of \link[rpart.plot]{prp}
#' 
#' @description ..
#' 
#' @param object \link[rpart]{rpart} object
#' 
#' @param main \link[base]{character} scalar
#' 
#' @param type,extra,under,branch.lwd,varlen,faclen,... see \link[rpart.plot]{prp}
#' 
#' @note 
#' Function `ggdendro::ggdendrogram` will plot \link[rpart]{rpart} object, but not very good.
#' 
#' Another option is `?partykit::plot.party`, which seems to requires numeric endpoint and/or branching criterion (?)
#' 
#' @returns 
#' Function [prp_] does not have a returned value.
#' 
#' @references 
#' \url{http://www.di.fc.ul.pt/~jpn/r/tree/tree.html}
#' 
#' Inspired by function `heat.tree()` in \url{http://www.milbo.org/rpart-plot/prp.pdf}
#' 
#' @importFrom grDevices hsv
#' @importFrom rpart.plot prp
#' @importFrom stats predict
#' @export
prp_ <- function(
    object, main = deparse1(object$terms[[2L]]), 
    type = 4L, 
    extra = 101, 
    under = TRUE, 
    branch.lwd = 2.5, 
    varlen = 0L, 
    faclen = 0L, 
    ...
) {
  
  yval <- as.factor(object$frame$yval) # not just leaves, branch color depends on nodes.
  # red (high risk) to green (low risk)
  cols <- hsv(h = seq.int(.36, 0, length.out = length(attr(yval, which = 'levels', exact = TRUE))))[unclass(yval)]
  # see ?rattle::fancyRpartPlot for further tunings
  
  rmse <- if (is.data.frame(data_orig <- tryCatch(eval(object$call$data), error = identity))) {
    # original data available
    if (length(object$na.action)) data_orig <- data_orig[-object$na.action, ]
    fit <- predict(object = object, newdata = data_orig) # ?rpart:::predict.rpart
    obs <- eval(object$terms[[2L]], envir = data_orig)
    if (is.vector(obs, mode = 'numeric')) sqrt(mean.default((obs - fit)^2)) # else NULL
  } # else NULL
  
  suppressWarnings(prp(
    object, 
    main = if (length(rmse)) sprintf(fmt = '%s (RMSE = %.3f)', main, rmse) else main,
    type = type, 
    branch.col = cols, 
    box.col = cols, 
    extra = extra, 
    under = under, 
    branch.lwd = branch.lwd, 
    varlen = varlen, 
    faclen = faclen, 
    ...))
  
  return(invisible()) # ?rpart.plot::prp returned object is not usable directly
  
}
