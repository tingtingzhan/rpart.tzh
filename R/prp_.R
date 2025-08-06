

#' @title My favorite customization of \link[rpart.plot]{prp}
#' 
#' @description
#' Overwrites function `rpart:::print.rpart` !!!
#' 
#' @param x \link[rpart]{rpart} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @note 
#' Function `ggdendro::ggdendrogram` will plot \link[rpart]{rpart} object, but not very good.
#' 
#' Another option is `?partykit::plot.party`, which seems to requires numeric endpoint and/or branching criterion (?)
#' 
#' @returns 
#' Function [print.rpart()] does not have a returned value.
#' 
#' @references 
#' \url{http://www.di.fc.ul.pt/~jpn/r/tree/tree.html}
#' 
#' Inspired by function `heat.tree()` in \url{http://www.milbo.org/rpart-plot/prp.pdf}
#' 
#' @keywords internal
#' @importFrom grDevices hsv
#' @importFrom rpart.plot prp
#' @export print.rpart
#' @export
print.rpart <- function(x, ...) {
  
  yval <- as.factor(x$frame$yval) # not just leaves, branch color depends on nodes.
  # red (high risk) to green (low risk)
  cols <- hsv(h = seq.int(.36, 0, length.out = length(attr(yval, which = 'levels', exact = TRUE))))[unclass(yval)]
  # see ?rattle::fancyRpartPlot for further tunings
  
  suppressWarnings(prp(
    x, 
    main = deparse1(x$terms[[2L]]),
    type = 4L, 
    branch.col = cols, 
    box.col = cols, 
    extra = 101, 
    under = TRUE, 
    branch.lwd = 2.5, 
    varlen = 0L, 
    faclen = 0L
  ))

  return(invisible()) # ?rpart.plot::prp returned object is not usable directly
  
}
