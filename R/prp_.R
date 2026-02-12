

#' @title My favorite customization of \link[rpart.plot]{prp}
#' 
#' @description
#' Do NOT overwrites function `rpart:::print.rpart()`.
#' 
#' @param x an \link[rpart]{rpart.object}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @note 
#' Function `ggdendro::ggdendrogram` will plot \link[rpart]{rpart.object}, but not very good.
#' 
#' Another option is `?partykit::plot.party`, which seems to requires numeric endpoint and/or branching criterion (?)
#' 
#' @returns 
#' Function [prp_()] does not have a returned value.
#' 
#' @references 
#' \url{http://www.di.fc.ul.pt/~jpn/r/tree/tree.html}
#' 
#' Inspired by function `heat.tree()` in \url{http://www.milbo.org/rpart-plot/prp.pdf}
#' 
#' @keywords internal
#' @importFrom grDevices hsv
#' @importFrom rpart.plot prp
#' @export
prp_ <- function(x, ...) {
  
  yval <- as.factor(x$frame$yval) # not just leaves, branch color depends on nodes.
  
  col <- yval |>
    attr(which = 'levels', exact = TRUE) |>
    length() |>
    seq.int(from = .36, to = 0, length.out = _) |> # red (high risk) to green (low risk)
    hsv(h = _)
  # see ?rattle::fancyRpartPlot for further tunings
  
  x |>
    prp(
      main = deparse1(x$terms[[2L]]),
      type = 4L, 
      extra = 101, 
      under = TRUE, 
      branch.lwd = 2.5, 
      varlen = 0L, 
      faclen = 0L,
      branch.col = col[unclass(yval)], 
      box.col = col[unclass(yval)]#, 
      #box.palette = c("pink", "palegreen3")
    ) |>
    suppressWarnings()

  return(invisible()) # ?rpart.plot::prp returned object is not usable directly
  
}
