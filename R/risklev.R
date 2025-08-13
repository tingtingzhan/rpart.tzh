
#' @title Risk Level of \link[rpart]{rpart} Object
#' 
#' @description
#' ..
#' 
#' @param object \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [risklev()] returns a \link[base]{factor}.
#' 
#' @note
#' Function \link[rpart]{labels.rpart} (for S3 generic \link[base]{labels}) only captures the last branching information, which is not ideal.
#' 
#' @examples
#' rpart(Kyphosis ~ Age + Start, data = kyphosis) |> 
#'  risklev()
#' @keywords internal
#' @export
risklev <- function(object, ...) {
  
  # use `$model`, i.e., after removing missingness from original data
  
  if (!inherits(object, what = 'rpart')) stop('input must be rpart')
  
  id <- (object$frame$var == '<leaf>')
  #leaf_y <- object$frame$yval[id] # may have duplicates!!!
  #leaf_lab <- labels(object, pretty = FALSE)[id]# order as in the leaves, from left to right
  
  # labels(object, pretty = TRUE)[id] # no good
  
  whr <- object$where # which `id` goes to every (non-missing) observation
  # stopifnot(setequal(which(id), whr)) 
   
  y0 <- object$frame$yval[whr] # numeric
  risk <- factor(y0) # levels in ascending order
  
  if (FALSE) {
    # this is WRONG!!
    # did not consider duplicates in `leaf_y` !!
    #attr(risk, which = 'levels') <- sprintf(fmt = '%s (%.2g)', leaf_lab, leaf_y)[order(leaf_y)]
  }
  
  attr(risk, which = 'levels') <- y0 |>
    unique.default() |> 
    sort.default() |>
    sprintf(fmt = '%.2g')
  
  return(risk)
  
}







