

#' @title Create Markdown Lines for \link[rpart]{rpart} Objects
#' 
#' @description
#' ..
#' 
#' @param x \link[rpart]{rpart} object
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [md_.rpart()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' library(rmd.tzh); list(
#'  '`rpart`' = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, model = TRUE)
#' ) |> render_(file = 'rpart')
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @export md_.rpart
#' @export
md_.rpart <- function(x, ...) {
  
  attr(x, which = 'text') <- sprintf(
    fmt = 'Recursive partitioning and regression tree for **`%s`** based on potential covariate(s) %s is provided by <u>**`R`**</u> package <u>**`rpart`**</u>. The data-driven partition, i.e., the collection of cutoff value(s), is based on %d observations.', 
    x$terms[[2L]] |> deparse1(),
    x$terms[[3L]] |> 
      all.vars() |>
      sprintf(fmt = '`%s`') |> 
      paste(collapse = ', '),
    x$frame$n[1L]
  ) |> 
    new(Class = 'md_lines', package = 'rpart')
  
  md_.default(x, ...)
  
}  
