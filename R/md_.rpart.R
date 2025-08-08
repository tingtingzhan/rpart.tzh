

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
#' list(
#'  '`rpart`' = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, model = TRUE)
#' ) |> rmd.tzh::render_(file = 'rpart')
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export md_.rpart
#' @export
md_.rpart <- function(x, ...) {
  
  attr(x, which = 'text') <- sprintf(
    fmt = 'Recursive partitioning and regression tree [@Breiman84] for **`%s`** based on potential covariate(s) %s is provided by <u>**`R`**</u> package <u>**`rpart`**</u>. The data-driven partition, i.e., the collection of cutoff value(s), is based on %d observations.', 
    x$terms[[2L]] |> deparse1(),
    x$terms[[3L]] |> 
      all.vars() |>
      sprintf(fmt = '`%s`') |> 
      paste(collapse = ', '),
    x$frame$n[1L]
  ) |> 
    new(Class = 'md_lines', package = 'rpart', bibentry = bibentry(
      bibtype = 'Book', key = 'Breiman84',
      author = c('Leo Breiman', 'Jerome Friedman', 'Richard A. Olshen', 'Charles J. Stone'),
      title = 'Classification and Regression Trees',
      year = '1984',
      edition = '1',
      publisher = 'Chapman and Hall/CRC',
      address = 'New York',
      doi = '10.1201/9781315139470'
    ))
  
  md_.default(x, ...)
  
}  
