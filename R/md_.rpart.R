

#' @title Create Markdown Lines for \link[rpart]{rpart} Objects
#' 
#' @description
#' ..
#' 
#' @param x \link[rpart]{rpart} object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [md_.rpart()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' library(survival.tzh)
#' vet = survival::veteran |>
#'  within.data.frame(expr = {
#'   time = as.difftime(time, units = 'days')
#'   os = Surv(time, status)
#'  })
#' 
#' library(ggplot2); list(
#'  'non survival' = rpart(Kyphosis ~ Age + Start, data = kyphosis, model = TRUE),
#'  'survival v1' = rpart(os ~ age, data = vet, maxdepth = 2L, model = TRUE),
#'  'survival v2' = rpart(Surv(time, status) ~ age, data = vet, maxdepth = 2L, model = TRUE)
#' ) |> rmd.tzh::render_(file = 'rpart')
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom survival.tzh KaplanMeier58
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export md_.rpart
#' @export
md_.rpart <- function(x, xnm, ...) {
  
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
  
  z1 <- md_.default(x, xnm = xnm, ...)
  
  sf <- x |> 
    survfit.rpart()
  
  if (length(sf)) {
    z21 <- '@KaplanMeier58 estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survival`**</u>.' |>
      new(Class = 'md_lines', package = 'survival', bibentry = KaplanMeier58())
    z22 <- c(
      '```{r, dev = \'ragg_png\'}',
      '#| echo: false',
      xnm |> sprintf(fmt = 'p = (%s) |> survfit.rpart() |> autoplot.survfit()'),
      xnm |> sprintf(fmt = 'sd = (%s) |> survdiff_rpart(object)'),
      'p + ggplot2::labs(caption = sd$pvalue |> label_pvalue_sym(add_p = TRUE)() |> paste(\'Log-rank (unweighted)\'))',
      '```'
    ) |> new(Class = 'md_lines')
    z2 <- c(z21, z22)
  } else z2 <- NULL
  
  c(z1, z2)
  
}  
