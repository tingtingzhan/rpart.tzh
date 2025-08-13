

#' @title Sample Size in \link[rpart]{rpart} Object
#' 
#' @description 
#' Method dispatch for S3 generic \link[stats]{nobs}.
#' 
#' @param object \link[rpart]{rpart} object
#' 
#' @param ... additional parameters, not currently in use
#' 
#' @returns 
#' Function [nobs.rpart()] returns an \link[base]{integer} scalar.
#' 
#' @keywords internal
#' @importFrom stats nobs
#' @export nobs.rpart
#' @export
nobs.rpart <- function(object, ...) object$frame$n[1L]





#' @title Survival Curves based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param formula \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @returns
#' Function [survfit.rpart()] returns a `'survfit.rpart'` object,
#' an derived class of `S3` class \link[survival]{survfit}.
#' 
#' @examples
#' library(survival)
#' rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE) |> 
#'   survfit()
#' 
#' @keywords internal
#' @importFrom survival survfit survfit.formula survdiff
#' @importFrom rmd.tzh label_pvalue_sym
#' @export survfit.rpart
#' @export
survfit.rpart <- function(formula, ...) {
  
  object <- formula; formula <- NULL
  
  # ?rpart::rpart.exp changes 'Surv' endpoint `object$y` to 'matrix'
  # I need to read more (about why this is necessary), before writing to the authors
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run with `rpart(., model = TRUE)`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  # since packageDate('rpart') # 2025-01-06
  # \link[rpart]{rpart} return does not contain `y` even if `y = TRUE` is called ..
  # x |> terms() |> attr(which = 'dataClasses', exact = TRUE) gives 'nmatrix.2', not 'Surv'
  
  leafRisk <- risklev(object, ...)
  
  d <- data.frame(y = y, leafRisk = leafRisk)
  ynm <- names(model_)[1L] |>
    str2lang()
  
  if (is.symbol(ynm)) {
    names(d)[1L] <- as.character(ynm)
    fom <- call(name = '~', ynm, quote(leafRisk)) |> eval()
  } else {
    fom <- (y ~ leafRisk)
  }
  
  sf <- list(formula = fom, data = d) |> 
    do.call(what = survfit.formula, args = _)
  
  sdf <- list(formula = fom, data = d) |> 
    do.call(what = survdiff, args = _)
  attr(sf, which = 'survdiff') <- sdf$pvalue |> 
    label_pvalue_sym(add_p = TRUE)() |> 
    paste('Log-rank (unweighted)')
  
  class(sf) <- c('survfit.rpart', class(sf)) |> 
    unique.default()
  return(sf)
  
}



#' @title [autoplot.survfit.rpart()]
#' 
#' @description
#' ..
#' 
#' @param object returned value of function [survfit.rpart()]
#' 
#' @param ... ..
#' 
#' @returns
#' Function [autoplot.survfit.rpart()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @keywords internal
#' @importFrom ggplot2 autoplot labs
#' @importFrom survival.tzh autoplot.survfit
#' @method autoplot survfit.rpart
#' @export autoplot.survfit.rpart
#' @export
autoplot.survfit.rpart <- function(object, ...) {
  
  autoplot.survfit(object, ...) +
    labs(
      caption = attr(object, which = 'survdiff', exact = TRUE)
    )
  
}







#' @title Create Markdown Lines for \link[rpart]{rpart} Objects
#' 
#' @description
#' ..
#' 
#' @param x \link[rpart]{rpart} object
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [md_.rpart()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' library(survival)
#' vet = survival::veteran |>
#'  within.data.frame(expr = {
#'   time = as.difftime(time, units = 'days')
#'   os = Surv(time, status)
#'  })
#' 
#' list(
#'  'non survival' = rpart(Kyphosis ~ Age + Start, data = kyphosis, model = TRUE),
#'  'survival, `os`' = rpart(os ~ age, data = vet, maxdepth = 2L, model = TRUE),
#'  'survival, `Surv(time, status)`' = rpart(Surv(time, status) ~ age, data = vet, 
#'    maxdepth = 2L, model = TRUE)
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
  
  z1 <- sprintf(
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
      publisher = 'Chapman and Hall/CRC', address = 'New York',
      doi = '10.1201/9781315139470'
    ))
  
  z2 <- c(
    '```{r}',
    '#| echo: false',
    xnm |> sprintf(fmt = 'prp_(%s)'),
    '```'
  ) |> new(Class = 'md_lines')
  
  sf <- x |> 
    survfit.rpart()
  
  if (length(sf)) {
    
    z31 <- '@KaplanMeier58 estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survival`**</u>.' |>
      new(Class = 'md_lines', package = 'survival', bibentry = KaplanMeier58())
    
    z32 <- c(
      '```{r}',
      '#| echo: false',
      '#| dev: \'ragg_png\'', # unicode support!!
      xnm |> sprintf(fmt = '(%s) |> survfit.rpart() |> autoplot.survfit.rpart()'),
      '```'
    ) |> new(Class = 'md_lines')
    
    z3 <- c(z31, z32)
    
  } else z3 <- NULL
  
  c(z1, z2, z3)
  
}  








#' @title Test Survival Curve Differences based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param object \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @importFrom survival survdiff
#' @export
survdiff_rpart <- function(object, ...) {
  
  .Defunct(msg = 'inside [survfit.rpart()] now')
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  
  leafRisk <- risklev(object, ...)
  
  return(survdiff(y ~ leafRisk))
  
} 


