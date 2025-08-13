
#' @title Survival Curves based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param formula \link[rpart]{rpart}
#' 
#' @param ... ..
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
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
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
