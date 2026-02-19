

#' @title Sample Size in \link[rpart]{rpart.object}
#' 
#' @description 
#' Method dispatch for S3 generic \link[stats]{nobs}.
#' 
#' @param object an \link[rpart]{rpart.object}
#' 
#' @param ... additional parameters, not currently in use
#' 
#' @returns 
#' Function [nobs.rpart()] returns an \link[base]{integer} scalar.
#' 
#' @keywords internal
#' @export nobs.rpart
#' @export
nobs.rpart <- function(object, ...) object$frame$n[1L]




#' @title Does an \link[rpart]{rpart.object} Has a \link[survival]{Surv} Endpoint?
#' 
#' @param object an \link[rpart]{rpart.object}
#' 
#' @keywords internal
#' @importFrom survival.tzh is.Surv.endpoint
#' @method is.Surv.endpoint rpart
#' @export is.Surv.endpoint.rpart
#' @export
is.Surv.endpoint.rpart <- function(object) {
  # 2025-01-06: ?rpart::rpart.exp changes 'Surv' endpoint `object$y` to 'matrix'
  # I need to read more (about why this is necessary), before writing to the authors
  m <- object$model
  if (is.null(m) || !is.data.frame(m)) stop('Re-run with `rpart(., model = TRUE)`')
  y <- m[[1L]] # units.Surv carries hahaha!!
  inherits(y, what = 'Surv')
}





#' @title Kaplan-Meier Curves of \link[rpart]{rpart.object}
#' 
#' @description
#' Kaplan-Meier curves of an \link[rpart]{rpart.object}, 
#' if the endpoint is a \link[survival]{Surv} object.
#' 
#' @param formula an \link[rpart]{rpart.object}
#' 
#' @param fmt \link[base]{character} scalar, 
#' string formatting of the leaf-risk,
#' see function \link[base]{sprintf}
#' 
#' @param ... additional parameters of function \link[survival]{survfit.formula}
#' 
#' @returns
#' The `S3` method [survfit.rpart()] returns a \link[survival]{survfit.object}.
#' 
#' @examples
#' library(survival)
#' rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE) |> 
#'   survfit()
#' 
#' @keywords internal
#' @importFrom survival survfit survfit.formula
#' @importFrom survival.tzh units.Surv more_units<-
#' @export survfit.rpart
#' @export
survfit.rpart <- function(formula, fmt = '%.2g', ...) {
  
  object <- formula; formula <- NULL
  
  if (!is.Surv.endpoint.rpart(object)) return(invisible()) # exception handling
  
  leaf <- object |> 
    predict(type = 'vector') |> # ?rpart:::predict.rpart
    sprintf(fmt = fmt) |> 
    as.factor() # silly but works!!
  
  sf <- survfit.formula(object$model[[1L]] ~ leaf, ...)
  more_units(sf) <- units.Surv(object$model[[1L]]) # survival.tzh::`more_units<-.survfit`
  sf$call$formula[[2L]] <- object$call$formula[[2L]] # y-axis label
  
  return(sf)
  
}







#' @title Test Survival Curve Differences of \link[rpart]{rpart.object}
#' 
#' @description
#' Test survival curve differences of an \link[rpart]{rpart.object}, 
#' if the endpoint is a \link[survival]{Surv} object.
#' 
#' @param object an \link[rpart]{rpart.object}
#' 
#' @param fmt \link[base]{character} scalar, 
#' string formatting of the leaf-risk,
#' see function \link[base]{sprintf}
#' 
#' @param ... additional parameters of function \link[survival]{survdiff}
#' 
#' @examples
#' library(survival.tzh)
#' rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE) |> 
#'  survdiff_()
#' 
#' @importFrom survival.tzh survdiff_
#' @importFrom survival survdiff
#' @export survdiff_.rpart
#' @export
survdiff_.rpart <- function(object, fmt = '%.2g', ...) {

  if (!is.Surv.endpoint.rpart(object)) return(invisible()) # exception handling
  
  leaf <- object |> 
    predict(type = 'vector') |> # ?rpart:::predict.rpart
    sprintf(fmt = fmt) |> 
    as.factor() # silly but works!!
  
  sdf <- survdiff(object$model[[1L]] ~ leaf, ...)
  sdf$call$formula[[2L]] <- object$call$formula[[2L]] # y-axis label
  
  return(sdf)
} 





#' @title Creates ggplot from \link[rpart]{rpart.object}
#' 
#' @description
#' ..
#' 
#' @param object an \link[rpart]{rpart.object}
#' 
#' @param ... additional parameters of function \link[survival.tzh]{autoplot.survfit}
#' 
#' @returns
#' Function [autoplot.rpart()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @keywords internal
#' @importFrom ggplot2 autoplot labs
#' @importFrom survival.tzh autoplot.survfit
#' @importFrom fastmd label_pvalue_sym
#' @export autoplot.rpart
#' @export
autoplot.rpart <- function(object, ...) {
  
  if (is.Surv.endpoint.rpart(object)) {
    
    sdf_pval <- object |>
      survdiff_.rpart() |>
      getElement(name = 'pvalue') |>
      label_pvalue_sym(add_p = TRUE)() |> 
      paste('Log-rank (unweighted)')
    # maybe use ?survival.tzh:::desc_survdiff_rho in future
    
    p <- object |> 
      survfit.rpart() |>
      autoplot.survfit(...) +
      labs(caption = sdf_pval)
    return(p)

  }
    
  return(invisible())

}







#' @title Create Markdown Lines for \link[rpart]{rpart.object}
#' 
#' @description
#' ..
#' 
#' @param x an \link[rpart]{rpart.object}
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
#'  'survival' = rpart(os ~ age, data = vet, maxdepth = 2L, model = TRUE)
#' ) |> fastmd::render2html(file = 'rpart')
#' 
#' @keywords internal
#' @importFrom fastmd md_ md_.default md_autoplot_
#' @importClassesFrom fastmd md_lines
#' @importFrom survival.tzh .kaplan_meier58
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
    new(Class = 'md_lines', package = 'rpart', bibentry = .breiman84())
  
  z2 <- xnm |> 
    sprintf(fmt = 'prp_(%s)') |> 
    new(Class = 'md_lines', chunk.r = TRUE)
  
  if (is.Surv.endpoint.rpart(x)) {
    
    # um.. not ready to use
    # ?survival.tzh::md_.survfit
    # yet..
    
    z31 <- '@KaplanMeier58 estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survival`**</u>.' |>
      new(Class = 'md_lines', package = 'survival', bibentry = .kaplan_meier58())
    
    z32 <- md_autoplot_(x = x, xnm = xnm, ...)
    
    z3 <- c(z31, z32)
    
  } else z3 <- NULL
  
  c(z1, z2, z3)
  
}  







