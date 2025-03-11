

#' @title Create R Markdown Lines for \link[rpart]{rpart} Objects
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
#' Function [rmd_.rpart()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
rmd_.rpart <- function(x, xnm, ...) {
  
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 6
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  fom <- x$terms
  txt1 <- sprintf(
    fmt = 'Recursive partitioning and regression tree for **`%s`** based on potential covariate(s) %s is provided by <u>**`R`**</u> package <u>**`rpart`**</u>. The data-driven partition, i.e., the collection of cutoff value(s), is based on %d observations.', 
    deparse1(fom[[2L]]),
    paste0('`', all.vars(fom[[3L]]), '`', collapse = ', '),
    x$frame$n[1L])
  
  model_ <- x$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]]
  # \link[rpart]{rpart} returned value `ret$y` is a \link[base]{matrix}, *not* \link[survival]{Surv}
  # ret |> terms() |> attr(which = 'dataClasses', exact = TRUE) gives 'nmatrix.2'
  
  if (inherits(y, what = 'Surv')) {
    txt2 <- 'Kaplan-Meier estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survival`**</u>.'
    KM <- c(
      sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', 4, 7), 
      #sprintf(fmt = 'ggsurvplot_rpart(%s)', xnm), # too ugly!!
      sprintf(fmt = 'ggKM.rpart(%s)', xnm), # ?survival.tzh::ggKM.rpart
      '```'
    )
  } else txt2 <- KM <- NULL
  
  return(c(
    paste(txt1, txt2),
    '',
    sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'prp_(%s)', xnm), 
    '```',
    KM
  ))
}  
