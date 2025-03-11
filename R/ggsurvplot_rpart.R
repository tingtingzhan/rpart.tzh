
#' @title Kaplan-Meier Curves of \link[rpart]{rpart} via \CRANpkg{survminer}
#' 
#' @description
#' Kaplan-Meier curves based on recursive partitioning and regression trees \link[rpart]{rpart}.
#' 
#' @param fit \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' 
#' @examples 
#' library(survival)
#' library(rpart)
#' rp = rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' rp |> ggsurvplot_rpart()
#' 
#' @importFrom survminer ggsurvplot
#' @export
ggsurvplot_rpart <- function(fit, ...) {
  # works but too ugly
  
  sfit <- survfit.rpart(fit)
  if (!length(sfit)) return(invisible())
  
  p <- ggsurvplot(
    fit = sfit, data = sfit[['data']], 
    conf.int = TRUE,
    pval = TRUE, 
    pval.coord = c(750, .95), # p-value location
    legend = 'right', 
    ylab = deparse1(fit$terms[[2L]]))
  
  # @importFrom ggplot2 scale_y_continuous
  #suppressMessages(
  #  p$plot <- p$plot + 
  #    scale_y_continuous(labels = function(x) sprintf(fmt = '%.0f%%', 1e2*x))
  #)
  
  return(p)
  
}




