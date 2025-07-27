
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
#' Function [nobs.rpart()] returns an \link[base]{integer} scalar
#' 
#' @keywords internal
#' @importFrom stats nobs
#' @export nobs.rpart
#' @export
nobs.rpart <- function(object, ...) object$frame$n[1L]
