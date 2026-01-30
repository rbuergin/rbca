#' Coefficient of Determination (\eqn{R^2}) for Conjoint Analysis Models
#'
#' Computes the coefficient of determination (\eqn{R^2}) for a fitted conjoint analysis model
#' of class 'rbcam'. The function supports standard and adjusted \eqn{R^2} computation
#' for all levels of the hierarchical model.
#'
#' @param object An object of class 'rbcam' as returned by \code{rbcam}.
#' @param level Integer specifying which model level to evaluate: \code{1}, \code{2}, or \code{3}.
#'   Level 3 corresponds to the overall model, while levels 1 and 2 correspond to nested subject-specific models.
#' @param adjusted Logical. If \code{TRUE} (default), computes the adjusted \eqn{R^2}. If \code{FALSE},
#'   computes the conventional (unadjusted) \eqn{R^2}.
#'
#' @return A numeric value between 0 and 1 representing the (adjusted) coefficient of determination.
#'
#' @details
#' The \eqn{R^2} statistic measures the proportion of variance in the dependent variable that is predictable
#' from the independent variables. For hierarchical models (levels 1 and 2), \eqn{R^2} is computed by aggregating
#' the residuals and degrees of freedom across all sub-models.
#'
#' Adjusted \eqn{R^2} accounts for the number of predictors in the model and is generally preferred for model comparison.
#'
#' @seealso \code{rbcam}, \code{rbcam.msep}
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' rbcam.rsquared(object = tea.m, level = 3, adjusted = FALSE)
#'
#' rbcam.rsquared(object = tea.m, level = 1)
#'
#' @export


rbcam.rsquared <- function(object, level = 3, adjusted = TRUE) {

  ## checks
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")
  if (is.null(object[[paste0("mlist.level.", level)]]))
    stop("no models available on level '", level, "'.")

  ## extract name of response variable
  response.name <- all.vars(terms(object$mlist.level.3))[attr(terms(object$mlist.level.3), "response")]

  ## compute total sum of squares (which is model-independent)
  sstot <- sum((object$data[, response.name] - mean(object$data[, response.name]))^2)

  ## compute the residual sum of squares (which is model-dependent)
  if (level == 3) {
    ssres <- sum(residuals(object$mlist.level.3)^2)
    p <- sum(!is.na(coef(object$mlist.level.3)))
  } else {
    ssres <- sum(sapply(object[[paste0("mlist.level.", level)]], function(x) sum(residuals(x)^2)))
    p <- sum(sapply(object[[paste0("mlist.level.", level)]], function(x) sum(!is.na(coef(x)))))
  }

  ## apply adjustment if demanded (default)
  if (adjusted) {
    dftot <- nrow(object$data) - 1
    dfres <- nrow(object$data) - p
    rval <- 1 - (ssres / dfres) / (sstot / dftot)
  } else {
    rval <- 1 - ssres / sstot
  }

  ## return the computed R^2
  return(rval)
}
