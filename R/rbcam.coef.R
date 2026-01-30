#' Extract Coefficients from Conjoint Analysis Models
#'
#' Extracts estimated model coefficients from a fitted \code{\link{rbcam}} object, including the intercept.
#'
#' This function returns the estimated coefficients from the pooled model (level 3),
#' group-level models (level 2), or individual-level models (level 1), depending on the specified level.
#'
#' @param object An object of class 'rbcam' resulting from a call to \code{rbcam}.
#' @param level An integer value (1, 2, or 3) specifying which set of model coefficients to extract:
#'   \itemize{
#'     \item \code{1}: Individual-level models
#'     \item \code{2}: Group-level models
#'     \item \code{3}: Pooled model (default)
#'   }
#'
#' @return A named vector (for level 3), or a matrix (for levels 1 or 2), with the estimated model coefficients.
#'   \itemize{
#'     \item If \code{level = 3}, a named numeric vector with the coefficients of the pooled model.
#'     \item If \code{level = 1} or \code{2}, a matrix with rows corresponding to each group or individual, and columns to model terms.
#'   }
#'
#' @examples
#' # Load example data
#' data(tea)
#'
#' # Fit a conjoint model
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Extract pooled model coefficients
#' rbcam.coef(object = tea.m , level = 3)
#'
#' # Extract individual-level coefficients
#' rbcam.coef(object = tea.m , level = 1)
#'
#' @export

rbcam.coef <- function(object, level = 3) {

  ## check 'object'
  if (!inherits(object, "rbcam")) stop("'object' probably not fitted with 'rbcam()'")
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")

  if (level == 3) {
    rval <- coef(object$mlist.level.3)
  } else {
    if (is.null(object[[paste0("mlist.level.", level)]])) stop(paste0("no models on level ", level, "."))
    rval <- t(sapply(
      X = object[[paste0("mlist.level.", level)]],
      FUN = function(x) {
        rval <- coef(object$mlist.level.3) * NA
        rval[names(coef(x))] <- coef(x)
        return(rval)
      }
    ))
    rownames(rval) <- object[[paste0("var.level.", level, ".levels")]]
  }
  return(rval)
}
