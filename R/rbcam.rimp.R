#' Relative Importance of Attributes (RIMP)
#'
#' Computes the relative importance (RIMP) of attributes based on
#' part-worth utilities from a hierarchical conjoint model fitted
#' with \code{\link{rbcam}}. This metric quantifies the contribution of each
#' attribute to the overall preference by evaluating the range or
#' absolute values of part-worth utilities across levels.
#'
#' @param object An object of class `rbcam`, as returned by \code{rbcam}.
#' @param level Integer indicating the hierarchy level at which to compute RIMP.
#'   Must be one of:
#'   - `1`: Individual-level models
#'   - `2`: Group-level models
#'   - `3`: Population-level model (default)
#'
#' @return A numeric object:
#' \describe{
#'   \item{If `level = 3` (population level):}{a named numeric vector with RIMP values for each attribute.}
#'   \item{If `level = 1` or `level = 2`:}{a matrix with rows corresponding to respondents or groups, and columns for each attribute. Each cell represents the RIMP value of that attribute for that unit.}
#' }
#'
#' @details
#' For `level = 3`, RIMP is computed as the absolute maximum of each attribute's part-worths, divided by the sum across all attributes.
#' For `level = 1` or `2`, RIMP is computed as the range (max - min) of each attribute's part-worth function, normalized within each unit.
#'
#' This function is useful for assessing which attributes drive preferences the most at different levels of analysis.
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Compute RIMP at the population level
#' rbcam.rimp(object = tea.m, level = 3)
#'
#' # Compute RIMP at the individual level
#' rbcam.rimp(object = tea.m, level = 1)
#'
#' @export


rbcam.rimp <- function(object, level = 3) {

  ## checks
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")
  if (is.null(object[[paste0("mlist.level.", level)]]))
    stop("no models available on level '", level, "'.")

  ## get non-normalized parthworths
  pwu <- rbcam.pwu(object, level = level, normalized = FALSE)

  ## extract maximum for each partworth function
  if (level == 3) {
    pwu.max <- sapply(X = pwu, FUN = function(x) max(abs(x)))
    rval <- pwu.max / sum(pwu.max)
  } else {
    pwu.max <- sapply(X = pwu, FUN = function(x) apply(X = x, MARGIN = 1, function(x) diff(range(x))))
    rval <- pwu.max / rowSums(pwu.max)
  }

  return(rval)
}
