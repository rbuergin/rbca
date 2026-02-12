#' Expected Mean Squared Error of Prediction (EMSEP) for Conjoint Models
#'
#' Computes the Expected Mean Squared Error of Prediction (EMSEP) for a set of nested
#' conjoint analysis models fitted via \code{\link{rbcam}}. The metric evaluates the
#' expected prediction error increase when using a simpler model compared to a
#' more complex reference model, following Hagerty (1991).
#'
#' @param object.list A list of objects of class 'rbcam' representing nested
#'   conjoint models. Can also be a single \code{rbcam} object.
#' @param object.ref A single \code{rbcam} object representing the most complex model
#'   to which the others are compared.
#' @param level Integer value indicating the level of the hierarchical model to use
#'   for comparison. Must be \code{1}, \code{2}, or \code{3}.
#'
#' @details
#' EMSEP provides a quantitative evaluation of model complexity versus predictive accuracy.
#' It combines adjusted \eqn{R^2} values and the number of model parameters to estimate
#' the expected loss in predictive performance. The function assumes that the models
#' were fit to the same data (i.e., the same profiles were rated by all respondents) and
#' that each simpler model is nested within the reference model (i.e., its predictors
#' form a subset of those in the reference).
#'
#' If these assumptions are not met, EMSEP values may be invalid.
#'
#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{\code{r2.adj}}{Adjusted \eqn{R^2} for each model in \code{object.list}}
#'     \item{\code{nPars}}{Number of estimated parameters (non-NA coefficients)}
#'     \item{\code{nObs}}{Number of observations used in the model}
#'     \item{\code{emsep}}{Expected Mean Squared Error of Prediction for each model}
#'   }
#'
#' @references
#' Hagerty, M. R. (1991). Comparing the predictive powers of alternative multiple regression models.
#' \emph{Psychometrika}, \bold{56}(1), 77-85. doi: \url{https://doi.org/10.1007/BF02294587}
#'
#' @examples
#' data(tea)
#'
#' tea.m.price <- rbcam(
#'   formula = rating ~ price,
#'   var.level.1 = "respondent",
#'   data = tea)
#'
#' tea.m.variety <- rbcam(
#'   formula = rating ~ variety,
#'   var.level.1 = "respondent",
#'   data = tea)
#'
#' tea.m.int <- rbcam(
#'   formula = rating ~ price + variety + price * variety,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' rbcam.emsep(
#'   object.list = list(
#'     "IA, only price" = tea.m.price,
#'     "IA, only variety" = tea.m.variety,
#'     "IA, additive + price * variety" = tea.m.int),
#'   object.ref = tea.m.int,
#'   level = 1)
#'
#' @export

rbcam.emsep <- function(object.list, object.ref, level = 3) {

  ## checks
  if (inherits(object.list, "rbcam")) object.list <- list(object.list) # if only 1 model
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")
  if (!is.list(object.list)) stop("'object.list' must be a list or a single 'rbcam' object.")
  if (!all(sapply(object.list, function(x) inherits(x, "rbcam"))))
    stop("some elements of 'object.list' are not of class 'rbcam'.")
  if (!inherits(object.ref, "rbcam"))
    stop("'object.ref' not of class 'rbcam'.")
  if (!all(sapply( # check if models are hierarchical
    X = object.list,
    FUN = function(x) all(attr(terms(x$mlist.level.3), "term.labels") %in%
                          attr(terms(object.ref$mlist.level.3), "term.labels")))))
    stop("some models of 'object.list' involve terms that are not found in 'object.ref'")
  if (!all(sapply(X = object.list, FUN = function(x) identical( # data must be identical
    x$data[, c(all.vars(terms(object.ref$mlist.level.3)), object.ref$var.level.1)],
    object.ref$data[, c(all.vars(terms(object.ref$mlist.level.3)), object.ref$var.level.1)]))))
    stop("some models of 'object.list' are based on different datasets")
  if (!all(sapply(X = object.list, FUN = function(x) !is.null(x[[paste0("mlist.level.", level)]]))))
    stop(paste0("some models of 'object.list' do not contain models on level '", level, "'"))
  if (is.null(object.ref[[paste0("mlist.level.", level)]]))
    stop(paste0("'object.ref' does not contain models on level '", level, "'"))

  ## compute R^2 of individual models
  rsquared.adj <- sapply(
    X = object.list,
    FUN = rbcam.rsquared,
    level = level,
    adjusted = TRUE)

  ## compute R^2 of large model
  rsquared.ref <- rbcam.rsquared(
    object.ref,
    level = level,
    adjusted = TRUE)

  ## number of estimated coefficients
  k <- sapply(
    X = object.list,
    FUN = function(x) {
      return(sum(!is.na(rbcam.coef(x, level = level))))
    })

  ## number of profiles (this is actually only good for balanced designs)
  #m <- nrow(unique(object.ref$data[, object.ref$attributes, drop = FALSE]))
  n <- nrow(object.ref$data)  # Correction 2025-04-08

  ## compute emsep
  emsep <- rep(rsquared.ref, length(object.list)) - rsquared.adj +
    (1 - rep(rsquared.ref, length(object.list))) * (1 + k / rep(n, length(object.list)))

  ## collect data for return value
  rval <- data.frame(
    "r2.adj" = rsquared.adj,
    "nPars" = k,
    "nObs" = n,  # Correction 2025-04-08: 'nObs' statt 'nProf'
    "emsep" = emsep)
  if (!is.null(names(object.list))) rownames(rval) <- names(object.list)
  return(rval)
}
