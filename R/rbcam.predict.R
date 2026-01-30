#' Predict Ratings from a Conjoint Analysis Model
#'
#' Computes predicted ratings (i.e., total utilities) for new data based on a conjoint analysis model
#' fitted with \code{\link{rbcam}}. This function supports predictions from models at different levels:
#' the overall (pooled) model or group-specific models based on respondent or subgroup identifiers.
#'
#' @param object An object of class 'rbcam', typically the result of a call to \code{rbcam}.
#' @param newdata An optional data frame containing new observations for which predictions are to be made.
#'   If \code{NULL} (default), the original data used for model fitting is used.
#' @param level An integer indicating the model level to be used for prediction.
#'   Must be one of \code{1} (respondent-specific), \code{2} (group-specific), or \code{3} (pooled model).
#'
#' @details
#' At level 3, predictions are made using a single pooled model. At levels 1 or 2, the function applies
#' group-specific models based on factor levels of the grouping variable provided during model fitting
#' via the \code{var.level.1} or \code{var.level.2} arguments in \code{rbcam}.
#'
#' @return A numeric vector of predicted ratings (total utilities).
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' preds <- rbcam.predict(object = tea.m, level = 3)
#'
#' @export

rbcam.predict <- function(object, newdata = NULL, level = 3) {

  if (!inherits(object, "rbcam"))
    stop("'object' not of class 'rbcam'")
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")
  if (is.null(object[[paste0("mlist.level.", level)]]))
    stop("no models available on level '", level, "'.")

  if (is.null(newdata)) newdata <- object$data
  if (level == 3) {

    rval <- predict(object = object$mlist.level.3, newdata = newdata)

  } else {

    if (!object[[paste0("var.level.", level)]] %in% colnames(newdata))
      stop(paste0("level ", level, " variable '", object[[paste0("var.level.", level)]], "' not found in 'newdata'."))
    if (!(is.factor(newdata[, object[[paste0("var.level.", level)]]]) &&
          nlevels(newdata[, object[[paste0("var.level.", level)]]]) == nlevels(object$data[, object[[paste0("var.level.", level)]]]) &&
          all(levels(newdata[, object[[paste0("var.level.", level)]]]) == levels(object$data[, object[[paste0("var.level.", level)]]]))))
      stop(paste0(
        "'", object[[paste0("var.level.", level)]],
        "' must be a factor variable with the same levels as in the original data."))

    rval <- rep(NA, nrow(newdata))
    for (i in unique(newdata[, object[[paste0("var.level.", level)]]])) {
      rval[newdata[, object[[paste0("var.level.", level)]]] == i] <- predict(
        object = object[[paste0("mlist.level.", level)]][[i]],
        newdata = newdata[newdata[, object[[paste0("var.level.", level)]]] == i, ])
    }
    names(rval) <- rownames(newdata)
  }
  return(rval)
}

