#' Conjoint Analysis Model Function
#'
#' Computes conjoint analysis models for rating-based data.
#'
#' This function estimates group-wise conjoint analysis models based
#' on rating data and also provides the pooled model.
#'
#' @param formula A formula specifying the model, with the rating variable
#'    on the left-hand side and the attributes on the right-hand side,
#'    potentially including interactions. Attributes should be of class
#'    'integer', 'numeric', 'factor', or 'ordered'. Must consist of at least
#'    one attribute.
#' @param data A \code{data.frame} containing the dataset. The dataset must
#'    be complete with respect to the variables specified in the formula.
#' @param var.level.1 A single character string specifying the column
#'    name of a factor variable representing level 1 (individual respondents).
#'    The linear regression models will be fitted separately for each level.
#'    The variable must exist in 'data', and there should be multiple
#'    observations per factor level.
#' @param var.level.2 A single character string specifying the column
#'    name of a factor variable representing level 2 (predefined subgroups
#'    or clusters). Linear regression models will be fitted separately for
#'    each level of this variable. The variable must exist in 'data'
#'    and should have multiple factor levels. If \code{var.level.1} is specified,
#'    the levels of \code{var.level.1} should be nested within \code{var.level.2}.
#' @param contrasts.factor A single character string indicating the type of
#'    contrasts to apply to non-ordered factor attributes. For \code{'contr.treatment'},
#'    the last factor level is used as the base category (instead of the first).
#'    Ordered attributes always use \code{'contr.poly'}.
#'
#'
#' @return A \code{list} containing the following elements:
#' \itemize{
#'    \item \code{mlist.level.3} - A \code{\link{lm}} object representing the pooled model.
#'    \item \code{mlist.level.2} - A list of \code{\link{lm}} objects representing
#'       the level 2 models for each level of the level 2 variable. May be \code{NULL}.
#'    \item \code{mlist.level.1} - A list of \code{\link{lm}} objects representing
#'       the level 1 models for each level of the level 1 variable. May be \code{NULL}.
#'    \item \code{var.level.2} - The column name of the level 2 variable. May be \code{NULL}.
#'    \item \code{var.level.2.levels} - The levels of the level 2 variable. May be \code{NULL}.
#'    \item \code{var.level.1} - The column name of the level 1 variable. May be \code{NULL}.
#'    \item \code{var.level.1.levels} - The levels of the level 1 variable. May be \code{NULL}.
#'    \item \code{data} - The complete input dataset.
#'    \item \code{attributes} - A character vector containing the column names of the attributes.
#'    \item \code{attributes.class} - A list specifying the class of each attribute variable.
#'    \item \code{contrasts} - The contrast settings used for factor and ordered variables
#'       when fitting the model.
#' }
#'
#' @return A summary of the regression analysis.
#' @export
#'
#' @examples
#' data(tea) # load the data into your environment
#'
#' # Fit a simple conjoint model
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea)
#'
#' # Fit a group.wise model using 'respondent'
#' # (which specifies the respondent column in the data) as level 1
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Get the model output (model made for level 3 and each respondent)
#' tea.m
#'
#' # Fit a subgroup model using a 'group'
#' # (which specifies a factor attribute of the data) as level 2
#'
#' # create a group
#' set.seed(123)
#' tea$group <- as.factor(rep(sample(1:3, size = length(unique(tea$respondent)),replace = TRUE),
#'                           each = 13))
#'
#' # create the model with the group name as var.level.2
#' tea.group.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.2 = "group")
#'
#' # Get the model output (model made for level 3 and each group)
#' tea.group.m

rbcam <- function(
    formula, data, var.level.1 = NULL, var.level.2 = NULL,
    contrasts.factor = c("contr.treatment", "contr.sum")) {

  ## check inputs
  if (!inherits(formula, "formula")) stop("'formula' must be an object of class 'formula'.")
  if (!is.data.frame(data)) stop("'data' must be an object of class 'data.frame'.")
  data <- as.data.frame(data) # Correction 2025-04-08: ensures that no tibbles etc. enters, which produced errors
  if (!all(all.vars(formula) %in% colnames(data))) stop("some of the variables of 'formula' are not in 'data'.")
  if (nrow(na.omit(data[, all.vars(formula), drop = FALSE])) < nrow(data))
    stop("'data' contains missing values regaring the variables in 'formula'.")
  if (!is.null(var.level.1)) {
    if (!(is.character(var.level.1) && length(var.level.1) == 1 && var.level.1 %in% colnames(data)))
      stop("'var.level.1' must be a single character indicating a column name in 'data'.")
    if (!is.factor(data[, var.level.1])) stop("'var.level.1' must refer to a factor variable in 'data'.")
    if (nlevels(data[, var.level.1]) < 2) stop("variable 'var.level.1' must have at least two factor levels.")
    if (nlevels(data[, var.level.1]) > length(unique(data[, var.level.1])))
      stop(paste0("factor variable '", var.level.1, "' contains unused levels."))
  }
  if (!is.null(var.level.2)) {
    if (!(is.character(var.level.2) && length(var.level.2) == 1 && var.level.2 %in% colnames(data)))
      stop("'var.level.2' must be a single character indicating a column name in 'data'.")
    if (!is.factor(data[, var.level.2])) stop("'var.level.2' must refer to a factor variable in 'data'.")
    if (nlevels(data[, var.level.2]) < 2) stop("variable 'var.level.2' must have at least two factor levels.")
    if (nlevels(data[, var.level.2]) > length(unique(data[, var.level.2])))
      stop(paste0("factor variable '", var.level.2, "' contains unused levels."))
  }
  if (!is.null(var.level.1) & !is.null(var.level.2)) {
    tab <- as.matrix(table(data[, var.level.2], data[, var.level.1])) > 0
    if (any(colSums(tab) > 1))
      stop("levels of variable 'var.level.1' not nested within levels variable 'var.level.2'.")
  }
  contrasts.factor <- match.arg(contrasts.factor)

  ## set contrasts
  attr <- all.vars(formula)[-attr(terms(formula), "response")]
  con <- lapply(
    X = attr[sapply(X = data[, attr, drop = FALSE], FUN = is.factor)],
    FUN = function(x) {
      levs <- levels(data[, x])
      if (is.ordered(data[, x])) {
        rval <- contr.poly(n = length(levs))
      } else {
        if (contrasts.factor == "contr.treatment") {
          rval <- contr.treatment(n = length(levs), base = length(levs))
        } else {
          rval <- contr.sum(n = length(levs))
        }
        colnames(rval) <- levs[-length(levs)]
      }
      rownames(rval) <- levs
      return(rval)
    })
  names(con) <- attr[sapply(X = data[, attr, drop = FALSE], FUN = is.factor)]

  ## compute pooled model
  rval <- list(mlist.level.3 = lm(formula = formula, data = data, contrasts = con))

  ## compute the level 2 models
  if (!is.null(var.level.2)) {
    rval$mlist.level.2 <- lapply(
      X = seq_along(levels(data[, var.level.2])),
      FUN = function(i) lm(
        formula = formula,
        data = data[data[, var.level.2] == levels(data[, var.level.2])[i], ],
        contrasts = con))
    names(rval$mlist.level.2) <- levels(data[, var.level.2])
    rval$var.level.2 <- var.level.2
    rval$var.level.2.levels <- levels(data[, var.level.2])
  }

  ## compute the individual models
  if (!is.null(var.level.1)) {
    rval$mlist.level.1 <- lapply(
      X = seq_along(levels(data[, var.level.1])),
      FUN = function(i) lm(
        formula = formula,
        data = data[data[, var.level.1] == levels(data[, var.level.1])[i], ],
        contrasts = con))
    names(rval$mlist.level.1) <- levels(data[, var.level.1])
    rval$var.level.1 <- var.level.1
    rval$var.level.1.levels <- levels(data[, var.level.1])
  }

  ## add information on attributes
  rval$data <- data
  rval$attributes <- attr
  rval$attributes.class <- sapply(
    X = attr,
    FUN = function(v) ifelse(is.factor(data[, v]), "factor", "numeric"))
  rval$contrasts <- con

  ## add class
  class(rval) <- "rbcam"

  ## return list of models
  return(rval)
}
