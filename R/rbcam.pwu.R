#' Partworth Utilities from Conjoint Models
#'
#' Computes partworth utility functions for additive effects in models fitted with \code{\link{rbcam}}.
#'
#' This function extracts partworth utilities (PWUs) from the estimated regression models. PWUs are calculated
#' for all additive (main effect) terms in the model, either on the pooled level or by group/individual levels.
#' Categorical and continuous attributes are handled accordingly, and an optional normalization is available.
#'
#' @param object A fitted 'rbcam' object (a list of models returned by \code{rbcam}).
#' @param level Integer (1, 2, or 3). Indicates the level at which partworth utilities should be extracted:
#'   \itemize{
#'     \item \code{3}: pooled model (all data combined),
#'     \item \code{2}: group-level models (as defined by \code{var.level.2}),
#'     \item \code{1}: individual-level models (as defined by \code{var.level.1}).
#'   }
#' @param normalized Logical. If \code{TRUE}, the partworth utilities are normalized:
#'   \itemize{
#'     \item For categorical variables: scaled by the range of the partworths.
#'     \item For continuous variables: scaled by the absolute maximum value.
#'   }
#'
#' @return A named \code{list} of partworth utility values:
#'   \itemize{
#'     \item For \code{level = 3}: a list of named vectors, one per additive term.
#'     \item For \code{level = 1 or 2}: a list of matrices (one per additive term), where rows represent individuals or groups, and columns are levels or values of the attribute.
#'   }
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Get partworth utilities on pooled level
#' rbcam.pwu(object = tea.m, level = 3)
#'
#' # Get partworth utilities for individuals (level 1), normalized
#' rbcam.pwu(object = tea.m, level = 1, normalized = TRUE)
#'
#' @export


rbcam.pwu <- function(object, level = 3, normalized = FALSE) {

  ## checks
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!(is.numeric(level) && length(level) == 1 && level %in% 1:3))
    stop("'level' must be '1', '2' or '3'")
  if (is.null(object[[paste0("mlist.level.", level)]]))
    stop("no models available on level '", level, "'.")

  ## function to extract partworth utilities
  FUN <- function(m) {
    ## get additive terms
    terms <- attr(terms(m), "term.labels")
    terms.subs <- which(sapply(X = terms, FUN = function(x) length(all.vars(as.formula(paste0("~ ", x))))) == 1)
    terms <- terms[terms.subs]
    ## compute for each additive term the partworth function
    rval <- lapply(
      X = seq_along(terms),
      FUN = function(i) {
        x <- model.frame(m)[, terms[i]]
        if (is.factor(x)) {
          ## if categorical, extract coefficients for each category
          x.vals <- data.frame(x = factor(x = levels(x), levels = levels(x)))
          colnames(x.vals) <- terms[i]
          x.con <- m$contrasts[terms[i]]
          x.vals.mm <- model.matrix(
            as.formula(paste0("~ -1 + ", terms[i])), data = x.vals, contrasts = x.con)
          x.coef <- c(x.con[[terms[i]]] %*% coef(m)[attr(model.matrix(m), "assign") == terms.subs[i]])
          names(x.coef) <- rownames(x.con[[terms[i]]])
          rval <- c(x.vals.mm %*% x.coef)
          if (normalized) rval <- rval / diff(range(rval))
          names(rval) <- levels(x.vals[, terms[i]])
        } else {
          ## otherwise, extract partial function for each unique value
          x.vals <- data.frame(x = sort(unique(x)))
          colnames(x.vals) <- terms[i]
          x.vals.mm <- model.matrix(
            as.formula(paste0("~ -1 + ", terms[i])), data = x.vals)
          x.coef <- coef(m)[attr(model.matrix(m), "assign") == terms.subs[i]]
          rval <- c(x.vals.mm %*% x.coef)
          if (normalized) rval <- rval / abs(max(rval))
          names(rval) <- format(x = x.vals[, terms[i]], digits = 3)
        }
        return(rval)
      })
    names(rval) <- terms
    return(rval)
  }

  ## apply the function above
  if (level == 3) {
    rval <- FUN(object$mlist.level.3)
  } else {
    rval <- lapply(X = object[[paste0("mlist.level.", level)]], FUN = FUN)

    ## collapse lists for group models
    pwu.level.3 <- FUN(object$mlist.level.3)
    rval <- lapply(
      X = names(pwu.level.3),
      FUN = function(v) {
        rval <- sapply(
          X = rval,
          FUN = function(x) {
            rval <- pwu.level.3[[v]] * NA
            rval[names(x[[v]])] <- x[[v]]
          })
        if (is.matrix(rval)) {
          rval <- t(rval)
          rownames(rval) <- object[[paste0("var.level.", level, ".levels")]]
        } else {
          rval <- matrix(
            data = rval,
            ncol = 1,
            dimnames = list(object[[paste0("var.level.", level, ".levels")]], rval[[v]]))
        }
        return(rval)
      })
    names(rval) <- names(pwu.level.3)

  }

  return(rval)
}
