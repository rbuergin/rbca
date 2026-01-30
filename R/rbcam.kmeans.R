#' Cluster Partworth Utilities Using k-Means
#'
#' Applies the \code{\link[stats]{kmeans}} algorithm to partworth utility functions
#' estimated by a hierarchical conjoint analysis model fitted with \code{\link{rbcam}}.
#'
#' @param object An object of class 'rbcam' resulting from a call to
#'   \code{rbcam}. The object must contain level 1 models.
#' @param centers An integer specifying the number of clusters (centroids) to be used
#'   in the k-means clustering algorithm.
#' @param ... Additional arguments passed to \code{\link[stats]{kmeans}}.
#'
#' @details
#' This function extracts individual-level partworth utility estimates from level 1 models
#' and performs k-means clustering on the resulting coefficient matrix. Each row of the
#' matrix corresponds to a respondent, and each column to an estimated utility for a
#' specific attribute level.
#'
#' The function is useful for segmenting respondents based on their preference structures,
#' helping identify latent consumer segments with similar utility profiles.
#'
#' Requires \code{kmeans}.
#'
#' @return An object of class \code{"kmeans"} as returned by \code{\link[stats]{kmeans}},
#' containing elements such as:
#' \describe{
#'   \item{\code{cluster}}{Cluster assignment for each respondent}
#'   \item{\code{centers}}{Cluster centers (average utility vectors)}
#'   \item{\code{tot.withinss}}{Total within-cluster sum of squares}
#'   \item{\code{betweenss}}{Between-cluster sum of squares}
#'   \item{\code{size}}{Number of respondents in each cluster}
#' }
#'
#' @seealso \code{rbcam}, \code{\link[stats]{kmeans}}
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' tea.kmeans <- rbcam.kmeans(object = tea.m, centers = 3)
#'
#' tea.kmeans$cluster
#'
#' @export


rbcam.kmeans <- function(object, centers, ...) {
  if (!inherits(object, "rbcam")) stop("'object.ref' not of class 'rbcam'.")
  if (is.null(object$mlist.level.1)) stop("'object' does not contain level 1 models.")
  coef <- rbcam.coef(object, level = 1)
  rval <- kmeans(x = coef, centers = centers, ...)
  return(rval)
}
