#' Evaluate Within-Cluster Sum of Squares for k-Means Clustering
#'
#' Computes and optionally plots the total within-cluster sum of squares for different
#' numbers of clusters based on partworth utilities estimated from a hierarchical
#' conjoint analysis model. Useful for determining the optimal number of clusters via
#' the "elbow method".
#'
#' @param object An object of class 'rbcam as returned by \code{rbcam}.
#'   Must contain level 1 models.
#' @param centers.seq An integer vector specifying the sequence of cluster numbers
#'   to evaluate. Each value represents a different number of clusters used in
#'   \code{kmeans}.
#' @param plot Logical. If \code{TRUE}, the plot is displayed. Otherwise, a \code{ggplot2} object is returned invisibly.
#' @param ... Additional arguments passed to \code{\link[stats]{kmeans}}.
#'
#' @details
#' This function applies k-means clustering to the level 1 partworth utility estimates
#' extracted from the model. For each number of clusters in \code{centers.seq}, the
#' total within-cluster sum of squares is computed. Plotting the results helps identify
#' the "elbow" point where increasing the number of clusters leads to diminishing
#' improvements in model fit.
#'
#' If \code{centers.seq} is missing, a default range from 1 to two-thirds the number of
#' respondents is used.
#'
#' Requires \code{kmeans}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{plot}}{A ggplot object showing the total within-cluster sum of squares
#'     for each number of clusters.}
#'   \item{\code{tot.withinss}}{A numeric vector of total within-cluster sum of squares.}
#'   \item{\code{centers.seq}}{The sequence of cluster numbers that was evaluated.}
#' }
#'
#' @seealso \code{\link{rbcam}}, \code{\link{rbcam.kmeans}}, \code{\link[stats]{kmeans}}
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Evaluate within-cluster sum of squares for 1 to 40 clusters
#' km.withinss <- rbcam.kmeans.withinss(object = tea.m, centers.seq = 1:40)
#'
#' # Access the raw values
#' km.withinss$tot.withinss
#'
#' @export


rbcam.kmeans.withinss <- function(object, centers.seq, plot = TRUE, ...) {
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (is.null(object$mlist.level.1)) stop("'object' does not contain level 1 models.")
  coef <- rbcam.coef(object, level = 1)
  if (missing(centers.seq)) {
    centers.seq <- 1:pmax(floor(2 / 3 * nrow(coef)), 1)
  } else if (!(
    is.numeric(centers.seq) &&
    all(as.integer(na.omit(centers.seq)) > 0) &&
    max(as.integer(na.omit(centers.seq))) < nrow(coef) &&
    length(unique(as.integer(na.omit(centers.seq)))) == length(centers.seq) &&
    all(unique(as.integer(na.omit(centers.seq))) == centers.seq))) {
    stop(paste0(
      "'centers.seq' is a increasing sequence of integers larger than value 0 and smaller than ",
      nrow(coef)," (i.e., less than the number of level 1 units)."))
  }
  if (missing(centers.seq)) centers.seq <- 1:pmax(floor(2 / 3 * nrow(coef)), 1)
  # there may still be an error ...
  kmeans.list <- lapply(X = centers.seq, FUN = function(x) kmeans(x = coef, centers = x, ...))
  pdat <- data.frame(
    centers = centers.seq,
    tot.withinss = sapply(X = kmeans.list, FUN = function(x) x$tot.withinss))
  p <- ggplot(
    data = pdat,
    mapping = aes(x = centers, y = tot.withinss)) +
    geom_line() +
    geom_point() +
    theme_minimal()
  if (plot) print(p)
  rval <- list(
    tot.withinss <- sapply(X = kmeans.list, FUN = function(x) x$tot.withinss),
    centers.seq <- centers.seq)
  return(rval)
}
