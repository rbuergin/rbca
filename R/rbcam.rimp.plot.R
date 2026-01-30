#' Plot Relative Importance (RIMP) of Attributes
#'
#' Visualizes the relative importance (RIMP) of attributes estimated by a
#' hierarchical conjoint analysis model fitted with \code{\link{rbcam}}. The plot can show
#' importance at the population level (level 3), as well as optional overlays
#' for group (level 2) and individual (level 1) levels. Optionally, uncertainty
#' can be visualized using simulated predictions.
#'
#'
#' @param object An object of class `rbcam` as returned by \code{rbcam}.
#' @param show.level.1 Logical. If `TRUE`, displays level 1 (individual-level) RIMP with error bars and means.
#' @param show.level.2 Logical. If `TRUE`, displays level 2 (group-level) RIMP with error bars and means.
#' @param plot Logical. If `TRUE` (default), prints the plot to the active graphics device.
#' @param sim Logical. If `TRUE`, simulates uncertainty for level 3 estimates using bootstrapped predictions.
#' @param nsim Integer. Number of simulations to perform if `sim = TRUE`. Default is 100.
#'
#' @return An invisible `ggplot2` object representing the RIMP plot. Use `plot = FALSE` to return without printing.
#'
#' @details
#' This function requires the **ggplot2** package. The plot shows the proportionate contribution of each attribute
#' to the overall preference model. When `show.level.1` or `show.level.2` is enabled, 95% quantile intervals are drawn,
#' along with mean values. When `sim = TRUE`, simulated confidence intervals are drawn around level 3 estimates.
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Basic plot of RIMP at population level with distribution of level 1 individuals
#' rbcam.rimp.plot(object = tea.m, show.level.1 = TRUE)
#'
#' # With simulation-based uncertainty
#' rbcam.rimp.plot(object = tea.m, sim = TRUE, nsim = 50)
#'
#' @export


rbcam.rimp.plot <- function(object, show.level.1 = FALSE, show.level.2 = FALSE, plot = TRUE, sim = FALSE, nsim = 100) {

  ## checks
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!(is.logical(show.level.1) & length(show.level.1) == 1)) stop("'show.level.1' must be a single logical")
  if (!(is.logical(show.level.2) & length(show.level.2) == 1)) stop("'show.level.2' must be a single logical")
  if (!(is.logical(plot) && length(plot) == 1)) stop("'plot' must be a single logical")
  if (!(is.logical(sim) && length(sim) == 1)) stop("'sim' must be a single logical.")
  if (!(is.numeric(nsim) && length(nsim) == 1 && round(nsim) > 0))
    stop("'sim' must be a single positive integer.")
  nsim <- round(nsim)

  ## get RIMP values
  rimp.level.3 <- rbcam.rimp(object, level = 3)
  rimp.level.2 <- rimp.level.1 <- NULL
  if (show.level.2) rimp.level.2 <- rbcam.rimp(object, level = 2)
  if (show.level.1) rimp.level.1 <- rbcam.rimp(object, level = 1)

  ## get RIMP values from simulated data from the pooled model
  if (sim) {
    data <- object$data
    data$fitted <- rbcam.predict(object = object, level = 3)
    sd <- summary(object$mlist.level.3)$sigma
    FUN = function() {
      data$ysim <- data$fitted + rnorm(nrow(data), sd = sd)
      m <- rbcam(formula = update(formula(object$mlist.level.3), ysim ~ .), data = data)
      return(rbcam.rimp(m, level = 3))
    }
    rimp.sim <- t(replicate(n = nsim, FUN()))
    rimp.sim <- as.data.frame(rimp.sim)
    rimp.sim$sim <- 1:nrow(rimp.sim)
    rimp.sim <- reshape(
      data = rimp.sim,
      varying = list(names(rimp.level.3)),
      v.names = "rimp",
      idvar = "sim",
      timevar = "attribute",
      times = names(rimp.level.3),
      direction = "long")
    rimp.sim$attribute <- factor(x = rimp.sim$attribute, levels = names(rimp.level.3))
    rimp.sim$attribute.NUM <- as.integer(rimp.sim$attribute)
  }

  ## prepare the data for plotting
  plotdata.level.3 <- data.frame(
    attribute = factor(x = names(rimp.level.3), levels = names(rimp.level.3)),
    rimp = rimp.level.3)
  plotdata.level.3$attribute.NUM <- as.integer(plotdata.level.3$attribute)
  if (show.level.2) {
    plotdata.level.2 <- as.data.frame(rimp.level.2)
    plotdata.level.2$group <- factor(x = object$var.level.2.levels, levels = object$var.level.2.levels)
    plotdata.level.2 <- reshape(
      data = plotdata.level.2,
      varying = list(names(rimp.level.3)),
      v.names = "rimp",
      timevar = "attribute",
      times = names(rimp.level.3),
      idvar = "group",
      direction = "long")
    plotdata.level.2$attribute <- factor(x = plotdata.level.2$attribute, levels = names(rimp.level.3))
    plotdata.level.2$attribute.NUM = as.integer(plotdata.level.2$attribute)
    levels(plotdata.level.2$group) <- paste0("level.2.", levels(plotdata.level.2$group))
    plotdata.level.2$group <- as.character(plotdata.level.2$group)
  }
  if (show.level.1) {
    plotdata.level.1 <- as.data.frame(rimp.level.1)
    plotdata.level.1$group <- factor(x = object$var.level.1.levels, levels = object$var.level.1.levels)
    plotdata.level.1 <- reshape(
      data = plotdata.level.1,
      varying = list(names(rimp.level.3)),
      v.names = "rimp",
      timevar = "attribute",
      times = names(rimp.level.3),
      idvar = "group",
      direction = "long")
    plotdata.level.1$attribute <- factor(x = plotdata.level.1$attribute, levels = names(rimp.level.3))
    plotdata.level.1$attribute.NUM = as.integer(plotdata.level.1$attribute)
    levels(plotdata.level.1$group) <- paste0("level.1.", levels(plotdata.level.1$group))
    plotdata.level.1$group <- as.character(plotdata.level.1$group)
  }


  ## plot the data
  p <- ggplot(
    data = plotdata.level.3,
    mapping = aes(x = attribute.NUM, y = rimp)) +
    theme_minimal()
  p <- p +
    geom_bar(stat = "identity", mapping = aes(fill = "level 3")) +
    scale_x_continuous(
      name = "attributes",
      breaks = seq_along(rimp.level.3),
      labels = names(rimp.level.3)) +
    scale_y_continuous(
      labels = scales::percent) +
    scale_fill_manual(name = "", values = c("level 3" = "grey50"))
  if (sim) {
    p <- p  +
      geom_errorbar(
        data = aggregate(rimp ~ attribute + attribute.NUM, data = rimp.sim,
                         FUN = function(x) quantile(x, c(0.025, 0.975))),
        mapping = aes(
          x = attribute.NUM - 0.2 * show.level.2 + 0.2 * show.level.1,
          y = NULL, ymin = rimp[, 1], ymax = rimp[, 2], col = "simulated"),
        width = 0.2)
  }
  if (show.level.2) {
    p <- p  +
      geom_errorbar(
        data = aggregate(rimp ~ attribute + attribute.NUM, data = plotdata.level.2,
                         FUN = function(x) quantile(x, c(0.025, 0.975))),
        mapping = aes(
          x = attribute.NUM + 0.2 * (show.level.1 | sim),
          y = NULL, ymin = rimp[, 1], ymax = rimp[, 2], col = "level 2"),
        width = 0.2)  +
      geom_point(
        data = aggregate(rimp ~ attribute + attribute.NUM, data = plotdata.level.2, FUN = mean),
        mapping = aes(x = attribute.NUM + 0.2 * (show.level.1 | sim), col = "level 2"))
  }
  if (show.level.1) {
    p <- p  +
      geom_errorbar(
        data = aggregate(rimp ~ attribute + attribute.NUM, data = plotdata.level.1,
                         FUN = function(x) quantile(x, c(0.025, 0.975))),
        mapping = aes(
          x = attribute.NUM - 0.2 * (show.level.2 | sim),
          y = NULL, ymin = rimp[, 1], ymax = rimp[, 2], col = "level 1"),
        width = 0.2)  +
      geom_point(
        data = aggregate(rimp ~ attribute + attribute.NUM, data = plotdata.level.1, FUN = mean),
        mapping = aes(x = attribute.NUM - 0.2 * (show.level.2 | sim), col = "level 1"))
  }
  if (show.level.1 | show.level.2 | sim) {
    p <- p +
      scale_color_manual(
        name = "",
        values = c(
          if (show.level.1) "level 1" = "red",
          if (show.level.2) "level 2" = "blue",
          if (sim) "simulated" = "black"),
        labels = c(
          if (show.level.1) paste0(object$var.level.1, " (level 1)\n(mean, 95%-interval)"),
          if (show.level.2) paste0(object$var.level.2, " (level 2)\n(mean, 95%-interval)"),
          if (sim) "sim. level 3\n(95%-interval)"))
  }
  if (plot) print(p)

  return(invisible(p))
}
