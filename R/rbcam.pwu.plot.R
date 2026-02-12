globalVariables(c("l2group", "resp", "respondent"))
#' Plot Partworth Utilities from Conjoint Models
#'
#' Visualizes partworth utility functions for additive terms in models fitted with \code{\link{rbcam}}.
#' Can optionally show individual-level or group-level utility curves alongside the pooled average.
#'
#' @param object A fitted 'rbcam' object (list of models created by \code{rbcam}).
#' @param term Character string (length 1). The name of an additive term (e.g., attribute) to be plotted.
#' @param normalized Logical. If \code{TRUE}, utilities are normalized (divided by their max absolute value or range).
#' @param plot Logical. If \code{TRUE}, the plot is displayed. Otherwise, a \code{ggplot2} object is returned invisibly.
#' @param show.level.1 Logical. Whether individual-level (level 1) partworths should be shown.
#' @param show.level.2 Logical. Whether group-level (level 2) partworths should be shown.
#' @param label.level.2 Logical. Whether group-level (level 2) units should be distinguished by different line types. Ignored if \code{show.level.2 = FALSE}.
#' @param jitter Logical. If set to \code{TRUE}, individual-level (level 1) partworth estimates that overlap will be visually jittered to improve readability in the plot.
#' @param jitter.width Numeric. Controls the magnitude of the jitter applied to overlapping level 1 estimates; higher values result in greater dispersion (default is 0.1).
#'
#' @details
#' Requires \code{ggplot2}, \code{tidyr}.
#'
#' The plot shows:
#' \itemize{
#'   \item \strong{Level 3} (pooled model) partworth utility in red.
#'   \item \strong{Level 2} (group-level) lines in blue, optionally labeled by group.
#'   \item \strong{Level 1} (individual-level) lines in grey.
#' }
#' If \code{normalized = TRUE}, group/individual means are shown as bold lines and points.
#'
#' @return A \code{ggplot2} object (invisibly, unless \code{plot = TRUE}).
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent")
#'
#' # Plot the partworth utility function for "price"
#' rbcam.pwu.plot(object = tea.m, term = "price", normalized = TRUE, show.level.1 = TRUE)
#'
#' # Plot including group-level lines (requires var.level.2 in the model)
#'
#' # create a group
#' set.seed(123)
#' tea$group <- as.factor(rep(sample(1:3, size = length(unique(tea$respondent)),replace = TRUE),
#'                            each = 13))
#'
#' tea.group.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.2 = "group")
#'
#' rbcam.pwu.plot(object = tea.group.m, term = "kind", show.level.2 = TRUE, label.level.2 = TRUE)
#'
#' @export

rbcam.pwu.plot <- function(
    object, term, normalized = FALSE, plot = TRUE, show.level.1 = FALSE, show.level.2 = FALSE,
    label.level.2 = show.level.2, jitter = FALSE, jitter.width = 0.1) {

  ## checks
  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!(is.logical(show.level.1) & length(show.level.1) == 1)) stop("'show.level.1' must be a single logical")
  if (!(is.logical(show.level.2) & length(show.level.2) == 1)) stop("'show.level.2' must be a single logical")

  if ((!is.character(term)) && length(term) == 1)
    stop("'term' must be a character of length 1.")
  if (!show.level.2 && label.level.2) {
    label.level.2 <- FALSE
    warning("'label.level.2' set to FALSE")
  }
  if (!show.level.1 && jitter) {
    warning("'jitter' is ignored because 'show.level.1' is set to FALSE.")
  }

  ## partworth utility on level 3
  pwu.level.3 <- rbcam.pwu(object, level = 3, normalized = normalized)

  ## check if 'term' is available
  if (!(is.character(term) && term %in% names(pwu.level.3))) {
    stop(paste0(
      "'term' must be on of ",
      paste(paste0("'", names(pwu.level.3), "'"), collapse = ", ")))
  }

  ## compute partworth utilities on levels 1 and 2
  pwu.level.2 <- pwu.level.1 <- NULL
  if (show.level.2) pwu.level.2 <- rbcam.pwu(object, level = 2, normalized = normalized)
  if (show.level.1) pwu.level.1 <- rbcam.pwu(object, level = 1, normalized = normalized)


  ## data preparation for plotting
  plotdata.level.3 <- data.frame(
    value = factor(x = names(pwu.level.3[[term]]), levels = names(pwu.level.3[[term]])),
    value.NUM = if (object$attributes.class[term] == "factor")
      seq_along(pwu.level.3[[term]]) else as.numeric(names(pwu.level.3[[term]])),
    utility = pwu.level.3[[term]],
    group = "level.3")
  plotdata.level.1 <- plotdata.level.2 <- NULL
  if (show.level.2) {
    plotdata.level.2 <- as.data.frame(pwu.level.2[[term]], check.names = FALSE)
    plotdata.level.2$group <- factor(x = object$var.level.2.levels, levels = object$var.level.2.levels)
    plotdata.level.2 <- reshape(
      data = plotdata.level.2,
      varying = list(colnames(pwu.level.2[[term]])),
      v.names = "utility",
      idvar = "group",
      timevar = "value",
      times = colnames(pwu.level.2[[term]]),
      direction = "long")
    plotdata.level.2$value <- factor(x = plotdata.level.2$value, levels = colnames(pwu.level.2[[term]]))
    plotdata.level.2$value.NUM <- if (object$attributes.class[term] == "factor")
      as.integer(plotdata.level.2$value) else as.numeric(as.character(plotdata.level.2$value))
    levels(plotdata.level.2$group) <- paste0("level.2.", levels(plotdata.level.2$group))
    plotdata.level.2$group <- as.character(plotdata.level.2$group)
    plotdata.level.2$l2group <- as.character(rep(1:length(object$var.level.2.levels),ncol(pwu.level.2[[term]])))
  }
  if (show.level.1) {
    plotdata.level.1 <- as.data.frame(pwu.level.1[[term]], check.names = FALSE)
    plotdata.level.1$group <- factor(x = object$var.level.1.levels, levels = object$var.level.1.levels)
    plotdata.level.1 <- reshape(
      data = plotdata.level.1,
      varying = list(colnames(pwu.level.1[[term]])),
      v.names = "utility",
      idvar = "group",
      timevar = "value",
      times = colnames(pwu.level.1[[term]]),
      direction = "long")
    plotdata.level.1$value <- factor(x = plotdata.level.1$value, levels = colnames(pwu.level.1[[term]]))
    plotdata.level.1$value.NUM <- if (object$attributes.class[term] == "factor")
      as.integer(plotdata.level.1$value) else as.numeric(as.character(plotdata.level.1$value))
    levels(plotdata.level.1$group) <- paste0("level.1.", levels(plotdata.level.1$group))
    plotdata.level.1$group <- as.character(plotdata.level.1$group)

    if (show.level.2){ # get grouping information into the plotdata.level.1 to adjust the linetype
      plotdata.level.1$resp <- as.factor(rep(1:(nrow(plotdata.level.1)/ncol(pwu.level.1[[term]])), n = ncol(pwu.level.1[[term]])))

      resp_group <- unique(data.frame(
        respondent = object$data[[object$var.level.1]],
        group = object$data[[object$var.level.2]]
      )
      )

      plotdata.level.1 <- plotdata.level.1 %>%
        left_join(
          y = resp_group %>%
            dplyr::mutate(l2group = as.character(as.integer(as.factor(group)))) %>%
            dplyr::select(respondent, l2group),
          by = c("resp" = "respondent")) %>%
        dplyr::select(-resp)
    }

    if (jitter) {
      pivoted_data <- plotdata.level.1[order(plotdata.level.1$value.NUM),c(1:3)] %>%
        tidyr::pivot_wider(names_from = value, values_from = utility) %>%
        dplyr::arrange(group)

      dupl <- duplicated(pivoted_data[, -1]) | duplicated(pivoted_data[, -1], fromLast = TRUE)

      normal <- pivoted_data[!dupl,] %>%
        tidyr::pivot_longer(cols = !group, values_to = "utility", names_to = "value")
      jitt <- pivoted_data[dupl,] %>%
        tidyr::pivot_longer(cols = !group, values_to = "utility", names_to = "value")

      normal$value <- factor(normal$value, levels = unique(levels(plotdata.level.1$value[order(plotdata.level.1$value.NUM)])))
      jitt$value <- factor(jitt$value, levels = unique(levels(plotdata.level.1$value[order(plotdata.level.1$value.NUM)])))

      normal$value.NUM <- as.integer(as.factor(normal$value))
      jitt$value.NUM <- as.integer(as.factor(jitt$value))

      if (show.level.2){
        group_info <- plotdata.level.1 %>%
          dplyr::distinct(group, l2group)
        normal <- normal %>% dplyr::left_join(group_info, by = "group")
        jitt   <- jitt   %>% dplyr::left_join(group_info, by = "group")
      }

    }
  }


  ## plot results
  p <- ggplot2::ggplot(
    data = plotdata.level.3,
    mapping = aes(x = value.NUM, y = utility, groups = group)) +
    theme_minimal()
  if (show.level.1) {
    if (jitter){
      if (show.level.2){
        p <- p +
          ggplot2::geom_line(
            data = normal, mapping = aes(x = value.NUM, y = utility, group = group, col = "level 1", linetype = l2group)) +
          ggplot2::geom_line(
            data = jitt, mapping = aes(x = value.NUM, y = utility, group = group, col = "level 1", linetype = l2group),
            position=position_dodge(width=jitter.width)) + geom_hline(yintercept = 0, col = "black", lty = 2)

      } else {
        p <- p +
          ggplot2::geom_line(
            data = normal, mapping = aes(x = value.NUM, y = utility, group = group, col = "level 1"), lty = 1) +
          ggplot2::geom_line(
            data = jitt, mapping = aes(x = value.NUM, y = utility, group = group, col = "level 1"), lty = 1,
            position=position_dodge(width=jitter.width)) + geom_hline(yintercept = 0, col = "black", lty = 2)
      }
    } else {
      if (show.level.2){
        p <- p +
          ggplot2::geom_line(data = plotdata.level.1, mapping = aes(col = "level 1", linetype = l2group))
      } else
        p <- p +
          ggplot2::geom_line(data = plotdata.level.1, mapping = aes(col = "level 1"), lty = 1)
    }
    if (normalized) {
      plotdata.level.1.agg <- aggregate(utility ~ value + value.NUM, data = plotdata.level.1, FUN = mean)
      plotdata.level.1.agg$group <- "level 1"
      p <- p +
        ggplot2::geom_line(
          data = plotdata.level.1.agg,
          mapping = aes(col = "level 1 average"),
          lwd = 1) +
        ggplot2::geom_point(
          data = plotdata.level.1.agg,
          mapping = aes(col = "level 1 average"))
    }
  }
  if (show.level.2) {
    if (label.level.2)  {
      p <- p +
        ggplot2::geom_line(
          data = plotdata.level.2,
          mapping = aes(col = "level 2", linetype = l2group)) +
        ggplot2::scale_linetype(
          name = "cluster (level 2)",
          labels = object$var.level.2.levels)
    } else {
      p <- p +
        ggplot2::geom_line(
          data = plotdata.level.2,
          mapping = aes(col = "level 2"),
          lty = 1)
    }
    if (normalized) {
      plotdata.level.1.agg <- aggregate(utility ~ value + value.NUM, data = plotdata.level.2, FUN = mean)
      plotdata.level.1.agg$group <- "level 2"
      p <- p +
        ggplot2::geom_line(
          data = plotdata.level.1.agg,
          mapping = aes(col = "level 2 average"),
          lwd = 1) +
        ggplot2::geom_point(
          data = plotdata.level.1.agg,
          mapping = aes(col = "level 2 average"))
    }
  }
  p <- p +
    ggplot2::geom_line(mapping = aes(col = "level 3"), lwd = 1) +
    ggplot2::geom_point(mapping = aes(col = "level 3")) +
    ggplot2::geom_hline(yintercept = 0, col = "black", lty = 2) +
    ggplot2::scale_x_continuous(
      name = term,
      breaks = plotdata.level.3$value.NUM,
      labels = levels(plotdata.level.3$value)) +
    ggplot2::scale_y_continuous(
      name = ifelse(normalized, "normalized utility", "utility")) +
    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "level 1" = if (show.level.1) "grey" else NULL,
        "level 1 average" = if (show.level.1 & normalized) "black" else NULL,
        "level 2" = if (show.level.2) "blue" else NULL,
        "level 2 average" = if (show.level.2 & normalized) "blue" else NULL,
        "level 3" = "red"
      ),
      labels = c(
        if (show.level.1) paste0(object$var.level.1, " (level 1)") else NULL,
        if (show.level.1 & normalized) paste0("mean of ", object$var.level.1) else NULL,
        if (show.level.2) paste0(object$var.level.2, " (level 2)") else NULL,
        if (show.level.2 & normalized) paste0("mean of ", object$var.level.2) else NULL,
        "level 3"
      ))
  if (plot) print(p)
  return(invisible(p))
}
