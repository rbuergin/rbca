#' Likelihood Ratio Test for 'rbcam' Model Objects
#'
#' Performs a likelihood ratio test (LRT) to compare two level-3 models of class `'rbcam'`,
#' as produced by the \code{\link{rbcam}} function. The function tests whether a more complex
#' model provides a significantly better fit to the data than a simpler nested model.
#'
#'
#' @param object1 An object of class `rbcam` as returned by \code{rbcam}.
#' @param object2 An object of class `rbcam` as returned by \code{rbcam}.
#'
#' @return A printed summary comparing the two models, including:
#' \itemize{
#'   \item Number of degrees of freedom in each model
#'   \item Log-likelihood values
#'   \item Difference in degrees of freedom
#'   \item Chi-squared statistic and p-value for the likelihood ratio test
#' }
#'
#' @details
#' The likelihood ratio test (LRT) is used to compare nested models fitted to the same dataset.
#' The models must differ only by the inclusion of additional terms (e.g., predictors or interactions).
#' The test statistic follows a chi-squared distribution with degrees of freedom equal to the
#' difference in the number of estimated parameters.
#'
#' The function determines which model has more parameters and calculates the likelihood ratio
#' statistic accordingly. It then evaluates the statistical significance of the difference using
#' the chi-squared distribution.
#'
#' @examples
#' data(tea)
#'
#' # Fit base model
#' tea.m1 <- rbcam(
#'   formula = rating ~ price + variety,
#'   data = tea,
#'   var.level.1 = "respondent"
#' )
#'
#' # Fit extended model
#' tea.m2 <- rbcam(
#'   formula = rating ~ price + variety + aroma,
#'   data = tea,
#'   var.level.1 = "respondent"
#' )
#'
#' # Compare models using likelihood ratio test
#' rbcam.lrtest(object1 = tea.m1, object2 = tea.m2)
#'
#'
#' @export


rbcam.lrtest <- function(object1, object2) {
  ## checks
  if (!inherits(object1, "rbcam")) stop("'object1' not of class 'rbcam'")
  if (!inherits(object2, "rbcam")) stop("'object2' not of class 'rbcam'")

  m1 = object1$mlist.level.3
  m2 = object2$mlist.level.3

  # Extract formulas
  formula1 <- deparse(formula(m1))
  formula2 <- deparse(formula(m2))

  # Extract log-likelihoods and degrees of freedom
  loglik1 <- logLik(m1)
  loglik2 <- logLik(m2)

  df1 <- attr(loglik1, "df")
  df2 <- attr(loglik2, "df")

  ll1 <- as.numeric(loglik1)
  ll2 <- as.numeric(loglik2)

  # Calculate test statistic
  df_diff <- df2 - df1
  chisq_stat <- 2 * ifelse(df1 < df2, ll2- ll1 , ll1 - ll2)
  p_val <- pchisq(chisq_stat, abs(df_diff), lower.tail = FALSE)

  # Significance stars
  signif_stars <- symnum(p_val, corr = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                         symbols = c("***", "**", "*", ".", " "))

  # Create output table
  out <- data.frame(
    Df = c(df1, df2),
    LogLik = round(c(ll1, ll2),1),
    Df_diff = c("", df_diff),
    Chisq = c("", round(chisq_stat, 3)),
    Pr = c("", signif(p_val, digits = 4)),
    Signif = c("", as.character(signif_stars))
  )
  colnames(out) <- c("#Df", "LogLik", "Df", "Chisq", "Pr(>Chisq)","")
  rownames(out) <- c("1", "2")

  # Print result
  cat("Likelihood ratio test\n\n")
  cat("Model 1:", formula1, "\n")
  cat("Model 2:", formula2, "\n")
  print(out, quote = FALSE, right = TRUE)
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}
