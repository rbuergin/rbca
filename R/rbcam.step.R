#' Stepwise Model Selection for 'rbcam' Objects Using AIC
#'
#' Performs stepwise model selection (forward, backward, or both) based on Akaike Information Criterion (AIC)
#' for level 3 objects of class `'rbcam'` (\code{\link{rbcam}}). The selection process iteratively adds or removes predictors to
#' find an optimal model that minimizes AIC. Additionally the selection can also be made with
#' the Expected Mean Squared Error of Prediction (EMSEP) criterion from \code{\link{rbcam.emsep}}
#'
#' @param object An object of class `'rbcam'` as returned by \code{rbcam}.
#' @param scope A formula specifying the upper scope of the model. This defines which terms are allowed to be added
#'   during forward selection. Required when using `"forward"` or `"both"` directions. Ignored for `"backward"`.
#' @param direction Character string specifying the mode of stepwise search. Options are: "both", "backward", or "forward",
#' with a default of "both". If the scope argument is missing the default for direction is "backward".
#' @param criterion Character string specifying the model selection criterion, either `"aic"` (default) or `"emsep"`.
#' `"aic"` follows the standard behavior of the \code{\link{step}} function, while `"emsep"` applies the
#' \code{\link{rbcam.emsep}} method for model comparison.
#' @param trace Logical. If `TRUE` (default), stepwise progress and information is printed
#' @param steps Integer. The maximum number of iterations allowed. Defaults to `1000`. It is typically used to stop
#' the process early.
#'
#' @details
#' The function automates model selection by evaluating models that differ from the current model by the
#' addition or removal of a single predictor (or interaction). At each step, the function compares the
#' AIC of all candidate models and updates the current model if an improvement is found.
#'
#' For categorical predictors, contrasts are preserved or updated during the selection process. In the case of
#' interaction terms, multiple underlying variables may be impacted, and care is taken to manage contrasts accordingly.
#'
#' The function uses `update()` internally
#'
#' Note that for EMSEP criterion only the direction backward and forward are available.
#' Additionally, the backward step of EMSEP stops after the second last attribute was removed from the
#' formula, since the function \code{\link{rbcam}} does not support empty formulas of the form: response ~ 1.
#'
#'
#' @return An updated object of class `'rbcam'` corresponding to the model with the lowest AIC found during the stepwise search.
#' The returned object has the same structure as the original but reflects the new model specification.
#
#'
#' @examples
#' data(tea)
#'
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea)
#'
#' tea.m0 <- rbcam(
#'   formula = rating ~ kind,
#'   data = tea)
#'
#' # Stepwise selection using both directions
#' rbcam.step(object = tea.m, scope = formula(rating ~ price + variety + kind + aroma + kind:aroma))
#'
#' # Backward direction
#' rbcam.step(object = tea.m, direction = "backward")
#'
#' # Forward direction with scope
#' rbcam.step(object = tea.m0, scope = formula(tea.m$mlist.level.3), direction = "forward")
#'
#' # Backward direction with criterion emsep
#' rbcam.step(object = tea.m, direction = "backward", criterion = "emsep")
#'
#' # Forward direction with scope
#' rbcam.step(object = tea.m0, scope = formula(rating ~ price + variety + kind + aroma),
#'   direction = "forward", criterion = "emsep")
#'
#'
#' @export

rbcam.step <- function(object, scope = NULL, direction = c("both", "backward", "forward"), criterion = "aic",
                     trace = TRUE, steps = 1000) {

  if (!inherits(object, "rbcam")) stop("'object' not of class 'rbcam'")
  if (!is.null(scope) && !inherits(scope, "formula")) stop("'formula' must be an object of class 'formula'.")

  direction <- ifelse(length(direction) > 1, "both", direction)

  if (!is.null(scope) && direction == "forward" &&
      !all(attr(terms(object$mlist.level.3), "term.labels") %in% attr(terms(scope), "term.labels")))
    stop("Some attributes in the model are not included in the scope. The object.ref from emsep needs to compare models which have comparable attributes.")
  if (direction == "both" && criterion == "emsep")
    stop("The direction 'both' is not yet implemented for the 'emsep' criterion, as object.list must consist of models whose terms are also included in object.ref")

  current_model <- object$mlist.level.3
  data <- object$data
  response <- all.vars(formula(current_model))[1]

  if (!is.null(scope) && response != all.vars(scope)[1])
    stop("predictor variable of scope not same as in model")

  if (!is.null(scope)) {
    full_scope_terms <- attr(terms(scope), "term.labels")
    # to have a valid object.ref the "biggest" model out of the scope is taken as reference
    emsep_ref <- rbcam(formula = as.formula(scope), data = data)
  } else {
    full_scope_terms <- attr(terms(object$mlist.level.3), "term.labels")
  }

  con <- object$contrasts
  best_aic <- AIC(current_model) - 2 + 2 * (-nrow(data)/2 * (log(2*pi) + 1))
  improving <- TRUE
  step <- 0

  while (improving && step < steps) {
    step <- step + 1
    improving <- FALSE
    list_models <- list("<none>" = if (criterion == "emsep")
      rbcam(formula = formula(current_model), data = data)
      else
        update(current_model, as.formula(current_model), data = data, contrasts = con))



    if (direction %in% c("backward", "both")) {
      terms_out <- attr(terms(current_model), "term.labels")
      for (term in terms_out) {
        fmla <- as.formula(paste(response, "~ . -", term))
        vars_in_term <- all.vars(as.formula(paste("~", term))) # for interaction terms
        con <- con[!names(con) %in% vars_in_term]
        m <- update(current_model, fmla, data = data, contrasts = con)
        key <- paste("-", term)
        list_models[[key]] <- if (criterion == "emsep") rbcam(formula = formula(m), data = data) else m
      }
      emsep_ref <- list_models[[1]]
    }

    if (direction %in% c("forward", "both")) {
      terms_in <- setdiff(full_scope_terms, attr(terms(current_model), "term.labels"))
      for (term in terms_in) {
        fmla <- paste(response, "~", as.character(formula(current_model)[-c(1,2)]), "+", term)
        vars_in_term <- all.vars(as.formula(paste("~", term))) # for interaction terms
        con <- con[names(con) %in% vars_in_term]
        m <- update(current_model, fmla, data = data, contrasts = con)
        key <- paste("+", term)
        list_models[[key]] <- if (criterion == "emsep") rbcam(formula = formula(m), data = data) else m
      }
    }

    if (criterion == "emsep") {
      stats <- data.frame(rbcam.emsep(object.list = list_models, object.ref = emsep_ref))
      stats$r2.adj <- round(stats$r2.adj, 4)
      stats$emsep <- round(stats$emsep, 4)
      stats <- stats[order(stats$emsep), ]
      best_candidate <- rownames(stats)[1]
      best_emsep <- stats$emsep[1]

      if (trace) {
        cat(ifelse(step == 1, "\nStart:   EMSEP =", "\nStep:   EMSEP ="), format(best_emsep, digits = 6), "\n")
        print(formula(current_model)); cat("\n")
        print(format(stats, digits = 5, justify = "right"), quote = FALSE, right = TRUE)
      }

      if (best_candidate != "<none>") {
        # rbcam cannot handle a model without any term. Therefore for criterion emsep and direction backward it stops
        # before the second last attribute is removed, as rbcam(response ~ 1, data = data) does not work.
        improving <- ifelse(direction == "backward" && length(attr(terms(current_model), "term.labels")) < 3, FALSE, TRUE)
        fmla <- paste(response, "~", paste(list_models[[best_candidate]]$attributes, collapse = " + "))
        current_model <- update(current_model, fmla, data = data, contrasts = con)
      }

    } else {
      aic_values <- sapply(list_models, function(x) AIC(x) - 2 + 2 * (-nrow(data)/2 * (log(2*pi) + 1)))
      rss_values <- sapply(list_models, deviance)
      df_values <- sapply(list_models, df.residual)
      base_df <- df_values[["<none>"]]; base_rss <- rss_values[["<none>"]]

      stats <- data.frame(
        Df = c("", abs(df_values[-1] - base_df)),
        `Sum of Sq` = c("", abs(round(rss_values[-1] - base_rss, 2))),
        RSS = rss_values, AIC = aic_values
      )
      row.names(stats) <- names(aic_values)
      stats <- stats[order(stats$AIC, stats$RSS), ]
      best_candidate <- rownames(stats)[1]

      if (trace) {
        cat(ifelse(step == 1, "\nStart:   AIC =", "\nStep:   AIC ="), format(best_aic, digits = 6), "\n")
        print(formula(current_model)); cat("\n")
        print(format(stats, digits = 5, justify = "right"), quote = FALSE, right = TRUE)
      }

      if (best_candidate != "<none>") {
        current_model <- list_models[[best_candidate]]
        best_aic <- stats[best_candidate, "AIC"]
        improving <- TRUE
      }
    }
  }
  return(current_model)
}
