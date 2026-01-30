#' Sample data for conjoint analysis
#'
#' Sample data where respondents rated different tea variations
#'
#' @format
#' \describe{
#'   \item{respondent}{Vector of respondent (100 respondents)}
#'   \item{profile}{Tea profile (1-13)}
#'   \item{rating}{Rating of the tea (1-10)}
#'   \item{price}{Price of the tea (3 categories)}
#'   \item{variety}{Variety of the tea (3 categories)}
#'   \item{kind}{In which form is the tea available (3 categories)}
#'   \item{aroma}{Does the tea have added aroma (yes or no)}
#' }
#'
#'
#' @source Data collected in the survey conducted by M. Baran in 2007.
#' @examples
#' data(tea) ## load the data
#' head(tea)
#' tea.m <- rbcam(
#'   formula = rating ~ price + variety + kind + aroma,
#'   data = tea,
#'   var.level.1 = "respondent",
#'   contrasts.factor = "contr.sum"
#' )
#' tea.m
#' @name tea
#' @docType data
#' @keywords datasets
"tea"


