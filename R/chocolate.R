#' Sample data for conjoint analysis
#'
#' Sample data where respondents rated different chocolate variations
#'
#' @format
#' \describe{
#'   \item{respondent}{Vector of respondent (87 respondents)}
#'   \item{profile}{Chocolate profile (1-16)}
#'   \item{rating}{Rating of the chocolate (1-16)}
#'   \item{kind}{Which kind of chocolate it is (4 categories)}
#'   \item{price}{Price of the chocolate (3 categories)}
#'   \item{packaging}{Packaging of the chocolate (paperback, hardback)}
#'   \item{weight}{Weight of the chocolate (3 categories)}
#'   \item{aroma}{How much calorie does the chocolate have (little, much)}
#' }
#'
#'
#' @source Data collected in the survey conducted by W. Nowak in 2000.
#' @examples
#' data(chocolate) ## load the data
#' head(chocolate)
#'
#' choc.m <- rbcam(
#'   formula = rating ~ kind + price + packing + weight + calorie,
#'   data = chocolate,
#'   var.level.1 = "respondent",
#'   contrasts.factor = "contr.sum"
#' )
#' choc.m
#' @name chocolate
#' @docType data
#' @keywords datasets
"chocolate"
