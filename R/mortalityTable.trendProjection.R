#' @include mortalityTable.R mortalityTable.period.R
NULL

#' Class mortalityTable.trendProjection - Cohort mortality table with age-specific trend
#'
#' A cohort mortality table, obtained by a trend projection from a given base table
#' (PODs for a given observation year). Typically, the trend is obtained by
#' the Lee-Carter method or some other trend estimation.
#' The dampingFunction can be used to modify the cumulative years (e.g. G(tau+x)
#' instead of tau+x)
#' If trend2 is given, the G(tau+x) gives the weight of the first trend,
#' 1-G(tau+x) the weight of the second trend
#'
#' @slot baseYear The base year of the trend projection (\code{baseTable}
#'                describes the death probabilities in this year)
#' @slot trend    The yearly improvements of the log-death probabilities (per age)
#' @slot dampingFunction A possible damping of the trend. This is a function
#'                       \code{damping(delta_years)} that gets a vector of years
#'                       from the baseYear and should return the dampened values.
#' @slot trend2   The alternate trend. If given, the damping function
#'                interpolates between \code{trend} and \code{trend2}, otherwise
#'                the dumping function simply modifies the coefficients of
#'                \code{trend}.
#'
#' @examples
#' obsTable = mortalityTable.trendProjection(
#'     name = "Const. table with trend",
#'     baseYear = 2018,
#'     ages = 0:15,
#'     deathProbs = rep(0.02, 16),
#'     trend = c(
#'          0.045, 0.04, 0.03, 0.04, 0.042, 0.041, 0.038, 0.035,
#'          0.032, 0.031, 0.028, 0.020, 0.015, 0.01, 0.005, 0))
#' # In 2018 the flat mortality can be seen
#' plotMortalityTables(obsTable, Period = 2018, title = "Period death probabilities 2018")
#' # In 2038, the age-specific trend affected the probabilities differently for 20 years:
#' plotMortalityTables(obsTable, Period = 2038, title = "Period death probabilities 2038")
#' # Consequently, a person born 2018 will also not have constand death probabilities
#' plotMortalityTables(obsTable, YOB = 2018, title = "Cohort death probabilities, YOB 2018")
#'
#' plotMortalityTables(
#'     lapply(2018:2033, function(y) getCohortTable(obsTable, YOB = y)),
#'     title = "Cohort tables for different YOBs", legend.position = c(0.99, 0.01))
#' plotMortalityTables(
#'    lapply(2018:2033, function(y) getPeriodTable(obsTable, Period = y)),
#'    title = "Period tables for different years", legend.position = c(0.99, 0.01))
#'
#'
#' @export mortalityTable.trendProjection
#' @exportClass mortalityTable.trendProjection
mortalityTable.trendProjection = setClass(
    "mortalityTable.trendProjection",
    slots = list(
        baseYear = "numeric",
        trend = "numeric",
        dampingFunction = "function",
        trend2 = "numeric"
    ),
    prototype = list(
        baseYear = 1980,
        trend = rep(0,120),
        dampingFunction = identity,
        trend2 = 0
    ),
    contains = "mortalityTable.period"
)
