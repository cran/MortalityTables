#' @include mortalityTable.R
NULL

#' Return the period life table as a \code{mortalityTable.period} object
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param Period The observation year, for which the death probabilities should
#'        be determined. If missing, the base year of the table is used.
#' @param ... Other parameters (currently unused)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' tb17 = getPeriodTable(AVOe2005R.male, Period = 2017)
#' # The tb17 is a fixed table with no trend any more
#' plot(AVOe2005R.male, tb17, YOB = 1975)
#'
#' @exportMethod getPeriodTable
setGeneric("getPeriodTable",
           function(object, Period, ...)
               standardGeneric("getPeriodTable")
);

#' @describeIn getPeriodTable Return the period life table as a
#'             \code{mortalityTable.period} object
setMethod("getPeriodTable","mortalityTable",
          function (object, Period, ...) {
              if(missing(Period)) {
                  Period = baseYear(object)
              }
              mortalityTable.period(
                  name = paste0(object@name, ", Period ", Period),
                  baseYear = Period,
                  ages = ages(object),
                  deathProbs = periodDeathProbabilities(object, Period = Period, ...)
              )
          })
