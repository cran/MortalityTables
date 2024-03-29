#' @include mortalityTable.period.R mortalityTable.mixed.R mortalityTable.joined.R
NULL

#' Return the defined ages of the life table
#'
#' @param object A life table object (instance of a \code{\linkS4class{mortalityTable}} class)
#' @param ... Currently unused
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' ages(AVOe2005R.male)
#' ages(AVOe1996R.male)
#'
#' mortalityTables.load("Austria_Census")
#' ages(mort.AT.census.2011.male)
#'
#' @exportMethod ages
setGeneric("ages", function(object, ...) standardGeneric("ages"));


#' @describeIn ages Return the defined ages of the period life table
setMethod("ages", "mortalityTable.period",
          function(object, ...) {
              object@ages;
          })

#' @describeIn ages Return the defined ages of the mixed life table
setMethod("ages", "mortalityTable.mixed",
          function(object, ...) {
              ages(object@table1);
          })

# #' @describeIn ages Return the defined ages of the joined life table
# setMethod("ages", "mortalityTable.joined",
#           function (object, ...) {
#               ages(object@table1);
#           })

