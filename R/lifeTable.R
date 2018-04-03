#' @include mortalityTable.R
NULL

#' Return the lifetable object (package lifecontingencies) for the cohort life table
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Parameters to be passed to the \code{deathProbabilities} method
#'            of the life table
#'
#' @examples
#' library("lifecontingencies")
#' mortalityTables.load("Austria_Annuities")
#' lifeTable(AVOe2005R.male, YOB = 2017)
#' axn(lifeTable(AVOe2005R.male, YOB = 1975), x = 65, i = 0.03)
#' axn(lifeTable(AVOe2005R.male, YOB = 2017), x = 65, i = 0.03)
#'
#' @exportMethod lifeTable
setGeneric("lifeTable", function(object, ...) standardGeneric("lifeTable"));

#' @describeIn lifeTable Return the lifetable object (package lifecontingencies)
#'             for the cohort life table
setMethod("lifeTable","mortalityTable",
          function(object,  ...) {
              qx = deathProbabilities(object, ...);
              if (qx[[length(qx)]] != 1) {
                  qx = c(qx, 1, 1);
              }
              lifecontingencies::probs2lifetable(qx, type = "qx")
          })