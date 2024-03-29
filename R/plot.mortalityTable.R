#' Plot multiple mortality tables (life tables) in one plot
#'
#' \code{plot.mortalityTable} displays multiple life tables (objects of child
#' classes of \code{mortalityTable}) in one plot, with a legend showing the
#' names of the tables. If the argument \code{reference} not given, all
#' mortality rates are plotted on a log-linear scale for comparison. If the
#' argument \code{reference} is given and is a valid life table, then all
#' death probabilities are scaled by the given reference table and the y-axis
#' shows the death rates as percentage of the reference table.
#'
#' @param x First life table to be plotted. Must be a \code{mortalityTable}
#'          object for the dispatcher to call this function
#' @param ... Additional life tables to be plotted (\code{mortalityTable}
#'            objects) as well as any of the following parameters (which are
#'            passed on to \code{\link{plotMortalityTables}} or
#'            \code{\link{plotMortalityTableComparisons}}):
#'                \describe{
#'                  \item{\code{xlim,ylim}}{Axes limitatation (as a
#'                          two-element vectors)}
#'                  \item{\code{xlab,ylab}}{Axes labels (default for
#'                          x-axis: "Alter", default for y-axis:
#'                          "Sterbewahrscheinlichkeit q_x")}
#'                  \item{\code{title}}{The plot title}
#'                  \item{\code{legend.position}}{The position of
#'                          the legend (default is \code{c(0.9,0.1)})}
#'                  \item{\code{legend.key.width}}{The keywith of the
#'                          lines in the  legend (default is
#'                          \code{unit(25,"mm")})}
#'                }
#' @param reference The reference table that determines the 100\% values.
#'                  If not given, the absolute mortality values are
#'                  compared and plotted on a log-linear scale.
#' @param trend If set to \code{TRUE}, the function \code{\link{plotMortalityTrend}}
#'              is used to plot the trends of the given tables.
#'
#'
#' @examples
#' # Load the Austrian census data
#' mortalityTables.load("Austria_Census")
#'
#' # Plot some select census tables in a log-linear plot
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title="Austrian census tables",
#'      ylab=expression(q[x]), xlab="Age",
#'      xlim=c(0,90),
#'      legend.position=c(0.95,0.05))
#'
#' # Compare some census tables with the mortality of 2011 Austrian males
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title="Austrian Census tables, relative to 2011 males",
#'      reference=mort.AT.census.2011.male)
#'
#' @seealso \code{\link{plotMortalityTables}} and \code{\link{plotMortalityTableComparisons}}
#'
#' @import scales
#' @export
plot.mortalityTable = function(x, ..., reference = NULL, trend = FALSE) {
    if (!missing(trend) && isTRUE(trend)) {
        if (!missing(reference) && !is.null(reference)) {
            plotMortalityTrend(x, ..., reference = reference)
        } else {
            plotMortalityTrend(x, ...)
        }
    } else if (!missing(reference) && !is.null(reference)) {
        plotMortalityTableComparisons(x, ..., reference = reference)
    } else {
        plotMortalityTables(x, ...)
    }
}

