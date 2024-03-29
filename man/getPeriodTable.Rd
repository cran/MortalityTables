% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPeriodTable.R
\name{getPeriodTable}
\alias{getPeriodTable}
\alias{getPeriodTable,mortalityTable-method}
\title{Return the period life table as a \code{mortalityTable.period} object}
\usage{
getPeriodTable(object, Period, ...)

\S4method{getPeriodTable}{mortalityTable}(object, Period, ...)
}
\arguments{
\item{object}{The life table object (class inherited from mortalityTable)}

\item{Period}{The observation year, for which the death probabilities should
be determined. If missing, the base year of the table is used.}

\item{...}{Other parameters (currently unused)}
}
\description{
Return the period life table as a \code{mortalityTable.period} object
}
\section{Methods (by class)}{
\itemize{
\item \code{getPeriodTable(mortalityTable)}: Return the period life table as a
\code{mortalityTable.period} object

}}
\examples{
mortalityTables.load("Austria_Annuities")
tb17 = getPeriodTable(AVOe2005R.male, Period = 2017)
# The tb17 is a fixed table with no trend any more
plot(AVOe2005R.male, tb17, YOB = 1975)

}
