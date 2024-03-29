% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/undampenTrend.R
\name{undampenTrend}
\alias{undampenTrend}
\alias{undampenTrend,mortalityTable.trendProjection-method}
\title{Return a \code{mortalityTable.trendProjection} object with the trend damping removed.}
\usage{
undampenTrend(object)

\S4method{undampenTrend}{mortalityTable.trendProjection}(object)
}
\arguments{
\item{object}{The life table object (class inherited from mortalityTable)}
}
\description{
Return a \code{mortalityTable.trendProjection} object with the trend damping removed.
}
\section{Methods (by class)}{
\itemize{
\item \code{undampenTrend(mortalityTable.trendProjection)}: Return a \code{mortalityTable.trendProjection}
object with the trend damping removed.

}}
\examples{
mortalityTables.load("Austria_Annuities")
AVOe2005R.male.undamped = undampenTrend(AVOe2005R.male)
AVOe2005R.male.undamped@name = paste(AVOe2005R.male.undamped@name, "no trend dampening")

plot(AVOe2005R.male, AVOe2005R.male.undamped,
    title = "AVOe 2005R with trend dampening and without", YOB = 2000)

}
