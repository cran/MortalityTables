% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilityFunctions.R
\name{mT.setTrend}
\alias{mT.setTrend}
\alias{mT.addTrend}
\title{Set/Add a trend vector for the probabilities of the given \code{mortalityTable} object(s). Returns a \code{mortalityTable.trendProjection} object}
\usage{
mT.setTrend(
  table,
  trend,
  trendages = NULL,
  baseYear = NULL,
  dampingFunction = identity
)

mT.addTrend(
  table,
  trend,
  trendages = NULL,
  baseYear = NULL,
  dampingFunction = identity
)
}
\arguments{
\item{table}{A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects}

\item{trend}{Trend vector to be applied to the mortality table}

\item{trendages}{Ages corresponding to the values of the \code{trend} vector}

\item{baseYear}{Base year for the trend projection (passed on to \code{mortalityTable.trendProjection})}

\item{dampingFunction}{Trend damping (passed on to \code{mortalityTable.trendProjection})}
}
\description{
Set/Add a trend vector for the probabilities of the given \code{mortalityTable} object(s). Returns a \code{mortalityTable.trendProjection} object
}
\section{Functions}{
\itemize{
\item \code{mT.addTrend()}: Add a trend to the mortality table (returns a mortalityTable.trendProjection obect)

}}
