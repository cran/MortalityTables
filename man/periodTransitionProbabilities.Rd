% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pensionTable.R
\name{periodTransitionProbabilities}
\alias{periodTransitionProbabilities}
\alias{periodTransitionProbabilities,pensionTable-method}
\title{Return all period transition probabilities of the pension table}
\usage{
periodTransitionProbabilities(object, ...)

\S4method{periodTransitionProbabilities}{pensionTable}(
  object,
  Period = 2017,
  ...,
  ages = NULL,
  OverallMortality = FALSE,
  retirement = NULL,
  invalids.retire = object@invalids.retire,
  as.data.frame = TRUE
)
}
\arguments{
\item{object}{A pension table object (instance of a \code{\linkS4class{pensionTable}} class)}

\item{...}{Currently unused}

\item{Period}{Observation year}

\item{ages}{Desired age range (if NULL, the probabilities of the age range provided by the table will be returned), missing ages will be filled with NA}

\item{OverallMortality}{Whether the overall mortality should be returned for actives, or the active mortality}

\item{retirement}{Override the retirement transition probabilities of the pension table. Possible values are:\itemize{
\item Single age (describing a deterministric retirement at the given age)
\item mortalityTable object: transition probabilities for retirement
}}

\item{invalids.retire}{Override the \code{\linkS4class{pensionTable}}'s
\code{invalids.retire} flag, which indicates whether
invalids retire like actives (i.e. same death
probabilities after retirement) or stay invalid until
death.}

\item{as.data.frame}{Whether the return value should be a data.frame or an array containing transition matrices}
}
\description{
Return all period transition probabilities of the pension table
}
\section{Methods (by class)}{
\itemize{
\item \code{periodTransitionProbabilities(pensionTable)}: Return all transition probabilities of the pension table for the period Period

}}
\examples{
pensionTables.load("USA_PensionPlans")
# transitionProbabilities internally calls periodTransitionProbabilities
# if a Period is given:
transitionProbabilities(RP2014.male, Period = 1955)
periodTransitionProbabilities(RP2014.male, Period = 1955)
periodTransitionProbabilities(RP2014.male, Period = 2025)

}
