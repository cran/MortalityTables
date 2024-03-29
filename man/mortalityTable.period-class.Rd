% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortalityTable.period.R
\docType{class}
\name{mortalityTable.period-class}
\alias{mortalityTable.period-class}
\alias{mortalityTable.period}
\title{Class mortalityTable.period - Period life tables}
\description{
A period life table, giving death probabilities for each age, up to
maximum age \code{omega}. The \code{baseYear} slot can be used to hold
information about the period.
}
\section{Slots}{

\describe{
\item{\code{ages}}{The ages corresponding to the entries of the deathProbs}

\item{\code{deathProbs}}{The one-year death probabilities for the ages}

\item{\code{exposures}}{(Optional) exposured used to determine death probabilities
(can be used as weights for smoothing, for variances, etc.)}
}}

\examples{
linTable = mortalityTable.period(name="linear mortality", ages = 0:50, deathProbs = 0:50/50)
constTable = mortalityTable.period(name="const. mortality", ages = 0:50,
                                   deathProbs = c(rep(0.1, 50), 1))
plot(linTable, constTable, title="Comparison of linear and constand death probabilities")

# A sample observation table with exposures and raw probabilities
obsTable = mortalityTable.period(
    name = "trivial observed table",
    ages = 0:15,
    deathProbs = c(
        0.0072, 0.00212, 0.00081, 0.0005, 0.0013,
        0.001, 0.00122, 0.00142, 0.007, 0.0043,
        0.0058, 0.0067, 0.0082, 0.0091, 0.0075, 0.01),
    exposures = c(
        150, 222, 350, 362, 542,
        682, 1022, 1053, 1103, 1037,
        968, 736, 822, 701, 653, 438))
plot(obsTable, title = "Observed death probabilities")

}
