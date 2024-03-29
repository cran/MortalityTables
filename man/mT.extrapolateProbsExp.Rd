% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilityFunctions.R
\name{mT.extrapolateProbsExp}
\alias{mT.extrapolateProbsExp}
\title{Extrapolate base table of a mortalityTable using an exponential function}
\usage{
mT.extrapolateProbsExp(table, age, up = TRUE)
}
\arguments{
\item{table}{A life table object (instance of a \code{mortalityTable} class)
or a list, table or array of mortalityTable objects}

\item{age}{Index (typically age) of the position of the fit}

\item{up}{Whether the fit is forward- or backward-facing (i.e. to old or young ages)}
}
\description{
Extrapolate the base table of a \code{mortalityTable} object using an exponential
function (i.e. the death probabilities decreases towards 0 exponentially).
While death probabilities trending towards 0 for old ages is not realistic for
overall deaths, it can be useful to model causes of death that vanish in older age.
It is, however, most useful to extrapolate an observed base table to low ages
(e.g. for an insurance portfolio with practically no persons aged below 16). A
decline towards 0 for low ages makes sense in this case.
}
\details{
The function needs only one age, from which the extrapolation using an exponential
function is applied. the strength of the exponential function is derived from the death probability at that age.
}
\examples{
mortalityTables.load("Austria_Census")
# use the Austrian population mortalities for ages 18-95 and exponentially
# extrapolate them to lower ages
AT11.lowAgesExp = mT.extrapolateProbsExp(mort.AT.census.2011.male, 18, up = FALSE)
plotMortalityTables(mT.setName(AT11.lowAgesExp, "Ages below 16 are extrapolated exponentially"),
                    mort.AT.census.2011.male)
}
