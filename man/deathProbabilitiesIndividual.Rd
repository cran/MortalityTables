% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortalityTable.jointLives.R
\name{deathProbabilitiesIndividual}
\alias{deathProbabilitiesIndividual}
\title{Return a matrix of the persons' individual death probabilities of a joint-life
table (instance of \code{\link{mortalityTable.jointLives}})}
\usage{
deathProbabilitiesIndividual(tables, YOB, ageDifferences)
}
\arguments{
\item{tables}{List of life table objects (object inherited from
\code{\link{mortalityTable}})}

\item{YOB}{The birth year for the first person}

\item{ageDifferences}{The age differences to the first person}
}
\description{
Return a matrix of the persons' individual death probabilities of a joint-life
table (instance of \code{\link{mortalityTable.jointLives}})
}
\examples{
mortalityTables.load("Germany_Census")
deathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, 0))
deathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, -5, 13))

}
