% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortalityTable.MakehamGompertz.R
\docType{class}
\name{mortalityTable.MakehamGompertz-class}
\alias{mortalityTable.MakehamGompertz-class}
\alias{mortalityTable.MakehamGompertz}
\title{Class mortalityTable.MakehamGompertz - Mortality table with Makeham-Gompertz's law}
\description{
A period life table following Makeham and Gompertz's law of a mortality rate
\eqn{\mu} increasing exponentially with age \eqn{x} (\eqn{\mu_{x+t} = A + B c^{(x+t)}}).
The only required slots are the parameters \eqn{A}, \eqn{B} and \eqn{c}, all probabilities
are calculated from them, for technical reasons a maximum age of 120 is
technically assumed.  Optionally, a name and loading can be passed
(inherited from \code{\link{mortalityTable}}).
}
\section{Slots}{

\describe{
\item{\code{A}}{Parameter A of the Makeham-Gompertz distribution}

\item{\code{B}}{Parameter B of the Makeham-Gompertz distribution}

\item{\code{c}}{Parameter c of the Makeham-Gompertz distribution}

\item{\code{omega}}{Maximum age (default: 150)}
}}

\examples{
# A Gompertz mortality, roughtly approximating the Austrian annuitants 2017
gmp = mortalityTable.MakehamGompertz(A = 0, B = 0.00001, c = 1.11)
mortalityTables.load("Austria_Annuities_AVOe2005R")
plot(gmp, AVOe2005R.male, Period=2017)

# A Makeham-Gompertz mortality, approximating the Austrian annuitants 2017
mg = mortalityTable.MakehamGompertz(A = 0.0002, B = 0.00001, c = 1.11)
plot(mg, gmp, AVOe2005R.male, Period=2017)

}
