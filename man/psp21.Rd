\name{psp21}
\alias{psp21}
\docType{data}
\title{
Czech 2021 parliamentary election results
}
\description{
A dataset containing vote totals of all 22 parties, movements, and coalitions which contested the 2021 parliamentary election in the Czech Republic.
}
\usage{data("psp21")}
\format{
  A data frame with 22 observations on the following 17 variables.
  \describe{
    \item{\code{Parties}}{Names of parties contesting the election.}
    \item{\samp{... Region}}{Party vote gains in each electoral region. (14 columns)}
    \item{\code{Total}}{National vote totals for all parties.}
    \item{\code{Threshold}}{Threshold for entering parliament for all parties.}
  }
}
\source{
Source: Czech Statistical Bureau, 2021
}
\examples{
data(psp21)
psp21$Parties
}
\keyword{datasets}
