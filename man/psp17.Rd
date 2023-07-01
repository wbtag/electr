\name{psp17}
\alias{psp17}
\docType{data}
\title{
Czech 2017 parliamentary election results
}
\description{
A dataset containing vote totals of all 31 parties, movements, and coalitions which contested the 2017 parliamentary election in the Czech Republic.
}
\usage{data("psp21")}
\format{
  A data frame with 31 observations on the following 17 variables.
  \describe{
    \item{\code{Parties}}{Names of parties contesting the election.}
    \item{\samp{... Region}}{Party vote gains in each electoral region. (14 columns)}
    \item{\code{Total}}{National vote totals for all parties.}
    \item{\code{Threshold}}{Threshold for entering parliament for all parties.}
  }
}
\source{
Data: Czech Statistical Bureau, 2017
}

\examples{
data(psp17)
psp17$Parties
}
\keyword{datasets}
