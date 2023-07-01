\name{seat_dist}
\alias{seat_dist}
\title{
Czech seat distribution mechanism
}
\description{
seat_dist() converts raw party votes to parliamentary seats pursuant to the mechanisms enshrined in Czech election law (Act N. 189/2021 Coll.), used in Czech parliamentary elections since 2021.
}
\usage{
seat_dist(parties,V,total,seats=200,threshold=0.05)
}

\arguments{
  \item{parties}{Column containing party names.}
  \item{V}{
  Columns containing party vote totals for each electoral district (regions for the Czech Republic).}
  \item{total}{Column containing party vote totals for all electoral districts combined.}
  \item{seats}{Total number of seats contested in the election.}
  \item{threshold}{Minimum proportion of votes entitling a party to parliamentary representation.}
}
}

\value{
A data frame of party seat gains in electoral districts and in the election as a whole. Returns the "Total" column thereof.
}
\references{
Law N. 189/2021 Coll.
}
\author{
Vojtěch Vít Greger is a graduate of Political Science at the Faculty of Social Sciences of the Charles University in Prague.
}
\examples{
data(psp21)

# Standard usage
seat_dist(psp21$Parties,psp21[2:15],psp21$Total)

# Custom threshold
seat_dist(psp21$Parties,psp21[2:15],psp21$Total,threshold=0.01)

# Custom seats
seat_dist(psp21$Parties,psp21[2:15],psp21$Total,seats=100)

}


