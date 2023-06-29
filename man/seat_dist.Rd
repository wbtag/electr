\name{seat_dist}
\alias{seat_dist}
\title{
Convert party votes to seats
}
\description{
seat_dist() converts raw party votes to parliamentary seats pursuant to the mechanisms enshrined in the Czech election law no. 247/1995.
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
\details{

}
\value{
A data frame of party seat gains in electoral districts and in the election as a whole.
}
\references{

}
\author{
Vojtěch Vít Greger is a graduate of Political Science at the Faculty of Social Sciences of the Charles University in Prague.
}
