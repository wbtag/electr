\name{seat_dist_02}
\alias{seat_dist_02}

\title{
2002 seat distribution mechanism
}
\description{
\code{seat_dist_02()} converts raw party votes to parliamentary seats pursuant to the mechanisms formerly enshrined in Czech election law (Act N. 37/2002 Coll.). These mechanisms were used in Czech parliamentary elections between 2002 and 2021.
}
\usage{
seat_dist_02(parties, V, total, seats = 200, threshold = 0.05)
}
\arguments{
  \item{parties}{
Column containing party names.
}
  \item{V}{
Columns containing party vote totals for each electoral district (regions for the Czech Republic).
}
  \item{total}{
Column containing party vote totals for all electoral districts combined (national results).
}
  \item{seats}{
Total number of seats contested in the election.
}
  \item{threshold}{
Minimum proportion of votes entitling a party to parliamentary representation.
}
}
\value{
A data frame of party seat gains in electoral districts and in the election as a whole. Returns the "Total" column thereof.
}
\references{
Law N. 204/2000 Coll.

Law N. 37/2002 Coll.
}
\author{
Vojtěch Vít Greger is a graduate of Political Science at the Faculty of Social Sciences of the Charles University in Prague.
}
\examples{
data(psp17)

# Standard parameters
df <- seat_dist_02(psp17$Parties,psp17[2:15],psp17$Total)

# Custom threshold
df <- seat_dist_02(psp17$Parties,psp17[2:15],psp17$Total,threshold=0.01)

# Custom seats
df <- seat_dist_02(psp17$Parties,psp17[2:15],psp17$Total,seats=100)

df[1:4]

}
