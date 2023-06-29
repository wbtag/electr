\name{rmc}
\alias{rmc}
\title{
National mandate number calculation
}
\description{
This is an auxiliary function intended for use within the \code{\link{seat_dist}} function. The function calculates the national mandate number (republikové mandátové číslo - RMČ) based on the total number of votes. The national mandate number acts as a quota for determining the apportionment of seats to individual regions.
}
\usage{
rmc(total, seats = 200)
}

\arguments{
  \item{total}{
Column containing party vote totals.
}
  \item{seats}{
Total number of seats contested, by default 200 (all seats in the Czech Chamber of Deputies).
}
}
\references{}
\note{}
\seealso{}

