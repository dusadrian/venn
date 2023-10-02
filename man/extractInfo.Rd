\name{extractInfo}

\alias{extractInfo}

\title{Extract information about a list object.}

\description{
Extracts useful information such as the counts or the actual intersections
between several sets.
}

\usage{
extractInfo(
  x, what = c("counts", "intersections", "both"), use.names = FALSE
)
}

\arguments{
  \item{x}{A list object containing set elements}
  \item{what}{What to extract}
  \item{use.names}{Logical, use the set names to indicate intersections}
}


\value{
A dataframe, when extracting the counts or a list if extracting intersections.
}

\examples{

set.seed(12345)
x <- list(First = 1:20, Second = 10:30, Third = sample(25:50, 15))

extractInfo(x) # counts by default

extractInfo(x, what = "intersections")

extractInfo(x, what = "both")

}


\keyword{functions}
