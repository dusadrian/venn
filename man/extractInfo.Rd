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
  \item{x}{A list object containing set elements, or a list of binary values}
  \item{what}{What to extract}
  \item{use.names}{Logical, use the set names to indicate intersections}
}

\details{
When the argument \bold{\code{x}} is a list, the counts are taken from the number
of common values for each intersection, and when \bold{\code{x}} is a dataframe,
(comprised of exclusively binary values 0 and 1) the counts are taken from the
number of similar rows.
}


\value{
A dataframe, when extracting the counts or a list if extracting intersections.
}

\examples{

set.seed(12345)
xlist <- list(A = 1:20, B = 10:30, C = sample(25:50, 15))
xdf <- as.data.frame(matrix(
  sample(0:1, 90, replace = TRUE),
  ncol = 3
))
colnames(xdf) <- LETTERS[1:3]

extractInfo(xlist) # counts by default

extractInfo(xlist, what = "intersections")

extractInfo(xlist, what = "both")

extractInfo(xdf)

}


\keyword{functions}
