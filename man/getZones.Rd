\name{getZones}

\alias{getZones}

\title{Calculate the union(s) of set intersections.}

\description{
This function uses a metacommand to calculate the shape of a specific
zone or a list of zones.
}

\usage{
getZones(area, snames, ellipse = FALSE)
}

\arguments{
  \item{area}{A chgaracter expression written in sum of products form.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{ellipse}{Logical, get the zones from the shape of an ellipse, where possible}
}

\details{
         
A SOP ("sum of products") is also known as a DNF ("disjunctive normal form"), or in other
words a "union of intersections", for example \bold{\code{A*D + B*c}}.

The same expression can be written in curly brackets notation:
\bold{\code{A{1}*D{1} + B{1}*C{0}}}.


The expression \bold{\code{B{1}*C{0}}} can also be written in a pseudo-language, as
"-10-" (assuming there are only four sets).


A "zone" is a union of set intersections. There are exactly \bold{\code{2^k}} intersections
in a Venn diagram, where \bold{\code{k}} is the number of sets. To highlight an entire set,
we need a union of all possible intersections which form that set.


The argument \bold{\code{ellipse}} retrieves the data from the shape of an ellipse, and it only
works with 4 and 5 sets.

}


\value{
A list of self-enclosed polygons, for each independent zone.
}

\examples{

venn(3)

area <- getZones("A", snames = "A, B, C")
# a list of length 1

polygon(area[[1]], col="lightblue")


# The very same result is obtained with:
zone <- getZones("1--")



# for 5 sets, the content of the 5th set but not in the first set is a
# list of two zones

venn(5)

zones <- getZones("0---1")
# this time a list of length 2

# (re)coloring the first zone (union)
polygon(zones[[1]], col="lightblue")

# and the second zone (union)
polygon(zones[[2]], col="lightblue")

}


\keyword{functions}
