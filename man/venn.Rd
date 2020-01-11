\name{venn}

\alias{venn}

\title{Draw and display a Venn diagram}

\description{
This function uses a variety of input data to draw and display a Venn diagram with
up to 7 sets.
}

\usage{
venn(x, snames = "", counts, ilabels = FALSE, ellipse = FALSE,
     zcolor = "bw", opacity = 0.3, plotsize = 15, ilcs = 0.6, sncs = 0.85,
     borders = TRUE, box = TRUE, par = TRUE, ggplot = FALSE, ...)
}

\arguments{
  \item{x}{A single number (of sets), or a metacommand formula (see details),
           or a list containing set values, or a dataset containing boolean values.}
  \item{snames}{An optional parameter containing the names for each set.}
  \item{ilabels}{Logical: print the labels for each intersection.}
  \item{counts}{A numerical vector of counts for each set intersection.}
  \item{ellipse}{Logical, force the shape to an ellipse, where possible}
  \item{zcolor}{A vector of colors for the custom zones, or predefined colors
        if "style"}
  \item{opacity}{Degree of opacity for the color(s) specified with
        \code{zcolor} (less opacity, more transparency).}
  \item{plotsize}{Plot size, in centimeters.}
  \item{ilcs}{Character expansion (in base plots) or size (in ggplots) 
        for the intersection labels}
  \item{sncs}{Character expansion (in base plots) or size (in ggplots)
        for the set names}
  \item{borders}{Logical: draw all intersection borders}
  \item{box}{Logical: draw the outside square}
  \item{par}{Logical: use the default, custom par settings}
  \item{ggplot}{Logical: plot the Venn diagram using ggplot}
  \item{...}{Additional parameters, mainly for the outer borders of the sets}
}

\details{
    
The argument \bold{\code{x}} can be either:\cr
- a single number (of sets), between 1 and 7\cr
- a metacommand (character) to draw custom intersection zones\cr
- a list, containing values for the different sets: each component is a set,
  and only up to 7 components are processed.\cr
- a dataset of boolean values.\cr
         
A "zone" is a union of set intersections. There are exactly \bold{\code{2^k}} intersections
in a Venn diagram, where \bold{\code{k}} is the number of sets. To highlight an entire set,
we need a union of all possible intersections which form that set.

For example, in a 3 sets diagram, the (overall) first set is composed by four
intersections:\cr
\bold{\code{100}} for what is in the first set but outside sets 2 and outside set 3\cr
\bold{\code{101}} for the intersection between sets 1 and 3, outside set 2\cr
\bold{\code{110}} for the intersection between sets 1 and 2, outside set 3\cr
\bold{\code{111}} for the intersection between all three sets.

A meta-language can be used to define these intersections, using the values of
\bold{\code{1}} for what is inside the set, \bold{\code{0}} for what is outside the set, and
\bold{\code{-}} when its either inside or outside of the set.

The command \bold{\code{"1--"}} is translated as "display only the first, entire set" is
equivalent with the union of the four intersections \bold{\code{"100 + 101 + 110 + 111"}}.

The parameter \bold{\code{setnames}} should have the same length as the number of sets
specified by the parameter \bold{\code{x}}.

When the parameter \bold{\code{x}} is used as a metacommand, the number of sets is calculated
as the number of characters in each intersection of the metacommand. One such
character command is \bold{\code{"100 + 101 + 110 + 111"}} or \bold{\code{"1--"}}, and all
intersections have exactly three characters.

It is also possible to use a regular, disjunctive normal form, like \bold{\code{"A"}},
which is equivalent with \bold{\code{"Abc + AbC + ABc + ABC"}}. When \bold{\code{x}}
is an expression written in DNF, if a valid R statement then quoting is not even necessary.

The argument \bold{\code{snames}} establishes names for the different sets, or in its absence
it is taken from \bold{\code{LETTERS}}. When \bold{\code{x}} is a list or a dataframe,
\bold{\code{snames}} is taken from their names. The length of the \bold{\code{snames}}
indicates the total number of sets.

A numerical vector can be supplied with the argument \bold{\code{counts}}, when the argument
\bold{\code{x}} is a single number of sets. The counts should match the increasing order of
the binary representation for the set intersections. When the argument \bold{\code{x}} is a
list, the counts are taken from the number of common values for each intersection, and when
\bold{\code{x}} is a data frame, (comprised of exclusively boolean values 0 and 1) the counts
are taken from the number of similar rows. If a particular intersection does not have any
common values (or no rows), the count "0" is left blank and not displayed in the diagram.

The argument \bold{\code{ellipse}} differentiates between two types of diagrams for 4 and 5 sets.
The idea is to allow for as much space as possible for each intersection (also as equal
as possible) and that is impossible if preserving the shape of an ellipse. The default is
to create large space for the intersections, but users who prefer an ellipse might want
to set this argument to \bold{\code{TRUE}}.

Colors to fill the desired zones (or entire sets) can be supplied via the argument
\bold{\code{zcolor}} (the default is \bold{\code{"bw"}} black and white, which means no colors at all).
Users can either chose the predefined color style, using \bold{\code{zcolor = "style"}}, or supply
a vector of custom colors for each zone. If only one custom color is supplied, it will
be recycled for all zones.

When using \bold{\code{zcolor = "style"}}, any other additional arguments for the borders are
ignored.

A different set of predefined colors is used, when argument \bold{\code{x}} is a QCA type object
(a truth table, either from a class \bold{\code{tt}} or from a class \bold{\code{qca}}). If custom colors
are provided via \bold{\code{zcolor}}, it should have a length of 3 colors: the first for the 
absence of the outcome (\bold{\code{0}}), the second for the presence of the outcome (\bold{\code{1}}),
and the third for the contradictions (\bold{\code{C}}). Remainders have no color, by default.

The argument \bold{\code{ilcs}} works only if the intersection labels (\bold{\code{ilabels}}) or
intersection \bold{\code{counts}} are activated, and it sets the size of the labels via a
\bold{\code{cex}} argument. In the absence of a specific value from the user, it's default is
set to 0.6 for all Venn diagrams with up to five sets, and it automatically decreases to 0.5
for six sets and 0.45 for seven sets.

Via \bold{\code{...}}, users can specify additional parameters, mainly for the outer borders
of the sets, as specified by \bold{\code{\link[graphics]{par}()}}, and since version 1.9 it is
also used to pass additional aesthetics parameters for the ggplot2 graphics. All of them are
feeded either to the base function \bold{\code{\link[graphics]{lines}()}} which is responsible
with the borders, or to the function \bold{\code{\link[ggplot2]{geom_path}()}} from package
\pkg{ggplot2}.

For up to 3 sets, the shapes can be circular. For more than 3 sets, the shape cannot be
circular: for 4 and 5 sets they can be ellipses, while for more than 5 sets the
shapes cannot be continous (they might be monotone, but not continous). The 7 sets
diagram is called "Adelaide" (Ruskey, 2005).

The most challenging diagram is the one with 6 sets, where for many years it was
thought a Venn diagram didn't even exist. All diagrams are symetric, except for the
one with 6 sets, where some of the sets have different shapes. The diagram in this
package is an adaptation from Mamakani, K., Myrvold W. and F. Ruskey (2011).

The argument \bold{\code{border}} can be used only for custom intersections and/or unions,
it has no effect when \bold{\code{x}} is a list, or a data frame, or a truth table object.

The argument \bold{\code{par}} is used to define a custom set of parameters when producing
the plot, to ensure a square shape of about 15 cm and eliminate the outer regions. If
deactivated, users can define their own size and shape of the plot using the system function
\bold{\code{\link[graphics]{par}()}}. By default, the plot is always produced using a size of
1000 points for both horizontal and vertical, unless the argument \bold{\code{ggplot}} is
activated, when the argument \bold{\code{par}} will have no effect.
}


\references{

Ruskey, F. and M. Weston. 2005. \emph{Venn diagrams}. Electronic Journal of Combinatorics,
Dynamic Survey DS5.

Mamakani, K., Myrvold W. and F. Ruskey. 2011. \emph{Generating all Simple Convexly-drawable
Polar Symmetric 6-Venn Diagrams}. International Workshop on Combinatorial Algorithms, Victoria.
LNCS, 7056, 275-286.

}


\examples{

# A simple Venn diagram with 3 sets
venn(3)

# with a vector of counts: 1 for "000", 2 for "001" etc.
venn(3, counts = 1:8)

# display the first whole set
venn("1--")

# same with
venn("A", snames = "A, B, C")

# an equivalent command, from the union of all intersections
venn("100 + 110 + 101 + 111")

# same with
venn("A~B~C + AB~C + A~BC + ABC")

# adding the labels for the intersections
venn("1--", ilabels = TRUE)

# using different parameters for the borders
venn(4, lty = 5, col = "navyblue")

# using ellipses
venn(4, lty = 5, col = "navyblue", ellipse = TRUE)

# a 5 sets Venn diagram
venn(5)

# a 5 sets Venn diagram using ellipses
venn(5, ellipse = TRUE)

# a 5 sets Venn diagram with intersection labels
venn(5, ilabels = TRUE)

# and a predefined color style
venn(5, ilabels = TRUE, zcolor = "style")

# a union of two sets
venn("1---- + ----1")

# same with
venn("A + E", snames = "A, B, C, D, E")

# with different colors
venn("1---- , ----1", zcolor = "red, blue")

# same with
venn("A, E", snames = "A, B, C, D, E", zcolor = "red, blue")

# same colors for the borders
venn("1---- , ----1", zcolor = "red, blue", col = "red, blue")

# 6 sets diagram
venn(6)

# 7 sets "Adelaide"
venn(7)


# artistic version
venn(c("1000000", "0100000", "0010000", "0001000",
       "0000100", "0000010", "0000001", "1111111"))

# without all borders
venn(c("1000000", "0100000", "0010000", "0001000",
       "0000100", "0000010", "0000001", "1111111"),
     borders = FALSE)


# using sum of products notation
venn("A + B~C", snames = "A, B, C, D")


# when x is a list
set.seed(12345)
x <- list(First = 1:20, Second = 10:30, Third = sample(25:50, 15))
venn(x)


# when x is a dataframe
set.seed(12345)
x <- as.data.frame(matrix(sample(0:1, 150, replace = TRUE), ncol = 5))
venn(x)


# producing a ggplot2 graphics
venn(x, ggplot = TRUE)

# increasing the border size
venn(x, ggplot = TRUE, size = 1.5)

# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed")


\dontrun{
# produce Venn diagrams for QCA objects
library(QCA)

data(CVF)
obj <- truthTable(CVF, "PROTEST", incl.cut = 0.85)

venn(obj)

# to set opacity based on inclusion scores
# (less inclusion, more transparent)

venn(obj, opacity = obj$tt$incl)

# custom labels for intersections

pCVF <- minimize(obj, include = "?")
venn(pCVF$solution[[1]], zcol = "#ffdd77, #bb2020, #1188cc")
cases <- paste(c("HungariansRom", "CatholicsNIreland", "AlbaniansFYROM",
                 "RussiansEstonia"), collapse = "\n")
coords <- unlist(getCentroid(getZones(pCVF$solution[[1]][2])))
text(coords[1], coords[2], labels = cases, cex = 0.85)
}

}


\keyword{functions}
