# Copyright (c) 2016-2023, Adrian Dusa
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of enclosed data must cite this package according to
#       the citation("venn") command specific to this R package, along with the
#       appropriate weblink to the CRAN package "venn".
#     * Redistributions of enclosed data in other R packages must list package
#       "venn" as a hard dependency in the Imports: field.
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`extractInfo` <- function(
    x, what = c("counts", "intersections", "both"), use.names = FALSE
) {

    what <- match.arg(what)

    if (!is.list(x)) {
        admisc::stopError("Argument x should be a data frame or a list")
    }

    if (is.data.frame(x)) {
        nofsets <- ncol(x)
        if (nofsets > 7) {
            x <- x[, 1:7]
            nofsets <- 7
        }
        what <- "counts"
        if (!identical(names(table(unlist(x))), c("0", "1"))) {
            admisc::stopError("Values in x should only be 0s and 1s")
        }
    }
    else {
        nofsets <- length(x)
        if (nofsets > 7) {
            x <- x[seq(7)]
            nofsets <- 7
        }
    }


    if (any(names(x) == "")) {
        names(x) <- LETTERS[seq(nofsets)]
    }

    snames <- names(x)

    tt <- sapply(
        rev(seq(nofsets)),
        function(x) {
            rep.int(
                c(sapply(0:1, function(y) rep.int(y, 2^(x - 1)))),
                2^nofsets / 2^x
            )
        }
    )


    colnames(tt) <- snames

    if (!isTRUE(use.names)) {
        snames <- seq(length(snames))
    }

    rownames(tt) <- apply(
        tt,
        1,
        function(x) paste(snames[x == 1], collapse = ":")
    )

    if (is.data.frame(x)) {
        powers <- 2^seq(nofsets - 1, 0)
        tbl <- table(apply(x, 1, function(r) return(sum(r * powers) + 1)))
        ttcts <- rep(0, 2^nofsets)
        ttcts[as.numeric(names(tbl))] <- as.vector(tbl)
    }
    else {
        intersections <- apply(tt, 1,
            function(y) {
                setdiff(Reduce(intersect, x[y == 1]), unlist(x[y == 0]))
            }
        )

        names(intersections) <- rownames(tt)

        ttcts <- unlist(lapply(intersections, length))

        intersections <- intersections[ttcts > 0]
    }


    tt <- as.data.frame(cbind(tt, counts = ttcts))

    if (what == "counts") {
        return(tt)
    }

    if (what == "intersections") {
        return(intersections)
    }

    return(list(tt, intersections))
}
