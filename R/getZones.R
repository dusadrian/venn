# Copyright (c) 2016-2024, Adrian Dusa
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

`getZones` <- function(area, snames = "", ellipse = FALSE) {
    # borders <- getBorders()
    # ib <- getIntBord()

    funargs <- unlist(lapply(match.call(), deparse)[-1])

    if (is.character(area)) {
        x <- gsub("[[:space:]]", "", area)

        if (!all(gsub("0|1|-", "", x) == "")) {

            if (any(grepl("\\$solution", funargs["area"]))) {
                obj <- get(unlist(strsplit(funargs["area"], split = "[$]"))[1])
                snames <- obj$tt$options$conditions
                x <- paste(x, collapse = " + ")
            }

            x <- gsub("[[:space:]]", "", x)

            if (!all(gsub("0|1|-|\\+", "", x) == "")) {

                x <- admisc::translate(x, snames = snames)

                snames <- colnames(x)

                x <- paste(apply(x, 1, function(y) {
                    y[y < 0] <- "-"
                    paste(y, collapse="")
                }), collapse = "+")
            }

            # then check again
            if (!all(gsub("0|1|-|\\+", "", x) == "")) {
                cat("\n")
                stop("Invalid specification of the area.\n\n", call. = FALSE)
            }

            area <- unlist(strsplit(x, split="\\+"))
        }

        nofsets <- unique(nchar(area))

        if (length(nofsets) > 1) {
            cat("\n")
            stop("Different numbers of sets in the area.\n\n", call. = FALSE)
        }

        if (!identical(unique(gsub("1|0|-", "", area)), "")) {
            cat("\n")
            stop("The arguent \"area\" should only contain \"1\"s, \"0\"s and dashes \"-\".\n\n", call. = FALSE)
        }

        area <- sort(unique(unlist(lapply(strsplit(area, split = ""), function(x) {
            dashes <- x == "-"

            if (any(dashes)) {
                sumdash <- sum(dashes)
                tt <- sapply(rev(seq(sumdash)), function(x) {
                        rep.int(c(sapply(0:1, function(y) rep.int(y, 2^(x - 1)))), 2^sumdash/2^x)})

                for (i in as.numeric(x[!dashes])) {
                    tt <- cbind(tt, i)
                }

                mbase <- rev(c(1, cumprod(rev(rep(2, ncol(tt))))))[-1]
                tt <- tt[, match(seq(ncol(tt)), c(which(dashes), which(!dashes)))]
                return(as.vector(tt %*% mbase))

            }
            else {
                x <- as.numeric(x)
                mbase <- rev(c(1, cumprod(rev(rep(2, length(x))))))[-1]
                return(sum(x * mbase))
            }
        }))))
    }
    else {
        nofsets <- snames
    }

    area <- area + 1


    if (nofsets < 4 | nofsets > 5) {
        ellipse <- FALSE
    }

    if (identical(area, 1)) {
        area <- seq(2^nofsets)[-1]
    }

    if (length(area) > 1) {

        checkz <- logical(length(area))
        names(checkz) <- area
        checkz[1] <- TRUE

        result <- list()

        while(!all(checkz)) {
            checkz <- checkZone(as.numeric(names(checkz)[1]), area, checkz, nofsets, ib, ellipse)

            result[[length(result) + 1]] <- as.numeric(names(checkz)[checkz])
            area  <-  area[!checkz]
            checkz <- checkz[!checkz]

            if (length(checkz) > 0) {
                checkz[1] <- TRUE
            }
        }
    }
    else {
        result = list(area)
    }


    result <- lapply(result, function(x) {

        b <- ib$b[ib$s == nofsets & ib$v == as.numeric(ellipse) & is.element(ib$i, x)]

        if (any(duplicated(b))) {
            b <- setdiff(b, b[duplicated(b)])
            # b <- unique(b)
        }

        # print(ib[ib$s == nofsets & ib$v == as.numeric(ellipse) & ib$b %in% b, ])

        v2 <- borders[borders$s == nofsets & borders$v == as.numeric(ellipse) & borders$b == b[1], c("x", "y")]
        v2 <- v2[-nrow(v2), ] # get rid of the NAs, we want a complete polygon
        ends <- as.numeric(v2[nrow(v2), ])

        checkb <- logical(length(b))
        names(checkb) <- b
        checkb[1] <- TRUE

        counter <- 0

        while(!all(checkb)) {

            # do.call("rbind", lapply(... ???

            for (i in which(!checkb)) {

                temp <- borders[borders$s == nofsets & borders$v == as.numeric(ellipse) & borders$b == b[i], c("x", "y")]

                flag <- FALSE
                if (all(ends == as.numeric(temp[1, ]))) {
                    v2 <- rbind(v2, temp[-nrow(temp), ])
                    checkb[i] <- TRUE
                }
                else if (all(ends == as.numeric(temp[nrow(temp) - 1, ]))) {
                    temp <- temp[-nrow(temp), ]
                    v2 <- rbind(v2, temp[seq(nrow(temp), 1), ])
                    checkb[i] <- TRUE
                }

                if (checkb[i]) {
                    ends <- as.vector(v2[nrow(v2), ])
                }
            }

            counter <- counter + 1

            if (counter > length(checkb)) {
                # print(checkb)
                cat("\n")
                stop("Unknown error.\n\n", call. = FALSE)
            }
        }


        return(rbind(v2, rep(NA, 2)))
    })

    return(result)
}
