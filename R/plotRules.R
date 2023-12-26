# Copyright (c) 2016-2023, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
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

`plotRules` <- function (
    rules, zcolor = "bw", ellipse = FALSE, opacity = 0.3, allborders = TRUE,
    box = TRUE, gvenn = NULL, ...
) {
    sets <- getSets()
    zeroset <- matrix(c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0), ncol = 2)
    colnames(zeroset) <- c("x", "y")

    default <- identical(zcolor, "style")

    # assume the zones cover all sets (if rules is a number that is TRUE by default, anyways)
    allsets <- TRUE

    # create dummy global variables x and y to comply with R CMD check
    # when they are used for aes(x, y) in ggplots
    x <- NULL
    y <- NULL


    if (is.list(rules)) {

        if (identical(zcolor, "bw")) {
            zcolor <- rep("#96bc72", length.out = length(rules))
        }
        else if (identical(zcolor, "style")) {
            zcolor <- colorRampPalette(c("red", "blue", "green", "yellow"))(length(rules))
        }
        else {
            zcolor <- rep(zcolor, length.out = length(rules))
        }


        nofsets <- unique(unlist(lapply(rules, function(x) {
            nchar(unlist(strsplit(x, split = "\\+")))
        })))

        tt <- sapply(rev(seq(nofsets)), function(x) {
            rep(c(sapply(0:1, function(y) rep(y, 2^(x - 1)))), 2^nofsets/2^x)
        })

        rownames(tt) <- seq(nrow(tt)) - 1

        rowns <- lapply(rules, function(x) {
            sort(unique(unlist(lapply(strsplit(x, split = "\\+"), function(x) {
                unlist(lapply(strsplit(x, split = ""), function(x) {

                    ttc <- tt
                    for (j in seq(length(x))) {
                        if (x[j] != "-") {
                            ttc <- subset(ttc, ttc[, j] == x[j])
                        }
                    }

                    return(as.numeric(rownames(ttc)))
                }))

            }))))
        })


        # check if any of the remaining rows define a whole set

        # wholesets will be a numeric vector:
        # 0 if it's not a whole set
        # the number of the set (if it is whole), from the order in the truth table

        wholesets <- unlist(lapply(rules, function(x) {
            ifelse(nchar(gsub("-", "", x)) == 1, as.vector(regexpr("[0-9]", x)), 0)
        }))

        allwhole <- all(wholesets > 0)

        # verify if the rules cover all sets
        allsets <- length(rules) == nofsets & allwhole


        if (nofsets < 4 | nofsets > 5) {
            ellipse <- FALSE
        }

        zones <- vector("list", length(wholesets))

        irregular <- unlist(lapply(rowns, function(x) any(x == 0)))

        if (any(irregular)) { # inverse, the area outside a shape (or outside all shapes)
            for (i in which(irregular)) {
                zones[[i]] <- getZones(rowns[[i]], nofsets, ellipse)
                polygons <- rbind(zeroset, rep(NA, 2), zones[[i]][[1]])
                polygons <- polygons[-nrow(polygons), ] # needed...?

                if (is.null(gvenn)) {
                    polypath(polygons, rule = "evenodd", col = adjustcolor(zcolor[i], alpha.f = opacity), border = NA)
                } else {
                    gvenn <- gvenn + ggpolypath::geom_polypath(polygons, rule = "evenodd", col = adjustcolor(zcolor[i], alpha.f = opacity))
                }
            }
        }


        if (any(!irregular)) { # normal shapes
            if (any(wholesets > 0)) {
                for (i in which(wholesets > 0)) {
                    # [[1]] simulates getZones() because sometimes there might be multiple zones
                    zones[[i]][[1]] <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == wholesets[i], c("x", "y")]
                }
            }

            if (any(wholesets == 0)) {
                for (i in which(wholesets == 0 & !irregular)) {
                    zones[[i]] <- getZones(rowns[[i]], nofsets, ellipse)
                }
            }


            for (i in seq(length(zones))) {
                if (!irregular[i]) {
                    for (j in seq(length(zones[[i]]))) {
                        if (is.null(gvenn)) {
                            polygon(zones[[i]][[j]], col = adjustcolor(zcolor[i], alpha.f = opacity), border = NA)
                        } else {
                            gvenn <- gvenn + ggplot2::geom_polygon(data = zones[[i]][[j]], ggplot2::aes(x, y), fill = adjustcolor(zcolor[i], alpha.f = opacity))
                        }
                    }
                }
            }
        }
    }
    else if (is.numeric(rules)) {
        nofsets <- rules
        allsets <- TRUE
        allwhole <- TRUE

        if (identical(zcolor, "style")) {
            zcolor <- colorRampPalette(c("red", "yellow", "green", "blue"))(nofsets)
        }
        else if (!identical(zcolor, "bw")) {
            zcolor <- rep(zcolor, length.out = nofsets)
        }

        if (nofsets < 4 | nofsets > 5) {
            ellipse <- FALSE
        }
    }
    else {
        admisc::stopError("Something went wrong.")
    }


    other.args <- list(...)

    if (box) {
        if (is.null(gvenn)) {
            lines(zeroset)
        }
        else {
            gvenn <- gvenn + ggplot2::geom_path(data = as.data.frame(zeroset), ggplot2::aes(x, y))
        }
    }

    if (!identical(zcolor, "bw")) {
        # border colors, a bit darker
        bcolor <- rgb(t(col2rgb(zcolor)/1.4), maxColorValue = 255)
    }
    else {
        bcolor <- "#000000"
    }


    if (allsets & allwhole) {
        temp <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]
        if (is.numeric(rules) & !identical(zcolor, "bw")) {
            # the zones have not been plotted yet
            if (is.null(gvenn)) {
                polygon(temp, col = adjustcolor(zcolor, alpha.f = opacity), border = NA)
            }
            else {
                breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                start <- 1
                for (b in seq(length(breaks))) {
                    if (b > 1) start <- breaks[b - 1] + 1
                    gvenn <- gvenn + ggplot2::geom_polygon(data = temp[seq(start, breaks[b] - 1), ], ggplot2::aes(x, y), fill = adjustcolor(zcolor[b], alpha.f = opacity))
                }
            }
        }

        # now the borders

        if (default) {

            # the default set of colors ignores all other additional parameters for the borders

            for (i in seq(nofsets)) {
                temp <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == i, c("x", "y")]
                if (is.null(gvenn)) {
                    suppressWarnings( lines(temp, col = bcolor[i]))
                }
                else {
                    breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                    start <- 1
                    for (b in seq(length(breaks))) {
                        if (b > 1) start <- breaks[b - 1] + 1
                        gvenn <- gvenn + ggplot2::geom_path(ggplot2::aes(x, y), data = temp[seq(start, breaks[b] - 1), ], col = bcolor[i])
                    }
                }
            }
        }
        else {

            if (length(other.args) > 0) {

                # there might be different border colors for each zone (set in this case)
                # arguments are recycled to the length of the zones
                other.args <- lapply(other.args, function(x) {
                    rep(x, length.out = nofsets)
                })

                for (i in seq(nofsets)) {
                    plotdata <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == i, c("x", "y")]

                    if (is.null(gvenn)) {
                        seplines <- list(as.name("lines"), x = plotdata)
                        suppress <- list(as.name("suppressWarnings"))

                        for (j in names(other.args)) {
                            seplines[[j]] <- other.args[[j]][i]
                        }

                        suppress[[2]] <- as.call(seplines)

                        eval(as.call(suppress))
                    }
                    else {
                        seplines <- list(ggplot2::geom_path)
                        if (all(is.na(tail(plotdata, 1)))) {
                            # to remove the annoying warning "removed 1 row containing missing data"
                            plotdata <- plotdata[-nrow(plotdata), , drop = FALSE]
                        }
                        seplines$mapping <- ggplot2::aes(x, y)
                        seplines$data <- plotdata
                        for (j in names(other.args)) {
                            seplines[[j]] <- other.args[[j]][i]
                        }

                        gvenn <- gvenn + eval(as.call(seplines))
                    }

                }
            }
            else {
                # print borders in black
                temp <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]
                if (is.null(gvenn)) {
                    suppressWarnings(lines(temp))
                }
                else {
                    breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                    start <- 1
                    for (b in seq(length(breaks))) {
                        if (b > 1) start <- breaks[b - 1] + 1
                        gvenn <- gvenn + ggplot2::geom_path(ggplot2::aes(x, y), data = temp[seq(start, breaks[b] - 1), ])
                    }
                }
            }
        }
    }
    else {

        # first print all borders in black
        # (important to begin with this, the zones might not cover all intersections)
        if (allborders) {
            temp <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]
            if (is.null(gvenn)) {
                suppressWarnings(lines(temp))
            }
            else {
                breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                start <- 1
                for (b in seq(length(breaks))) {
                    if (b > 1) start <- breaks[b - 1] + 1
                    gvenn <- gvenn + ggplot2::geom_path(ggplot2::aes(x, y), data = temp[seq(start, breaks[b] - 1), ])
                }
            }
        }
        else {
            if (!is.element("col", names(other.args))) {
                other.args$col <- "black"
            }
        }

        # surely this is not numeric, there are zones already calculated


        if (default) {

            for (i in seq(length(zones))) {
                for (j in seq(length(zones[[i]]))) {
                    if (is.null(gvenn)) {
                        suppressWarnings(lines(zones[[i]][[j]], col = bcolor[i]))
                    }
                    else {
                        temp <- zones[[i]][[j]]
                        breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                        start <- 1
                        for (b in seq(length(breaks))) {
                            if (b > 1) start <- breaks[b - 1] + 1
                            gvenn <- gvenn + ggplot2::geom_path(ggplot2::aes(x, y), data = temp[seq(start, breaks[b] - 1), ], col = bcolor[i])
                        }
                    }
                }
            }

        }
        else {

            if (length(other.args) > 0) {

                # arguments are recycled to the length of the zones
                other.args <- lapply(other.args, function(x) {
                    rep(x, length.out = length(rules))
                })

                for (i in seq(length(zones))) {

                    for (j in seq(length(zones[[i]]))) {
                        if (is.null(gvenn)) {
                            seplines <- list(as.name("lines"), x = zones[[i]][[j]])
                            suppress <- list(as.name("suppressWarnings"))

                            if (any(names(other.args) == "col")) {
                                other.args$col <- admisc::splitstr(other.args$col)
                            }

                            for (j in names(other.args)) {
                                seplines[[j]] <- other.args[[j]][i]
                            }

                            suppress[[2]] <- as.call(seplines)

                            eval(as.call(suppress))
                        }
                        else {
                            temp <- zones[[i]][[j]]
                            breaks <- which(apply(temp, 1, function(x) any(is.na(x))))
                            start <- 1
                            for (b in seq(length(breaks))) {

                                if (b > 1) start <- breaks[b - 1] + 1

                                seplines <- list(ggplot2::geom_path)
                                seplines[["data"]] <- temp[seq(start, breaks[b] - 1), ]
                                seplines[["mapping"]] <- ggplot2::aes(x, y)

                                if (any(names(other.args) == "col")) {
                                    other.args$col <- admisc::splitstr(other.args$col)
                                }

                                for (j in names(other.args)) {
                                    seplines[[j]] <- other.args[[j]][i]
                                }

                                gvenn <- gvenn + eval(as.call(seplines))
                            }
                        }
                    }
                }
            }
        }
    }

    if (!is.null(gvenn)) {
        return(gvenn)
    }
}
