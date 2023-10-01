`venn` <- function(
    x, snames = "", ilabels = NULL, ellipse = FALSE, zcolor = "bw",
    opacity = 0.3, plotsize = 15, ilcs = 0.6, sncs = 0.85, borders = TRUE,
    box = TRUE, par = TRUE, ggplot = FALSE, ...
) {

    if (missing(x)) {
        admisc::stopError("Argument <x> is missing.")
    }

    dots <- list(...)
    counts <- dots$counts
    cts <- NULL

    tjqca <- is.element("trajectory", names(dots))
    trajectory <- dots$trajectory
    tjcases <- names(dots$trajectory)

    dots$trajectory <- NULL

    if (!is.null(ilabels)) {
        if (identical(ilabels, "counts")) {
            counts <- TRUE
            ilabels <- NULL
        }
        else {
            if (isTRUE(ilabels)) {
                counts <- NULL
            }
            else {
                if (is.atomic(ilabels) && !is.logical(ilabels)) {
                    cts <- ilabels
                    counts <- NULL
                    ilabels <- NULL
                }
            }
        }
    }

    if (is.null(counts)) {
        counts <- FALSE
    }
    else {
        if (is.atomic(counts) && !is.logical(counts)) {
            cts <- counts
            counts <- TRUE
        }

        counts <- isTRUE(counts)
    }


    if (ggplot) {
        ilcs <- ilcs * 2.5 / 0.6
        sncs <- sncs * 3.5 / 0.85
        if (
            !requireNamespace("ggplot2", quietly = TRUE) |
            !requireNamespace("ggpolypath", quietly = TRUE)
        ) {
            admisc::stopError(
                paste(
                    "Packages \"ggplot2\" and \"ggpolypath\" are needed",
                    "to make this work, please install."
                )
            )
        }
    }

    # to see what's in the "..." argument
    funargs <- unlist(lapply(match.call(), deparse)[-1])

    # backwards compatibility
    if (!is.element("cexil", names(funargs))) {
        names(funargs)[which(names(funargs) == "cexil")] <- "ilcs"
    }

    if (!is.element("cexsn", names(funargs))) {
        names(funargs)[which(names(funargs) == "cexsn")] <- "sncs"
    }

    if (inherits(tryCatch(eval(x), error = function(e) e), "error")) {
        x <- funargs["x"]
    }

    if (is.numeric(x)) {
        if (length(x) > 1) {
            admisc::stopError(
                "Argument <x> can be a single digit, for up to 7 sets."
            )
        }
    }

    if (!identical(zcolor, "bw") & !identical(zcolor, "style")) {
        zcolor <- admisc::splitstr(zcolor)

        testcolor <- tryCatch(col2rgb(zcolor), error = function(e) e)

        if (!is.matrix(testcolor)) {
            admisc::stopError("Invalid color(s) in argument <zcolor>.")
        }
    }

    nofsets <- 0

    if (!identical(snames, "")) {
        if (!is.character(snames)) {
            admisc::stopError("The argument <snames> should be character.")
        }
        if (length(snames) == 1) snames <- admisc::splitstr(snames)
        nofsets <- length(snames)
    }

    ttqca <- FALSE
    listx <- FALSE

    if (any(is.element(c("qca", "QCA_min", "tt", "QCA_tt"), class(x)))) {
        # if (inherits(x, "qca") | inherits(x, "tt")) {

        ttqca <- TRUE
        otype <- "input"

        if (any(is.element(c("tt", "QCA_tt"), class(x)))) {
            QCA <- all(
                which(
                    is.element(
                        c("minmat", "DCC", "options", "neg.out", "opts"),
                        names(x)
                    )
                ) < 4
            )
            otype <- "truth table"
            tt <- x$tt
            snames <- unlist(
                strsplit(
                    gsub("[[:space:]]", "", x$options$conditions),
                    split = ","
                )
            )
            noflevels <- x$noflevels

            rnms <- rownames(x$initial.data)
            ttcases <- x$tt$cases
        }
        else {
            QCA <- all(
                which(
                    is.element(
                        c("minmat", "DCC", "options", "neg.out", "opts"),
                        names(x$tt)
                    )
                ) < 4
            )
            otype <- "minimization"
            oq <- TRUE
            tt <- x$tt$tt
            snames <- unlist(
                strsplit(
                    gsub("[[:space:]]", "", x$tt$options$conditions),
                    split = ","
                )
            )
            noflevels <- x$tt$noflevels

            rnms <- rownames(x$tt$initial.data)
            ttcases <- x$tt$tt$cases
        }

        if (tjqca) {
            if (!identical(
                sort(tjcases),
                sort(unique(gsub("[0-9]", "", rnms)))
            )) {
                admisc::stopError("Case names do not match the truth table.")
            }
        }

        if (!QCA) {
            admisc::stopError(
                sprintf(
                    "Please create a proper %s object with package QCA.",
                    otype
                )
            )
        }

        if (any(noflevels != 2)) {
            admisc::stopError(
                "Venn diagrams are not possible for multivalue data."
            )
        }

        if (nofsets == 0) {
            nofsets <- length(snames)
        }

        if (nofsets > 7) {
            admisc::stopError(
                "Venn diagrams can only be drawn up to 7 explanatory conditions."
            )
        }

        if (nofsets < 4 | nofsets > 5) {
            ellipse <- FALSE
        }

        ttcolors <- c(
            "0" = "#ffd885",
            "1" = "#96bc72",
            "C" = "#1c8ac9",
            "?" = "#ffffff" # white
        )

        if (identical(zcolor, "style")) {
            zcolor <- "bw"
        }
        else if (!identical(zcolor, "bw")) {
            if (is.character(zcolor) & length(zcolor) >= 3) {
                ttcolors[c("0", "1", "C")] <- zcolor[1:3]
            }
        }

        individual <- length(opacity) == nrow(tt)

        gvenn <- do.call(
            openPlot,
            c(
                list(plotsize, par = par, ggplot = ggplot),
                dots
            )
        )

        if (individual) {

            for (i in seq(nrow(tt))) {

                if (tt$OUT[i] != "?") {

                    color <- adjustcolor(
                        ttcolors[tt$OUT[i]],
                        alpha.f = as.numeric(opacity[i])
                    )

                    if (i == 1) {

                        zeroset <- matrix(
                            c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0),
                            ncol = 2
                        )

                        colnames(zeroset) <- c("x", "y")

                        polygons <- rbind(
                            zeroset,
                            rep(NA, 2),
                            getZones(0, nofsets, ellipse)[[1]]
                        )

                        polygons <- polygons[-nrow(polygons), ]

                        if (is.null(gvenn)) {
                            polypath(
                                polygons,
                                rule = "evenodd",
                                col = color,
                                border = NA
                            )
                        }
                        else {
                            gvenn <- gvenn + ggpolypath::geom_polypath(
                                polygons,
                                rule = "evenodd",
                                col = color
                            )
                        }

                    }
                    else {
                        plotdata <- ints[
                            ints$s == nofsets &
                            ints$v == as.numeric(ellipse) &
                            ints$i == i,
                            c("x", "y")
                        ]

                        if (is.null(gvenn)) {
                            polygon(plotdata, col = color)
                        }
                        else {
                            gvenn <- gvenn + ggplot2::geom_polygon(
                                data = plotdata,
                                ggplot2::aes(x, y),
                                fill = color
                            )
                        }
                    }
                }
            }
        }
        else {

            for (i in names(ttcolors)[1:3]) {

                zones <- as.numeric(rownames(tt[tt$OUT == i, ]))

                if (length(zones) > 0) {

                    if (any(zones == 1)) {

                        zeroset <- matrix(
                            c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0),
                            ncol = 2
                        )

                        colnames(zeroset) <- c("x", "y")

                        polygons <- rbind(
                            zeroset,
                            rep(NA, 2),
                            getZones(0, nofsets, ellipse)[[1]]
                        )

                        polygons <- polygons[-nrow(polygons), ]

                        if (is.null(gvenn)) {
                            polypath(
                                polygons,
                                rule = "evenodd",
                                col = ttcolors[i],
                                border = NA
                            )
                        }
                        else {
                            gvenn <- gvenn + ggpolypath::geom_polypath(
                                polygons,
                                rule = "evenodd",
                                col = ttcolors[i]
                            )
                        }

                        zones <- zones[-1]
                    }

                    plotdata <- ints[
                        ints$s == nofsets & ints$v == as.numeric(ellipse) &
                        is.element(ints$i, zones),
                        c("x", "y")
                    ]

                    if (is.null(gvenn)) {
                        polygon(plotdata, col = ttcolors[i])
                    }
                    else {
                        gvenn <- gvenn + ggplot2::geom_polygon(
                            data = plotdata,
                            ggplot2::aes(x, y),
                        fill = ttcolors[i]
                            )
                    }
                }
            }
        }

        if (isTRUE(counts) & is.null(cts)) {
            cts <- tt$n
        }

        x <- nofsets


    }
    else if (is.numeric(x)) {

        nofsets <- x

        if (!identical(snames, "")) {
            if (length(snames) != nofsets) {
                admisc::stopError(
                    "Number of sets not equal with the number of set names."
                )
            }
        }

    }
    else if (is.character(x)) {

        if (any(grepl("\\$solution", funargs["x"]))) {
            obj <- get(unlist(strsplit(funargs["x"], split = "[$]"))[1])
            snames <- obj$tt$options$conditions
            nofsets <- length(snames)
        }

        # x <- admisc::splitstr(x) # this coerces to numbers, not good
        x <- unlist(strsplit(gsub("[[:space:]]", "", x), split = ","))

        if (all(grepl("[A-Za-z]", x))) { # x can be something like c("A", "B*c")

            if (identical(snames, "")) {
                y <- admisc::translate(
                    paste(x, collapse = "+"),
                    snames = snames
                )
                snames <- colnames(y)
                nofsets <- length(snames)
            }

            x <- lapply(x, function(x) {
                return(paste(apply(
                    admisc::translate(x, snames = snames),
                    1,
                    function(x) {
                        x[x < 0] <- "-"
                        return(paste(x, collapse = ""))
                    }),
                    collapse = "+"
                ))
            })

        }

        if (!is.list(x)) {
            if (!all(gsub("0|1|-|\\+", "", x) == "")) {
                admisc::stopError("Invalid codes in the rule(s).")
            }

            if (nofsets == 0) {
                nofsets <- unique(nchar(unlist(strsplit(x, split = "\\+"))))
            }

            x <- as.list(x)
        }

    }
    else if (is.data.frame(x)) {

        if (!is.null(names(x))) {
            if (all(names(x) != "")) {
                snames <- names(x)
            }
        }

        if (!all(is.element(unique(unlist(x)), c(0, 1)))) {
            admisc::stopError(
                "As a dataframe, argument <x> can only contain values 0 and 1."
            )
        }

        if (nofsets == 0) {
            nofsets <- length(x)
        }

        if (isTRUE(counts) & is.null(cts)) {
            cts <- apply(
                sapply(
                    rev(seq(nofsets)),
                    function(x) {
                        rep.int(
                            c(sapply(0:1, function(y) rep.int(y, 2^(x - 1)))),
                            2^nofsets / 2^x
                        )
                    }
                ),
                1,
                function(l1) {
                    sum(apply(x, 1, function(l2) {
                        all(l1 == l2)
                    }))
                }
            )
        }

        x <- nofsets
    }
    else if (is.list(x)) {

        if (any(grepl("\\$solution", funargs["x"]))) {
            obj <- get(
                unlist(
                    strsplit(funargs["x"], split = "[$]")
                )[1]
            )
            snames <- obj$tt$options$conditions
            nofsets <- length(snames)

            x <- admisc::translate(
                paste(unlist(x), collapse = " + "),
                snames = snames
            )

            x <- as.list(apply(x, 1, function(y) {
                y[y < 0] <- "-"
                return(paste(y, collapse = ""))
            }))

        }
        else {

            listx <- TRUE

            if (length(x) > 7) {
                x <- x[seq(7)]
            }

            if (!is.null(names(x))) {
                if (all(names(x) != "")) {
                    snames <- names(x)
                }
            }

            if (identical(snames, "")) {
                snames <- LETTERS[seq(length(x))]
            }

            if (nofsets == 0) {
                nofsets <- length(x)
            }

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

            intersections <- apply(tt, 1,
                function(y) {
                    setdiff(Reduce(intersect, x[y == 1]), unlist(x[y == 0]))
                }
            )

            names(intersections) <- apply(
                tt,
                1,
                function(x) paste(snames[x == 1], collapse = ":")
            )

            ttcts <- unlist(lapply(intersections, length))

            intersections <- intersections[ttcts > 0]

            tt <- as.data.frame(cbind(tt, counts = ttcts))

            attr(tt, "intersections") <- intersections

            if (isTRUE(counts) & is.null(cts)) {
                cts <- ttcts
            }

            x <- nofsets
        }
    }
    else {
        admisc::stopError("Unrecognised argument <x>.")
    }


    if (length(cts) != 2^nofsets) {
        cts <- NULL
        counts <- NULL
    }

    if (nofsets > 7) {
        admisc::stopError("Venn diagrams can only be drawn up to 7 sets.")
    }
    else if (nofsets < 4 | nofsets > 5) {
        ellipse <- FALSE
    }

    if (identical(snames, "")) {
        snames <- LETTERS[seq(nofsets)]
    }
    else {
        if (length(snames) != nofsets) {
            admisc::stopError(
                "Length of set names does not match the number of sets."
            )
        }
    }

    if (!is.element("ilcs", names(funargs))) {
        if (!ggplot) {
            ilcs <- ilcs - ifelse(nofsets > 5, 0.1, 0) - ifelse(nofsets > 6, 0.05, 0)
        }
    }

    # return(list(as.name("plotRules"), rules = x, zcolor = zcolor, ellipse = ellipse,
    #        opacity = opacity, allborders = borders, ... = ...))

    if (!ttqca) {
        gvenn <- openPlot(plotsize, par = par, ggplot = ggplot, ... = ...)
    }

    gvenn <- plotRules(
        x, zcolor, ellipse, opacity, allborders = borders, box = box,
        gvenn = gvenn, ... = ...
    )

    if (isTRUE(ilabels) | !is.null(cts) | tjqca) {

        if (isTRUE(ilabels)) {
            ilabels <- icoords$l[
                icoords$s == nofsets & icoords$v == as.numeric(ellipse)
            ]
        } else if (!is.null(cts)) {
            if (isTRUE(counts)) {
                cts[cts == 0] <- ""
            }

            ilabels <- cts
        }

        icoords <- icoords[
            icoords$s == nofsets & icoords$v == as.numeric(ellipse),
            c("x", "y")
        ]

        if (!is.null(ilabels)) {
            if (ggplot) {
                for (i in which(ilabels != "")) {
                    gvenn <- gvenn + ggplot2::annotate("text",
                        x = icoords$x[i], y = icoords$y[i],
                        label = ilabels[i],
                        size = ilcs
                    )
                }
            }
            else {
                text(icoords, labels = ilabels, cex = ilcs)
            }
        }

        if (tjqca) {
            ttcases <- strsplit(gsub(";", ",", ttcases), split = ",")
            caselist <- lapply(tjcases, function(x) {
                # local rnms <- global rnms (the local dissapears with the next x)
                rnms <- rnms[is.element(gsub("[0-9]", "", rnms), x)]
                rnmsindex <- c()
                for (i in seq(length(rnms))) {
                    rnmsindex <- c(
                        rnmsindex,
                        which(sapply(ttcases, function(x) {
                            any(x == rnms[i])
                        }))
                    )
                }

                return(rle(rnmsindex))
            })

            # names(caselist) <- tjcases
            # return(caselist)

            for (case in seq(length(tjcases))) {
                rlecase <- caselist[[case]]
                lengths <- rlecase$lengths
                values <- rlecase$values
                uvalues <- unique(values)
                jx <- jitter(icoords$x[uvalues], factor = 2)
                jy <- jitter(icoords$y[uvalues], factor = 2)
                x <- jx[match(values, uvalues)]
                y <- jy[match(values, uvalues)]
                tcase <- trajectory[[tjcases[case]]]

                if (is.null(tcase$length)) {
                    tcase$length <- 0.12
                }

                if (is.null(tcase$lwd)) {
                    tcase$lwd <- 2
                }

                if (is.null(tcase$col)) {
                    tcase$col <- "black"
                }

                if (length(values) == 1) {
                    points(
                        x,
                        y,
                        pch = ifelse(is.null(tcase$pch), 20, tcase$pch),
                        cex = ifelse(is.null(tcase$cex), 2, tcase$cex),
                        col = tcase$col
                    )
                }
                else {
                    i <- 1
                    j <- 2
                    while (i <= length(values) - 1) {
                        if (i == 1 & lengths[1] > 1) {
                            points(
                                x[1],
                                y[1],
                                pch = ifelse(is.null(tcase$pch), 20, tcase$pch),
                                cex = ifelse(is.null(tcase$cex), 1.5, tcase$cex),
                                col = tcase$col
                            )
                        }

                        back <- FALSE
                        while (j <= length(values)) {
                            if (j < length(values)) {
                                back <- values[j + 1] == values[i]
                            }

                            callist <- c(
                                list(x[i], y[i], x[j], y[j]),
                                tcase
                            )
                            callist$code <- 2 # + back

                            do.call(graphics::arrows, callist)
                            j <- j + 1 + back
                        }
                        i <- i + 1 + back
                    }
                }
            }
        }
    }

    scoords <- scoords[
        scoords$s == nofsets & scoords$v == as.numeric(ellipse),
        c("x", "y")
    ]

    if (ggplot) {
        for (i in seq(length(snames))) {
            gvenn <- gvenn + ggplot2::annotate("text",
                x = scoords$x[i], y = scoords$y[i],
                label = snames[i],
                size = sncs
            )
        }
    }
    else {
        text(scoords, labels = snames, cex = sncs)
    }

    if (ttqca) {
        # TRY 1: slow
        # for (i in 0:3) {
        #     polygon(110*i + c(0, 19, 19, 0), c(0, 0, 19, 19) - 35, col = ttcolors[i + 1])
        #     text(110*i + 40, 9 - 35, names(ttcolors)[i + 1], cex = 0.85)
        # }

        # TRY 2: rectangles not square
        # legend(-22, 3, horiz = TRUE, legend = c("0", "1", "C", "?"),
        #        bty = "n", fill = ttcolors, text.width = 60, cex = 0.9, x.intersp = 0.6)

        # TRY 3: perfect
        if (is.null(gvenn)) {
            points(
                seq(10, 340, length.out = 4),
                rep(-25, 4),
                pch = 22,
                bg = ttcolors,
                cex = 1.75
            )

            text(
                seq(40, 370, length.out = 4),
                rep(-26, 4),
                names(ttcolors),
                cex = 0.85
            )
        }
        else {
            gvenn <- gvenn +
            ggplot2::annotate("rect",
                xmin = 10, xmax = 32, ymin = -44, ymax = -22,
                fill = ttcolors[1],
                col = "black"
            ) +
            ggplot2::annotate("rect",
                xmin = 120, xmax = 142, ymin = -44, ymax = -22,
                fill = ttcolors[2],
                col = "black"
            ) +
            ggplot2::annotate("rect",
                xmin = 230, xmax = 252, ymin = -44, ymax = -22,
                fill = ttcolors[3],
                col = "black"
            ) +
            ggplot2::annotate("rect",
                xmin = 340, xmax = 362, ymin = -44, ymax = -22,
                fill = ttcolors[4],
                col = "black"
            ) +
            ggplot2::annotate("text",
                x = 50, y = -34,
                label = names(ttcolors)[1]
            ) +
            ggplot2::annotate("text",
                x = 160, y = -34,
                label = names(ttcolors)[2]
            ) +
            ggplot2::annotate("text",
                x = 270, y = -34,
                label = names(ttcolors)[3]
            ) +
            ggplot2::annotate("text",
                x = 380, y = -34,
                label = names(ttcolors)[4]
            )
        }
    }

    if (ggplot) {
        return(gvenn)
    }

    if (listx) {
        return(invisible(tt))
    }
}
