`extractInfo` <- function(
    x, what = c("counts", "intersections", "both"), use.names = FALSE
) {

    what <- match.arg(what)

    if (!is.list(x)) {
        admisc::stopError("Argument x should be a list")
    }

    if (length(x) > 7) {
        x <- x[seq(7)]
    }

    nofsets <- length(x)

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

    intersections <- apply(tt, 1,
        function(y) {
            setdiff(Reduce(intersect, x[y == 1]), unlist(x[y == 0]))
        }
    )

    if (!isTRUE(use.names)) {
        snames <- seq(length(snames))
    }

    names(intersections) <- apply(
        tt,
        1,
        function(x) paste(snames[x == 1], collapse = ":")
    )

    ttcts <- unlist(lapply(intersections, length))

    intersections <- intersections[ttcts > 0]

    tt <- as.data.frame(cbind(tt, counts = ttcts))

    if (what == "counts") {
        return(tt)
    }

    if (what == "intersections") {
        return(intersections)
    }

    return(list(tt, intersections))
}
