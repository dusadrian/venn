`extractDisjunctions` <- function(x, snames) {
    
    # purpose: allow complete disjunctions to be plotted
    # in version 1.5, something like "A + (B + C)" is treated as "A + B + C"
    if (missing(snames)) {
        snames <- ""
    }
    
    if (nchar(gsub("[A-Za-z]|\\+|\\*|\\(|\\)", "", x)) > 0) {
        cat("\n")
        stop(simpleError("Invalid expression.\n\n"))
    }
    
    transformx <- function(x) {
        unname(apply(x, 1, function(x) {
            x[x < 0] <- "-"
            paste(x, collapse = "")
        }))
    }
    
    y <- admisc::translate(x, snames = snames)
    snames <- colnames(y)
    
    if (grepl("\\(|\\)", x)) {
        opened <- gsub("[A-Za-z]|\\+|\\*|\\)", "", x)
        closed <- gsub("[A-Za-z]|\\+|\\*|\\(", "", x)
        if (nchar(opened) != nchar(closed)) {
            cat("\n")
            stop(simpleError("All opened / closed brackets should match.\n\n"))
        }
        
        if (grepl("\\)\\*\\(|\\)\\(", x)) {
            cat("\n")
            stop(simpleError("Brackets are allowed only for disjunctions.\n\n"))
        }
        
        sx <- unlist(strsplit(x, split = ""))
        wf <- which(sx == "(")[1]
        if (wf != 1) {
            if (sx[wf - 1] != "+") {
                cat("\n")
                stop(simpleError("Brackets are allowed only for disjunctions.\n\n"))
            }
        }
        
        x <- unlist(strsplit(x, split = ""))
        opened <- which(x == "(")
        closed <- which(x == ")")
        
        if (opened[1] == 1) {
            if (closed[length(closed)] != length(x)) {
                cat("\n")
                stop(simpleError("Invalid expression.\n\n"))
            }
            else {
                x <- x[-c(1, length(x))]
                opened <- opened[-1]
                closed <- closed[-length(closed)]
            }
        }
        
        if (length(opened) > 0) {
            
            if (opened[1] == 1) {
                cat("\n")
                stop(simpleError("Invalid expression.\n\n"))
            }
            
            result <- list(transformx(admisc::translate(paste(x[seq(1, opened[1] - 1)], collapse = ""), snames = snames)))
            for (i in seq(length(opened))) {
                result[[i + 1]] <- transformx(admisc::translate(paste(x[seq(opened[i] + 1, closed[i] - 1)], collapse = ""), snames = snames))
            }
            if (closed[length(closed)] != length(x)) {
                if (x[closed[length(closed)] + 1] != "+") {
                    cat("\n")
                    stop(simpleError("Brackets are allowed only for disjunctions.\n\n"))
                }
                
                result[[length(result) + 1]] <- transformx(admisc::translate(paste(x[seq(closed[length(closed)] + 2, length(x))], collapse = ""), snames = snames))
            }
            
            return(result)
        }
        else {
            return(list(transformx(y)))
        }
    }
    else {
        return(list(transformx(y)))
    }
    
}

