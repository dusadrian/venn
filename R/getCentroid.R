# Copyright (c) 2016-2023, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of contained data must cite this package according to
#       the citation("venn") command specific to this R package, along with the
#       appropriate weblink to the CRAN package "venn".
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

`getCentroid` <-
function(data) {
    return(lapply(data, function(x) {
        if (all(is.na(x[nrow(x), ]))) {
            x <- x[-nrow(x), ]
        }

        if (nrow(x) > 10) {
            vals <- seq(1, nrow(x), by = floor(nrow(x)/10))
            x <- x[c(vals, nrow(x)), ]
        }

        asum <- cxsum <- cysum <- 0

        for (i in seq(2, nrow(x))) {
            asum <- asum + x$x[i - 1]*x$y[i] - x$x[i]*x$y[i - 1]
            cxsum <- cxsum + (x$x[i - 1] + x$x[i])*(x$x[i - 1]*x$y[i] - x$x[i]*x$y[i - 1])
            cysum <- cysum + (x$y[i - 1] + x$y[i])*(x$x[i - 1]*x$y[i] - x$x[i]*x$y[i - 1])
        }

        return(c((1/(3*asum))*cxsum, (1/(3*asum))*cysum))
    }))
}


