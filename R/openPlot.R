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

`openPlot` <-
function(plotsize = 15, par = TRUE, ggplot = FALSE, ...) {

    if (ggplot) {
        cf <- eval(parse(text = "ggplot2::coord_fixed()"))
        cf$default <- TRUE
        outplot <- eval(parse(text = "ggplot2::ggplot()"))
        return(
            outplot +
            eval(parse(text = "ggplot2::geom_blank()")) +
            cf +
            eval(parse(text =
                "ggplot2::coord_fixed(xlim = c(0, 1000), ylim = c(0, 1000))"
            )) +
            eval(parse(text = paste(
                "ggplot2::theme(axis.line = ggplot2::element_blank(),",
                    "axis.text.x = ggplot2::element_blank(),",
                    "axis.text.y = ggplot2::element_blank(),",
                    "axis.ticks = ggplot2::element_blank(),",
                    "axis.title.x = ggplot2::element_blank(),",
                    "axis.title.y = ggplot2::element_blank(),",
                    "legend.position = 'none',",
                    "panel.background = ggplot2::element_blank(),",
                    "panel.border = ggplot2::element_blank(),",
                    "panel.grid.major = ggplot2::element_blank(),",
                    "panel.grid.minor = ggplot2::element_blank(),",
                    "plot.background = ggplot2::element_blank(),",
                    "axis.ticks.length.x = ggplot2::unit(.25, 'cm'),",
                    "axis.ticks.length.y = ggplot2::unit(.25, 'cm'),",
                    "plot.title = ggplot2::element_text(size = 0),",
                    "plot.subtitle = ggplot2::element_text(size = 0),",
                    "plot.tag = ggplot2::element_text(size = 0),",
                    "plot.caption = ggplot2::element_text(size = 0)",
                ")"
            )))
        )
    }
    else {
        if (par) {
            if (dev.cur() == 1) {
                dev.new(width = (plotsize + 1)/2.54, height = (plotsize + 1)/2.54)
            }
            
            par(new = FALSE, xpd = TRUE, mai = c(0.05, 0.05, 0.05, 0.05))
        }

        dots <- list(...)
        plot(x = 0:1000, type = "n", axes = FALSE, asp = 1, xlab = "", ylab = "")

        if (!is.null(dots$main)) {
            title(main = dots$main, line = dots$line)
        }
    }
}
