`openPlot` <-
function(plotsize = 15, par = TRUE, ggplot = FALSE, ...) {

    if (ggplot) {
        cf <- ggplot2::coord_fixed()
        cf$default <- TRUE
        return(ggplot2::ggplot() + ggplot2::geom_blank() + 
            cf +
            ggplot2::coord_fixed(xlim = c(0, 1000), ylim = c(0, 1000)) + 
            ggplot2::theme(axis.line = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_blank(),
                legend.position = "none",
                panel.background = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                plot.background = ggplot2::element_blank(),
                axis.ticks.length.x = ggplot2::unit(.25, "cm"),
                axis.ticks.length.y = ggplot2::unit(.25, "cm"),
                plot.title = ggplot2::element_text(size = 0),
                plot.subtitle = ggplot2::element_text(size = 0),
                plot.tag = ggplot2::element_text(size = 0),
                plot.caption = ggplot2::element_text(size = 0)))
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
