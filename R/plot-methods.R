setMethod("plot", signature(x="CCProfile", y="missing"),
    function(x, col=c("red", "blue"), standardize=TRUE, shades=NULL,
             legend="default", legendPos="topright", xlab="",
             ylab="weight", lwd.profile=1, lwd.axis=1, las=1,
             heptads=TRUE, annotate=TRUE, ...)
    {
        ## workaround
        if (identical(legend, "") || is.null(legend) || is.na(legend))
        {
            legend <- ""
            legendPos <- NA
        }

        if (identical(legend, "default") && nrow(profiles(x)) == 1 &&
            length(rownames(x)) == 0)
        {
            legend <- ""
            legendPos <- NA
        }

        plot(as(x, "PredictionProfile"), col=col,
             standardize=standardize, shades=shades,
             legend=legend, legendPos=legendPos, xlab=xlab, ylab=ylab,
             lwd.profile=lwd.profile, lwd.axis=lwd.axis, las=las,
             heptads=heptads, annotate=annotate, ...)
    }
)
