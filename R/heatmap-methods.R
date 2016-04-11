setMethod("heatmap", signature(x="CCProfile", y="missing"),
    function(x, y, ...)
    {
        if (nrow(x@profiles) < 3)
            stop("at least three profiles are required for ",
                 "plotting a heatmap")

        heatmap(as(x, "PredictionProfile"), ...)
    }
)
