setMethod("show", signature(object="CCModel"),
    function(object)
    {
        cat("\nCCModel object")
        cat("\n\tcoiled coil kernel with m=", object@m, sep="")
        cat("\n\t",
            ifelse(object@scaling, "with", "without"),
                " kernel normalization", sep="")
        cat("\n\t", length(object@weights),
            " patterns", sep="")
        cat("\n\tb =", object@b, "\n\n")
     }
)

setMethod("show", signature(object="CCProfile"),
    function(object)
    {
        cat("\nCCProfile object")
        cat("\n\tSample:", object@seq)
        cat("\n\t       ", object@reg)
        cat("\n\tdiscriminant function value =", object@disc)
        cat("\n\tpredicted as", object@pred, "\n\n")
    }
)
