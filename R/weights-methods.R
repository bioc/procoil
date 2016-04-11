setMethod("weights", signature(object="CCModel"),
    function(object)
    {
        object@weights[1, ]
    }
)
