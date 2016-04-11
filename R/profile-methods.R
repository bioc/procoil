setMethod("profile", signature(fitted="CCProfile"),
    function(fitted)
    {
        fitted@profiles
    }
)
