fitted.CCProfile <- function(object, decision.values=FALSE)
{
    res <- object@pred

    if (decision.values)
        attr(res, "decision.values") <- object@disc

    res
}

setMethod("fitted", signature(object="CCProfile"), fitted.CCProfile)
