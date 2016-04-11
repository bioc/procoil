setMethod("[", signature(x="CCProfile", i="index"),
    function(x, i)
    {
        if (is.character(i))
        {
            if (length(names(x@sequences)) < 1 ||
                any(is.na(names(x@sequences))))
                stop("missing names for subsetting\n")
            else
                i1 <- which(names(x@sequences) %in% i)

            if (length(i) != length(i1))
                stop("invalid names specified\n")

            i <- i1
        }
        else
        {
            ## convert negative subset
            if (all(i < 0))
                i <- (1:nrow(x@profiles))[i]
            else
            {
                if (min(i) < 1)
                    stop("subset indices must be all positive or",
                         " all negative\n")
            }

            if (min(i) < 1 || max(i) > nrow(x@profiles))
                stop("column subset must be between 1 and number",
                     " of sequences\n")
        }

        out <- as(as(x, "PredictionProfile")[i], "CCProfile")
        out@pred <- x@pred[i]
        out@disc <- x@disc[i]

        out
    }
)
