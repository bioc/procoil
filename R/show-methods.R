show.CCModel <- function(object)
{
    cat("An object of class ", dQuote(class(object)), "\n\n")

    cat("Model parameters:\n\tcoiled coil kernel with m=", object@m, " and",
        ifelse(object@scaling, "", " without"),
        " kernel normalization\n", sep="")
    cat("\toffset b=", format(object@b, digits=4), "\n\n")

    cat("Feature weights:\n")

    ord <- order(object@weights[1, ], decreasing=TRUE)
    sel <- ord[1:5]
    cat(paste0("\t",
               formatC(object@weights[1, sel], format="f", digits=4, width=8),
               " ... ", colnames(object@weights)[sel]), sep="\n")
    cat("\t", formatC("...", format="s", width=8), " ... ...\n", sep="")
    sel <- ord[(length(ord) - 4):length(ord)]
    cat(paste0("\t",
               formatC(object@weights[1, sel], format="f", digits=4, width=8),
               " ... ", colnames(object@weights)[sel]), sep="\n")
    cat("\n")
}

setMethod("show", signature(object="CCModel"), show.CCModel)


show.CCProfile <- function(object)
{
    getMethod("show", signature(object="PredictionProfile"))(object)

    noOfDigits <- 9
    colWidth <- noOfDigits + 3
    noOfBlocks <- 1
    blockSize <- length(object@pred)

    cat("\nPredictions:\n")

    if (length(object@pred) > 10)
    {
        noOfBlocks <- 2
        blockSize <- 5
    }

    if (length(names(object@pred)) > 0)
        nwidth <- min(max(names(object@pred)), 20)
    else
        nwidth <- ceiling(log10(length(object@pred))) + 2

    noPos <- ncol(object@profiles)
    offset <- 0

    for (i in 1:noOfBlocks)
    {
        if (i == 2)
            offset <- length(object@pred) - blockSize

        if (i == 1)
        {
            cat(format(" ", width=nwidth))

            cat(format("Score", width=colWidth + 1, justify="right"))
            cat(format("Class", width=7, justify="right"))

            cat("\n")
        }

        for (j in (1 + offset):(blockSize + offset))
        {
            if (length(names(object@pred)) > 0)
            {
                sampleName <- names(object@pred)[j]

                if (nchar(sampleName) > 20)
                     sampleName <- paste0(substring(sampleName, 1, 17), "...")
            }
            else
                sampleName <- format(paste0("[", j, "]"), nwidth,
                                     justify="right")

            cat(formatC(sampleName, format="s", width=nwidth))

            cat(formatC(object@disc[j], format="f", digits=noOfDigits,
                        width=colWidth + 1))
            cat(formatC(as.character(object@pred[j]), format="s", width=7))

            cat("\n")
        }

        if (i == 1 && noOfBlocks > 1)
        {
            cat(formatC(paste(rep(".", nwidth - 2), sep="", collapse=""),
                       format="s", width=nwidth))
            cat(formatC(paste(rep(".", 6), sep="", collapse=""),
                        format="s", width=colWidth + 1))
            cat(formatC(paste(rep(".", 4), sep="", collapse=""),
                        format="s", width=7))
            cat("\n")
        }
    }

    cat("\n")
}

setMethod("show", signature(object="CCProfile"), show.CCProfile)
