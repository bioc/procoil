writeCCModel <- function(object, file)
{
    if (!is(object, "CCModel"))
        stop("object is not of class 'CCModel'")

    df <- data.frame(c("_b", "_m", "_scaling", colnames(object@weights)),
                     c(object@b, object@m, as.integer(object@scaling),
                       object@weights[1, ]))

    write.table(df, file=file, quote=FALSE, sep=",", row.names=FALSE,
                col.names=FALSE)

    invisible(NULL)
}
