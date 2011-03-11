readCCModel <- function(file)
{
    datatable <- read.table(file, header=FALSE, sep=",",
                            stringsAsFactors=FALSE)

    if (ncol(datatable) != 2) stop("file is in invalid format")

    lst <- as.list(datatable[,2])
    names(lst) <- datatable[,1]

    model <- new("CCModel")

    if (is.null(lst[["_b"]]) || !is.numeric(lst[["_b"]]) ||
        length(lst[["_b"]]) != 1)
    {
        stop("file is in invalid format")
    }
    else
    {
        model@b <- lst[["_b"]]
        lst[["_b"]] <- NULL
    }

    if (is.null(lst[["_m"]]) || !is.numeric(lst[["_m"]]) ||
        length(lst[["_m"]]) != 1 || lst[["_m"]] < 0)
    {
        stop("file is in invalid format")
    }
    else
    {
        model@m <- as.integer(lst[["_m"]])
        lst[["_m"]] <- NULL
    }

    if (is.null(lst[["_scaling"]]))
    {
        stop("file is in invalid format")
    }
    else
    {
        model@scaling <- ifelse(lst[["_scaling"]], TRUE, FALSE)
        lst[["_scaling"]] <- NULL
    }

    model@weights <- lst

    model
}
