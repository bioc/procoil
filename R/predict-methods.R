predict.CCModel <- function(object, seq, reg)
{
    if (missing(seq))
        stop("seq missing\n")

    if (!missing(reg) && !is.character(reg))
        stop("reg must be a character vector")

    if (is(seq, "AAString"))
    {
        if (missing(reg) && !is.null(metadata(seq)[["reg"]]) &&
            nchar(metadata(seq)[["reg"]]) > 0)
            reg <- metadata(seq)[["reg"]]

        seq <- as.character(seq)
    }
    else if (is(seq, "AAStringSet"))
    {
        if (!is.null(annotationMetadata(seq)) && missing(reg))
            reg <- annotationMetadata(seq)

        if (missing(reg) && !is.null(metadata(seq)[["reg"]]) &&
            nchar(metadata(seq)[["reg"]]) > 0)
            reg <- metadata(seq)[["reg"]]

        seq <- as.character(seq)
    }
    else if (is(seq, "AAVector"))
    {
        if (!is.null(annotationMetadata(seq)) && missing(reg))
            reg <- annotationMetadata(seq)
    }
     else if (is.character(seq))
    {
        if (missing(reg) && !is.null(attr(seq, "reg")))
        {
            reg <- attr(seq, "reg")
            attr(seq, "reg") <- NULL
        }
    }
   else
        stop("seq has invalid class")

    if (missing(reg))
        stop("no heptad registers specified")

    if (length(seq) != length(reg) || any(nchar(seq) != nchar(reg)))
        stop("length/size mismatch between sequence and heptad registers")

    sel <- which(nchar(seq) < 2)

    if (length(grep("[^A-Z]", seq, perl=TRUE)) > 0)
        stop("sequences contain invalid characters\n")

    if (length(grep("[^abcdefg-]", reg, perl=TRUE)) > 0)
        stop("heptad registers contain invalid characters")

    if (length(grep("-", reg, fixed=TRUE)) > 0)
    {
        pos <- gregexpr("[a-g]+", reg, perl=TRUE)

        matches <- gregexpr("[a-g]+", reg, perl=TRUE)

        extract.FUN <- function(i)
        {
            start <- matches[[i]]

            if (start[1] == -1)
                return(NULL)

            end <- start + attr(start, "match.length") - 1

            seqSeg <- substring(seq[i], start, end)
            regSeg <- substring(reg[i], start, end)

            if (length(names(seq)) > 0)
                names(seqSeg) <- paste(names(seq)[i], 1:length(seqSeg),
                                       sep=".")

            list(seqSeg, regSeg)
        }

        rawList <- lapply(1:length(seq), extract.FUN)

        seq <- unlist(sapply(rawList, function(x) x[[1]]))
        reg <- unlist(sapply(rawList, function(x) x[[2]]))
    }

    if (length(seq) < 1)
        stop("no sequences contain coiled coils")

    if (length(sel) > 0)
    {
        seq <- seq[-sel]
        ref <- reg[-sel]

        if (length(seq) == 0)
            stop("all sequences/segments are shorter than 2 amino acids")

        warning("sequences/segments with less than 2 amino acids removed")
    }

    seq <- AAVector(seq)
    annotationMetadata(seq, annCharset="abcdefg") <- reg

    GP <- gappyPairKernel(k=1, m=object@m, annSpec=TRUE,
                          normalized=object@scaling)
    res <- getPredictionProfile(seq, kernel=GP, featureWeights=object@weights,
                                b=object@b)

    res <- as(res, "CCProfile")

    res@disc <- rowSums(res@profiles) - res@baselines * nchar(seq)
    res@pred <- factor(ifelse(res@disc >= 0, "trimer", "dimer"),
                       levels=c("dimer", "trimer"))

    res
}

setMethod("predict", signature(object="CCModel"), predict.CCModel)
