setMethod("predict", signature(object="CCModel"),
    function(object, seq, reg)
    {
        if (missing(seq) || !nchar(as.character(seq)))
            stop("sequence missing or empty\n")

        if (missing(reg))
        {
            if (!is.null(attr(seq, "reg")))
                reg <- as.character(attr(seq, "reg"))
            else if ((class(seq) == "AAString" ||
                     class(seq) == "BString") &&
                     !is.null(seq@metadata$reg))
            {
                reg <- as.character(seq@metadata$reg)
            }
            else
                reg <- ""
        }

        if (!nchar(reg))
            stop("register missing or empty\n")

        seq <- as.character(seq)

        if (nchar(seq) != nchar(reg))
            stop("lengths of sequence and register do not match\n")
        if (regexpr("[^A-Z]", seq, perl=TRUE) > 0)
            stop("sequence contains invalid characters\n")
        if (regexpr("[^abcdefg-]", reg, perl=TRUE) > 0)
            stop("register contains invalid characters")

        if (regexpr("-", reg, perl=TRUE) > 0)
        {
            start <- gregexpr("[a-g]+", reg, perl=TRUE)[[1]]
            end <- start + attr(start, "match.length") - 1

            seqSeg <- substring(seq, start, end)
            regSeg <- substring(reg, start, end)

            output <- mapply(function(s, r) {predict(object, s, r)},
                             seqSeg, regSeg)
            names(output) <- paste(start, end, sep="_")

            output
        }
        else
        {
            output <- new("CCProfile")

            output@seq <- seq
            output@reg <- reg
            output@b <- object@b

            n <- nchar(seq)

            prfl <- vector(mode="numeric", length=n)

            dotfill <- ""

            regarray <- c(0, 1, 2, 3, 4, 5, 6)
            regnames <- c("a", "b", "c", "d", "e", "f", "g")
            names(regarray) <- regnames

            features <- list()

            for (l in 0:object@m)
            {
                for (i in 1:(n - l - 1))
                {
                    firstreg <- substr(reg, i, i)
                    lastreg <- substr(reg, i + l + 1, i + l + 1)

                    if (lastreg ==
                        regnames[(regarray[firstreg] + l + 1) %% 7 +1])
                    {
                        pattern <- paste(substr(seq, i, i), dotfill,
                                         substr(seq, i + l + 1, i + l + 1),
                                         firstreg, sep="")

                        if (setequal(features[[pattern]], NULL))
                        {
                            features[[pattern]] <- 1
                        }
                        else
                        {
                            features[[pattern]] <- features[[pattern]] + 1
                        }

                        if (!setequal(object@weights[[pattern]], NULL))
                        {
                            val <- object@weights[[pattern]] / 2
                            prfl[i] <- prfl[i] + val
                            prfl[i + l + 1] <- prfl[i + l + 1] + val
                        }
                    }
                }

                dotfill <- paste(dotfill, ".", sep="")
            }

            scal <- 1

            if (object@scaling)
            {
                scal <- sqrt(sum(unlist(lapply(features,
                                               function(x){x^2}))))
            }

            output@profile <- prfl / scal
            output@disc <- sum(output@profile) + output@b
            output@pred <- ifelse(output@disc >= 0, "trimer", "dimer")

            output
        }
    }
)

