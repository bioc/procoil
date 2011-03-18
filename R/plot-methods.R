setMethod("plot", signature(x="CCProfile", y="missing"),
    function(x, col="red", rng=0, standardize=FALSE, shades=NULL,
             legend="", legend.pos="topright", ...)
    {
        if (!is.numeric(try(col2rgb(col), silent=TRUE)[1]))
            stop("argument `col' must be a string denoting a color\n")

        if (!is.null(shades) &&
            (!is.numeric(try(col2rgb(shades[1]), silent=TRUE)[1]) ||
             !is.numeric(try(col2rgb(shades[2]), silent=TRUE)[1])))
            stop("argument `shades' must be a vector of two colors\n")

        if (!is.null(legend) && !is.character(legend))
            stop("argument `legend' must be a string\n")

        n <-nchar(x@seq)

        prfl <- x@profile
        hlin <- (-x@b / n)

        if (standardize)
        {
            prfl <- prfl - hlin
            hlin <- 0
        }

        if (rng <= 0) rng <- max(abs(c(prfl, hlin)))

        plot(x=NULL,y=NULL, xlim=c(1, n + 1), ylim=c(-rng, rng),
             axes=FALSE, xlab="", ylab="weight", type="s", ...)

        if (!is.null(shades))
        {
            if (is.vector(shades) && length(shades) >= 2 &&
                is.character(shades[1]) && is.character(shades[2]))
            {
                polygon(c(1, n + 1, n + 1, 1), c(hlin, hlin, rng, rng),
                        col=shades[1], border=NA)
                polygon(c(1, n + 1, n + 1, 1), c(hlin, hlin, -rng, -rng),
                        col=shades[2], border=NA)
            }
            else
            {
                stop("shades must be a vector of 2 colors\n")
            }
        }

        sapply(gregexpr("a", x@reg)[[1]],
               function(i)
                   if (i > 0)
                       lines(c(i, i), c(-rng, rng), col="lightgray"))

        if (substr(x@reg, n, n)=="g")
            lines(c(n + 1, n + 1), c(-rng, rng), col="lightgray")

        lines(x=c(1, n + 1), y=c(hlin, hlin), col="lightgray")

        sapply(gregexpr("(a[^b]|b[^c]|c[^d]|d[^e]|e[^f]|f[^g]|g[^a])",
                        x@reg, perl=TRUE)[[1]],
               function(i)
                   if (i > 0)
                       lines(c(i + 1, i + 1), c(-rng, rng), col="red"))

        axis(side=2)

        aaSeq <- strsplit(x@seq, "")[[1]]

        mtext(side=1, at=(1:n + 0.5), line=0, text=aaSeq)
        mtext(side=3, at=(1:n + 0.5), line=0, text=aaSeq)
        text(1:n + 0.5, 0, strsplit(x@reg, "")[[1]], adj=c(0.5,0))

        lines(c(1:(n + 1)), c(prfl, prfl[n]),
              type="s", col=col)

        if (length(legend) == 1 && legend != "")
        {
            if (length(legend.pos) > 1)
                legend(x=legend.pos[1], y=legend.pos[2], col=col,
                       legend=legend, lwd=1, bg="white")
            else
                legend(x=legend.pos[1], col=col,
                       legend=legend, lwd=1, bg="white")
        }
    }
)


setMethod("plot", signature(x="CCProfile", y="CCProfile"),
    function(x, y, col=c("red", "blue"), rng=0,
             standardize=FALSE, shades=NULL, legend=NULL,
             legend.pos="topright", ...)
    {
        if (!is.null(col) &&
            (!is.numeric(try(col2rgb(col[1]), silent=TRUE)[1]) ||
             !is.numeric(try(col2rgb(col[2]), silent=TRUE)[1])))
            stop("argument `col' must be a vector of two colors\n")

        if (!is.null(shades) &&
            (!is.numeric(try(col2rgb(shades[1]), silent=TRUE)[1]) ||
             !is.numeric(try(col2rgb(shades[2]), silent=TRUE)[1])))
            stop("argument `shades' must be a vector of two colors\n")

        if (!is.null(legend) && (!is.character(legend) || length(legend) < 2))
            stop("argument `legend' must be a vector of two strings\n")

        n <- nchar(x@seq)
        b <- x@b

        if (nchar(y@seq) != n || nchar(x@reg) != n)
        {
            stop("length mismatch\n")
        }
        else if (y@reg != x@reg)
        {
            stop("register mismatch\n")
        }
        else if (y@b != b)
        {
            stop("offset mismatch\n")
        }

        minvalue <- min(c(x@profile, y@profile))
        maxvalue <- max(c(x@profile, y@profile))

        hlin <- (-b / n)

        if (rng <= 0)
        {
            if (standardize)
            {
                rng <- max(abs(c(minvalue - hlin, maxvalue - hlin)))
                hlin <- 0
            }
            else
            {
                rng <- max(abs(c(hlin, minvalue, maxvalue)))
            }
        }

        plot(x=NULL,y=NULL, xlim=c(1, n + 1), ylim=c(-rng, rng),
             axes=FALSE, xlab="", ylab="weight", type="s", ...)

        if (!is.null(shades))
        {
            if (is.vector(shades) && length(shades) >= 2 &&
                is.character(shades[1]) && is.character(shades[2]))
            {
                polygon(c(1, n + 1, n + 1, 1), c(hlin, hlin, rng, rng),
                        col=shades[1], border=NA)
                polygon(c(1, n + 1, n + 1, 1), c(hlin, hlin, -rng, -rng),
                        col=shades[2], border=NA)
            }
            else
            {
                stop("shades must be a vector of 2 colors\n")
            }
        }

        sapply(gregexpr("a", x@reg)[[1]],
               function(i)
                   if (i > 0)
                       lines(c(i, i), c(-rng, rng), col="lightgray"))

        if (substr(x@reg, n, n)=="g")
            lines(c(n + 1, n + 1), c(-rng, rng), col="lightgray")

        lines(x=c(1, n + 1), y=c(hlin, hlin), col="lightgray")

        sapply(gregexpr("(a[^b]|b[^c]|c[^d]|d[^e]|e[^f]|f[^g]|g[^a])",
                        x@reg, perl=TRUE)[[1]],
               function(i)
                   if (i > 0)
                       lines(c(i + 1, i + 1), c(-rng, rng), col="red"))

        axis(side=2)

        xAASeq <- strsplit(x@seq, "")[[1]]
        yAASeq <- strsplit(y@seq, "")[[1]]
        matchS <- (xAASeq == yAASeq)

        mtext(side=1, at=(1:n + 0.5), line=0, text=yAASeq,
              col=ifelse(matchS, "black", col[2]))
        mtext(side=3, at=(1:n + 0.5), line=0, text=xAASeq,
              col=ifelse(matchS, "black", col[1]))
        text(1:n + 0.5, 0, strsplit(x@reg, "")[[1]], adj=c(0.5,0))

        prfl <- x@profile
        if (standardize) prfl <- prfl + b / n
        lines(c(1:(n + 1)), c(prfl, prfl[n]), type="s", col=col[1])

        prfl <- y@profile
        if (standardize) prfl <- prfl + b / n
        lines(c(1:(n + 1)), c(prfl, prfl[n]), type="s", col=col[2])

        if (!is.null(legend))
        {
            if (length(legend.pos) > 1)
                legend(x=legend.pos[1], y=legend.pos[2], col=col,
                       legend=legend, lwd=1, bg="white")
            else
                legend(x=legend.pos[1], col=col,
                       legend=legend, lwd=1, bg="white")
        }
    }
)
