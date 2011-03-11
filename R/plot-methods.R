setMethod("plot", signature(x="CCProfile", y="missing"),
    function(x, col="red", rng=0, standardize=FALSE, shades=NULL,
             legend.show=FALSE, text="", xpos="topright", ypos=NULL, ...)
    {
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

        if (legend.show)
            legend(x=xpos, y=ypos, col=c(col),
                legend=c(text), lwd=1, bg="white")
    }
)


setMethod("plot", signature(x="CCProfile", y="CCProfile"),
    function(x, y, xcol="red", ycol="blue", rng=0,
             standardize=FALSE, shades=NULL, legend.show=FALSE,
             xtext="", ytext="", xpos="topright", ypos=NULL, ...)
    {
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
              col=ifelse(matchS, "black", ycol))
        mtext(side=3, at=(1:n + 0.5), line=0, text=xAASeq,
              col=ifelse(matchS, "black", xcol))
        text(1:n + 0.5, 0, strsplit(x@reg, "")[[1]], adj=c(0.5,0))

        prfl <- x@profile
        if (standardize) prfl <- prfl + b / n
        lines(c(1:(n + 1)), c(prfl, prfl[n]), type="s", col=xcol)

        prfl <- y@profile
        if (standardize) prfl <- prfl + b / n
        lines(c(1:(n + 1)), c(prfl, prfl[n]), type="s", col=ycol)

        if (legend.show)
            legend(x=xpos, y=ypos, col=c(xcol, ycol),
                legend=c(xtext, ytext), lwd=1, bg="white")
    }
)
