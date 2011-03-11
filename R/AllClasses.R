setClass("CCModel",
    representation=representation(b="numeric",
                                  m="integer",
                                  scaling="logical",
                                  weights="list")
)

setClass("CCProfile",
    representation=representation(seq="character",
                                  reg="character",
                                  profile="numeric",
                                  b="numeric",
                                  disc="numeric",
                                  pred="character")
)

setValidity("CCModel",
    function(object)
    {
        if (!is.numeric(object@b) || length(object@b) != 1)
        {
            return("slot @b must be a single number")
        }
        else if (!is.integer(object@m) || length(object@m) != 1 ||
                 object@m < 0)
        {
            return("slot @m must contain a non-negative integer number")
        }
        else if (!is.list(object@weights) || length(object@weights) == 0)
        {
            return("list of weights empty")
        }
    }
)

setValidity("CCProfile",
    function(object)
    {
        if (nchar(object@seq) != nchar(object@reg))
        {
            return("lengths of sequence and register do not match")
        }
        else if (regexpr("[^A-Z]", object@seq, perl=TRUE) > 0)
        {
            return("sequence contains invalid characters")
        }
        else if (regexpr("[^abcdefg]", object@reg, perl=TRUE) > 0)
        {
            return("register contains invalid characters")
        }
        else if (nchar(object@seq) != length(object@profile))
        {
            return("lengths of sequence and profile do not match")
        }
    }
)
