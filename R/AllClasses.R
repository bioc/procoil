setClass("CCModel",
    slots=c(b="numeric",
            m="integer",
            scaling="logical",
            weights="matrix")
)

setClass("CCProfile",
    slots=c(disc="numeric",
            pred="factor"),
    contains="PredictionProfile"
)
