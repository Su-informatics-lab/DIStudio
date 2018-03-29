## look up for code or name 
LU_name.NDFRT <- function(x, model, ignore.case=T) {
   with(model, DefConcept[grep(x, DefConcept$name, ignore.case=ignore.case), ])
}

LU_code.NDFRT <- function(x, model) {
   with(model, subset(DefConcept, DefConcept$code %in% x))
}

