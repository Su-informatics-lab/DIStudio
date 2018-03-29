## look up for code or name 
LU_name.NDFRT <- function(model, x, ignore.case=T) {
   with(model, DefConcept[grep(x, DefConcept$name, ignore.case=ignore.case), ])
}

LU_code.NDFRT <- function(model, x) {
   with(model, subset(DefConcept, DefConcept$code %in% x))
}


# Exposed Lookup
listNodes <- function(model, kinds=NULL, name=NULL, ignore.case=T) {
   nodes <- model$DefConcept

   if (!is.null(kinds)) {
      nodes <- nodes[nodes$kind %in% kinds, ]
   }
   
   if (!is.null(name)) {
      nodes <- nodes[grep(name, nodes$name, ignore.case=ignore.case), ]
   }

   nodes
}
