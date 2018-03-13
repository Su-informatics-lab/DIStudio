library(plyr)

# source("toDL.R")

Def_Association <- db.q("select * from ndfrt.ndfrt_associationdef;",nrows = NULL,verbose = FALSE)
Def_Concept <- db.q("select * from ndfrt.ndfrt_conceptdef;",nrows = NULL,verbose = FALSE)
DefiningConcepts <- db.q("select * from ndfrt.ndfrt_definingconcepts;",nrows = NULL,verbose = FALSE)
DefiningRoles <- db.q("select * from ndfrt.ndfrt_definingroles;",nrows = NULL,verbose = FALSE)
Def_Kind <- db.q("select * from ndfrt.ndfrt_kinddef;",nrows = NULL,verbose = FALSE)
Def_Namespace <- db.q("select * from ndfrt.ndfrt_namespacedef;",nrows = NULL,verbose = FALSE)
Def_Property <- db.q("select * from ndfrt.ndfrt_propertydef;",nrows = NULL,verbose = FALSE)
Def_Qualifier <- db.q("select * from ndfrt.ndfrt_qualifierdef;",nrows = NULL,verbose = FALSE)
Def_QualifierValue <- db.q("select * from ndfrt.ndfrt_qualifiervaluedef;",nrows = NULL,verbose = FALSE)
Def_Role <- db.q("select * from ndfrt.ndfrt_roledef;",nrows = NULL,verbose = FALSE)
Def_RoleValue <- db.q("select * from ndfrt.ndfrt_rolevaluedef;",nrows = NULL,verbose = FALSE)

ConceptAssociation <- db.q("select * from ndfrt.ndfrt_conceptassociation;",nrows = NULL,verbose = FALSE)
ConceptProperty <- db.q("select * from ndfrt.ndfrt_conceptproperty;",nrows = NULL,verbose = FALSE)
db.disconnect(conn)

rm(conn)

# Modify DefiningRoles data 
DefiningRoles <- ddply(DefiningRoles, .(concept_code),.fun = function(x) {
  idx <- which(sapply(x$role_value_ids,nchar) > 5)
  if (length(idx) > 0) {
    y <- x$role_value_ids[-idx]
    for (i in idx)  y <- union(y,unlist(strsplit(x$role_value_ids[i],"|",fixed =T)))
    Y <- data.frame(concept_code =rep(x$concept_code[1],length(y)), 
                    role_value_ids = y, stringsAsFactors = F )
  } else Y <- x
  return(Y)
}
)

RX_Norm <- ddply(ConceptProperty, .(concept_code),.fun = function(x) {
  idx <- match(c("C818","C819","C10490107818172"),x$name)
  if (! all(is.na(idx))) return(c(RX_CUI = x$value[idx[1]], RX_name = x$value[idx[2]],NUI =x$value[idx[3]]  ))}   
)

Rx_Norm.NDFRT <- subset(RX_Norm, RX_Norm$concept_code %in% Def_Concept$code[Def_Concept$kind =="C8"])
Rx_Norm.NDFRT <- cbind(concept_name =Def_Concept$name[match(Rx_Norm.NDFRT$concept_code,Def_Concept$code)],Rx_Norm.NDFRT)


save.image("NDFRT.Rdata")


