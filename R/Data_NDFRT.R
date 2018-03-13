createNdfrtDataFromDatabase <- function(conn) {
   Def_Association <- PivotalR::db.q("select * from ndfrt.ndfrt_associationdef;",nrows = NULL,verbose = FALSE)
   Def_Concept <- PivotalR::db.q("select * from ndfrt.ndfrt_conceptdef;",nrows = NULL,verbose = FALSE)
   DefiningConcepts <- PivotalR::db.q("select * from ndfrt.ndfrt_definingconcepts;",nrows = NULL,verbose = FALSE)
   DefiningRoles <- PivotalR::db.q("select * from ndfrt.ndfrt_definingroles;",nrows = NULL,verbose = FALSE)
   Def_Kind <- PivotalR::db.q("select * from ndfrt.ndfrt_kinddef;",nrows = NULL,verbose = FALSE)
   Def_Namespace <- PivotalR::db.q("select * from ndfrt.ndfrt_namespacedef;",nrows = NULL,verbose = FALSE)
   Def_Property <- PivotalR::db.q("select * from ndfrt.ndfrt_propertydef;",nrows = NULL,verbose = FALSE)
   Def_Qualifier <- PivotalR::db.q("select * from ndfrt.ndfrt_qualifierdef;",nrows = NULL,verbose = FALSE)
   Def_QualifierValue <- PivotalR::db.q("select * from ndfrt.ndfrt_qualifiervaluedef;",nrows = NULL,verbose = FALSE)
   Def_Role <- PivotalR::db.q("select * from ndfrt.ndfrt_roledef;",nrows = NULL,verbose = FALSE)
   Def_RoleValue <- PivotalR::db.q("select * from ndfrt.ndfrt_rolevaluedef;",nrows = NULL,verbose = FALSE)

   ConceptAssociation <- PivotalR::db.q("select * from ndfrt.ndfrt_conceptassociation;",nrows = NULL,verbose = FALSE)
   ConceptProperty <- PivotalR::db.q("select * from ndfrt.ndfrt_conceptproperty;",nrows = NULL,verbose = FALSE)


   # Modify DefiningRoles data 
   DefiningRoles <- plyr::ddply(DefiningRoles, plyr::.(concept_code),.fun = function(x) {
      idx <- which(sapply(x$role_value_ids,nchar) > 5)

      if (length(idx) > 0) {
         y <- x$role_value_ids[-idx]

         for (i in idx) {
            y <- union(y,unlist(strsplit(x$role_value_ids[i],"|",fixed =T)))
         }

         Y <- data.frame(
            concept_code =rep(x$concept_code[1],length(y)), 
            role_value_ids = y,
            stringsAsFactors = F
         )
      } else {
         Y <- x
      }

      return(Y)
   })

   RX_Norm <- plyr::ddply(ConceptProperty, plyr::.(concept_code),.fun = function(x) {
      idx <- match(c("C818","C819","C10490107818172"), x$name)

      if (! all(is.na(idx))) {
         return(c(
            RX_CUI = x$value[idx[1]],
            RX_name = x$value[idx[2]],
            NUI =x$value[idx[3]]
         ))
      }
   })

   Rx_Norm.NDFRT <- subset(RX_Norm, RX_Norm$concept_code %in% Def_Concept$code[Def_Concept$kind == "C8"])
   Rx_Norm.NDFRT <- cbind(
      concept_name = Def_Concept$name[match(Rx_Norm.NDFRT$concept_code,Def_Concept$code)],
      Rx_Norm.NDFRT
   )

   save(
      list=c(
         "Def_Association", "Def_Concept", "DefiningConcepts", "DefiningRoles",
         "Def_Kind", "Def_Namespace", "Def_Property", "Def_Qualifier",
         "Def_QualifierValue", "Def_Role", "Def_RoleValue",
         "ConceptAssociation", "ConceptProperty",
         "RX_Norm", "Rx_Norm.NDFRT"
      ),
      file="NDFRT.Rdata"
   )
}
