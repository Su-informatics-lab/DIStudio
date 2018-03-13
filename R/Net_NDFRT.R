Ontology.NDFRT <- function(kind, DC, DFC) {
   Ontology <- vector("list",3)
   names(Ontology) <- c("Nodes","Arcs","top")

   Nodes <- DC[DC$kind == kind, c(2,2,1)]
   Nodes <- cbind(Nodes, kind = kind)
   rownames(Nodes) <- NULL
   names(Nodes) <- c("id", "code", "c_name", "kind") 

   is.a  <- subset(DFC, DFC$concept_code %in% Nodes$code)
   rownames(is.a) <- NULL
   names(is.a) <- c("from", "to")

   net <- igraph::graph_from_data_frame(d=is.a, vertices=Nodes, directed=T) 

   Ontology$top <- names(which(igraph::degree(net, mode = "out") == 0))
   Ontology$Nodes <- Nodes
   Ontology$Arcs <- is.a

   return(Ontology)
}

DrugTo <- function(LinkCodes, DR, DRV, Exclude=c("C30", "C36", "C26", "C22", "C52")) {
   if (all(is.na(LinkCodes))) {
      X <- subset(DRV,!(DRV$name %in% Exclude))
   } else {
      X <- subset(DRV,DRV$name %in% LinkCodes)
   }

   from <- to <- character(0)
   for (i in 1:nrow(X)) {
      cL <- DR$concept_code[DR$role_value_ids == X$rid[i]]
      from <- c(from, cL); 
      to <- c(to, rep(X$value[i], length(cL)))
   }

   Y <- data.frame(from=from , to=to, stringsAsFactors=F)

   return(Y)
}


Node2Top <- function(CUI, net, TopC) {
   ridx <- 1
   AllNodes <- NA

   while (is.na(AllNodes[1])) {
      paths <- all_simple_paths(net, CUI, TopC[ridx], "out")  

      if (length(paths) > 0 ) {
         AllNodes <- paths[[1]]
      } else {
         ridx <- ridx + 1
      }
   }

   if (length(path) > 1) {
      for (j in 2:length(paths)) {
         AllNodes <- union(AllNodes, paths[[j]]) 
      }
   }
   
   for (i in TopC[-(1:ridx)]) {
      paths <- all_simple_paths(net, CUI, i, "out")  

      if (length(paths) > 0) {
         for (j in 1:length(paths)) {
            AllNodes <- union(AllNodes, paths[[j]])
         }
      }
   }

   return(AllNodes)
}


buildNdfrtNetworks <- function() {
   load("NDFRT.Rdata")


   ######################################## DRUG kind in NDFRT is not an Ontotlogy

   NDFRT.Drug  <- vector("list",4)
   names(NDFRT.Drug) <- c("Nodes","Arcs","is.a","association")

   NDFRT.Drug$Nodes <- data.frame(
      id=Rx_Norm.NDFRT$concept_code,
      code=Rx_Norm.NDFRT$concept_code,
      c_name=Rx_Norm.NDFRT$concept_name,
      stringsAsFactors=F
   )

   NDFRT.Drug$is.a <- subset(DefiningConcepts, DefiningConcepts$concept_code %in% NDFRT.Drug$Nodes$code)
   names(NDFRT.Drug$is.a) <- c("from", "to")

   NDFRT.Drug$association <- subset(ConceptAssociation, ConceptAssociation$value %in% NDFRT.Drug$Nodes$code)[, c(1, 3, 4)] 

   #Links.Assoc <- rbind(NDFRT.Drug$association[,c(1,2)],NDFRT.Drug$association[,c(2,1)])
   Links.Assoc <- NDFRT.Drug$association[, c(1, 2)]
   names(Links.Assoc) <- c("from", "to")

   NDFRT.Drug$Arcs <- unique(rbind(NDFRT.Drug$is.a, Links.Assoc))
   rownames(NDFRT.Drug$Arcs) <- NULL


   #################### Create Ontology of other Kind

   Ont.MoA <- Ontology.NDFRT("C12", Def_Concept, DefiningConcepts)
   Ont.DISEASE <- Ontology.NDFRT("C16", Def_Concept, DefiningConcepts)
   Ont.INGREDIENT <- Ontology.NDFRT("C10", Def_Concept, DefiningConcepts)
   Ont.DOSE_FORM <- Ontology.NDFRT("C18", Def_Concept, DefiningConcepts)
   Ont.THERAPEUTIC_CATEGORY <- Ontology.NDFRT("K20", Def_Concept, DefiningConcepts)
   Ont.PHARMACOKINETICS <- Ontology.NDFRT("C14", Def_Concept, DefiningConcepts)
   Ont.PHYSIOLOGIC_EFFECT <- Ontology.NDFRT("C6", Def_Concept, DefiningConcepts)

   save(
      NDFRT.Drug,
      Ont.MoA,
      Ont.DISEASE,
      Ont.INGREDIENT,
      Ont.DOSE_FORM,
      Ont.THERAPEUTIC_CATEGORY,
      Ont.PHARMACOKINETICS,
      Ont.PHYSIOLOGIC_EFFECT,
      file = "Ontologies.Rdata"
   )


   Tops <- c(
      Ont.MoA$top,
      Ont.DISEASE$top,
      Ont.DOSE_FORM$top,
      Ont.INGREDIENT$top,
      Ont.PHARMACOKINETICS$top,
      Ont.PHYSIOLOGIC_EFFECT$top,
      Ont.THERAPEUTIC_CATEGORY$top
   )


   ### pan net ## without CI_*

   Nodes_all <- Def_Concept[, c(2, 2, 1, 6)]
   names(Nodes_all) <-  c("id", "code", "c_name", "kind") 
   Links_all <- DrugTo(LinkCodes=NA, DR=DefiningRoles, DRV=Def_RoleValue)

   is.a_all <- DefiningConcepts
   names(is.a_all) <- c("from", "to")

   net_Pan <- igraph::graph_from_data_frame(
      d=unique(rbind(NDFRT.Drug$Arcs, Links_all, is.a_all)), 
      vertices=Nodes_all,
      directed=T
   )

   save(Tops, net_Pan, file="Pan_net.Rdata")


   ## NET_Drug_MOA : has_MoA  

   LinkCodes <- Def_Role$code[grep("has_MoA", Def_Role$name)]

   k <- DrugTo(LinkCodes=LinkCodes, DR=DefiningRoles, DRV=Def_RoleValue) 
   net_MoA_ <- igraph::graph_from_data_frame(
      d=unique(rbind(NDFRT.Drug$Arcs, DrugTo(LinkCodes=LinkCodes, DR=DefiningRoles, DRV=Def_RoleValue), Ont.MoA$Arc)), 
      vertices=rbind(NDFRT.Drug$Nodes, Ont.MoA$Nodes[, 1:3]),
      directed=T
   )
}
