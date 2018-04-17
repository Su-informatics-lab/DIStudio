##  not for Drug kind
##  define is.a "R11" (all kinds), asociation "R12" (drug)
Ontology.NDFRT <- function(kind, DC=Def_Concept, DFC=DefiningConcepts) {
   Ontology <- vector("list", 3)
   names(Ontology) <- c("Nodes", "Arcs", "top")

   Nodes <- DC[DC$kind == kind, c(2, 2, 1)]
   Nodes <- cbind(Nodes, kind=kind)
   rownames(Nodes) <- NULL
   names(Nodes) <- c("id", "code", "c_name", "kind") 

   is.a  <- subset(DFC, DFC$concept_code %in% Nodes$code)
   rownames(is.a) <- NULL
   names(is.a) <- c("from", "to")
   is.a$rel <- "is.a"
   is.a$rel.code <- "R11"

   net <- igraph::graph_from_data_frame(d=is.a, vertices=Nodes, directed=T) 

   Ontology$top <- names(which(igraph::degree(net, mode="out") == 0))
   Ontology$Nodes <- Nodes
   Ontology$Arcs <- is.a

   return(Ontology)
}


## Drug ontology 
Ontology.Drug <- function(rxnorm=Rx_Norm.NDFRT, is.a=DefiningConcepts, association=ConceptAssociation) {
   Drug  <- vector("list", 3)
   names(Drug) <- c("Nodes", "is.a", "association")

   Drug$Nodes <- data.frame(
      id=rxnorm$concept_code,
      code=rxnorm$concept_code,
      c_name=rxnorm$concept_name,
      kind="C8",
      stringsAsFactors=F
   )

   Drug$is.a <- subset(is.a,  is.a$concept_code %in% Drug$Nodes$code)
   names(Drug$is.a ) <- c("from", "to")
   Drug$is.a$rel <- "is.a"
   Drug$is.a$rel.code <- "R11"

   Drug$association <-  subset(association, association$value %in% Drug$Nodes$code)[, c(1, 3)] 
   names(Drug$association)[1:2] <- c("from", "to")
   Drug$association$rel <- "association"
   Drug$association$rel.code <- "R12"

   net <- igraph::graph_from_data_frame(
      d=rbind(Drug$is.a, Drug$association),
      vertices=Drug$Nodes,
      directed=T
   )

   Drug$top <- names(which(igraph::degree(net, mode="out") == 0))

   return(Drug)
}


# find in-between relations and generate links
DrugTo <- function(LinkCodes=NA, Exclude=c("C30", "C36", "C26", "C22", "C52") , DR=DefiningRoles, DRV=Def_RoleValue, DefR=Def_Role) {
   if (all(is.na(LinkCodes))) {
      X <- subset(DRV, !(DRV$name %in% Exclude))
   } else {
      X <- subset(DRV, DRV$name %in% LinkCodes)
   }

   from <- to <- rel <- rel.code <- character(0)

   for (i in 1:nrow(X)) {
      cL <- DR$concept_code[DR$role_value_ids == X$rid[i]]

      from <- c(from, cL); 
      to <- c(to, rep(X$value[i], length(cL)))

      rel.code <- c(rel.code, rep(X$name[i], length(cL)))
      rel <- c(rel, rep(DefR$name[which(DefR$code == X$name[i])], length(cL)))
   }

   Y <- data.frame(
      from=from, to=to,
      rel=rel, rel.code=rel.code,
      stringsAsFactors=F
   )

   return(Y)
}


# find upstream and downstream Nodes (or subgraph)
# kinds: which kinds notes
# subgraph or nodes (igraph)
find_nodes <- function(CUI, net, UD="both", kinds="All", SubGraph=F, NodeOnly=F) {
   if (UD == "both") {
      sc <- union(igraph::subcomponent(net, CUI, "out"), igraph::subcomponent(net, CUI, "in")) 
   }

   if (UD == "up") {
      sc <- igraph::subcomponent(net, CUI, "out")
   }

   if (UD == "down") {
      sc <- igraph::subcomponent(net, CUI, "in")
   }

   if (length(sc) == 1) {
      return(NULL)
   } else {
      if (kinds[1] != "All") {
         sc <- subset(sc, sc$kind %in% kinds)
      }

      if (SubGraph) {
         return(igraph::induced_subgraph(net, sc))
      } else if (NodeOnly) {
         return(sc$code)
      } else {
         return(sc)
      }
   }
}


## Graph of single ontology 
Ontology2Graph <- function(Ont, Drug.rel="is.a") {
   if (Ont$Nodes$kind[1]  != "C8") {
      return(igraph::graph_from_data_frame(
         d=Ont$Arcs,
         vertices=Ont$Nodes,
         directed=T
      ))
   } else {
      if (Drug.rel == "both") {
         return(igraph::graph_from_data_frame(d=rbind(Ont$is.a, Ont$association), vertices=Ont$Nodes, directed=T))
      }

      if (Drug.rel == "is.a") {
         return(igraph::graph_from_data_frame(d=Ont$is.a, vertices=Ont$Nodes, directed=T))
      }

      if (Drug.rel == "association") {
         return(igraph::graph_from_data_frame(d=Ont$association, vertices=Ont$Nodes, directed=T))
      }
   }
}


## Graph of Ontologies with Drug 
Ontologies2Graph <- function(Onts, LinkCodes=NA, Drug=Ont.Drug, Drug.rel="both") {
   nOnts <- length(Onts)
   nodes <- Drug$Nodes

   if (Drug.rel == "is.a") {
      arcs <- Drug$is.a
   } else if (Drug.rel == "association") {
      arcs <- Drug$association
   } else {
      arcs <- rbind(Drug$is.a, Drug$association)
   }

   if (is.na(LinkCodes[1])) {
      arcs <- rbind(arcs, DrugTo())
   } else {
      arcs <- rbind(arcs, DrugTo(LinkCodes))
   }

   for (i in 1:nOnts)  {
      nodes <- rbind(nodes, Onts[[i]]$Nodes)
      arcs <- rbind(arcs, Onts[[i]]$Arcs)
   }

   return(igraph::graph_from_data_frame(d=arcs, vertices= nodes, directed=T))
}  



## 
## coord <- layout.kamada.kawai(g)
## coord2 <- layout.drl(g, use.seed=TRUE, seed=coord)
## coord3 <- layout.drl(g, use.seed=TRUE, seed=coord)
## plot(g, layout=coord2)
## plot(g, layout=coord3)
