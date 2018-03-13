createRxNormDataFromDatabase <- function(conn) {
   RXN_cui_Change<- PivotalR::db.q("select * from rxn.rxn_rxncuichanges;",nrows = NULL,verbose = FALSE)
   RXN_cui <- PivotalR::db.q("select * from rxn.rxn_rxncui;",nrows = NULL,verbose = FALSE)
   RXN_ConSo <- PivotalR::db.q("select * from rxn.rxn_rxnconso;",nrows = NULL,verbose = FALSE)
   RXN_archive <- PivotalR::db.q("select * from rxn.rxn_rxnatomarchive;",nrows = NULL,verbose = FALSE)
   RXN_rel <- PivotalR::db.q("select * from rxn.rxn_rxnrel;",nrows = NULL,verbose = FALSE)

   save.image("RXNORM.Rdata")
}
