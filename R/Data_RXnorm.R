createRxNormDataFromDatabase <- function(conn) {
   RXN_cui_Change<- db.q("select * from rxn.rxn_rxncuichanges;",nrows = NULL,verbose = FALSE)
   RXN_cui <- db.q("select * from rxn.rxn_rxncui;",nrows = NULL,verbose = FALSE)
   RXN_ConSo <- db.q("select * from rxn.rxn_rxnconso;",nrows = NULL,verbose = FALSE)
   RXN_archive <- db.q("select * from rxn.rxn_rxnatomarchive;",nrows = NULL,verbose = FALSE)
   RXN_rel <- db.q("select * from rxn.rxn_rxnrel;",nrows = NULL,verbose = FALSE)

   db.disconnect(conn)
   rm(conn)

   save.image("RXNORM.Rdata")
}
