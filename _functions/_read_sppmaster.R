read_sppmaster <- function(database = "D:/OneDrive/BCSpeciesList/SpeciesTaxonomyMaster.accdb", project = "USysAllSpecs"){
  sppmaster <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", database,";"))
  taxon.all  <- dbReadTable(sppmaster, "USysAllSpecs")
  dbDisconnect(sppmaster)
  return(taxon.all)
}
