read_sppmaster_DSO <- function(database = getOption("bec.species_db"),
                           table = "USysAllSpecs") {
  if (is.null(database) || !nzchar(database) || !file.exists(database)) {
    stop("Species database not found. Set it first with:\n",
         "options(bec.species_db = 'path/to/SpeciesTaxonomyMaster.accdb')\n",
         "Current value: '", database, "'", call. = FALSE)
  }
  sppmaster <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", database, ";"
    )
  )
  on.exit(DBI::dbDisconnect(sppmaster), add = TRUE)
  DBI::dbReadTable(sppmaster, table)
}