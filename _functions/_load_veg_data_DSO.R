load_veg_data_DSO <- function(
    veg_file,
    veg_combine_db  = getOption("bec.lump_db"),
    veg_combine_tbl = "CorrelationII_Lump",
    use_subtaxa     = FALSE,
    species_db      = getOption("bec.species_db")
) {
  if (is.null(veg_combine_db) || !file.exists(veg_combine_db)) {
    stop("Lump database not found.\n",
         "Set bec.lump_db in local_paths.R or pass veg_combine_db.\n",
         "Current value: '", veg_combine_db, "'", call. = FALSE)
  }
  if (!file.exists(veg_file)) {
    stop("Vegetation file not found: '", veg_file, "'", call. = FALSE)
  }
  
  taxon_all <- read_sppmaster_DSO(database = species_db)
  
  taxon_exotic <- taxon_all %>%
    dplyr::filter(Native == "E") %>%
    dplyr::pull(Code)
  
  taxon_lifeform <- taxon_all %>%
    dplyr::filter(Codetype %in% c("U", "X", "D")) %>%
    dplyr::select(Code, ScientificName, EnglishName, Lifeform) %>%
    distinct()
  
  trees <- taxon_lifeform %>%
    dplyr::filter(Lifeform %in% c("1", "2")) %>%
    dplyr::pull(Code)
  
  tree_seedlings <- taxon_lifeform %>%
    dplyr::filter(Lifeform %in% c("1", "2")) %>%
    mutate(Code = paste0(Code, "D")) %>%
    pull(Code)
  
  veg_dat <- readRDS(veg_file) %>%
    dplyr::filter(
      !Species %in% taxon_exotic,
      !startsWith(Species, "POA_PRA")
    )
  
  veglump <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
      "DBQ=", veg_combine_db, ";"
    )
  )
  on.exit(DBI::dbDisconnect(veglump), add = TRUE)
  
  lump <- DBI::dbReadTable(veglump, veg_combine_tbl)
  
  veg_dat_lumped <- combine_taxa(
    vegdata = veg_dat,
    lump = lump,
    use.subtaxa = use_subtaxa
  ) %>%
    dplyr::filter(!Species %in% taxon_exotic) |> distinct()
  
  list(
    veg_dat = veg_dat,
    veg_dat_lumped = veg_dat_lumped,
    taxon_all = taxon_all,
    taxon_lifeform = taxon_lifeform,
    taxon_exotic = taxon_exotic,
    trees = trees,
    tree_seedlings = tree_seedlings,
    lump = lump
  )
}