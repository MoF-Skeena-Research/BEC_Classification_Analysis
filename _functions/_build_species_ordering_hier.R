
build_species_ordering_hier <- function(vdat, vsum = vegSum, code.lump = lump, siteUnits, hier.level = "Association") {
  vegdata <- lump_species(vdat, code.lump)
  hier.grp <- siteUnits %>% dplyr::select({{hier.level}}) %>% distinct
  cluster.choose <- unique(hier.grp[[hier.level]])
  su.choose <- siteUnits %>% filter(hier.grp[[hier.level]] %in% cluster.choose)
  setDT(vegdata)
  vegdata <- merge(su.choose, vegdata, by = "PlotNumber")
  #vegdata <- vegdata[PlotNumber %in% siteUnits$PlotNumber, ]
  # vegData <- vegData %>% filter(bgc %in% BGC)
  #vegdata <- vegdata[clusters %in% cluster.choose, ] %>% data.frame()
  SS <- vegdata %>%
    select(PlotNumber, SiteUnit) %>%
    distinct() %>% pull(SiteUnit)
  veg_anal <- vegdata %>%
    filter(Species %in% vsum$Species) %>%
    select(PlotNumber, Species, Cover) %>%
    group_by(PlotNumber, Species) %>%
    summarise(Cover = sum(Cover, na.rm = TRUE)) %>%
    ungroup() %>%
    data.frame()
  veg_anal <- matrify(veg_anal)
  
  
  n_units <- length(unique(vsum$SiteUnit))
  indval <- indicspecies::multipatt(veg_anal, SS,
                                    control = how(nperm = 9)
  )
  # summary(indval, alpha=1)
  indic.order <- indval$str %>%
    data.frame() %>%
    select(1:(all_of(n_units))) %>%
    rownames_to_column("spp") %>%
    pivot_longer(-spp, names_to = "siteunit", values_to = "indic") %>%
    group_by(spp) %>%
    mutate(max_indic = max(indic)) %>%
    filter(indic == max_indic) %>%
    ungroup() %>%
    left_join(taxon.lifeform, by = c(spp = "Code")) %>%
    arrange(desc(indic))
}
