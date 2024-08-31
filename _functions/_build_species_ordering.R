## order species in table using multipatt from the indicspecies package
build_species_ordering <- function(vdat, vsum = vegSum, code.lump = lump, siteUnits, BGC){
  vegdata <- lump_species2(vdat, code.lump) 
  vegdata <- merge(vegdata, siteUnits, by = 'PlotNumber')
  vegdata <- vegdata[PlotNumber %in% siteUnits$PlotNumber, ]
  #vegData <- vegData %>% filter(bgc %in% BGC)
  vegdata <- vegdata[bgc %in% BGC, ] %>% data.frame
  SS <- vegdata %>% select(PlotNumber, SiteUnit) %>% distinct
  SS <- SS$SiteUnit
  veg_anal <- vegdata %>% filter(Species %in% vsum$Species) %>% select(PlotNumber, Species,Cover) %>% group_by(PlotNumber, Species) %>% summarise(Cover = sum(Cover, na.rm = TRUE)) %>% ungroup() %>%  data.frame()
  veg_anal <- matrify(veg_anal)
  
  
  n_units <- length(unique(vsum$SiteUnit))
  indval <- indicspecies::multipatt(veg_anal, SS, 
                                    control = how(nperm=9)) 
  #summary(indval, alpha=1)
  indic.order <- indval$str %>% data.frame  %>% select(1:(all_of(n_units))) %>% rownames_to_column("spp") %>%  pivot_longer(-spp, names_to = "siteunit", values_to = "indic") %>% group_by(spp) %>% mutate(max_indic = max(indic)) %>% filter(indic == max_indic) %>% ungroup() %>% left_join(species, by=c(spp = 'Code')) %>%  arrange(desc(indic)) %>%  mutate(siteunit = str_replace(siteunit, "101", "109")) %>% arrange(siteunit)
  
}