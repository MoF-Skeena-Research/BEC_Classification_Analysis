## summarizes veg dat using su table
create_veg_sum <- function(vdat, siteUnits, minconstancy = 60, noiseconstancy = 20, BGC) {
  lookup <- c(Layer = "Lifeform")
  vdat <- vdat %>%
    rename(any_of(lookup))
    vdat <- lump_species2(vdat, lump=lump) 
  vdat <- merge(vdat, siteUnits, by = 'PlotNumber')
  vdat <- vdat[PlotNumber %in% siteUnits$PlotNumber, ]
  #vdat <- vdat %>% filter(bgc %in% BGC)
  vdat <- vdat[bgc %in% BGC, ]
    vdat <- vdat[,if(.N > 1) .SD, by = .(SiteUnit,Species)]
  vdat[ , nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  vdat <- vdat[,.(
    MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
    Constancy = (.N / unique(nplots)) * 100, 
    nplots = unique(nplots)
    #  ), by = .(SiteUnit, Species, Lifeform)]
  ), by = .(SiteUnit, Species, Layer)]
  vdat[ , maxcons := max(Constancy), by = .(Species)]
  vdat <- vdat[maxcons > minconstancy, ]
  vdat <- vdat[Constancy > noiseconstancy, ]
}
