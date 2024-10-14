## summarizes veg dat using su table
create_veg_sum <- function(vdat, siteUnits, minconstancy = 60, noiseconstancy = 10, minimportance = 0, strata.by = "Layer", BGC) {
  if (strata.by == "Layer") {
    vdat <- lump_species2(vdat, lump = lump)
  } else if (strata.by == "Lifeform") {
    vdat <- vdat <- lump_species(vdat, lump = lump)
  }

  vdat <- merge(vdat, siteUnits, by = "PlotNumber")
  vdat <- vdat[PlotNumber %in% siteUnits$PlotNumber, ]
  # vdat <- vdat %>% filter(bgc %in% BGC)
  vdat <- vdat[bgc %in% BGC, ]

  vdat <- vdat[, if (.N > 1) .SD, by = .(SiteUnit, Species)]
  vdat[, nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  if (strata.by == "Layer") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots),
      importance = (sum(Cover, na.rm = TRUE) / unique(nplots))^(1/2) * (.N / unique(nplots))
    ), by = .(SiteUnit, Species, Layer)]
  } else if (strata.by == "Lifeform") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots),
      importance = (sum(Cover, na.rm = TRUE) / unique(nplots))^(1/2) * (.N / unique(nplots))
    ), by = .(SiteUnit, Species, Lifeform)]
  }
  vdat[, maxcons := max(Constancy), by = .(Species)]
  vdat[, maximportance := max(importance), by = .(Species)]
  vdat <- vdat[maxcons > minconstancy, ]
  vdat <- vdat[Constancy > noiseconstancy, ]
  vdat <- vdat[importance > minimportance, ]
}

