 # su = SU
 # vegdata
create_su_vegdata <- function(vegdata, su, constancycut = 0){
  vegdat <- as.data.table(vegdata)
  vegdat[su, SiteUnit := i.SiteUnit, on = "PlotNumber"]
  vegdat <- vegdat[!is.na(SiteUnit) & SiteUnit != "",]
  
  vegdat <- unique(vegdat[!is.na(SiteUnit) & SiteUnit != "",])
  vegdat3 <- vegdat[,if(.N > 1) .SD, by = .(SiteUnit,Species)]
  vegdat3[,nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  vegsum <- vegdat3[,.(MeanCov = sum(totalcov, na.rm = TRUE)/nplots[1], Constancy = (.N/nplots[1])*100, nplots = nplots[1]), by = .(SiteUnit,Species)]
  return(vegsum)
}
