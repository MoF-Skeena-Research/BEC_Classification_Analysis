### Converts Vpro veg table into long form analysis set

convert_vpro_veg <- function(plot.veg, taxon.lifeform){
  plot.veg <- as.data.table(plot.veg)
  fields = c("PlotNumber", "Species", "TotalA", "TotalB", "Cover6", "Cover7")
  vegdat <- plot.veg %>% dplyr::select(fields) 
  vegdat[, Cover := rowSums(.SD, na.rm = TRUE), .SDcols = 3:6]
  vegdat <- vegdat %>% dplyr::select(PlotNumber, Species,Cover)
  ### add in lifeform
  vegdat[setDT(taxon.lifeform), "Lifeform" := Lifeform, on = c("Species" = "Code")]
  vegdat <- vegdat %>% filter(Cover > 0 )
  return(vegdat)
}
  