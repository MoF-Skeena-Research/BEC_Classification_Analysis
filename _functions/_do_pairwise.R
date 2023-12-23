##convert cover + constancy into importance value for analysis
## apply to table built from VegdatSUsummary function
#vegsum = veg_anal.tree; minimportance = 1; minconstancy = 60; noiseconstancy = 40; minplots = 0; covadj = .75
do_pairwise <- function(veg.dat, su, minimportance = 0.5, minconstancy = .6, noiseconstancy = 0, minplots = 1, covadj = 1, 
                        use.ksi = FALSE, ksi = NULL, ksi.value = 1, reduce.lifeform = TRUE, reduced.lifeforms = NULL, reduction = NULL){
    
 vegsum <-  create_diagnostic_veg(veg.dat, su, minimportance=minimportance, minconstancy=minconstancy, noiseconstancy=noiseconstancy, minplots=minplots, covadj=covadj, 
                                  use.ksi=use.ksi, ksi=ksi, ksi.value=ksi.value, reduce.lifeform = reduce.lifeform, reduced.lifeforms = reduced.lifeforms)
  
    pairs <- unique(vegsum$SiteUnit) %>% combn(m=2) %>% t %>% data.frame %>% dplyr::rename(Unit1 = 1, Unit2 = 2) %>% arrange(Unit1)
##for two-way pairs
pairs <- expand.grid(x = unique(vegsum$SiteUnit), y= unique(vegsum$SiteUnit) ) %>%  dplyr::rename(Unit1 = 1, Unit2 = 2)
pair = pairs#[1,]

setDT(pair)
setDT(vegsum)
setDT(taxon.lifeform)

# vegsum.pairs1
vegsum.pairs1 <- pair[vegsum, on = c("Unit1" = "SiteUnit"), allow.cartesian = TRUE]

# vegsum.pairs2
vegsum.pairs2 <- pair[vegsum, on = c("Unit2" = "SiteUnit"), allow.cartesian = TRUE]

# vegsum.pairs
vegsum.pairs <- merge(vegsum.pairs1, vegsum.pairs2, by = c("Unit1", "Unit2", "Species"), all = TRUE) 
vegsum.pairs <- vegsum.pairs %>% mutate_if(is.numeric, replace_na, replace = 0)
# vegsum.pairs with taxon.lifeform
vegsum.pairs <- merge(vegsum.pairs, taxon.lifeform, by.x = c("Species"), by.y = c("Code"))

setDT(vegsum.pairs)[, c("cov.diff", "const.diff") := .(MeanCov.x - MeanCov.y, Constancy.x - Constancy.y)]
setkey(vegsum.pairs, "Unit1", "Unit2", "Species")

vegsum.pairs[, `:=`(
  shared.diag = pmin(diagnostic.potential.x, diagnostic.potential.y, na.rm = TRUE)
), by = .(Unit1, Unit2, Species)]


# Differential

vegsum.temp = vegsum.pairs
vegsum.pairs[, d.type.x := cut(const.diff, breaks = const_cut, labels = const_labels)]
vegsum.pairs[, d.type.y := cut(const.diff, breaks = const_cut2, labels = const_labels2)]

# Assuming vegsum.pairs is a data.table
vegsum.pairs[, d.type.x := ifelse(Constancy.x < high_cons_cut, NA, paste0("",d.type.x))]
vegsum.pairs[, d.type.y := ifelse(Constancy.y < high_cons_cut, NA, paste0("",d.type.y))]

### d points                           
vegsum.pairs[, c("d.points.x", "d.points.y") := .(
  ifelse(d.type.x %in% "d1", 4,
         ifelse(d.type.x %in% "d2", 3,
                ifelse(d.type.x %in% "d3", 1, NA_real_))),
  ifelse(d.type.y %in% "d1", 4,
         ifelse(d.type.y %in% "d2", 3,
                ifelse(d.type.y %in% "d3", 1, NA_real_)))
)]  

dom.hi = 10
dom.lo = 5
####Unit1 dd points
vegsum.pairs[, `:=`(
  cover.pts.x = ifelse(MeanCov.x >= dom.hi, (MeanCov.x / MeanCov.y) / 10,
                       ifelse(MeanCov.x < dom.hi & MeanCov.x >= dom.lo, MeanCov.x / (MeanCov.y * 1.5) / 10, 0)))]
vegsum.pairs[, `:=`( 
  cover.pts.x = ifelse(cover.pts.x > 1, 1,
                       ifelse(cover.pts.x < 0.3, 0, cover.pts.x)))]#,
vegsum.pairs[, `:=`(  
  dd.pts.x = ifelse(MeanCov.x >= dom.hi, (cover.pts.x * 4),
                    ifelse(MeanCov.x < dom.hi & MeanCov.x > dom.lo, ((cover.pts.x * 4) / (MeanCov.x / 10)), 0))
)]


vegsum.pairs[, dd.type.x := fifelse((dd.pts.x >=4) & (constant_type.x == "cd"), "dd1",
                                    fifelse((dd.pts.x <4 & dd.pts.x >=2) &(constant_type.x == "cd"), "dd2",
                                            fifelse((dd.pts.x >=1.2 & dd.pts.x <2) &(constant_type.x == "cd"), "dd3",
                                                    fifelse((dd.pts.x >0.3 & dd.pts.x <1.2) &(constant_type.x == "cd"), "dd4", NA_character_))))]

###Unit2 dd points
vegsum.pairs[, `:=`(
  cover.pts.y = ifelse(MeanCov.y >= dom.hi, (MeanCov.y / MeanCov.x) / 10,
                       ifelse(MeanCov.y < dom.hi & MeanCov.y >= dom.lo, MeanCov.y / (MeanCov.x * 1.5) / 10, 0)))]
vegsum.pairs[, `:=`( 
  cover.pts.y = ifelse(cover.pts.y > 1, 1,
                       ifelse(cover.pts.y < 0.3, 0, cover.pts.y)))]
vegsum.pairs[, `:=`(  
  dd.pts.y = ifelse(MeanCov.y >= dom.hi, (cover.pts.y * 4),
                    ifelse(MeanCov.y < dom.hi & MeanCov.y > dom.lo, (cover.pts.y * 4) / (MeanCov.y / 10), 0))
)]


vegsum.pairs[, dd.type.y := fifelse((dd.pts.y >=4) & (constant_type.y == "cd"), "dd1",
                                    fifelse((dd.pts.y <4 & dd.pts.y >=2) &(constant_type.y == "cd"), "dd2",
                                            fifelse((dd.pts.y >=1.2 & dd.pts.y <2) &(constant_type.y == "cd"), "dd3",
                                                    fifelse((dd.pts.y >0.3 & dd.pts.y <1.2) &(constant_type.y == "cd"), "dd4", NA_character_))))]


vegsum.pairs[, `:=`(
  dd.points.x = ifelse(dd.type.x %in% "dd1", 4,
                       ifelse(dd.type.x %in% "dd2", 3,
                              ifelse(dd.type.x %in% "dd3", 2,
                                     ifelse(dd.type.x %in% "dd4", 1, NA_real_)))),
  dd.points.y = ifelse(dd.type.y %in% "dd1", 4,
                       ifelse(dd.type.y %in% "dd2", 3,
                              ifelse(dd.type.y %in% "dd3", 2,
                                     ifelse(dd.type.y %in% "dd4", 1, NA_real_))))
)]


# Adjust points for moss layer
if (isTRUE(reduce.lifeform)){ 
  vegsum.pairs[, `:=`(
  d.points.x = ifelse(Lifeform %in% reduced.lifeform, d.points.x * 0.1, d.points.x),
  d.points.y = ifelse(Lifeform %in% reduced.lifeform, d.points.y * 0.1, d.points.y),
  dd.points.x = ifelse(Lifeform %in% reduced.lifeform, dd.points.x * 0.1, dd.points.x),
  dd.points.y = ifelse(Lifeform %in% reduced.lifeform, dd.points.y * 0.1, dd.points.y))]
}
# Sum sum of diagnostic differentials by species
setkey(vegsum.pairs, "Unit1", "Unit2", "Species")

### adjust points for constancy
vegsum.pairs[, `:=`(
  diag.points.x = sum(d.points.x, dd.points.x, na.rm = TRUE) * (Constancy.x / 100),
  diag.points.y = sum(d.points.y, dd.points.y, na.rm = TRUE) * (Constancy.y / 100)
), by = .(Unit1, Unit2, Species)]

## adjust ponits for being d as well as dd
vegsum.pairs[, `:=`(
  diag.points.x = ifelse((!is.na(d.type.x) & !is.na(dd.type.x)), (diag.points.x * 1.25), diag.points.x),
  diag.points.y = ifelse((!is.na(d.type.y) & !is.na(dd.type.y)), (diag.points.y * 1.25), diag.points.y)
), by = .(Unit1, Unit2, Species)]

## adjust points for key indicators
if (isTRUE(use.ksi)){
vegsum.pairs[, `:=`(
  diag.points.x = ifelse(Species %in% key.site.indicators, (diag.points.x * ksi.value), diag.points.x),
  diag.points.y = ifelse(Species %in% key.site.indicators, (diag.points.x * ksi.value), diag.points.y)
), by = .(Unit1, Unit2, Species)]
}
## adjust points for key moss layer indicators
# vegsum.pairs[, `:=`(
#   diag.points.x = ifelse(Species %in% key.moss.indicators, (diag.points.x * (3/reduced.lifeform)), diag.points.x),
#   diag.points.y = ifelse(Species %in% key.moss.indicators, (diag.points.x * (3/reduced.lifeform)), diag.points.y)
# ), by = .(Unit1, Unit2, Species)]

# Sums by pair
vegsum.pairs[, `:=`(
  sum.shared.diag = sum(shared.diag),
  diag.tot.x = sum(diag.points.x, na.rm = TRUE),
  diag.tot.y = sum(diag.points.y, na.rm = TRUE)
), by = .(Unit1, Unit2)]


vegsum.pairs[ ,diag.tot :=rowSums(.SD, na.rm = TRUE), .SDcols = c("diag.tot.x", "diag.tot.y" ), by = .(Unit1, Unit2)]

vegsum.pairs[, `:=`(
  diag.potential.tot = sum(max(diagnostic.potential.x), max(diagnostic.potential.y, na.rm = TRUE)),
  diag.ratio = sum.shared.diag / (diag.tot + sum.shared.diag)
), by = .(Unit1, Unit2)]
# Ungroup the data.table
vegsum.pairs <- vegsum.pairs[, .SD, .SDcols = names(vegsum.pairs)]
return(vegsum.pairs)
}
