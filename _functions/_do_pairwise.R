##convert cover + constancy into importance value for analysis
## apply to table built from VegdatSUsummary function
# #vegsum = veg_anal.tree; minimportance = 1; minconstancy = 60; noiseconstancy = 40; minplots = 0; covadj = .75
# veg.dat = veg.dat2; su = su2; minimportance = 0.5; minconstancy = 57; noiseconstancy = 0; minplots = 1; covadj = .5
#  use.ksi = TRUE; ksi = NULL; ksi.value = 1; reduce.lifeform = TRUE; reduced.lifeforms = c(8, 9, 10, 11); reduction = .1
#  key.site.indicators <- c("TSUGMER")### priorities for zonal comparison, "THUJPLI", "TSUGHET", "RHODALB", "CALARUB"
#  ksi.value = 2
#  reduced.lifeforms = c(8, 9, 10, 11)## reduce diagnostic value of lifeforms for association development
#  reduce.multiplier = .1
#  round = 1

# vegsum <- fread("test_vegsum.csv")
do_pairwise <- function(veg.dat, su, minimportance = 0.5, minconstancy = 60, noiseconstancy = 0, minplots = 1, covadj = .5, 
                        use.ksi = FALSE, ksi = NULL, ksi.value = 1, reduce.lifeform = FALSE, reduced.lifeforms = NULL, reduction = NULL, domcov = 10, minor = 1){
    
###---Create vegetation summary
  su.choice <- su %>% select(SiteUnit)
  vegdat <- as.data.table(veg.dat)
  vegdat[su, SiteUnit := i.SiteUnit, on = "PlotNumber"] ## limit data to those listed in SiteUnit
  vegdat <- vegdat[!is.na(SiteUnit) & SiteUnit != "",]
  vegdat <- unique(vegdat[!is.na(SiteUnit) & SiteUnit != "",])
  vegdat3 <- vegdat[,if(.N > 1) .SD, by = .(SiteUnit,Species)]
  vegdat3[,nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  vegsum <- vegdat3[,.(MeanCov = sum(Cover, na.rm = TRUE)/nplots[1], Constancy = (.N/nplots[1])*100, nplots = nplots[1]), by = .(SiteUnit,Species)]
  
   ##--------Create the analysis set
  vegsum <- as.data.frame(vegsum)
  vegsum$MeanCov[vegsum$MeanCov >100] <- 100
  vegsum$spp_importance <- vegsum$MeanCov^0.5
  vegsum$spp_importance[vegsum$spp_importance < minimportance] <- NA 
  #vegsum$spp_importance[vegsum$spp_importance < 1.1] <- 0.1 
  #vegsum$spp_importance[is.na(vegsum$spp_importance)] <- 0 
  vegsum <- vegsum %>% dplyr::mutate(spp_importance = spp_importance * (Constancy/100)) %>% dplyr::filter(spp_importance > minimportance)
  ### remove species where the maximum constancy for species is < contancy
  speciesmax <- vegsum  %>% dplyr::group_by(Species) %>% dplyr::summarise(maxcons = max(Constancy)) %>% dplyr::filter(maxcons >= minconstancy)
  vegsum <- vegsum %>%  dplyr::filter(Species %in% speciesmax$Species) %>% dplyr::filter(nplots >= minplots) %>% filter(Constancy > noiseconstancy)
  
  ##--------assign potential diagnostics-----------------
   vegsum <- vegsum %>% rowwise() %>% mutate(constant_type = ifelse((Constancy >=minconstancy & MeanCov >=domcov), "cd",
                                                                   ifelse((Constancy >=minconstancy & MeanCov <= minor), "cm",
                                                                          ifelse(Constancy >= minconstancy, "c", NA))))
  ###Calculate diagnostic potential
  # vegsum <- vegsum %>% mutate(d.potential = ifelse(constant_type %in% c("c", "cd"), 4,
  #                                                  ifelse(constant_type %in% c("cm"), 2,0)))     
  vegsum <- vegsum %>% mutate(d.potential = ifelse(constant_type %in% c("c","cd"), (Constancy^(1/2)/10)*4,
                                                   ifelse(constant_type %in% c("cm"), (Constancy^(1/2)/10)*2,0)))   
  vegsum <- vegsum %>% mutate(d.potential = ifelse(d.potential <0.67, 0, d.potential))
                                                  
    vegsum <- vegsum %>% mutate(dd.potential = ifelse(constant_type == "cd", ((MeanCov^(1/3))), 0)) 
  vegsum <- vegsum %>% mutate(dd.potential = ifelse(dd.potential >4 , 4,
                                                    ifelse(dd.potential <0, 0, dd.potential)))
  
  vegsum <- vegsum %>% mutate(diagnostic.potential = ifelse((Constancy >= minconstancy & MeanCov >=domcov), ((d.potential+dd.potential)*1.25), 
                                                            ifelse(Constancy >= minconstancy, d.potential, 0)))
  
  if (isTRUE(reduce.lifeform)){
    vegsum <- left_join(vegsum, taxon.lifeform, by = c("Species" = "Code")) %>% 
      mutate(diagnostic.potential = ifelse(Lifeform %in% reduced.lifeforms, (diagnostic.potential  * reduction),diagnostic.potential))%>% select(-Lifeform)
  }
  
  if (isTRUE(use.ksi)){
    vegsum <- vegsum %>% mutate(diagnostic.potential = ifelse(Species %in% ksi, diagnostic.potential * ksi.value,diagnostic.potential))
  }
  
  vegsum <- vegsum %>% dplyr::group_by(SiteUnit) %>% mutate(unit.diag.sum = sum(diagnostic.potential))
  vegsum <- vegsum %>% dplyr::group_by(SiteUnit) %>% mutate(n_constants = sum(!is.na(constant_type)))                          
                                                            
  
  ###________________DO PAIR-WISE Comparison_______
  const_cut = c( 37, 60, 80, 101)
  const_cut2 = c(-101,-80, -60, -37)
  const_labels = c("d3", "d2", "d1")
  const_labels2 = c("d1", "d2", "d3")
  
##build pairs  
pairs <- unique(vegsum$SiteUnit) %>% combn(m=2) %>% t %>% data.frame %>% dplyr::rename(Unit1 = 1, Unit2 = 2) %>% arrange(Unit1)
##for two-way pairs
#pairs <- expand.grid(x = unique(vegsum$SiteUnit), y= unique(vegsum$SiteUnit) ) %>%  dplyr::rename(Unit1 = 1, Unit2 = 2)
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

vegsum.pairs <- vegsum.pairs %>%  filter(!is.na(Unit1), !is.na(Unit2)) 
# vegsum.pairs with taxon.lifeform
vegsum.pairs <- merge(vegsum.pairs, taxon.lifeform, by.x = c("Species"), by.y = c("Code"))

setDT(vegsum.pairs)[, c("cov.diff", "const.diff") := .(MeanCov.x - MeanCov.y, Constancy.x - Constancy.y)]
setkey(vegsum.pairs, "Unit1", "Unit2", "Species")

vegsum.pairs[, `:=`(
  shared.diag = pmin(diagnostic.potential.x, diagnostic.potential.y, na.rm = TRUE)
), by = .(Unit1, Unit2, Species)]


# Differential
## assigns differential type based on cut-off ranges
vegsum.temp = vegsum.pairs
vegsum.pairs = vegsum.temp
vegsum.pairs[, d.type.x := cut(const.diff, breaks = const_cut, labels = const_labels)]
vegsum.pairs[, d.type.y := cut(const.diff, breaks = const_cut2, labels = const_labels2)]

## if minimum constancy is not met sets to NA
vegsum.pairs[, d.type.x := ifelse(Constancy.x < minconstancy, NA, paste0("",d.type.x))]
vegsum.pairs[, d.type.y := ifelse(Constancy.y < minconstancy, NA, paste0("",d.type.y))]

### d points  
vegsum.pairs[, `:=`(const2.x = Constancy.x^(1/2),
                    const2.y = Constancy.y^(1/2)
)
][, const2.diff := const2.x - const2.y]

vegsum.pairs[, c("d.points.x", "d.points.y") := .(
  ifelse(constant_type.x %in% c("c", "cd"), (const2.diff/10)*4,
         ifelse(constant_type.x %in% c("cm"), (const2.diff/10)*2, NA_real_)),
  ifelse(constant_type.y %in% c("c", "cd"), (0-(const2.diff/10))*4,
          ifelse(constant_type.y %in% c("cm"), (0-(const2.diff/10))*2, NA_real_))
)] 



# vegsum.pairs[, c("d.points.x", "d.points.y") := .(
#   ifelse(d.type.x %in% "d1", 4,
#          ifelse(d.type.x %in% "d2", 3,
#                 ifelse(d.type.x %in% "d3", 1, NA_real_))),
#   ifelse(d.type.y %in% "d1", 4,
#          ifelse(d.type.y %in% "d2", 3,
#                 ifelse(d.type.y %in% "d3", 1, NA_real_)))
# )]  

## reduce diagnostic value of species with minor cover
vegsum.pairs[, c("d.points.x", "d.points.y") := .(
  ifelse(d.points.x <0.67, NA_real_, d.points.x),
  ifelse(d.points.y <0.67, NA_real_, d.points.y)
)]


# vegsum.pairs[, c("d.points.x", "d.points.y") := .(
#   ifelse(d.points.x %in% "d1" & constant_type.x %in% "cm", 2,
#          ifelse(d.type.x %in% "d2" & constant_type.x %in% "cm", 1,
#                 ifelse(d.type.x %in% "d3" & constant_type.x %in% "cm", 0, d.points.x))),
#   ifelse(d.type.y %in% "d1" & constant_type.x %in% "cm", 2,
#          ifelse(d.type.y %in% "d2" & constant_type.x %in% "cm", 1,
#                 ifelse(d.type.y %in% "d3" & constant_type.x %in% "cm", 0, d.points.y)))
# )]  

## dd points
# Ensure vegsum.pairs is a data.table
# Perform the transformations
vegsum.pairs[, `:=`(cover2.x = MeanCov.x^(1/3),
                    cover2.y = MeanCov.y^(1/3)
)
][, cover2.diff := cover2.x - cover2.y]



# Convert vegsum.pairs to a data.table if it's not already
setDT(vegsum.pairs)

# Add or modify the columns as required
vegsum.pairs[, `:=`(
  dd.points.x = fifelse((constant_type.x == "cd") & (cover2.diff > 1), cover2.diff,
                        fifelse((constant_type.x == "cd") & (cover2.diff < 1 & cover2.diff > 0.66), cover2.diff, 0)),
  dd.points.y = fifelse((constant_type.y == "cd") & (cover2.diff < -1), (0 - cover2.diff),
                        fifelse((constant_type.x == "cd") & (cover2.diff > -1 & cover2.diff < -0.66), 0 - cover2.diff, 0))
)][, `:=`(
  dd.points.x = fifelse(dd.points.x > 4, 4, dd.points.x),
  dd.points.y = fifelse(dd.points.y > 4, 4, dd.points.y)
)]



# Assuming vegsum.pairs is a data.table
vegsum.pairs[, `:=`(
  dd.type.x = ifelse((cover2.diff >= 5) & (constant_type.x == "cd"), "dd1",
                     ifelse((cover2.diff >= 3 & cover2.diff < 4) & (constant_type.x == "cd"), "dd2",
                            ifelse((cover2.diff >= 2 & cover2.diff < 3) & (constant_type.x == "cd"), "dd3",
                                   ifelse((cover2.diff >= 0.3 & cover2.diff < 2) & (constant_type.x == "cd"), "dd4", NA)))),
  dd.type.y = ifelse((cover2.diff  <= -5) & (constant_type.x == "cd"), "dd1",
                     ifelse((cover2.diff  <= -3 & dd.points.y > -4) & (constant_type.x == "cd"), "dd2",
                            ifelse((cover2.diff <= -2 & cover2.diff  > -3) & (constant_type.x == "cd"), "dd3",
                                   ifelse((cover2.diff  <= -0.3 & cover2.diff  > -2) & (constant_type.x == "cd"), "dd4", NA)))
  ))]




# Adjust points for moss layer
if (isTRUE(reduce.lifeform)){ 
  vegsum.pairs[, `:=`(
  d.points.x = ifelse(Lifeform %in% reduced.lifeforms, d.points.x * 0.1, d.points.x),
  d.points.y = ifelse(Lifeform %in% reduced.lifeforms, d.points.y * 0.1, d.points.y),
  dd.points.x = ifelse(Lifeform %in% reduced.lifeforms, dd.points.x * 0.1, dd.points.x),
  dd.points.y = ifelse(Lifeform %in% reduced.lifeforms, dd.points.y * 0.1, dd.points.y))]
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
  diag.points.x = ifelse(Species %in% ksi, (diag.points.x * ksi.value), diag.points.x),
  diag.points.y = ifelse(Species %in% ksi, (diag.points.x * ksi.value), diag.points.y)
), by = .(Unit1, Unit2, Species)]
}
## adjust points for key moss layer indicators
# vegsum.pairs[, `:=`(
#   diag.points.x = ifelse(Species %in% key.moss.indicators, (diag.points.x * (3/reduced.lifeforms)), diag.points.x),
#   diag.points.y = ifelse(Species %in% key.moss.indicators, (diag.points.x * (3/reduced.lifeforms)), diag.points.y)
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
