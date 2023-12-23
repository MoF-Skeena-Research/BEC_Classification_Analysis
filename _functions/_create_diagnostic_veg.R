##convert cover + constancy into importance value for analysis
## apply to table built from VegdatSUsummary function
#vegsum = veg_anal.tree; minimportance = 1; minconstancy = 60; noiseconstancy = 40; minplots = 0; covadj = .75
create_diagnostic_veg <- function(veg.dat, su, minimportance = 0.5, minconstancy = .6, noiseconstancy = 0, minplots = 5, covadj = 1,
                                  use.ksi = FALSE, ksi, ksi.value, reduce.lifeform = FALSE, reduced.lifeforms, reduction = 1){
  vegsum <- create_su_vegdata(veg.dat, su) %>% create_analysis_vegsum(minimportance = minimportance, minconstancy = minconstancy, noiseconstancy = noiseconstancy, minplots = minplots, covadj = covadj)
  
  vegsum <- vegsum %>% rowwise() %>% mutate(constant_type = ifelse((Constancy >=high_cons_cut & MeanCov >=dom), "cd",
                                                                   ifelse((Constancy >=high_cons_cut & MeanCov <= minor), "cm",
                                                                          ifelse(Constancy >=high_cons_cut, "c", NA))))
  
  
  ###Calculate diagnostic potential
  
  vegsum <- vegsum %>% mutate(diagnostic.potential = ifelse((Constancy >= high_cons_cut & MeanCov >=dom), ((d1+dd1)*1.25)*(Constancy/100), 
                                                            ifelse(Constancy >= high_cons_cut, d1 *(Constancy/100), 0)))
  
  if (isTRUE(reduce.lifeform)){
  vegsum <- left_join(vegsum, taxon.lifeform, by = c("Species" = "Code")) %>% 
    mutate(diagnostic.potential = ifelse(Lifeform %in% reduced.lifeform, (diagnostic.potential  * reduction),diagnostic.potential))%>% select(-Lifeform)
  }
  
  if (isTRUE(use.ksi)){
  vegsum <- vegsum %>% mutate(diagnostic.potential = ifelse(Species %in% ksi, diagnostic.potential * ksi.value,diagnostic.potential))
  }
  
  vegsum <- vegsum %>% dplyr::group_by(SiteUnit) %>% mutate(unit.diag.sum = sum(diagnostic.potential)) 
  return(vegsum)
}
