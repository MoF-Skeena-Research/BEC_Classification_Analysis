cluster_groups <- function(unit.compare, cut.level = .2, minclus = 2, group.label = "Association"){
  compared <- unit.compare 
  dis.matrix <- bec_dist_matrix(compared) 
  ss_clst <- agnes(dis.matrix,
                   diss = TRUE, stand = TRUE,
                   method = "average")
  dendro_hc <- as.hclust(ss_clst)
  dend.co <- stats::cophenetic(dendro_hc)
  dend.dis <- as.dist(dis.matrix)
  cluster_grps <- cutreeHybrid(dendro_hc, distM = dis.matrix, cutHeight = cut.level,
                               minClusterSize = minclus, deepSplit = 2)
  groups <- cluster_grps$labels
  su_grps <- cbind(as.data.frame(groups), as.data.frame(row.names(dis.matrix))) %>% rename(!!group.label := groups, SiteUnit = 2 )
  return( su_grps)
}
