####Script for producing veg units from density based clusering
### from http://www.sthda.com/english/wiki/wiki.php?id_contents=7940summary 
# Welcome! Related Books: `Practical Guide To Cluster Analysis in R` at https://goo.gl/13EFCZ
##MacKenzie, march 2019

.libPaths("E:/R packages351")
#install.packages("Hmisc")
require(reshape)
require(reshape2)
require(vegan)
require(caret)
require(tcltk)
require(randomForest)
require(Matrix)
require(labdsv)
require(gdata)
require(MASS)
require(openxlsx)
require (C50)
require(tidyr)
require(stringr)
require(rpart)
require(tree)
require(rattle)
require(rpart.plot)
require(partykit)
require(vegclust)
require(standardize)
require(dplyr)
require(tictoc)
require(plyr)
require(Hmisc)
require(ggplot2)
require(ggdendro)
require(pvclust)
require(dendextend)
require(ape)
require (dbscan)
require(factoextra)
require(fpc)
require(cluster)
rm(list=ls())
#wd=tk_choose.dir(); setwd(wd)
#data("wetland")

###Import vegetation plot data produced from 0_BEC_data_import_clean.R script
load("./outputs/VegDat_Clean.RData") ###variable = vegData
vegData <- vegData %>% select(-Type)

VegMatrix <- dcast(vegData, PlotNumber ~ Species)###Convert to site unit by species matrix
Plots <- VegMatrix[1]
rownames(VegMatrix) <- VegMatrix$PlotNumber
VegMatrix <- VegMatrix[-1]
VegMatrix[is.na(VegMatrix)] <- 0
Vegchord = decostand(VegMatrix,"normalize")
dd <- dist(Vegchord)
#hc <- hclust(dd, method = "ward.D2")
flexbeta <- function (dis,beta) 
{
  alpha <- (1-beta)/2
  out <- agnes(dis,meth='flex',par.method=alpha)
  out
}
hc <- flexbeta(dd,-0.25)
hc <- as.hclust(hc)
dend <- as.dendrogram(hc)
####plots from ape package
plot(as.phylo(hc), type = "unrooted", cex = 0.5,no.margin = TRUE, lab4ut = "axial")

plot(as.phylo(hc), cex = 0.5, label.offset = 0.5)
#plot(as.phylo(hc), type = "cladogram", cex = 0.5, label.offset = 0.5)
#plot(as.phylo(hc), type = "fan")

library(cluster)
fit <- kmeans(dd, 10)
#clusplot(Vegchord, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
out <- cbind(Plots, clusterNum = fit$cluster)
head(out)
write.csv(out, "./outputs/ClusterMembership.csv", row.names = FALSE)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(dd, fit$cluster)

##Import SU table of plots desired for cluster analysis
#SUhier <- read.csv("FebHierarchyLevelSU.csv", stringsAsFactors = FALSE)
#### Reduce vegdata to those listed in SUHier

########Function to define optimal eps
dbscan::kNNdistplot(dd, k =  4)
abline(h = 0.4, lty = 2)
###compute DBSAB using two different packages
set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(dd, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(dd, 0.4, 4)
#Make sure that both version produce the same results:
  
  all(res.fpc$cluster == res.db)

 # The result can be visualized as follow:
    
    fviz_cluster(res.fpc, dd, geom = "point")
    
    
  dbscan(data, eps, MinPts = 5, scale = FALSE, 
       method = c("hybrid", "raw", "dist"))
# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(dd, eps = 0.15, MinPts = 5, method = "dist")
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)
# Print DBSCAN
print(db)
## or plot with factoextra
fviz_cluster(db, dd, stand = FALSE, frame = FALSE, geom = "point")
km.res <- kmeans(dd, 5, nstart = 25)
fviz_cluster(km.res, dd, frame = FALSE, geom = "point")




###########################################################################
#####Noise Clustering with predefined classes#######################
###Create training and testing data

vegDat.chord <- decostand(SUsumMatrix[-1], "normalize") ##standardise data
vegMat <- vegDat.chord
vegMat <- cbind(SUsumMatrix[,1],vegDat.chord)

####create training and testing data sets
vegNew <- vegMat[rownames(vegMat) %in% sample(rownames(vegMat), 100, replace = FALSE),] 
vegOld <- vegMat[!(rownames(vegMat) %in% rownames(vegNew)),]
#actualClass <- vegNew[,1:2] ###siteseries to grouping lookup
#rownames(vegNew)  <- vegNew[1] ###set rownames to siteseries
k <- ncol(vegOld)
n <- nrow(vegOld)
###grouping of training set
grouping <- vegOld[1]
vegOld <- vegOld[,-(1)]
vegNew <- vegNew[,-(1)]
k <- ncol(vegOld)
n <- nrow(vegOld)

grouping <- as.vector.factor(grouping)
vegOld.clust <- as.vegclust(vegOld, grouping)###create noise clustering with grouping as classes
###Kmeans Clustering
vegOld.kmclst <- vegclust(x = vegOld[,-1], mobileCenters=5, method = "KM", nstart=20)###create noise clustering with grouping as classes
t(vegOld.kmclst$memb)
###NC Clustering
vegOld.kmclst <- vegclust(x = vegOld[,-1], mobileCenters=5, method = "NC", m=1.2, dnoise=0.8, nstart=20)###create noise clustering with grouping as classes
round(t(vegOld.kmclst$memb), dig=2)
groups = defuzzify(vegOld.kmclst, method="cut", alpha=0.8)$cluster
table(groups)


vegComb <- vegclass(vegOld.clust, vegNew) ##classify vegNew
vegComb.memb <- vegComb[["memb"]]
newGroup <- dematrify(vegComb.memb) ##extract classification
newGroup <- newGroup[,1:2]
colnames(newGroup) <- c("SiteSeries","Class")
newGroup <- cbind(newGroup, actualClass$Class)####merge actual classification for comparison
colnames(newGroup)[3] <- "Actual"

###MDS for visualisation
MDS <- metaMDS(vegNew, distance = "bray", k = 2, trymax = 200)
MDS.df <- as.data.frame(scores(MDS, display = "sites"))###extract mds scores
MDS.df$SiteSeries <- rownames(MDS.df)
MDS.df <- merge(MDS.df,newGroup, by = "SiteSeries") ##merge predicted and actual classification
MDS.df <- MDS.df[,-1]

colnames(MDS.df)[3:4] <- c("Actual","Predict")

ggplot(MDS.df)+
  geom_point(mapping = aes(x = NMDS1, y = NMDS2, colour = Predict), size = 2.5, shape = 17)+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "none")