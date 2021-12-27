##Tests if information theory
require(philentropy)
require(dplyr)
require(tidyverse)
require(data.table)
test_raw <- fread("./inputs/infotheory_testdata.csv") 
test <- test_raw %>% column_to_rownames("siteunits") %>% as.matrix
test <- test^0.5
test[test < 0.1] <- NA 
test[test < 1.1] <- 0.1 
test[is.na(test)] <- 0
test_jac <- philentropy::distance(test, method = "jaccard")
test_jac2 <- 1- test_jac
test_jac2
test_js <- philentropy::distance(test, method = "jensen-shannon")
JSD(test_js, est.prob = "empirical")
test_fid <- philentropy::distance(test, method = "divergence")
test_sor <- philentropy::distance(test, method = "sorensen")
JSD(test, est.prob = "empirical")
KL(test_sor)
##convert cover + constancy into importance value for analysis
