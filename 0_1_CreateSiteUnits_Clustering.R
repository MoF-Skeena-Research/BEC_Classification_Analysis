---
title: "Cluster of Plot Data"
author: "William H MacKenzie"
date: "18/12/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
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
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
