###Optional application of lump species
#lumpfile = LUMP
lump_species <- function(vegdata, lumpfile, use.subtaxa = FALSE){
setDT(vegdata)[setDT(lumpfile), "Species" := LumpCode, on = c("Species" = "SppCode")]
 if (isFALSE(use.subtaxa)){
   vegdata$Species <-   gsub('[0-9]+', '', vegdata$Species)
 }
return(vegdata)
}