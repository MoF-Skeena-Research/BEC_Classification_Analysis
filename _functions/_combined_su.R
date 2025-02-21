## combines all _SU tables in a database into a single SU table
combined_su <- function(db){
  correlation <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", db,";"))
  
  all_su <- lapply(setNames(nm = (dbListTables(correlation)%>% 
                                    str_subset("_SU"))), dbReadTable, conn = correlation)
  dbDisconnect(correlation)
  SU <- do.call(rbind.data.frame, all_su)
 SU <- SU %>% mutate(bgc = substr(SiteUnit,1,9)) %>% drop_na() %>% distinct(PlotNumber, .keep_all = TRUE) %>%
   arrange(desc(PlotNumber)) %>% mutate(bgc = gsub(" ", "", bgc, fixed = TRUE))
  ss.unique <- SU %>% select(SiteUnit) %>% distinct
  su <- SU %>% dplyr::select(PlotNumber, SiteUnit, bgc)
  su$SiteUnit.orig <- su$SiteUnit
  su <- su %>% mutate(SiteUnit = gsub("/", "_", su$SiteUnit)) 
  su <- su %>% mutate(SiteUnit = gsub(" ", "", su$SiteUnit, fixed = TRUE)) 
  #su.lookup <- su
  #su <- su %>% select(-SiteUnit.orig)
  su <- su %>% 
    filter(!str_detect(SiteUnit, '[$]')) %>% filter(!str_detect(SiteUnit, "^x"))
  return(su)
}
