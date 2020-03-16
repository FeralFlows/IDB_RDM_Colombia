library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to (1) increase electric vehicle (EV) usage by 2030 and 2050 per Colombian
# policy. This file builds XML so it it resembles Transportation_UCD_CORE.xml. 
# This requires a unique header, which is in colombia_policy/headers_rdm.txt.

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/ElectricVehicle/')

# Next, increase the shareweight of BEV technology within each LDV mode.
modes <- c('Compact Car', 'Large Car and SUV', 'Mini Car', 'Subcompact Car')
LDV_BEV_Interp <- data.frame("region" = character(), 'supplysector' = character(), 'trn_pass_road_LDV_4W' = character(), 
                              'tranSubsector' = character(), 'stub-technology' = numeric(), 'apply-to' = character(),
                              'delete' = numeric(), 'from-year' = numeric(), 'to-year' = numeric(), 
                              'to-value' = numeric(), 'interpolation-function' = character())  # Create base data frame
for(mode in modes){
  LDV_BEV_Interp_1 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road_LDV_4W', 'tranSubsector' = mode,
                               'stub-technology' = 'BEV', 'apply-to' = 'share-weight', 'delete' = 1,  
                               'from-year' = 2010, 'to-year' = 2030, 'to-value' = 2, 'interpolation-function' = 'linear')  
  LDV_BEV_Interp_2 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road_LDV_4W', 'tranSubsector' = mode,
                               'stub-technology' = 'BEV', 'apply-to' = 'share-weight', 'delete' = 1,  
                               'from-year' = 2030, 'to-year' = 2050, 'to-value' = 3, 'interpolation-function' = 'linear')  
  LDV_BEV_Interp_3 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road_LDV_4W', 'tranSubsector' = mode,
                               'stub-technology' = 'BEV', 'apply-to' = 'share-weight', 'delete' = 1,  
                               'from-year' = 2050, 'to-year' = 2100, 'to-value' = 4, 'interpolation-function' = 'linear')
  LDV_BEV_Interp <- rbind(LDV_BEV_Interp, LDV_BEV_Interp_1, LDV_BEV_Interp_2, LDV_BEV_Interp_3)
}

# Auto-produce XML from CSV
gcamdata_variable <- "LDV_BEV_Interp" #  "AgProdChange"
imported_data <- tibble::as.tibble(LDV_BEV_Interp)
xmlpath <- paste0(base_directory, 'LDV_BEV_Interp.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()