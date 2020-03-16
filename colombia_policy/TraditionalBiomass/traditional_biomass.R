library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to reduce traditional biomass usage in Colombia (residential heating and
# residential others). The resulting xml is designed to resemble similar details in building_det.xml. 
# This requires a unique header, which appears in colombia_policy/headers_rdm.txt.

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/TraditionalBiomass/')
# Residential heating
TradBiomass_SW_ResidHeat_1 <- data.frame('region' = 'Colombia', 'supplysector' = 'resid heating', 
                                       'subsector' = 'traditional biomass', 'apply-to' = 'share-weight', 'delete' = 1,
                                       'from-year' = 2010, 'to-year' = 2030, 'to-value' = 0.5, 
                                       'interpolation-function' = 'linear')
TradBiomass_SW_ResidHeat_2 <- data.frame('region' = 'Colombia', 'supplysector' = 'resid heating', 
                                         'subsector' = 'traditional biomass', 'apply-to' = 'share-weight', 'delete' = 1,
                                         'from-year' = 2030, 'to-year' = 2050, 'to-value' = 0.3, 
                                         'interpolation-function' = 'linear')
TradBiomass_SW_ResidHea <- rbind(TradBiomass_SW_ResidHeat_1, TradBiomass_SW_ResidHeat_2)
# Residential others
TradBiomass_SW_ResidOthers_1 <- data.frame('region' = 'Colombia', 'supplysector' = 'resid others', 
                                         'subsector' = 'traditional biomass', 'apply-to' = 'share-weight', 'delete' = 1,
                                         'from-year' = 2010, 'to-year' = 2030, 'to-value' = 0.5, 
                                         'interpolation-function' = 'linear')
TradBiomass_SW_ResidOthers_2 <- data.frame('region' = 'Colombia', 'supplysector' = 'resid others', 
                                         'subsector' = 'traditional biomass', 'apply-to' = 'share-weight', 'delete' = 1,
                                         'from-year' = 2030, 'to-year' = 2050, 'to-value' = 0.3, 
                                         'interpolation-function' = 'linear')
TradBiomass_SW_ResidOthers <- rbind(TradBiomass_SW_ResidOthers_1, TradBiomass_SW_ResidOthers_2)
TradBiomass_SW <- rbind(TradBiomass_SW_ResidHea, TradBiomass_SW_ResidOthers)

# Auto-produce XML from CSV
gcamdata_variable <- "TradBiomass_SW" #  "AgProdChange"
imported_data <- tibble::as.tibble(TradBiomass_SW)
xmlpath <- paste0(base_directory, 'TradBiomass_SW.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()