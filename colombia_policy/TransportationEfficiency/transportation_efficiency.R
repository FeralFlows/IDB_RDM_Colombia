library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to (1) reduce VMT demand as a function of income growth, and 
# (2) transition more VMT in buses versus LDV.

# xml so it resembles building_det.xml. This requires a unique header, which is in colombia_policy/headers_rdm.txt.

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/TransportationEfficiency/')

# Read in base data (taken from Gcam data system in gcamdata/outputs after building). We will modify the income
# elasticity in the base data to create a new set of assumptions and associated file.
L254.IncomeElasticity_trn <- read.csv(paste0(base_directory,'L254.IncomeElasticity_trn.csv'), skip=1)

L254.IncomeElasticity_trn.Colombia <- L254.IncomeElasticity_trn %>% 
  filter(region=='Colombia') %>%
  filter(energy.final.demand=='trn_pass')
L254.IncomeElasticity_trn.Colombia$income.elasticity[L254.IncomeElasticity_trn.Colombia$year==2020] <- 1.3
L254.IncomeElasticity_trn.Colombia$income.elasticity[L254.IncomeElasticity_trn.Colombia$year==2025] <- 1.6
L254.IncomeElasticity_trn.Colombia$income.elasticity[L254.IncomeElasticity_trn.Colombia$year>=2030] <- 2
L254.IncomeElasticity_trn.Colombia <- L254.IncomeElasticity_trn.Colombia %>% 
  mutate(perCapitaBased=1) %>% 
  mutate('final-energy-consumer'=1)

# Reorder columns
L254.IncomeElasticity_trn.Colombia <- L254.IncomeElasticity_trn.Colombia[c('region', 'energy.final.demand',	
                                                                           'perCapitaBased', 'final-energy-consumer', 
                                                                           'year', 'income.elasticity')]
# Auto-produce XML from CSV
gcamdata_variable <- "IncomeElasticity_trn" #  "AgProdChange"
imported_data <- tibble::as.tibble(L254.IncomeElasticity_trn.Colombia)
xmlpath <- paste0(base_directory, 'IncomeElasticity_trn.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()

# Next, increase the shareweight of buses relative to ldv within trn_pass_road.
#Buses
TranSubsecInterp_Bus_1 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road', 'tranSubsector' = 'Bus', 
                                   'apply-to' = 'share-weight', 'delete' = 1,  'from-year' = 2010, 'to-year' = 2030, 
                                   'to-value' = 2, 'interpolation-function' = 'linear')
TranSubsecInterp_Bus_2 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road', 'tranSubsector' = 'Bus', 
                                     'apply-to' = 'share-weight', 'delete' = 1,  'from-year' = 2030, 'to-year' = 2050, 
                                     'to-value' = 3, 'interpolation-function' = 'linear')
TranSubsecInterp_Bus_3 <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road', 'tranSubsector' = 'Bus', 
                                     'apply-to' = 'share-weight', 'delete' = 1,  'from-year' = 2050, 'to-year' = 2100, 
                                     'to-value' = 3, 'interpolation-function' = 'linear')
TranSubsecInterp_Bus <- rbind(TranSubsecInterp_Bus_1, TranSubsecInterp_Bus_2, TranSubsecInterp_Bus_3)

#LDV
TranSubsecInterp_LDV <- data.frame('region' = 'Colombia', 'supplysector' = 'trn_pass_road', 'tranSubsector' = 'LDV', 
                                   'apply-to' = 'share-weight', 'delete' = 1,  'from-year' = 2010, 'to-year' = 2100, 
                                   'to-value' = 1.001, 'interpolation-function' = 'linear')
TranSubsecInterp <- rbind(TranSubsecInterp_Bus, TranSubsecInterp_LDV)
# Auto-produce XML from CSV
gcamdata_variable <- "TranSubsecInterp" #  "AgProdChange"
imported_data <- tibble::as.tibble(TranSubsecInterp)
xmlpath <- paste0(base_directory, 'TranSubsecInterp.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()