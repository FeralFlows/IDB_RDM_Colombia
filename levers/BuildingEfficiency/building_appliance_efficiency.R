library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to improve commercial and residential building appliance efficiency in Colombia. 

# The way building appliance efficiencies are handled in GCAM is as follows. Thechnological change rates for USA are set in
# inst/extdata/energy/A44.USA_TechChange.csv, and the multipliers to go from those assumptions to other regions are in:
# inst/extdata/energy/A44.tech_eff_mult_RG3.csv. Note that it was implemented using GCAM 3 regions 
# (e.g., Latin America). These files need to be downloaded from the GCAM LAC branch, not the base values from gcamdata.

# These files above then feed into L244.StubTechEff_bld.csv. It is this file that ultimately feeds into
# building_det.xml--the only relevant XML in this case. So the code below is going to read in L244.StubTechEff_bld.csv,
# then change the values consistent with Colombia's policy, then use gcamdata to just create the
# xml so it resembles building_det.xml. This requires a unique header, which is in colombia_policy/headers_rdm.txt.

# the changes will be applied broadly to a bunch of building equipment within any given fuel type. e.g., water heaters, 
# lights, kitchen appliances, laundry appliances, misc electronics. If you built the CSV directly by making changes in
# the RG3 or USA files, you'd end up changing values for ALL of Latin America. This is why we are writing our own CSV
# directly to only change values for Colombia.

#base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/BuildingEfficiency/')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
base_directory <- getwd()

# Read in base data (taken from Gcam data system after building). We will modify the efficiencies in the base data to
# create a new set of assumptions and associated file.
L244.StubTechEff_bld <- read.csv(file.path(base_directory,'L244.StubTechEff_bld.csv'), skip=1)

supplysectors <- list('comm others'=c('electricity', 'gas'), 'resid others'=c('electricity', 'gas'), 
                      'comm cooling'=c('electricity'), 'resid cooling'=c('electricity'), 
                      'comm heating'=c('electricity', 'gas'), 'resid heating'=c('electricity', 'gas'))

L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld %>% 
  filter(region=='Colombia', supplysector %in% names(supplysectors))
L244.StubTechEff_bld.Colombia_INI <- L244.StubTechEff_bld.Colombia
L244.StubTechEff_bld.Colombia_FNL <- data.frame("region" = character(), 'supplysector' = character(), 'subsector' = character(), 
                                                'stub-technology' = character(), 'year' = numeric(),
                                                'minicam.energy.input' = character(), 'efficiency' = numeric(),
                                                'market.name' = character()
                                                )  # Create base data frame

# Decide improvement rate
# New Assumption: improvement rate doubles by 2050 comparing to the Base
# For example, improvement rate = [2*(Base_2050 - Base_2015)/Base_2015]/(2050 - 2015)
# Note, for different sectors, the improvement rate will be different due to different base values
start_year <- 2015
end_year <- 2050
BldgAppEff_high <- L244.StubTechEff_bld.Colombia_INI %>%
  dplyr::filter((supplysector %in% c('comm others', 'resid others', 'comm heating', 'resid heating') &
                   subsector %in% c('electricity', 'gas')) |
                  (supplysector %in% c('comm cooling', 'resid cooling') &
                     subsector %in% c('electricity'))) %>% 
  dplyr::group_by(supplysector, subsector, stub.technology) %>% 
  dplyr::mutate(value.start= efficiency[year == start_year],
                value.end = efficiency[year == end_year],
                improvement.rate = 1 + (2*(value.end - value.start)/value.start)/(end_year-start_year)*(year - start_year),
                efficiency.new = if_else(year >= start_year, value.start * improvement.rate, efficiency)) %>% 
  dplyr::select(-value.start, -value.end, -improvement.rate)

L244.StubTechEff_bld.Colombia_FNL <- BldgAppEff_high %>% 
  dplyr::mutate(efficiency.new = if_else(!subsector %in% 'electricity',
                                         if_else(efficiency.new > 1, 1, efficiency.new),
                                         efficiency.new))

write.csv(L244.StubTechEff_bld.Colombia_FNL, 'L244.StubTechEff_bld.Colombia_FNL_Plot.csv', row.names = FALSE)
L244.StubTechEff_bld.Colombia_FNL <- L244.StubTechEff_bld.Colombia_FNL %>% 
  select(-efficiency) %>% 
  rename(efficiency = efficiency.new)
L244.StubTechEff_bld.Colombia_FNL <- L244.StubTechEff_bld.Colombia_FNL[c(1,2,3,4,5,6,8,7)]


# Auto-produce XML from CSV
L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia_FNL
gcamdata_variable <- "Bldg_Appliance_Eff" #  "AgProdChange"
imported_data <- tibble::as_tibble(L244.StubTechEff_bld.Colombia)
xmlpath <- file.path(base_directory, 'Bldg_Appliance_Eff_High.xml')
# mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
mi_header <- file.path('..', 'headers_rdm.txt')
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()


# ============================== Not in Use =================================
if(F){
  # improvement.rate.2030 <- 0.22/(2030-2015)
  # improvement.rate.2050 <- (0.26-0.22)/(2050-2030)
  
  # Assumption 1: Assume an average annual change rate of 0.015, so the value in 2050 will be
  # Value_2050 = Value_2015 * (1 + 0.015)^(2050-2015)
  # The improvement rate in 2050 = (1.015^(2050-2015) - 1)/(2050-2015)
  # Assumption 2: Assume reaching 4.38 by 2050 for comm cooling electricity
  # Value_2015 for comm cooling electricity = 2.5651377
  # The improvement rate in 2050 = (4.38/2.5651377-1)/(2050-2015)
  improvement.rate.2050 <- (4.38/2.5651377-1)/(2050-2015)
  for (supply_sector in names(supplysectors)){
    for(sub_sector in supplysectors[[supply_sector]]){
      L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia_INI %>%
        filter(supplysector==supply_sector, subsector==sub_sector)
      # Get 2015 values since policy defined in terms of percentage decline from 2015
      appl.eff.2015.Col <- (L244.StubTechEff_bld.Colombia %>% 
                              filter(year==2015))$efficiency
      L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia %>% 
        mutate(efficiency.new = if_else(year<=2015, 
                                        efficiency,
                                        (1+(year-2015)*improvement.rate.2050)*appl.eff.2015.Col))
      # Deal first with portion through 2030
      # L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia %>% 
      #   mutate(efficiency.new = if_else(year<=2015, 
      #                                   efficiency,
      #                                   if_else(year<=2030, 
      #                                           (1+(year-2015)*improvement.rate.2030)*appl.eff.2015.Col, 
      #                                           efficiency)))
      # Grab 2030 values for comm and resid, then project through 2050 using that.
      # appl.eff.2030.Col <- (L244.StubTechEff_bld.Colombia %>% 
      #                         filter(year==2030))$efficiency.new
      # L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia %>% 
      #   mutate(efficiency.new = if_else(year<=2030, 
      #                                   efficiency.new,
      #                                   (1+(year-2030)*improvement.rate.2050)*appl.eff.2030.Col))
      
      if(!sub_sector %in% c('electricity')){
        # Heating can have efficiency > 1 so don't reset it.
        L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia %>% 
          mutate(efficiency.new=if_else(efficiency.new>1,1,efficiency.new))
      }
      # if(supply_sector %in% c('resid heating', 'comm heating', 'resid others', 'comm others')){
      #   # Heating can have efficiency > 1 so don't reset it.
      #   L244.StubTechEff_bld.Colombia <- L244.StubTechEff_bld.Colombia %>% 
      #     mutate(efficiency=if_else(efficiency>1,1,efficiency))
      # }
      L244.StubTechEff_bld.Colombia_FNL <- rbind(L244.StubTechEff_bld.Colombia_FNL, L244.StubTechEff_bld.Colombia)
    }
  }
  
  write.csv(L244.StubTechEff_bld.Colombia_FNL, 'L244.StubTechEff_bld.Colombia_FNL.csv')
  L244.StubTechEff_bld.Colombia_FNL <- L244.StubTechEff_bld.Colombia_FNL %>% 
    select(-efficiency) %>% 
    rename(efficiency = efficiency.new)
  L244.StubTechEff_bld.Colombia_FNL <- L244.StubTechEff_bld.Colombia_FNL[c(1,2,3,4,5,6,8,7)]
}
