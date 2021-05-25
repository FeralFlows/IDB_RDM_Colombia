library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This function creates a single .xml file designed to constrain CO2 emissions in two markets: (1) Colombia, and (2) Rest of
# World (RoW). The relevant headers for creating the XML are in colombia_policy/headers_rdm.txt.

emiss_constraint <- function(base_directory, start_yr, end_yr, mi_header, colombia_co2_constraint=NULL,
                             global_co2_constraint=NULL,  xmlpath_colombia=NULL, xmlpath_other_regions=NULL){
  # Establish semi-populated dataframe colombia and global emissions
  ddp_colombia_base <- data.frame('region' = character(), 'ghgpolicy' = character(),
                                      'market' = numeric(), 'year' = numeric(), 'constraint' = numeric())
  year_list = seq(start_yr, end_yr, 5)
  # Loop through years and store entry in DataFrame for Co2 constraint for each year
  for (year in year_list){
    ddp_colombia_year <- data.frame('region' = 'Colombia', 'ghgpolicy' = 'GHG', 'market' = 'Colombia', 'year' = year,
                                        'constraint' = colombia_co2_constraint[[toString(year)]])
    # Apply global constraint first to USA. Then link in other GCAM regions separately.
    ddp_global_year <- data.frame('region' = 'USA', 'ghgpolicy' = 'GHG', 'market' = 'row', 'year' = year,
                                      'constraint' = global_co2_constraint[[toString(year)]])
    ddp_colombia_base <- rbind(ddp_colombia_base, ddp_colombia_year, ddp_global_year)
  }

  # Auto-produce XML from CSV
  gcamdata_variable <- "GHG_Constraint"
  imported_data <- tibble::as.tibble(ddp_colombia_base)
  gcamdata::create_xml(xmlpath_colombia, mi_header = mi_header) %>%
    gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
    gcamdata::run_xml_conversion()


  ddp_regions_link <- data.frame('region' = character(), 'ghgpolicy' = character(), 'market' = numeric())
  link_regions <- c('Africa_Northern', 'Africa_Southern', 'Africa_Western', 'Argentina', 'Brazil', 'Canada',
                    'Central America and Caribbean', 'Central Asia', 'China', 'EU-12', 'EU-15', 'Europe_Eastern',
                    'Europe_Non_EU', 'European Free Trade Association', 'India', 'Indonesia', 'Japan', 'Mexico',
                    'Middle East', 'Pakistan', 'Russia', 'South Africa', 'South America_Northern',
                    'South America_Southern', 'South Asia', 'South Korea', 'Southeast Asia', 'Taiwan', 'Uruguay',
                    'Africa_Eastern', 'Australia_NZ')
  # Loop through extra regions (aside from Colombia and USA) to link to the "row" market
  for (gcam_region in link_regions){
    region_link_df <- data.frame('region' = gcam_region, 'ghgpolicy' = 'GHG', 'market' = 'row')
    ddp_regions_link <- rbind(ddp_regions_link, region_link_df)
  }
  gcamdata_variable <- "GHG_Constraint_Add_Region"
  imported_data <- tibble::as.tibble(ddp_regions_link)
  gcamdata::create_xml(xmlpath_other_regions, mi_header = mi_header) %>%
    gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
    gcamdata::run_xml_conversion()

}

