library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)
library(data.table)
library(stringr)


# This script creates a single .csv file to increase industrial efficiency. It applies a simple function that improves
# industrial efficiency over time across sectors/subsectors/stub techs. It needs to be run through
# the GCAM model interface to create a XML file. Need to figure out if these xml files can actually be kept separate,
# in which case we can automate the XML creation process

# base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/IndustryEfficiency/')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
base_directory <- getwd()

# Create base data from L232.GlobalTechEff_ind.csv and L232.StubTechCoef_industry.csv
# Both raw data can be found in gcam-core-gcam-v5.3\input\gcamdata\outputs
# Use this step to update IndustrialStubTechEff_Base.csv if there is new versions of GCAM and gcamdata
rebuild_Base <- FALSE
if(rebuild_Base){
  L232.GlobalTechEff <- data.table::fread('L232.GlobalTechEff_ind.csv')
  L232.StubTechCoef <- data.table::fread('L232.StubTechCoef_industry.csv')
  col_names <- names(L232.StubTechCoef)
  
  ind_energy_eff <- L232.GlobalTechEff %>% 
    dplyr::filter(sector.name %in% 'industrial energy use', !subsector.name %in% 'district heat') %>% 
    dplyr::rename(supplysector = sector.name,
                  subsector = subsector.name,
                  stub.technology = technology,
                  period = year) %>% 
    dplyr::mutate(region = 'Colombia',
                  market.name = 'Colombia')
  col_order <- c('region', 'supplysector', 'subsector', 'stub.technology', 'period', 'minicam.energy.input', 'efficiency', 'market.name')
  data.table::setcolorder(ind_energy_eff, col_order)
  
  write(c('INPUT_TABLE,,,,,,,',
          'Variable ID,,,,,,,',
          'StubTechEff,,,,,,,',
          ',,,,,,,'),
        file = 'IndustrialStubTechEff_Base.csv')
  write.table(x = ind_energy_eff,
              file = 'IndustrialStubTechEff_Base.csv', 
              row.names=FALSE,
              sep = ',',
              append=TRUE,
              quote=FALSE)

}


# Read in base data (taken from Gcam data system after building). We will modify the efficiencies in the base data to
# create a new set of assumptions and associated file.
data <- read.csv(file.path(base_directory,'IndustrialStubTechEff_Base.csv'), skip=4)


# Current Assumption -----------------------------------------------------------
# Percent increase in 2050 relative to 2015:
# Hydrogen - 20% increase
# Biomass and Electricity - 10% increase
# Other bulk fuel (gas, coal, refined liquids) - 5% increase
start_year <- 2015
end_year <- 2050

IndEE_high <- data %>%
  dplyr::group_by(stub.technology) %>% 
  dplyr::mutate(value.start= efficiency[period == start_year],
                value.end = efficiency[period == end_year],
                improvement.rate = if_else(subsector %in% c('hydrogen'), 0.2,
                                           if_else(subsector %in% c('biomass', 'electricity'), 0.1, 0.05)),
                improvement.rate = 1 + improvement.rate/(end_year-start_year)*(period - start_year),
                efficiency.new = if_else(period >= start_year, value.start * improvement.rate, efficiency))


# Plot -------------------------------------------------------------------------
df_plot <- IndEE_high %>% 
  dplyr::mutate(High = (improvement.rate-1) * 100,
                Low = (efficiency - value.start)/value.start * 100) %>% 
  dplyr::select(subsector, stub.technology, period, High, Low) %>% 
  tidyr::gather(key = 'improvement.pct', value = 'value', c('High', 'Low')) %>% 
  dplyr::mutate(group = str_to_title(paste0(improvement.pct, ' Efficiency - ', stub.technology)),
                period = as.numeric(period)) %>% 
  as.data.frame()

line_plot <- function(df_plot, subsector_i, supplysector){
  
  df <- df_plot %>% 
    dplyr::filter(subsector %in% subsector_i, period %in% seq(start_year, end_year, by = 5))
  
  title_name <- str_to_title(paste0(supplysector, ' - ', subsector_i))
  stub_length <- length(unique(df$stub.technology))
  if(stub_length == 2){
    color_pattern <- c('royalblue', 'royalblue', 'chocolate1', 'chocolate1')
    line_pattern <- c('solid', 'dashed', 'solid', 'dashed')
  } else {
    color_pattern <- c('royalblue', 'chocolate1')
    line_pattern <- c('solid', 'solid')
  }
  
  P <- ggplot(df, aes(x = period, y = value, group = group)) +
    geom_line(aes(linetype = group, color = group), size=1) +
    geom_point(aes(fill = group), shape=21, color="black", size=3, stroke=1.5) +
    scale_fill_manual(values = color_pattern) +
    scale_colour_manual(values = color_pattern) +
    scale_linetype_manual(values = line_pattern) +
    labs(x = NULL,
         y = 'Percent Change Relative to 2015 (%)',
         title = title_name) +
    guides(fill = guide_legend(ncol = 2)) +
    theme(
      title = element_text(size = 16),
      panel.background = element_blank(),
      panel.border = element_rect(colour = 'black', fill = NA, size = 1.2),
      panel.grid.major.x = element_line(colour = 'grey'),
      panel.grid.minor.x = element_line(colour = 'grey'),
      axis.text = element_text(colour = 'black', size = 14),
      axis.title = element_text(size = 14),
      strip.background = element_rect(fill = NA),
      strip.placement = 'outside',
      legend.position = 'bottom',
      legend.box = 'horizontal',
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(2,'cm'),
      legend.box.margin = margin(t=0, r=0,b=0,l=-30,unit='pt'),
      legend.margin = margin(0,0,0,0),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.ticks = element_line(size = 1.2),
      plot.margin = margin(t = 0, r = 20, b = 0, l = 0),
    )
  P
  save_path <- file.path(getwd(), 'figures')
  if(!file.exists(save_path)){
    dir.create(save_path)
  }
  ggsave(file.path(save_path, paste0(title_name, '.png')), height = 4.5, width = 7, unit = 'in', dpi = 600)
}

line_plot(df_plot, 'biomass', 'Industrial Efficiency')
line_plot(df_plot, 'electricity', 'Industrial Efficiency')
line_plot(df_plot, 'hydrogen', 'Industrial Efficiency')
line_plot(df_plot, 'coal', 'Industrial Efficiency')
line_plot(df_plot, 'gas', 'Industrial Efficiency')
line_plot(df_plot, 'refined liquids', 'Industrial Efficiency')


# Merge new efficiencies -------------------------------------------------------
# (which only apply to future years) with historical efficiencies
IndEE_high <- IndEE_high %>% 
  dplyr::select(-value.start, -value.end, -improvement.rate)
output <- data %>% 
  left_join(IndEE_high %>% select(-efficiency), 
            by=c('region', 'supplysector', 'subsector', 'stub.technology', 'period', 'minicam.energy.input', 
                 'market.name')) %>%
  mutate(efficiency = if_else(is.na(efficiency.new)==TRUE, efficiency, efficiency.new)) %>% 
  dplyr::select(-efficiency.new)
desired_output_column_order <- names(data)
output <- output[desired_output_column_order]  # Reorder coluns to order required by header

# Write top column labels required as proper formatting to create XML ----------
output_file <- file.path(base_directory, 'IndustrialStubTechEff_High.csv')
if(file.exists(output_file)){
  file.remove(output_file)
}
write.table('INPUT_TABLE,,,,,,,', 
            output_file,
            append=TRUE,
            sep=',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('Variable ID,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('StubTechEff,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            quote=FALSE,
            col.names=FALSE)
write.table(',,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
# Export data
write.table(output, output_file, 
            row.names=FALSE,
            sep = ',',
            append=TRUE,
            quote=FALSE)

# Auto-produce XML from CSV ----------------------------------------------------
gcamdata_variable <- "StubTechEff" #  "AgProdChange"
imported_data <- tibble::as_tibble(read.csv(output_file, skip = 4, stringsAsFactors = F)) %>% 
  rename(year=period)
xmlpath <- file.path(base_directory, 'Strategy_2_High_IndEE.xml') # old name IndustrialStubTechEff_High.xml
# mi_header <- 'C:/Users/twild/Downloads/gcam-v5.1.3-Windows-Release-Package/input/gcamdata/inst/extdata/mi_headers/ModelInterface_headers.txt'
mi_header <- file.path('..', 'headers_rdm.txt')
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable) %>%
  gcamdata::run_xml_conversion()


# ============================ Not in USE =================================
# Rate of increase per year of efficiency improvement. Begins at 30% in 2030 and continues to 2030. 
# Will backcast to 2020 from 2030, and project from 2030 to 2050.
# eff_rate_policy <- (0.5-0.3)/(2050-2030)
# official_policy_begin_yr <- 2030
# policy_application_begin_yr <- 2020
# 
# desired_output_column_order <- names(data)
# #Modify the efficiency using rate of change rule
# modified_efficiency <- data %>% 
#   filter(period>=policy_application_begin_yr) %>% 
#   mutate(efficiency_improvement = 0.3 + (period-official_policy_begin_yr)*eff_rate_policy) %>% 
#   mutate(new_efficiency = efficiency*(1 + efficiency_improvement)) %>% 
#   mutate(new_efficiency = if_else(new_efficiency>1, 1, new_efficiency))


# Decide improvement rate
# New Assumption: improvement rate doubles by 2050 comparing to the Base
# For example, improvement rate = [2*(Base_2050 - Base_2015)/Base_2015]/(2050 - 2015)
# Note, for different sectors, the improvement rate will be different due to different base values
# start_year <- 2015
# end_year <- 2050
# 
# IndEE_high <- data %>%
#   dplyr::group_by(stub.technology) %>% 
#   dplyr::mutate(value.start= efficiency[period == start_year],
#                 value.end = efficiency[period == end_year],
#                 improvement.rate = 1 + (2*(value.end - value.start)/value.start)/(end_year-start_year)*(period - start_year),
#                 efficiency.new = if_else(period >= start_year, value.start * improvement.rate, efficiency)) %>% 
#   dplyr::select(-value.start, -value.end, -improvement.rate)