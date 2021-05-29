#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Install and load packages -----------------------------------------------

rm(list=ls())

if('tibble' %in% rownames(installed.packages()) == F){install.packages('tibble')}
library(tibble)
if('rgdal' %in% rownames(installed.packages()) == F){install.packages('rgdal')}
library(rgdal)
if('ggplot2' %in% rownames(installed.packages()) == F){install.packages('ggplot2')}
library(ggplot2)
if('dplyr' %in% rownames(installed.packages()) == F){install.packages('dplyr')}
library(dplyr)
if('tidyr' %in% rownames(installed.packages()) == F){install.packages('tidyr')}
library(tidyr)
if('reshape' %in% rownames(installed.packages()) == F){install.packages('reshape')}
library(reshape)
if('data.table' %in% rownames(installed.packages()) == F){install.packages('data.table')}
library(data.table)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if('rgcam' %in% rownames(installed.packages()) == F){devtools::install_github(repo="JGCRI/rgcam")}
library(rgcam)
if('metis' %in% rownames(installed.packages()) == F){devtools::install_github(repo="JGCRI/metis")}
library(metis)
# library(rfasst)


# Set directories, load extra files ---------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
metrics_folder <- getwd()
run_name <- args[0] # Tom please change the args[??] to reflect command_line inputs
scenario_name <- args[1] # Tom please change the args[??] to reflect command_line inputs
gcam_dir <- file.path('..', 'relationships', 'gcam')

base_dir <- file.path(gcam_dir, 'outputs', 'post_processed', run_name)
if(!file.exists(base_dir)){dir.create(base_dir)}
export_dir <- file.path(metrics_folder, 'output', run_name)
if(!file.exists(export_dir)){dir.create(export_dir)}

## Import Experiment CSV
path_to_EXP <- file.path(gcam_dir, 'config', 'output', 'doe')
exp <- data.table::fread(paste0(path_to_EXP, '/DOE_XLRM_', scenario_name, '.csv'), header = TRUE)
exp <- exp %>% 
  mutate(scenario = str_replace(scenario, '_', '')) %>% 
  mutate(experiment = as.character(experiment))

## Import Reference tables
GWP <- read.csv(paste(metrics_folder, 'GWP.csv', sep = '/'), header = TRUE, sep = ",", dec = ".")
## Import Correction factors
CF <- read.csv(paste(metrics_folder, 'CorreccionFactor.csv', sep = '/'), header = TRUE, sep = ",", dec = ".")
## Import deforestation emission trajectory
ghgtraj <- read.csv(paste(metrics_folder, 'GHG traj.csv', sep = '/')) %>% ##BY 12-17-2020
  gather(key = "year", "value", c("X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050"))
ghgtraj$year <- as.numeric(substring(ghgtraj$year, 2))

plot_scens <- c("DDPXL") # "Reference",
x_lbl <- 'Time'
title <- ''
x_min <- 2015
x_max <- 2050

# Source necessary functions ----------------------------------------------
source(paste(metrics_folder, 'RDM_plotting_distribution.R', sep = '/'))
source(paste(metrics_folder, 'RDM_CART_fns.R', sep = '/'))

# --------------------------------------------------------------------------
# 1. Intermittent Renewables
# --------------------------------------------------------------------------
qry <- "Electricity generation by aggregate technology.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

ElecGenAggTech <- prj$data$`Electricity generation by aggregate technology`

TotalPowGen <- ElecGenAggTech %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value) * 2.777 * (10 ** 2)) %>%
  mutate(Metric = "Total Power Generation", Units = "Thous GWh")

## Wind and Solar
PowGenRenewIntGW <- ElecGenAggTech %>%
  filter(technology %in% c("Solar", "Wind")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value) * 2.777 * (10 ** 2)) %>%
  mutate(Metric = "Wind and Solar Power Generation", Units = "Thous GWh")

PowGenRenewInt <- PowGenRenewIntGW %>%
  left_join(TotalPowGen,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'Units', 'year'),
            suffix = c('.int', '.total')) %>%
  mutate(value = value.int/value.total * 100, Metric = "Wind and Solar Percentage", Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Renewable Power Generation
PowGenRenewGW <- ElecGenAggTech %>%
  filter(technology %in% c("Solar", "Wind", 'Biomass', 'Biomass w/CCS', 'Hydro')) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value) * 2.777 * (10 ** 2)) %>%
  mutate(Metric = "Renewable Power Generation", Units = "Thous GWh")

PowGenRenewPct <- PowGenRenewGW %>%
  left_join(TotalPowGen,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'Units', 'year'),
            suffix = c('.renew', '.total')) %>%
  mutate(value = value.renew/value.total * 100, Metric = "Renewable Percentage", Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Low Carbon Power Generation
PowGenLowCarb <- ElecGenAggTech %>%
  filter(technology %in% c("Solar", "Wind", 'Hydro', 'Biomass', 'Biomass w/CCS', 'Nuclear', 'Gas w/CCS')) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value) * 2.777 * (10 ** 2)) %>%
  mutate(Metric = "Low Carbon Power Generation", Units = "Thous GWh")

PowGenLowCarbPct <- PowGenLowCarb %>%
  left_join(TotalPowGen,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'Units', 'year'),
            suffix = c('.lowcarb', '.total')) %>%
  mutate(value = value.lowcarb/value.total * 100, Metric = "Low Carbon Power Percentage", Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Hydro
PowGenHydroGW <- ElecGenAggTech %>%
  filter(technology %in% c("Hydro")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value) * 2.777 * (10 ** 2)) %>%
  mutate(Metric = "Hydro Power Generation", Units = "Thous GWh")

PowGenHydro <- PowGenHydroGW %>%
  left_join(TotalPowGen,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'Units', 'year'),
            suffix = c('.hydro', '.total')) %>%
  mutate(value = value.hydro/value.total * 100, Metric = "Hydro Percentage", Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

qry <- "elec gen by gen tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

## RPS Calculations
total_RPS <- prj$data$`elec gen by gen tech` %>%
  filter(technology != "hydrogen cogen") %>%
  filter(technology != "rooftop_pv") %>%
  group_by(scenario, region, year, experiment, old_scen_name) %>%
  summarise(total_value = sum(value))

ren_RPS <- prj$data$`elec gen by gen tech` %>%
  filter(technology %in% c("PV", "PV_storage", "wind", "wind_storage", "wind_offshore", "hydro",
                           "biomass (IGCC CCS)", "biomass (IGCC)", "biomass (conv CCS)", "biomass (conv)")) %>%
  group_by(scenario, region, year, experiment, old_scen_name) %>%
  summarise(ren_value = sum(value))

RPS_pct <- ren_RPS %>%
  left_join(total_RPS) %>%
  mutate(pct = ren_value / total_value)



# GHG Emissions --------------------------------------------------
## Negative CO2 Emissions
qry <- "CO2 emissions by sector.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

CO2Sector <- prj$data$`CO2 emissions by sector`

NegativeCO2 <- CO2Sector %>%
  filter(sector %in% c('regional biomass', 'regional biomassOil', 'regional sugar for ethanol')) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value) * 44/12) %>%
  mutate(Metric = 'Negative CO2 Emission',
         Units = 'MTCO2')


Metrics <- rbind(as_tibble(PowGenRenewInt),
                 as_tibble(PowGenRenewIntGW),
                 as_tibble(PowGenRenewPct),
                 as_tibble(PowGenLowCarbPct),
                 as_tibble(PowGenHydro),
                 as_tibble(PowGenHydroGW),
                 as_tibble(TotalPowGen),
                 as_tibble(NegativeCO2))

rdm_cart(PowGenRenewInt, exp, export_dir)
rdm_cart(PowGenRenewIntGW, exp, export_dir)
rdm_cart(PowGenRenewPct, exp, export_dir)
rdm_cart(PowGenLowCarbPct, exp, export_dir)
rdm_cart(PowGenHydro, exp, export_dir)
rdm_cart(PowGenHydroGW, exp, export_dir)
rdm_cart(TotalPowGen, exp, export_dir)
rdm_cart(NegativeCO2, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', '1-IntermittentRenewables'))){
  dir.create(file.path(export_dir, 'distribution', '1-IntermittentRenewables'))
}
# select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', '1-IntermittentRenewables', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
  }


qry <- "nonCO2 emissions by resource production.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

NonCO2ResProd <- prj$data$`nonCO2 emissions by resource production` %>%
  mutate(sector = prj$data$`nonCO2 emissions by resource production`$resource,
         resource = NULL)

NonCO2ResProd <- NonCO2ResProd %>%
  group_by(scenario, region, sector, ghg, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))


qry <- "nonCO2 emissions by sector.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

NonCO2Sector <- prj$data$`nonCO2 emissions by sector`

NonCO2_species <- rbind(NonCO2ResProd, NonCO2Sector)

NonCO2_species_MTCO2e <- NonCO2_species %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL)
NonCO2_total_MTCO2e <- NonCO2_species_MTCO2e %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Nonco2 Emissions")


# --------------------------------------------------------------------------
# LUC emissions
# --------------------------------------------------------------------------
qry <- "Land Use Change Emission (future).proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

LUC_emissions <- prj$data$`Land Use Change Emission (future)` %>%
  mutate(Units = "MtCO2e/yr", value = value * 44/12) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "LUC Emissions")

Metrics <- as_tibble(LUC_emissions) 

rdm_cart(LUC_emissions, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', 'LUC_Emissions'))){
  dir.create(file.path(export_dir, 'distribution', 'LUC_Emissions'))
}
select_exps <- c('0', '16', '32', '48', '64', '80', '96', '112',
                 '128', '144', '160', '176', '192', '208', '224', '240')
# select_exps <- c('NoPol_0', 'NoPol_16', 'NoPol_32', 'NoPol_48')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', 'LUC_Emissions', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    distribution = NULL,
    plot_by_select_experiment = select_exps)
}


# select_exps <- c('0', '16', '32', '48', '64', '80', '96', '112',
#                  '128', '144', '160', '176', '192', '208', '224', '240')
# # select_exps <- c('NoPol_0', 'NoPol_16', 'NoPol_32', 'NoPol_48')
# LUC_emissions_filtered <- LUC_emissions %>%
#   filter(experiment %in% select_exps)
# 
# LUC_emissions_filtered$experiment <- factor(LUC_emissions_filtered$experiment,
#                                             levels = select_exps)
#                                             
# 
# fig_path <- c(paste(file.path(export_dir, 'distribution', 'LUC_Emissions'), '/', "LUC_Emissions_select", '.png', sep = ""))
# 
# plot <- LUC_emissions_filtered %>%
#   filter(year >= 2015 & year <= 2050) %>%
#   ggplot(aes(x = year, y = value)) +
#   geom_line(size = 1, aes( color = experiment)) +
#   # coord_cartesian(ylim = c(-8E9, 8E9))+
#   scale_color_manual(values = c('darkred', 'darkred','darkred', 'darkred','darkred', 'darkred','darkred', 'darkred',
#                                 'forestgreen', 'forestgreen', 'forestgreen', 'forestgreen', 'forestgreen', 'forestgreen', 'forestgreen', 'forestgreen'))+
#   labs(y = "LUC Emissions (MTCO2e/yr)") +
#   theme_bw() +
#   theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme(
#     text =                element_text(family = NULL, face = "plain",colour = "black", size = 8 ,hjust = 0.5,
#                                        vjust = 0.5, angle = 0, lineheight = 0.9)
#     ,axis.text.x =        element_text(size=8)
#     ,axis.text.y =        element_text(size=8)
#     # ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
#     ,axis.title.x =       element_blank()
#     ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
#     ,legend.key =         element_blank()
#     ,legend.key.size =    unit(1.0, 'lines')
#     ,legend.text =        element_text(size = 6, colour = "black")
#     ,legend.title =       element_text(size = rel(1), face = NULL, hjust = 0, colour = "black")
#     ,strip.background =   element_rect(fill = NA, colour = "black")
#     ,plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines")
#     # ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -1, margin = margin(b=20), size=8)
#     ,plot.title=          element_blank()
#     ,legend.position =    c('right')  # c(0.95, 0.95)
#   )+
#   ggsave(fig_path, height = 3.4, width = 4, units = "in")

# --------------------------------------------------------------------------
# 2. Air Pollution
# --------------------------------------------------------------------------

## NOx
NOx <- NonCO2_species %>%
  filter(ghg %in% c("NOx_AGR","NOx_AWB","NOx")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "NOx Emissions")

## SO2
SO2 <- NonCO2_species %>%
  filter(ghg %in% c("SO2_3","SO2_3_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "SO2 Emissions")

## BC
BC <- NonCO2_species %>%
  filter(ghg %in% c("BC","BC_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "BC Emissions")

## OC
OC <- NonCO2_species %>%
  filter(ghg %in% c("OC","OC_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "OC Emissions")

## PM2.5
### This results is based on the output from rfasst
# pm25 <- m2_get_conc_pm25(db_path = NULL,
#                          query_path = NULL,
#                          db_name = NULL,
#                          prj_name = NULL,
#                          scen_name = 'NoPol_0',
#                          queries = NULL,
#                          prj_path=base_dir,
#                          read_prj=T,
#                          saveOutput = F,
#                          map = F)
# PM2.5 <- pm25 %>%
#   dplyr::filter(region %in% 'RSAM') %>%
#   dplyr::mutate(scenario = 'DDPXL',
#                 old_scen_name = paste0(scenario, '_', Scenario),
#                 Metric = 'PM 2.5') %>%
#   dplyr::rename(Units = units,
#                 experiment = Scenario) %>% 
#   dplyr::mutate(year = as.numeric(as.character(year)))


Metrics <- rbind(as_tibble(NOx),
                 as_tibble(SO2),
                 as_tibble(BC),
                 as_tibble(OC))

rdm_cart(NOx, exp, export_dir)
rdm_cart(SO2, exp, export_dir)
rdm_cart(BC, exp, export_dir)
rdm_cart(OC, exp, export_dir)


if(!file.exists(file.path(export_dir, 'distribution', '2-AirPollution'))){
  dir.create(file.path(export_dir, 'distribution', '2-AirPollution'))
  }
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', '2-AirPollution', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
  }

# --------------------------------------------------------------------------
# 3. Crops & Biomass
# --------------------------------------------------------------------------

qry <- "aggregated land allocation.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

AggLandAlloc <- prj$data$`aggregated land allocation`

## Unmanaged land
UnmanagedLand <- AggLandAlloc %>%
  filter(landleaf %in% c("forest (unmanaged)", "pasture (other)")) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Unmanaged Land')

## Forestland
ForestLand <- AggLandAlloc %>%
  filter(landleaf %in% c("forest (unmanaged)", "forest (managed)")) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Forest Land')

##Value of forestland
ValueForestLand <- ForestLand %>%
  # From Costanza et al. (2014) paper, value of forest in 2011 was 3800 $2007/ha/yr
  # Use GDP deflator 100/88.3 to convert to $2015/ha/yr
  mutate(value = value * 3800 * 100/88.3,
         Units = "Value of Forest Land")


## Crop land
CropLand <- AggLandAlloc %>%
  filter(landleaf %in% c("crops", 'biomass')) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Crop Land')


qry <- "primary energy consumption by region (avg fossil efficiency).proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

PriEne <- prj$data$`primary energy consumption by region (avg fossil efficiency)`

all <- PriEne %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Total Primary Energy Consumption")

## Biomass primary energy (EJ)
biomass <- PriEne %>%
  filter(fuel %in% c("d biomass")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Biomass Primary Energy", Units = "EJ") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Biomass percentage of primary energy (%)
biomassp <- biomass %>%
  left_join(all, by = c("scenario", "region", "experiment", "old_scen_name", "year")) %>%
  mutate(value = (value.x / value.y) * 100,
         Metric = "Biomass Share of Primary Energy",
         Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Traditional biomass
biomass_trad <- PriEne %>%
  filter(fuel %in% c("j traditional biomass")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Traditional Biomass Primary Energy", Units = "EJ") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Traditional Biomass percentage of primary energy (%)
biomassp_trad <- biomass_trad %>%
  left_join(all, by = c("scenario", "region", "experiment", "old_scen_name", "year")) %>%
  mutate(value = (value.x / value.y) * 100,
         Metric = "Traditional Biomass Share of Primary Energy",
         Units = "%") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Biomass Share of Transport Final Energy

### Biofuel proportion of refined liquids production
qry <- "refined liquids production by subsector.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

RefLiqProd <- prj$data$`refined liquids production by subsector`

TotalRefLiq <- RefLiqProd %>%
  group_by(scenario, region, Units, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value))
biomass_liq_ratio <- RefLiqProd %>%
  filter(subsector %in% 'biomass liquids') %>%
  left_join(TotalRefLiq,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.biomass', '.total')) %>%
  mutate(value = value.biomass/value.total) %>%
  select(scenario, region, experiment, old_scen_name, year, value)

### Biofuel proportion of electricity generation
qry <- "elec gen by gen tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

ElecGenTech <- prj$data$`elec gen by gen tech`

TotalElecGenTech <- ElecGenTech %>%
  group_by(scenario, region, Units, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value))
biomass_elec_ratio <- ElecGenTech %>%
  filter(subsector %in% 'biomass') %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(TotalElecGenTech,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.biomass', '.total')) %>%
  mutate(value = value.biomass/value.total) %>%
  select(scenario, region, experiment, old_scen_name, year, value)


### Biofuel proportion of gas production
qry <- "gas production by tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

GasProTech <- prj$data$`gas production by tech`

TotalGasProTech <- GasProTech %>%
  group_by(scenario, region, Units, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value))
biomass_gas_ratio <- GasProTech %>%
  filter(subsector %in% 'biomass gasification') %>%
  left_join(TotalGasProTech,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.biomass', '.total')) %>%
  mutate(value = value.biomass/value.total) %>%
  select(scenario, region, experiment, old_scen_name, year, value)

## Transportation Final Energy
qry <- "transport final energy by fuel.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

TransFinal <- prj$data$`transport final energy by fuel`

TotalTransFinal <- TransFinal %>%
  group_by(scenario, region, Units, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value))

## Biomass Final Energy in Liquids, Electricity, and Gas Transportation Final Energy
BioTransFinal_liq <- TransFinal %>%
  filter(input %in% 'refined liquids enduse') %>%
  select(scenario, region, Units, experiment, old_scen_name, year, value) %>%
  left_join(biomass_liq_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)
BioTransFinal_elec <- TransFinal %>%
  filter(input %in% 'elect_td_trn') %>%
  select(scenario, region, Units, experiment, old_scen_name, year, value) %>%
  left_join(biomass_elec_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)
BioTransFinal_gas <- TransFinal %>%
  filter(input %in% 'delivered gas') %>%
  select(scenario, region, Units, experiment, old_scen_name, year, value) %>%
  left_join(biomass_gas_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)


### Biofuels as a proportion of transportation energy consumption (%)
BiomassTrans <- Reduce(function(...)
  left_join(..., by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year')),
  list(BioTransFinal_liq, BioTransFinal_elec, BioTransFinal_gas, TotalTransFinal)) %>%
  dplyr::rename(bio_liq = value.x,
         bio_elec = value.y,
         bio_gas = value.x.x,
         total_trans = value.y.y) %>%
  mutate(value = (bio_liq + bio_elec + bio_gas)/total_trans * 100,
         Metric = 'Biofuels Share of Transportation Energy',
         Units = '%') %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)



## Value of Crop Production
# Note: this does not include inputs to livestock products: FeedCrops, FodderHerb, FodderGrass
qry <- "prices of all markets.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

PriceAllMarkets <- prj$data$`prices of all markets`

CropPrices <- PriceAllMarkets %>%
  mutate(market = gsub("Colombia", "", market)) %>%
  filter(market %in% c("Corn", "FiberCrop", "Wheat",
                       "SugarCrop", "RootTuber", "Rice", "OtherGrain",
                       "OilCrop", "MiscCrop", "PalmFruit", "biomass"))

qry <- "Ag Production by Crop Type.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

CropProdType <- prj$data$`Ag Production by Crop Type`


ValueCropProd <- CropProdType %>%
  # filter only crops that have prices (value)
  filter(sector %in% c("Corn", "FiberCrop", "Wheat",
                       "SugarCrop", "RootTuber", "Rice", "OtherGrain",
                       "OilCrop", "MiscCrop", "PalmFruit", "biomass")) %>%
  # join prices
  left_join(CropPrices, by = c("scenario", "output" = "market", "year", "experiment", "old_scen_name"),
            suffix = c(".Prod", ".Price")) %>%
  # value of crop production = sum of (crop production X price of crop)
  # non-biomass crops: Mt * 1975$/kg * 10^9 * 4.25 2015$/1975$ = 2015$
  # biomass crops: EJ * 1975$/GJ * 10^9 * 4.25 2015$/1975$ = 2015$
  mutate(value = if_else(output != "biomass",
                         value.Prod * value.Price * 10^9 * 4.25,
                         value.Prod * value.Price * 10^9 * 4.25)) %>%
  group_by(scenario, experiment, old_scen_name, year) %>%
  # add up the value of crops together
  dplyr::summarise(value = sum(value)) %>%
  mutate(Units = "2015$",
         region = "colombia",
         Metric = "Value of Crop Production") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Crop Exports
qry <- "Crop exports.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

CropExports <- prj$data$`Crop exports`

### Crop Exports Value in $
ValueCropExports <- CropExports %>%
  mutate(subsector = gsub('Colombia traded ', '', subsector)) %>%
  filter(subsector %in% c("corn", "fiberCrop", "Wheat",
                       "sugarCrop", "rice", "othergrain",
                       "oilCrop", "miscCrop", "palmfruit", "biomass")) %>%
  left_join(CropPrices, by = c("scenario", "input" = "market", "year", "experiment", "old_scen_name"),
            suffix = c(".Exports", ".Price")) %>%
  # value of crop exports = sum of (crop exports X price of crop)
  # non-biomass crops: Mt * 1975$/kg * 10^9 * 4.25 2015$/1975$ = 2015$
  # biomass crops: EJ * 1975$/GJ * 10^9 * 4.25 2015$/1975$ = 2015$
  mutate(value = if_else(input != "biomass",
                         value.Exports * value.Price * 10^9 * 4.25,
                         value.Exports * value.Price * 10^9 * 4.25)) %>%
  group_by(scenario, experiment, old_scen_name, year) %>%
  # add up the value of crops together
  dplyr::summarise(value = sum(value)) %>%
  mutate(Units = "2015$",
         region = "colombia",
         Metric = "Value of Crop Exports") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)


### Biomass Exports in EJ
BiomassExports <- CropExports %>%
  filter(sector %in% c('traded biomass') & subsector %in% 'Colombia traded biomass') %>%
  mutate(Metric = 'Biomass Exports') %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)


Metrics <- rbind(as_tibble(UnmanagedLand),
                 as_tibble(ForestLand),
                 as_tibble(ValueForestLand),
                 as_tibble(CropLand),
                 as_tibble(biomass),
                 as_tibble(biomass_trad),
                 as_tibble(biomassp),
                 as_tibble(biomassp_trad),
                 as_tibble(ValueCropProd),
                 as_tibble(ValueCropExports),
                 as_tibble(BiomassExports),
                 as_tibble(BiomassTrans))

rdm_cart(UnmanagedLand, exp, export_dir)
rdm_cart(ForestLand, exp, export_dir)
rdm_cart(ValueForestLand, exp, export_dir)
rdm_cart(CropLand, exp, export_dir)
rdm_cart(TCO2eqAgri, exp, export_dir) # need to run in another script.
rdm_cart(biomass, exp, export_dir)
rdm_cart(biomass_trad, exp, export_dir)
rdm_cart(biomassp, exp, export_dir)
rdm_cart(biomassp_trad, exp, export_dir)
rdm_cart(ValueCropProd, exp, export_dir)
rdm_cart(ValueCropExports, exp, export_dir)
rdm_cart(BiomassExports, exp, export_dir)
rdm_cart(BiomassTrans, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', '3-Crop&Biomass'))){
  dir.create(file.path(export_dir, 'distribution', '3-Crop&Biomass'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', '3-Crop&Biomass', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
  }

# --------------------------------------------------------------------------
# 4. Electricity Investments
# --------------------------------------------------------------------------

# Create dataFrame that will store all uncertainty results to be plotted
plot_df <- data.frame(scenario = character(), experiment = integer(), region = character(), param = character(),
                      year = integer(), value = numeric(), Units = integer())

# Use Metis to process certain complex queries that (1) require significant post-processing, and (2) for which
# specific Metis functions/handing have already been developed. For example, this includes electricity investments. We
# produce a single ".proj" file on PIC that stores the GCAM queries for hundreds or thousands of GCAM runs. This
# ".proj" file is already in the format required by Metis, so very little pre-processing is required below. However,
# some post-processing of metis-results is required to get the Metis outputs formatted for plotting.

# Select relevant parameters that we want to process in Metis. The rest (which are simpler) will be processed here in
# this script, not in Metis.
paramsSelect_i <- c("elecCumCapGW", "elecNewCapGW", "elecCumCapCost", "elecNewCapCost", "elecAnnualRetPrematureGW",
                    "elecCumRetPrematureGW", "elecAnnualRetPrematureCost", "elecCumRetPrematureCost")   # c('emissCO2BySectorNoBio')  # c('All')


# qry <- 'elec gen by gen tech and cooling tech and vintage.proj'
# prj_path <- file.path(base_dir, qry)
# prj <- loadProject(prj_path)
# elec_vint <- prj$data$`elec gen by gen tech and cooling tech and vintage` %>% 
#   filter(output %in% 'electricity') %>% 
#   mutate(scenario = old_scen_name)
# 
# qry <- 'Electricity generation by aggregate technology.proj'
# prj_path <- file.path(base_dir, qry)
# prj <- loadProject(prj_path)
# elec_tech <- prj$data$`Electricity generation by aggregate technology` %>% 
#   mutate(scenario = old_scen_name)
# 
# RDM_results_for_Metis <- addQueryTable(prj, elec_tech, 'Electricity generation by aggregate technology', clobber = TRUE)


qry <- "select_queries_metis.proj"
prj_path <- file.path(base_dir, qry)
dataProj_i <- prj_path  # Use if gcamdata has been saved as .proj file
RDM_results_for_Metis <- loadProject(dataProj_i)
scenOrigNames_i <- listScenarios(RDM_results_for_Metis)
regionsSelect_i <- c("Colombia")

dataGCAM <- metis.readgcam(reReadData = F,  # TRUE create a .proj file
                           scenOrigNames = scenOrigNames_i,
                           dataProj = dataProj_i,
                           regionsSelect = regionsSelect_i,
                           paramsSelect = paramsSelect_i)

# dataGCAM <- plutus::gcamInvest(reReadData = F,  # TRUE create a .proj file
#                                scenOrigNames = scenOrigNames_i,
#                                dataProj = RDM_results_for_Metis,
#                                regionsSelect = regionsSelect_i,
#                                saveData = FALSE)

# Post-process Metis outputs so that you have Scenarios and experiments correctly broken out. For example, there are 80
# experiments per scenario. All should have the same scenario name (e.g., "Reference")
reorg_dataGCAM_data <- dataGCAM$data %>%
  mutate(scenario = gsub('DDP_Delayed_EndPt', 'DelayedEndPt', scenario)) %>%
  mutate(scenario = gsub('DDP_Delayed_CumEmiss', 'DelayedCumEmiss', scenario)) %>%
  mutate(scenario = gsub('DDP_XL', 'DDPXL', scenario)) %>%
  mutate(experiment = substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
  mutate(old_scen_name = scenario) %>%
  mutate(scenario = substring(scenario, 0, regexpr("_", scenario) - 1)) %>%
  dplyr::rename(year = x) %>%
  dplyr::rename(Units = units)
plot_df_append <- reorg_dataGCAM_data %>%
  dplyr::group_by(scenario, region, experiment, year, param, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()
plot_df_append <- plot_df_append[, c(1,3,2,6,5,7,4)]

# Load reference scenario
if(F){
  dataGCAM_ref <- metis.readgcam(reReadData = F,  # TRUE create a .proj file
                               scenOrigNames = 'Reference',
                               dataProj = 'E:/NEXO-UA/Results/metis/gcam_database/outputs/dataProj_gcam5p3_HadGEM2-ES_rcp8p5.proj',
                               regionsSelect = regionsSelect_i,
                               paramsSelect = paramsSelect_i)
reorg_dataGCAM_ref <- dataGCAM_ref$data %>%
  dplyr::rename(year = x) %>%
  dplyr::rename(Units = units) %>%
  dplyr::group_by(scenario, region, year, param, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()
# Calculate net investment or GW by substracting value from Reference scenario
plot_df_append <- plot_df_append %>%
  left_join(reorg_dataGCAM_ref,
            by = c('region', 'param', 'Units', 'year'),
            suffix = c('.exp', '.ref')) %>%
  mutate(value = value.exp - value.ref) %>%
  dplyr::rename(scenario = scenario.exp) %>%
  dplyr::select(-value.exp, -value.ref, -scenario.ref)
}


plot_df <- rbind(plot_df, plot_df_append)
plot_df <- plot_df %>%
  mutate(Units = ifelse(grepl('Cum', param), paste0('Cum ', Units), Units))


# Plot investments/stranded assets for a select subset of runs.
params <- c("elecCumCapGW", "elecNewCapGW", "elecCumCapCost", "elecNewCapCost", "elecAnnualRetPrematureGW",
            "elecCumRetPrematureGW", "elecAnnualRetPrematureCost", "elecCumRetPrematureCost")
ymin_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = -6, "elecCumRetPrematureGW" = -15, # was "elecCumRetPrematureGW" = -5,
                  "elecAnnualRetPrematureCost" = -5, "elecCumRetPrematureCost" = -20) # was "elecCumRetPrematureCost" = -10
ymax_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = 1, "elecCumRetPrematureGW" = 1,
                  "elecAnnualRetPrematureCost" = 1, "elecCumRetPrematureCost" = 1)

if(!file.exists(file.path(export_dir, 'distribution', '4-ElecInvestments'))){
  dir.create(file.path(export_dir, 'distribution', '4-ElecInvestments'))
}
# select_exps <- c('42', '58', '46', '62')
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
for(paramSelect in params){
  title <- paramSelect
  fig_path <- file.path(export_dir, 'distribution', '4-ElecInvestments', paste0(paramSelect, "_select", '.png'))
  plot_df_sub <- plot_df %>%
    filter(param == paramSelect)
  y_lbl <- unique(plot_df_sub$Units)[1]
  line_plot(
    plot_df_sub,
    fig_path,
    plot_scens,
    plot_XLfacs,
    y_lbl = y_lbl,
    x_min = x_min,
    x_max = x_max,
    y_min = ymin_list[[paramSelect]],
    y_max = ymax_list[[paramSelect]],
    plot_by_scen = FALSE,
    plot_by_XLfac = FALSE,
    title = NULL,
    plot_by_select_experiment = FALSE,
    gray_ribbon = TRUE,
    distribution = TRUE
  )

  df_cart <- plot_df_sub %>%
    mutate(old_scen_name = scenario,
           Metric = strsplit(y_lbl, '\\(|\\)')[[1]][1],
           Units = strsplit(y_lbl, '\\(|\\)')[[1]][2]) %>%
    select(-param)
  rdm_cart(df_cart, exp, export_dir)
}


# --------------------------------------------------------------------------
# 5. Electrification and Alternative Fuels
# --------------------------------------------------------------------------

qry <- "final energy consumption by sector and fuel.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

FinalEneFuel <- prj$data$`final energy consumption by sector and fuel`

TotalFinalEneFuel <- FinalEneFuel %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

## Electricity percentage of final energy
ElecFinalEneFuel <- FinalEneFuel %>%
  filter(input == "electricity") %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

ElecPctFinalEneFuel <- ElecFinalEneFuel %>%
  left_join(TotalFinalEneFuel, by = c("scenario", "region", "Units", "year", "experiment", "old_scen_name"),
            suffix = c(".elec", ".total")) %>%
  mutate(value = value.elec / value.total * 100,
         Units = "%",
         Metric = "Electricity Share of Final Energy") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Hydrogen percentage of final energy
HydrogenFinalEneFuel <- FinalEneFuel %>%
  filter(input == "hydrogen") %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

HydrogenPctFinalEneFuel <- HydrogenFinalEneFuel %>%
  left_join(TotalFinalEneFuel, by = c("scenario", "region", "Units", "year", "experiment", "old_scen_name"),
            suffix = c(".hydrogen", ".total")) %>%
  mutate(value = value.hydrogen / value.total * 100,
         Units = "%",
         Metric = "Hydrogen Share of Final Energy") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Bioenergy percentage of final energy
## Biomass Final Energy in the form of Liquids, Electricity, Gas, and Biomass

BioFinalEne_liq <- FinalEneFuel %>%
  filter(input %in% 'refined liquids') %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(biomass_liq_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)
BioFinalEne_elec <- FinalEneFuel %>%
  filter(input %in% 'electricity') %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(biomass_elec_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)
BioFinalEne_gas <- FinalEneFuel %>%
  filter(input %in% 'gas') %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(biomass_gas_ratio,
            by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year'),
            suffix = c('.total', '.ratio')) %>%
  mutate(value = value.ratio * value.total) %>%
  select(-value.ratio, -value.total)
BioFinalEne_biomass <- FinalEneFuel %>%
  filter(input %in% 'biomass') %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))


### Calculate the biofuels as a proportion of transportation energy consumption (%)
BiomassFinalEne <- Reduce(function(...)
  left_join(..., by = c('scenario', 'region', 'experiment', 'old_scen_name', 'year')),
  list(BioFinalEne_liq, BioFinalEne_elec, BioFinalEne_gas, BioFinalEne_biomass, TotalFinalEneFuel)) %>%
  dplyr::rename(bio_liq = value.x,
         bio_elec = value.y,
         bio_gas = value.x.x,
         bio_final = value.y.y,
         total_final = value) %>%
  mutate(value = (bio_liq + bio_elec + bio_gas + bio_final)/total_final * 100,
         Metric = 'Bioenergy Share of Final Energy',
         Units = '%') %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)


Metrics <- rbind(as_tibble(ElecPctFinalEneFuel),
                 as_tibble(HydrogenPctFinalEneFuel),
                 as_tibble(BiomassFinalEne))

rdm_cart(ElecPctFinalEneFuel, exp, export_dir)
rdm_cart(HydrogenPctFinalEneFuel, exp, export_dir)
rdm_cart(BiomassFinalEne, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', '5-Electrification&AltFuels'))){
  dir.create(file.path(export_dir, 'distribution', '5-Electrification&AltFuels'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', '5-Electrification&AltFuels', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
  }


# --------------------------------------------------------------------------
# 6. Global Oil & Gas Price
# --------------------------------------------------------------------------

## Global oil price
oilprice2020 <- PriceAllMarkets %>%
  filter(market %in% c("globalcrude oil"), year %in% c("2020")) %>%
  group_by(scenario, region, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))
oilprice <- PriceAllMarkets %>%
  filter(market %in% c("globalcrude oil")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(oilprice2020, by = c("scenario", "region", "experiment", "old_scen_name")) %>%
  mutate(value = ((value.x - value.y) / value.y) + 1, Units = "2020 Reference",
         Metric = "Global Oil Price", value.x = NULL, value.y = NULL)

## Global natural gas price
ngprice2020 <- PriceAllMarkets %>%
  filter(market %in% c("globalnatural gas"), year %in% c("2020")) %>%
  group_by(scenario, region, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))
ngprice <- PriceAllMarkets %>%
  filter(market %in% c("globalnatural gas")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(ngprice2020, by = c("scenario", "region", "experiment", "old_scen_name")) %>%
  mutate(value = ((value.x - value.y) / value.y) + 1, Units = "2020 Reference",
         Metric = "Global Natural Gas Price", value.x = NULL, value.y = NULL)


## Colombia oil
oil <- PriEne %>%
  filter(fuel %in% c("a oil")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Units = "EJ",
         Metric = "Oil Primary Energy Consumption")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
oilp <- oil %>%
  left_join(all, by = c("scenario", "region", "experiment", "old_scen_name", "year")) %>%
  mutate(value = (value.x / value.y) * 100, Units = "%", value.x = NULL, value.y =
           NULL, Units.x = NULL, Units.y = NULL, Metric = "Oil Primary Energy Consumption") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

## Colombia natural gas
gas <- PriEne %>%
  filter(fuel %in% c("b natural gas")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Units = "EJ",
         Metric = "Gas Primary Energy Consumption")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
gasp <- gas %>%
  left_join(all, by = c("scenario", "region", "experiment", "old_scen_name", "year")) %>%
  mutate(value = (value.x / value.y) * 100, Units = "%", value.x = NULL, value.y = NULL,
         Units.x = NULL, Units.y = NULL, Metric = "Gas Primary Energy Consumption") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)


## Global oil consumption
## Global natural gas
qry <- "demand of all markets.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

DemandAllMarkets <- prj$data$`demand of all markets`

GlobalOil <- DemandAllMarkets %>%
  filter(market %in% 'globalcrude oil') %>%
  mutate(Metric = 'Global Oil Consumption') %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
GlobalNG <- DemandAllMarkets %>%
  filter(market %in% 'globalnatural gas') %>%
  mutate(Metric = 'Global Natural Gas Consumption') %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

Metrics <- rbind(as_tibble(oilprice),
                 as_tibble(ngprice),
                 as_tibble(oil),
                 as_tibble(gas),
                 as_tibble(GlobalOil),
                 as_tibble(GlobalNG))

rdm_cart(oilprice, exp, export_dir)
rdm_cart(ngprice, exp, export_dir)
rdm_cart(oil, exp, export_dir)
rdm_cart(gas, exp, export_dir)
rdm_cart(GlobalOil, exp, export_dir)
rdm_cart(GlobalNG, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', '6-GlobalOilGasPrice'))){
  dir.create(file.path(export_dir, 'distribution', '6-GlobalOilGasPrice'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', '6-GlobalOilGasPrice', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
}


# Transportation externality costs ----------------------------------------------------

qry <- "transport final energy by mode and fuel.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

TrnFinalEneModeFuel <- prj$data$`transport final energy by mode and fuel`

qry <- "transport service output by tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

TrnServTech <- prj$data$`transport service output by tech`

#Get fuel use in 2015 for gasoline and diesel vehicles
#Passenger vehicles
GasolinePass_2015 <- TrnFinalEneModeFuel %>%
  filter(year == 2015,
         mode %in% c("2W and 3W", "Car", "Mini Car", "Large Car and Truck"),
         input == "refined liquids enduse") %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))

DieselPass_2015 <- TrnFinalEneModeFuel %>%
  filter(year == 2015,
         mode %in% c("Bus"), 
         input == "refined liquids enduse") %>%
  group_by(scenario, region, experiment, old_scen_name, year,  Units) %>%
  dplyr::summarise(value = sum(value))

#Freight vehicles
Freight_2015 <- TrnFinalEneModeFuel %>%
  filter(year ==  2015,
         sector == "trn_freight_road",
         input == "refined liquids enduse") %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))


#Get refined liquids service demand in 2015
PassRoad_Liquid_2015 <- TrnServTech %>%
  filter(year == 2015,
         technology == "Liquids",
         subsector %in% c("2W and 3W", "Car", "Mini Car", "Large Car and Truck", "Bus")) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))

FreightRoad_Liquid_2015 <- TrnServTech %>%
  filter(year == 2015,
         technology == "Liquids",
         sector == "trn_freight_road") %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))


#Get all service demand, across years
PassRoad <- TrnServTech %>%
  filter(subsector %in% c("2W and 3W", "Car", "Mini Car", "Large Car and Truck", "Bus")) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))

FreightRoad <- TrnServTech %>%
  filter(sector == "trn_freight_road") %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value))

# Calculate externality costs:
# Fuel use [EJ] * IMF costs [$/L] * [L/MJ] * 10^12 MJ/EJ 

#Congestion
CongestionGasolinePass_2015 <- GasolinePass_2015 %>%
  mutate(value = value * 0.12 * 0.03 * 10^12,
         Units = "2015$")

CongestionDieselPass_2015 <- DieselPass_2015 %>%
  mutate(value = value * 0.10 * 0.026 * 10^12,
         Units = "2015$")

#add diesel and gasoline costs together
#divide by service demand in 2015 to obtain normalized cost
CongestionPass_2015 <- bind_rows(CongestionDieselPass_2015,
                            CongestionGasolinePass_2015) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(PassRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
            suffix = c(".USD", ".mill_pass_km")) %>%
  mutate(value = value.USD/value.mill_pass_km,
         Units = "2015$/mill_pass_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
CongestionPass <- PassRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(CongestionPass_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
          suffix = c(".mill_pass_km", ".2015USD_mill_pass_km")) %>%
  mutate(value = value.mill_pass_km * value.2015USD_mill_pass_km,
         year = year.mill_pass_km,
         Units = "2015$")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#divide by service demand in 2015 to obtain normalized cost
CongestionFreight_2015 <- Freight_2015 %>%
  mutate(value = value * 0.10 * 0.026 * 10^12,
         Units = "2015$") %>%
  left_join(FreightRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
                                   suffix = c(".USD", ".mill_ton_km")) %>%
  mutate(value = value.USD/value.mill_ton_km,
         Units = "2015$/mill_ton_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
CongestionFreight <- FreightRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(CongestionFreight_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
            suffix = c(".mill_ton_km", ".2015USD_mill_ton_km")) %>%
  mutate(value = value.mill_ton_km * value.2015USD_mill_ton_km,
         year = year.mill_ton_km,
         Units = "2015$")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#Accidents
AccidentGasolinePass_2015 <- GasolinePass_2015 %>%
  mutate(value = value * 0.19 * 0.03 * 10^12,
         Units = "2015$")
  
AccidentDieselPass_2015 <- DieselPass_2015 %>%
  mutate(value = value * 0.09 * 0.026 * 10^12,
         Units = "2015$")

#add diesel and gasoline costs together
#divide by service demand in 2015 to obtain normalized cost
AccidentPass_2015 <- bind_rows(AccidentDieselPass_2015,
                                 AccidentGasolinePass_2015) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(PassRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
            suffix = c(".USD", ".mill_pass_km")) %>%
  mutate(value = value.USD/value.mill_pass_km,
         Units = "2015$/mill_pass_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
AccidentPass <- PassRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(AccidentPass_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
            suffix = c(".mill_pass_km", ".2015USD_mill_pass_km")) %>%
  mutate(value = value.mill_pass_km * value.2015USD_mill_pass_km,
         year = year.mill_pass_km,
         Units = "2015$")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#divide by service demand in 2015 to obtain normalized cost
AccidentFreight_2015 <- Freight_2015 %>%
  mutate(value = value * 0.09 * 0.026 * 10^12,
         Units = "2015$") %>%
  left_join(FreightRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
            suffix = c(".USD", ".mill_ton_km")) %>%
  mutate(value = value.USD/value.mill_ton_km,
         Units = "2015$/mill_ton_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
AccidentFreight <- FreightRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(AccidentFreight_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
            suffix = c(".mill_ton_km", ".2015USD_mill_ton_km")) %>%
  mutate(value = value.mill_ton_km * value.2015USD_mill_ton_km,
         year = year.mill_ton_km,
         Units = "2015$") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#Road damages
RdDamageGasolinePass_2015 <- GasolinePass_2015 %>%
  mutate(value = value * 0 * 0.03 * 10^12,
         Units = "2015$")

RdDamageDieselPass_2015 <- DieselPass_2015 %>%
  mutate(value = value * 0.01 * 0.026 * 10^12,
         Units = "2015$")

#add diesel and gasoline costs together
#divide by service demand in 2015 to obtain normalized cost
RdDamagePass_2015 <- bind_rows(RdDamageDieselPass_2015,
                               RdDamageGasolinePass_2015) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(PassRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
            suffix = c(".USD", ".mill_pass_km")) %>%
  mutate(value = value.USD/value.mill_pass_km,
         Units = "2015$/mill_pass_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
RdDamagePass <- PassRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(RdDamagePass_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
            suffix = c(".mill_pass_km", ".2015USD_mill_pass_km")) %>%
  mutate(value = value.mill_pass_km * value.2015USD_mill_pass_km,
         year = year.mill_pass_km,
         Units = "2015$")%>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#divide by service demand in 2015 to obtain normalized cost
RdDamageFreight_2015 <- Freight_2015 %>%
  mutate(value = value * 0.01 * 0.026 * 10^12,
         Units = "2015$") %>%
  left_join(FreightRoad_Liquid_2015, by = c("scenario", "region", "experiment", "old_scen_name", "year"),
            suffix = c(".USD", ".mill_ton_km")) %>%
  mutate(value = value.USD/value.mill_ton_km,
         Units = "2015$/mill_ton_km") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

#multiply normalized cost by service demand across years
RdDamageFreight <- FreightRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(RdDamageFreight_2015, by = c("scenario", "region", "experiment", "old_scen_name"),
            suffix = c(".mill_ton_km", ".2015USD_mill_ton_km")) %>%
  mutate(value = value.mill_ton_km * value.2015USD_mill_ton_km,
         year = year.mill_ton_km,
         Units = "2015$") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Units)

# Total transportation externality costs
TrnExternalityPass <- bind_rows(CongestionPass,
                                AccidentPass,
                                RdDamagePass) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Passenger transport externality costs")

TrnExternalityFreight <- bind_rows(CongestionFreight,
                                   AccidentFreight,
                                   RdDamageFreight) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Freight transport externality costs")

TrnCongestion <- bind_rows(CongestionPass,
                           CongestionFreight) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Congestion costs")

TrnAccidents <- bind_rows(AccidentPass,
                          AccidentFreight) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Accident costs")

TrnRdDamage <- bind_rows(RdDamagePass,
                         RdDamageFreight) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Road damage costs")

TrnExternality <- bind_rows(TrnExternalityPass,
                            TrnExternalityFreight) %>%
  group_by(scenario, region, experiment, old_scen_name, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(Metric = "Transportation externality costs")

TrnPassRoadServ <- PassRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  mutate(Metric = "Road Transport passenger service demand")

TrnFreightRoadServ <- FreightRoad %>%
  filter(year >= 2015 & year <= 2050) %>%
  mutate(Metric = "Road Transport freight service demand")

Metrics <- rbind(as_tibble(TrnCongestion),
                 as_tibble(TrnAccidents),
                 as_tibble(TrnRdDamage),
                 as_tibble(TrnExternality),
                 as_tibble(TrnPassRoadServ),
                 as_tibble(TrnFreightRoadServ))

rdm_cart(TrnCongestion, exp, export_dir)
rdm_cart(TrnAccidents, exp, export_dir)
rdm_cart(TrnRdDamage, exp, export_dir)
rdm_cart(TrnExternality, exp, export_dir)
rdm_cart(TrnPassRoadServ, exp, export_dir)
rdm_cart(TrnFreightRoadServ, exp, export_dir)

if(!file.exists(file.path(export_dir, 'distribution', 'TrnExternalityCosts'))){
  dir.create(file.path(export_dir, 'distribution', 'TrnExternalityCosts'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- file.path(export_dir, 'distribution', 'TrnExternalityCosts', paste0(unique(Metrics$Metric)[i], '.png'))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    plot_df,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = TRUE
  )
}


# VALUE OF AG PRODUCTION AND EXPORTS --------------------------------------
if(F){
  
  biomass_exports <- CropExports %>%
    filter(grepl("biomass", sector))
  
  qry <- "Quantity available for crop commodity demand (domestic and imported).proj"
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  biomass_imports_domestic <- prj$data$`Quantity available for crop commodity demand (domestic and imported)` %>%
    filter(grepl("biomass", sector))
  
  qry <- "traded ag commodity sources.proj"
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  exports <- prj$data$`traded ag commodity sources`
  
  qry <- "regional ag commodity sources.proj"
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  imports_domestic <- prj$data$`regional ag commodity sources`
  
  qry <- "traded ag commodity prices.proj" # we dont have this proj file
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  traded_ag_commodity_prices <- prj$data$`traded ag commodity prices`
  
  qry <- "regional ag commodity price.proj" # we dont have this proj file
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  regional_ag_commodity_prices <- prj$data$`regional ag commodity price`
  
  qry <- "ag commodity prices.proj"
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  crop_prices <- prj$data$`ag commodity prices`
  
  qry <- "regional biomass commodity price.proj" # we dont have this proj file
  prj_path <- file.path(base_dir, qry)
  prj <- loadProject(prj_path)
  biomass_commodity_prices <- prj$data$`regional biomass commodity price`
  
  
  region_name <- "Colombia"
  #process biomass
  region_biomass_exports <- biomass_exports %>%
    filter(grepl(region_name, subsector)) %>%
    mutate(region = region_name,
           sector = gsub("traded ", "", sector)) %>%
    select(-input, -subsector)
  
  region_biomass_imports <- biomass_imports_domestic %>%
    filter(grepl("imported", subsector), region == region_name) %>%
    mutate(region = region_name,
           sector = gsub("total ", "", sector)) %>%
    select(-input, -subsector, -technology)
  
  region_biomass_domestic <- biomass_imports_domestic %>%
    filter(grepl("domestic", subsector), region == region_name) %>%
    mutate(region = region_name,
           sector = gsub("total ", "", sector)) %>%
    select(-input, -subsector, -technology)
  
  #combine biomass with the rest of crop and livestock
  region_exports <- exports %>%
    filter(grepl(region_name, subsector)) %>%
    mutate(region = region_name,
           sector = gsub("traded ", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-input, -subsector) %>%
    bind_rows(region_biomass_exports)
  
  region_imports <- imports_domestic %>%
    filter(grepl("imported", subsector), region == region_name) %>%
    mutate(sector = gsub("regional ", "", sector)) %>%
    mutate(sector = gsub("total ", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-subsector, -input) %>%
    bind_rows(region_biomass_imports)
  
  region_domestic <- imports_domestic %>%
    filter(grepl("domestic", subsector), region == region_name) %>%
    mutate(sector = gsub("regional ", "", sector)) %>%
    mutate(sector = gsub("total", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-subsector, -input) %>%
    bind_rows(region_biomass_domestic)
  
  #prices
  region_domestic_biomass_prices <- biomass_commodity_prices %>%
    filter(subsector == "domestic biomass", region == region_name) %>%
    mutate(sector = gsub("total ", "", sector)) %>%
    select(-subsector)
  
  region_imported_biomass_prices <- biomass_commodity_prices %>%
    filter(subsector == "imported biomass", region == region_name) %>%
    mutate(sector = gsub("total ", "", sector)) %>%
    select(-subsector)
  
  region_domestic_prices <- regional_ag_commodity_prices %>%
    filter(grepl("domestic ", subsector), region == region_name) %>%
    mutate(sector = gsub("regional ", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-subsector) %>%
    bind_rows(region_domestic_biomass_prices) %>%
    rename(price.domestic = value)
  
  region_import_prices <- regional_ag_commodity_prices %>%
    filter(grepl("imported ", subsector), region == region_name) %>%
    mutate(sector = gsub("regional ", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-subsector) %>%
    bind_rows(region_imported_biomass_prices) %>%
    rename(price.imports = value)
  
  region_export_prices <- traded_ag_commodity_prices %>%
    filter(grepl(region_name, subsector),
           grepl("traded ", subsector)) %>%
    mutate(region = region_name) %>%
    mutate(sector = gsub("traded ", "", sector)) %>%
    mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
    select(-subsector) %>%
    rename(price.exports = value)
  
  
  #Combine region's imports, exports, and calculate net trade volume by commodity
  region_net_trade_volume <- region_imports %>%
    full_join(region_exports, by = c("region", "Units", "scenario", "sector", "year"), suffix = c(".imports", ".exports")) %>%
    filter(year %in% c(seq(2015, 2050, 5))) %>%
    #Imports are negative, exports are positive
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(value.imports = value.imports * -1,
           value.exports = if_else(is.na(value.exports), 0, value.exports),
           value.net = value.exports + value.imports)
  
  #value of net trade by commodity
  region_net_trade_value <- region_net_trade_volume %>%
    left_join(region_import_prices, by = c("scenario", "region", "sector", "year")) %>%
    select(-Units.x, -Units.y) %>%
    left_join(region_export_prices, by = c("scenario", "region", "sector", "year")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(value.imports = value.imports * price.imports * 10^9 * 3.5,
           value.exports = value.exports * price.exports * 10^9 * 3.5) %>%
    mutate(value.net = value.exports + value.imports,
           Units = "2015$")
  
  #total value of net trade
  region_total_net_trade_value <- region_net_trade_value %>%
    group_by(scenario, region, year, Units) %>%
    summarise_at( c("value.imports", "value.exports", "value.net"), sum)
  
  #value of domestic commodity
  region_domestic_value <- region_domestic %>%
    filter(year %in% c(seq(2015, 2050, 5))) %>%
    left_join(region_domestic_prices, by = c("scenario", "region", "sector", "year")) %>%
    mutate(value.domestic = value * price.domestic * 10^9 * 3.5,
           Units = "2015$") %>%
    select(-Units.x, -Units.y, -value)
  
  #total value of domestic commodities
  region_total_domestic_value <- region_domestic_value %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    group_by(scenario, region, year, Units) %>%
    dplyr::summarise(value.domestic = sum(value.domestic))
  
  #value of production by commodity
  region_production_value <- region_domestic_value %>%
    full_join(region_net_trade_value, by = c("scenario", "region", "sector", "year", "Units")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(value.production = value.domestic + value.exports)
  
  #total value of production
  region_total_production_value <- region_production_value %>%
    group_by(scenario, region, year, Units) %>%
    dplyr::summarise(value.production = sum(value.production))
}

# Save all environmental data for post processing
# save.image(file='E:/NEXO-UA/Results/RDM_Colombia/runs_512_02_08_2021/runs_512_01_08_2021.RData')