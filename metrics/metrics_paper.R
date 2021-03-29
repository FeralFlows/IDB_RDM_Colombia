
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

# Set directories, load extra files ---------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
metrics_folder <- getwd()
run_name <- 'runs_512_02_08_2021' # only need to change this to the run folder name
setwd(paste('..', run_name, sep = '/'))

base_dir <- paste(getwd(), 'query_proj/', sep = '/')
export_dir <- paste(getwd(), 'output_science_questions/', sep = '/')
if(!file.exists(export_dir)){dir.create(export_dir)}

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


# 1. Intermittent Renewables ----------------------------------------------
qry <- "Electricity generation by aggregate technology.proj"
prj_path <- paste0(base_dir, qry)
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
prj_path <- paste0(base_dir, qry)
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


## ------------------------------- NEW 1 -------------------------------
## Negative CO2 Emissions
qry <- "CO2 emissions by sector.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

CO2Sector <- prj$data$`CO2 emissions by sector`

NegativeCO2 <- CO2Sector %>% 
  filter(sector %in% c('regional biomass', 'regional biomassOil', 'regional sugar for ethanol')) %>% 
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarise(value = sum(value) * 44/12) %>%
  mutate(Metric = 'Negative CO2 Emission',
         Units = 'MTCO2')

## ------------------------------- END NEW 1 -------------------------------


Metrics <- rbind(as_tibble(PowGenRenewInt),
                 as_tibble(PowGenRenewIntGW),
                 as_tibble(PowGenHydro),
                 as_tibble(PowGenHydroGW),
                 as_tibble(NegativeCO2))

rdm_cart(PowGenRenewInt)
rdm_cart(PowGenRenewIntGW)
rdm_cart(PowGenHydro)
rdm_cart(PowGenHydroGW)
rdm_cart(NegativeCO2)

if(!file.exists(paste0(export_dir, '1-IntermittentRenewables/'))){
  dir.create(paste0(export_dir, '1-IntermittentRenewables/'))
}
# select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir, '1-IntermittentRenewables/', unique(Metrics$Metric)[i], '.png', sep = ""))
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


## GHG emissions -----------------------------------------------------------

qry <- "nonCO2 emissions by resource production.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

NonCO2ResProd <- prj$data$`nonCO2 emissions by resource production` %>%
  mutate(sector = prj$data$`nonCO2 emissions by resource production`$resource,
         resource = NULL)

NonCO2ResProd <- NonCO2ResProd %>%
  group_by(scenario, region, sector, ghg, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))


qry <- "nonCO2 emissions by sector.proj"
prj_path <- paste0(base_dir, qry)
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


# 2. Air Pollution --------------------------------------------------------

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

Metrics <- rbind(as_tibble(NOx),
                 as_tibble(SO2),
                 as_tibble(BC),
                 as_tibble(OC))

rdm_cart(NOx)
rdm_cart(SO2)
rdm_cart(BC)
rdm_cart(OC)


if(!file.exists(paste0(export_dir, '2-AirPollution/'))){
  dir.create(paste0(export_dir, '2-AirPollution/'))
  }
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir, '2-AirPollution/', unique(Metrics$Metric)[i], '.png', sep = ""))
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


# 3. Crops & Biomass -----------------------------------------------------

qry <- "aggregated land allocation.proj"
prj_path <- paste0(base_dir, qry)
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

## Crop land
CropLand <- AggLandAlloc %>%
  filter(landleaf %in% c("crops")) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Crop Land')


qry <- "primary energy consumption by region (avg fossil efficiency).proj"
prj_path <- paste0(base_dir, qry)
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

## ------------------------------- NEW 2 -------------------------------
## Biomass Share of Transport Final Energy

### Biofuel proportion of refined liquids production
qry <- "refined liquids production by subsector.proj"
prj_path <- paste0(base_dir, qry)
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
prj_path <- paste0(base_dir, qry)
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
prj_path <- paste0(base_dir, qry)
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
prj_path <- paste0(base_dir, qry)
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

## ------------------------------- END NEW 2 -------------------------------


## Value of Crop Production
# Note: this does not include inputs to livestock products: FeedCrops, FodderHerb, FodderGrass
qry <- "prices of all markets.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

PriceAllMarkets <- prj$data$`prices of all markets`

CropPrices <- PriceAllMarkets %>%
  mutate(market = gsub("Colombia", "", market)) %>%
  filter(market %in% c("Corn", "FiberCrop", "Wheat",
                       "SugarCrop", "RootTuber", "Rice", "OtherGrain",
                       "OilCrop", "MiscCrop", "PalmFruit", "biomass"))

qry <- "Ag Production by Crop Type.proj"
prj_path <- paste0(base_dir, qry)
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
prj_path <- paste0(base_dir, qry)
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
                 as_tibble(CropLand),
                 as_tibble(biomass),
                 as_tibble(biomassp),
                 as_tibble(ValueCropProd),
                 as_tibble(ValueCropExports),
                 as_tibble(BiomassExports),
                 as_tibble(BiomassTrans))

rdm_cart(UnmanagedLand)
rdm_cart(CropLand)
rdm_cart(biomass)
rdm_cart(biomassp)
rdm_cart(ValueCropProd)
rdm_cart(ValueCropExports)
rdm_cart(BiomassExports)
rdm_cart(BiomassTrans)

if(!file.exists(paste0(export_dir, '3-Crop&Biomass/'))){
  dir.create(paste0(export_dir, '3-Crop&Biomass/'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir, '3-Crop&Biomass/', unique(Metrics$Metric)[i], '.png', sep = ""))
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


# 4. Electricity Investments ----------------------------------------------

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

qry <- "select_queries_metis.proj"
prj_path <- paste0(base_dir, qry)
dataProj_i <- prj_path  # Use if gcamdata has been saved as .proj file

RDM_results_for_Metis <- loadProject(dataProj_i)
scenOrigNames_i <- listScenarios(RDM_results_for_Metis)
regionsSelect_i <- c("Colombia")
dataGCAM <- metis.readgcam(reReadData = F,  # TRUE create a .proj file
                           scenOrigNames = scenOrigNames_i,
                           dataProj = dataProj_i,
                           regionsSelect = regionsSelect_i,
                           paramsSelect = paramsSelect_i)

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

if(!file.exists(paste0(export_dir, '4-ElecInvestments/'))){
  dir.create(paste0(export_dir, '4-ElecInvestments/'))
}
# select_exps <- c('42', '58', '46', '62')
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
for(paramSelect in params){
  title <- paramSelect
  fig_path <- paste0(export_dir, '4-ElecInvestments/', paramSelect, "_select", '.png')
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
  rdm_cart(df_cart)
}



# 5. Electrification and Alternative Fuels --------------------------------

qry <- "final energy consumption by sector and fuel.proj"
prj_path <- paste0(base_dir, qry)
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

## ----------------------------------- NEW 3 -----------------------------------
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

##------------------------------- END NEW 3 ------------------------------------

Metrics <- rbind(as_tibble(ElecPctFinalEneFuel),
                 as_tibble(HydrogenPctFinalEneFuel),
                 as_tibble(BiomassFinalEne))

rdm_cart(ElecPctFinalEneFuel)
rdm_cart(HydrogenPctFinalEneFuel)
rdm_cart(BiomassFinalEne)

if(!file.exists(paste0(export_dir, '5-Electrification&AltFuels/'))){
  dir.create(paste0(export_dir, '5-Electrification&AltFuels/'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir, '5-Electrification&AltFuels/', unique(Metrics$Metric)[i], '.png', sep = ""))
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


# 6. Global Oil & Gas Price -----------------------------------------------

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


##---------------------------------- NEW 4 ------------------------------------
## Global oil consumption
## Global natural gas 
qry <- "demand of all markets.proj"
prj_path <- paste0(base_dir, qry)
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

##------------------------------- END NEW 4 ------------------------------------

Metrics <- rbind(as_tibble(oilprice),
                 as_tibble(ngprice),
                 as_tibble(oil),
                 as_tibble(gas),
                 as_tibble(GlobalOil),
                 as_tibble(GlobalNG))

rdm_cart(oilprice)
rdm_cart(ngprice)
rdm_cart(oil)
rdm_cart(gas)
rdm_cart(GlobalOil)
rdm_cart(GlobalNG)

if(!file.exists(paste0(export_dir, '6-GlobalOilGasPrice/'))){
  dir.create(paste0(export_dir, '6-GlobalOilGasPrice/'))
}
#select_exps <- c('66', '82', '70', '86')
for(i in seq(length(unique(Metrics$Metric)))){
  M <- Metrics %>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir, '6-GlobalOilGasPrice/', unique(Metrics$Metric)[i], '.png', sep = ""))
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
