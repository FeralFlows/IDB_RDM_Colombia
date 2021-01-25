
# Install and load packages -----------------------------------------------

# install.packages('fansi')
# install.packages("backports")
# install.packages("devtools")
# install.packages("sf")
# install.packages("magick")
# library(devtools)
# install_github('JGCRI/rgcam', build_vignettes=TRUE, force = TRUE)
# install.packages("rgdal")
# install.packages("tmap")
# 
# library('tmap')
# install_github('JGCRI/metis', build_vignettes=TRUE, force = TRUE)


library(tibble)
library(rgdal)
library('rgcam')
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')
library(reshape)
library(data.table)

# Set directories, load extra files ---------------------------------------

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('D:/RDM/runs_512_1_14_2021')

#base_dir <- c('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021/query_proj/')
#export_dir <- c('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021/output_color/')
base_dir <- c('D:/RDM/runs_512_1_14_2021/query_proj/')
export_dir <- c('D:/RDM/runs_512_1_14_2021/output_grey/')

if(!file.exists(export_dir)){  dir.create(export_dir)}

# #Import Reference tables
# GWP<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/GWP.csv', header = TRUE, sep = ",", dec = ".")
# #Import Correction factors
# CF<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/CorreccionFactor.csv', header = TRUE, sep = ",", dec = ".")
# #Import deforestation emission trajectory
# ghgtraj<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/GHG traj.csv') %>%
#   ##BY 12-17-2020
#   gather(key = "year", "value", c("X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050"))
#   #ghgtraj<-melt(ghgtraj, id.vars=c( "Units" ),variable.name = c("year"))
#  ghgtraj$year<-as.numeric(substring(ghgtraj$year,2))
GWP<-read.csv('D:/RDM/IDB_RDM_Colombia/metrics/GWP.csv', header = TRUE, sep = ",", dec = ".")
#Import Correction factors
CF<-read.csv('D:/RDM/IDB_RDM_Colombia/metrics/CorreccionFactor.csv', header = TRUE, sep = ",", dec = ".")
#Import deforestation emission trajectory
ghgtraj<-read.csv('D:/RDM/IDB_RDM_Colombia/metrics/GHG traj.csv') %>%
  ##BY 12-17-2020
  gather(key = "year", "value", c("X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050"))
#ghgtraj<-melt(ghgtraj, id.vars=c( "Units" ),variable.name = c("year"))
ghgtraj$year<-as.numeric(substring(ghgtraj$year,2))

plot_scens <- c("DDPXL")#"Reference",
x_lbl <- 'Time'
title <- ''
x_min <- 2015
x_max <- 2050

# Source necessary functions ----------------------------------------------

#source('E:/NEXO-UA/Results/RDM_Colombia/metrics/RDM_plotting.R')
#source('E:/NEXO-UA/Results/RDM_Colombia/RDM_CART_fns.R')
source('D:/RDM/IDB_RDM_Colombia/metrics/RDM_plotting.R')
source('D:/RDM/IDB_RDM_Colombia/metrics/RDM_CART_fns.R')


# 1. Intermittent Renewables ----------------------------------------------

qry <- "Electricity generation by aggregate technology.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

TotalPowGen<-prj$data$`Electricity generation by aggregate technology`%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarise(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Total power gen.",Units="Thous GWh")

#Wind and Solar
PowGenRenewInt<-prj$data$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Solar","Wind"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarise(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Wind and Solar Percentage",Units="%")
PowGenRenewInt$value=(PowGenRenewInt$value/TotalPowGen$value)*100

PowGenRenewIntGW<-prj$data$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Solar","Wind"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarise(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Wind and Solar Power generation",Units="Thous GWh")

#Hydro
PowGenHydro<-prj$data$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Hydro"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarise(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Hydro Percentage",Units="%")
PowGenHydro$value=(PowGenHydro$value/TotalPowGen$value)*100
PowGenHydroGW<-prj$data$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Hydro"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarise(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Hydro Power generation",Units="Thous GWh")


Metrics <- rbind(as_tibble(PowGenRenewInt),
                 as_tibble(PowGenRenewIntGW),
                 as_tibble(PowGenHydro),
                 as_tibble(PowGenHydroGW))

rdm_cart(PowGenRenewInt)
rdm_cart(PowGenRenewIntGW)
rdm_cart(PowGenHydro)
rdm_cart(PowGenHydroGW)

#select_exps <- c('66', '82', '70', '86')
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=FALSE)}


# GHG emissions -----------------------------------------------------------

qry <- "nonCO2 emissions by resource production.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

NonCO2ResProd<-prj$data$`nonCO2 emissions by resource production`%>% mutate(sector=prj$data$`nonCO2 emissions by resource production`$resource,resource=NULL)

NonCO2ResProd <- NonCO2ResProd %>%
  group_by(scenario, region, sector, ghg, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))


qry <- "nonCO2 emissions by sector.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

NonCO2Sector <-prj$data$`nonCO2 emissions by sector`

NonCO2_species <-rbind(NonCO2ResProd,NonCO2Sector)

NonCO2_species_MTCO2e<- NonCO2_species %>% left_join(GWP, by="ghg") %>%   mutate(Units="MTCO2e",value=value*AR5,SAR=NULL,AR5=NULL,AR4=NULL,SARall=NULL,AR5all=NULL,AR4all=NULL)
NonCO2_total_MTCO2e<-NonCO2_species_MTCO2e%>% group_by(scenario, region, experiment, old_scen_name,Units, year) %>%dplyr::summarise(value=sum(value))%>%mutate(Metric="Nonco2 Emissions")

#TO-DO: Negative emissions


# 2. Air Pollution --------------------------------------------------------

#NOx
NOx <- NonCO2_species %>%  filter(ghg %in% c("NOx_AGR","NOx_AWB","NOx")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%  dplyr::summarise(value=sum(value)) %>% mutate(Metric="NOx Emissions")

#SO2
SO2<- NonCO2_species %>%  filter(ghg %in% c("SO2_3","SO2_3_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%  dplyr::summarise(value=sum(value)) %>% mutate(Metric="SO2 Emissions")

#BC
BC <- NonCO2_species %>%  filter(ghg %in% c("BC","BC_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%  dplyr::summarise(value=sum(value)) %>% mutate(Metric="BC Emissions")

#OC
OC <- NonCO2_species %>%  filter(ghg %in% c("OC","OC_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%  dplyr::summarise(value=sum(value)) %>% mutate(Metric="OC Emissions")

Metrics <- rbind(as_tibble(NOx),
                 as_tibble(SO2),
                 as_tibble(BC),
                 as_tibble(OC))

rdm_cart(NOx)
rdm_cart(SO2)
rdm_cart(BC)
rdm_cart(OC)

#select_exps <- c('66', '82', '70', '86')
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=FALSE)}


#3. Crops & Biomass -----------------------------------------------------

qry <- "aggregated land allocation.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

AggLandAlloc <- prj$data$`aggregated land allocation`

#Unmanaged land
UnmanagedLand <- AggLandAlloc %>%
  filter(landleaf %in% c("forest (unmanaged)", "pasture (other)"), year %in% c(2010:2050)) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Unmanaged land')

#Crop land
CropLand <- AggLandAlloc %>%
  filter(landleaf %in% c("crops"), year %in% c(2010:2050)) %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(value = value*100,
         Units = "Thous. Ha",
         Metric = 'Crop land')

qry <- "primary energy consumption by region (avg fossil efficiency).proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

PriEne <- prj$data$`primary energy consumption by region (avg fossil efficiency)`

all<-PriEne %>%  group_by(scenario, region, experiment, old_scen_name,Units,year) %>%
  dplyr::summarise(value=sum(value))%>%mutate(Metric="Total primary energy consumption")%>%filter(year%in% c(2010:2050))

#Biomass primary energy (EJ)
biomass<-PriEne %>%filter(fuel %in% c("d biomass"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
  dplyr::summarise(value=sum(value))%>%filter(year%in% c(2010:2050)) %>% mutate(Metric="Biomass primary energy", Units = "EJ") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

#Biomass percentage of primary energy (%)
biomassp<-biomass%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
  mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Biomass share of primary energy", Units = "%")%>%filter(year%in% c(2010:2050)) %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

#TO-DO: Biomass share of transport final energy
#Transport final energy does not report out biomass, will need to figure out a different way to calculate this.

#Value of Crop Production
#Note: this does not include inputs to livestock products: FeedCrops, FodderHerb, FodderGrass
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
  # value of crop production = crop production X price of crop
  # non-biomass crops: Mt * 1975$/kg * 10^9 * 4.25 2015$/1975$ = 2015$
  #biomass crops: EJ * 1975$/GJ * 10^6 * 4.25 2-15$/1975$ = 2015$
  mutate(value = if_else(output != "biomass", 
                         value.Prod * value.Price * 10^9 * 4.25,
                         value.Prod * value.Price * 10^6 * 4.25)) %>%
  group_by(scenario, experiment, old_scen_name, year) %>%
  # add up the value of crops together
  dplyr::summarise(value = sum(value)) %>%
  mutate(Units = "2015$",
         region = "colombia",
         Metric = "Value of Crop Production") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
  
#TO-DO: biomass exports
#T0-D0: crop exports
  
Metrics <- rbind(as_tibble(UnmanagedLand),
                 as_tibble(CropLand),
                 as_tibble(biomass),
                 as_tibble(biomassp),
                 as_tibble(ValueCropProd))

rdm_cart(UnmanagedLand)
rdm_cart(CropLand)
rdm_cart(biomass)
rdm_cart(biomassp)
rdm_cart(ValueCropProd)

#select_exps <- c('66', '82', '70', '86')
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=FALSE)}


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
dataProj_i <-prj_path  # Use if gcamdata has been saved as .proj file

RDM_results_for_Metis <- loadProject(dataProj_i)
scenOrigNames_i <- listScenarios(RDM_results_for_Metis)
regionsSelect_i <- c("Colombia")
dataGCAM<-metis.readgcam(reReadData = F,  # F
                         scenOrigNames = scenOrigNames_i,
                         dataProj = dataProj_i,
                         regionsSelect = regionsSelect_i,
                         paramsSelect=paramsSelect_i)

# Post-process Metis outputs so that you have Scenarios and experiments correctly broken out. For example, there are 80
# experiments per scenario. All should have the same scenario name (e.g., "Reference")
reorg_dataGCAM_data <- dataGCAM$data %>%
  mutate(scenario = gsub('DDP_Delayed_EndPt', 'DelayedEndPt', scenario)) %>%
  mutate(scenario = gsub('DDP_Delayed_CumEmiss', 'DelayedCumEmiss', scenario)) %>%
  mutate(scenario = gsub('DDP_XL', 'DDPXL', scenario)) %>%
  mutate(experiment=substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
  mutate(old_scen_name=scenario) %>%
  mutate(scenario=substring(scenario, 0, regexpr("_", scenario) - 1)) %>%
  dplyr::rename(year = x) %>%
  dplyr::rename(Units = units)
plot_df_append <- reorg_dataGCAM_data %>%
  dplyr::group_by(scenario, region, experiment, year, param, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()
plot_df_append <- plot_df_append[, c(1,3,2,6,5,7,4)]
plot_df <- rbind(plot_df, plot_df_append)


# Plot investments/stranded assets for a select subset of runs.
params <- c("elecCumCapGW", "elecNewCapGW", "elecCumCapCost", "elecNewCapCost", "elecAnnualRetPrematureGW",
            "elecCumRetPrematureGW", "elecAnnualRetPrematureCost", "elecCumRetPrematureCost")
ymin_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = -5, "elecCumRetPrematureGW" = -5,
                  "elecAnnualRetPrematureCost" = -5, "elecCumRetPrematureCost" = -10)
ymax_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = 1, "elecCumRetPrematureGW" = 1,
                  "elecAnnualRetPrematureCost" = 1, "elecCumRetPrematureCost" = 1)
select_exps <- c('42', '58', '46', '62')
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
for(paramSelect in params){
  title <- paramSelect
  fig_path <- paste0(export_dir, paramSelect, "_select", '.png')
  plot_df_sub <- plot_df %>%
    filter(param==paramSelect)
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
    plot_by_select_experiment = select_exps,
    gray_ribbon = TRUE
  )
}

#TO-DO: add CART for investment plots



# 5. Electrification and Alternative Fuels --------------------------------

qry <- "final energy consumption by sector and fuel.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

FinalEneFuel <- prj$data$`final energy consumption by sector and fuel`

TotalFinalEneFuel <- FinalEneFuel %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

#Electricity percentage of final energy
ElecFinalEneFuel <- FinalEneFuel %>%
  filter(input == "electricity") %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

ElecPctFinalEneFuel <- ElecFinalEneFuel %>%
  left_join(TotalFinalEneFuel, by = c("scenario", "region", "Units", "year", "experiment", "old_scen_name"), 
            suffix = c(".elec", ".total")) %>%
  mutate(value = value.elec / value.total,
         Units = "%",
         Metric = "Electricity share of final energy") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
  
#Hydrogen percentage of final energy
HydrogenFinalEneFuel <- FinalEneFuel %>%
  filter(input == "hydrogen") %>%
  group_by(scenario, region, Units, year, experiment, old_scen_name) %>%
  dplyr::summarise(value = sum(value))

HydrogenPctFinalEneFuel <- HydrogenFinalEneFuel %>%
  left_join(TotalFinalEneFuel, by = c("scenario", "region", "Units", "year", "experiment", "old_scen_name"), 
            suffix = c(".hydrogen", ".total")) %>%
  mutate(value = value.hydrogen / value.total,
         Units = "%",
         Metric = "Hydrogen share of final energy") %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)


#TO-DO: Bioenergy percentage of final energy

Metrics <- rbind(as_tibble(ElecPctFinalEneFuel),
                 as_tibble(HydrogenPctFinalEneFuel))

rdm_cart(ElecPctFinalEneFuel)
rdm_cart(HydrogenPctFinalEneFuel)

#select_exps <- c('66', '82', '70', '86')
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=FALSE)}


# 6. Global Oil & Gas Price -----------------------------------------------

qry <- "prices of all markets.proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

PriceAllMarkets <- prj$data$`prices of all markets`

#Global oil price
oilprice2020<-PriceAllMarkets%>%filter(market %in% c("globalcrude oil"),year %in% c("2020"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%dplyr::summarise(value=sum(value))
oilprice<-PriceAllMarkets%>%filter(market %in% c("globalcrude oil"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%dplyr::summarise(value=sum(value))%>%left_join(oilprice2020, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2020 reference",Metric="Oil price",value.x=NULL,value.y=NULL)

#Global natural gas price
ngprice2020<-PriceAllMarkets%>%filter(market %in% c("globalnatural gas"),year %in% c("2020"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%dplyr::summarise(value=sum(value))
ngprice<-PriceAllMarkets%>%filter(market %in% c("globalnatural gas"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%dplyr::summarise(value=sum(value))%>%left_join(ngprice2020, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2020 reference",Metric="Natural gas price",value.x=NULL,value.y=NULL)

qry <- "primary energy consumption by region (avg fossil efficiency).proj"
prj_path <- paste0(base_dir, qry)
prj <- loadProject(prj_path)

PriEne <- prj$data$`primary energy consumption by region (avg fossil efficiency)`

#TO-DO: Global oil consumption
#TO-DO: Global natural gas consumption

#Colombia oil
oil<-PriEne%>%filter(fuel %in% c("a oil"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
  dplyr::summarise(value=sum(value))%>%filter(year%in% c(2010:2050)) %>%
  mutate(Units="EJ",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Oil primary energy consumption")%>%filter(year%in% c(2010:2050)) %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
oilp<-oil%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
  mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Oil primary energy consumption")%>%filter(year%in% c(2010:2050)) %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

#Colombia natural gas
gas<-PriEne%>%filter(fuel %in% c("b natural gas"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
  dplyr::summarise(value=sum(value))%>%filter(year%in% c(2010:2050)) %>%
  mutate(Units="EJ",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Gas primary energy consumption")%>%filter(year%in% c(2010:2050)) %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)
gasp<-gas%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
  mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Gas primary energy consumption")%>%filter(year%in% c(2010:2050)) %>%
  select(scenario, region, experiment, old_scen_name, year, value, Metric, Units)

Metrics <- rbind(as_tibble(oilprice),
                 as_tibble(ngprice),
                 as_tibble(oil),
                 as_tibble(gas))

rdm_cart(oilprice)
rdm_cart(ngprice)
rdm_cart(oil)
rdm_cart(gas)

#select_exps <- c('66', '82', '70', '86')
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M
  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=FALSE)}
