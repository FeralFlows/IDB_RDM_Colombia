library('rgcam')
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library('readr')
library('metis')
library(data.table)
# Source plotting function
source('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/RDM_plotting.R')

#-----------------------------------------------------------------------------------------------------------------------
# Importing and Reorganizing data file that contains GCAM query results across the hundreds of RDM GCAM runs.

# Load single file produced from RDM experiment.
base_dir <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/')
output_file <- c('05242020.dat')
prj_path <- paste0(base_dir, output_file)
prj <- loadProject(prj_path)

# See what size of data we are working with
print(object.size(prj), units = 'Mb')

# Create empty data frame
queries <- listQueries(prj)

# Loop through all data outputs, rename scenario as regular scenario name, and create new "experiment" column with
# corresponding experiment number
for(experiment in names(prj)){
  for(query in queries){
    exp <- prj[[experiment]]
    qry <- exp[[query]]
    qry <- qry %>%
      mutate(scenario = gsub('DDP_Delayed_EndPt', 'DelayedEndPt', scenario)) %>%
      mutate(scenario = gsub('DDP_Delayed_CumEmiss', 'DelayedCumEmiss', scenario)) %>%
      mutate(experiment=substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
      mutate(old_scen_name=scenario) %>%
      mutate(scenario=substring(scenario, 0, regexpr("_", scenario)-1))
    prj[[experiment]][[query]] <- qry  # replace query result for this query and experiment with modified dataframe
  }
}

# Create dataFrame that will store all uncertainty results to be plotted
plot_df <- data.frame(scenario = character(), experiment = integer(), region = character(), param = character(),
                      year = integer(), value = numeric(), Units = integer())
#-----------------------------------------------------------------------------------------------------------------------
# Process data for non-Metis-based analysis
#Eliminate unneeded queries
queries_relevant <- queries  # c('CO2 emissions by sector (no bio)')
# Create a single dataframe that stores all all experiments under a single query.
reorg_prj <- list()
for (query in queries_relevant){
    prj_isolate_query <- list()
    for (experiment in names(prj)){
      prj_isolate_query[[experiment]] <- prj[[experiment]][[query]]
    }
      # Across all experiments, isolate query, so we can run rbindlist on prj_isolate_query and store it in reorg_prj
    reorg_prj[[query]] <- rbindlist(prj_isolate_query)
    print(paste0("Completing ", "query: ", query))
}
#-----------------------------------------------------------------------------------------------------------------------
# Use Metis to process certain complex queries that (1) require significant post-processing, and (2) for which
# specific Metis functions/handing have already been developed. For example, this includes electricity investments. We
# produce a single ".proj" file on PIC that stores the GCAM queries for hundreds or thousands of GCAM runs. This
# ".proj" file is already in the format required by Metis, so very little pre-processing is required below. However,
# some post-processing of metis-results is required to get the Metis outputs formatted for plotting.

# Select relevant parameters that we want to process in Metis. The rest (which are simpler) will be processed here in
# this script, not in Metis.
paramsSelect_i <- c('elecNewCapCost')   # c('emissCO2BySectorNoBio')  # c('All')
dataProjPath_i <- paste("C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output") # Path to dataProj file.
dataProj_i <-"select_queries_metis.proj"  # Use if gcamdata has been saved as .proj file
RDM_results_for_Metis <- loadProject(paste0(dataProjPath_i, "/", dataProj_i))
scenOrigNames_i <- listScenarios(RDM_results_for_Metis)
regionsSelect_i <- c("Colombia")
dataGCAM<-metis.readgcam(reReadData = F,  # F
                         scenOrigNames = scenOrigNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
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
  dplyr::summarize(value = sum(value)) %>%
  ungroup()
plot_df_append <- plot_df_append[, c(1,3,2,6,5,7,4)]
plot_df <- rbind(plot_df, plot_df_append)

# Merge in Spreadsheet that lists X/L factors
XL_data <- read.csv('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/uncertainty/DOE_XLRM_DDP_XL.csv')
XL_data$scenario <- as.character(XL_data$scenario)
XL_data$experiment <- as.character(XL_data$experiment)
plot_df <- plot_df %>% left_join(XL_data, by=c('scenario', 'experiment'))

#-----------------------------------------------------------------------------------------------------------------------
# Produce uncertainty plots that look across hundreds of Reference, DDP, and Current_Policy scenarios

# General plot assumptions
fig_base_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/')
x_min <- 2025
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('w/ CCS', 'w/o CCS')
x_lbl <- 'Time'
plot_scenarios <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # ,  c('DDP', 'ColPol')

# Plot CO2 emissions across numerous RDM runs
param <- 'CO2Emissions_NoBio'
y_lbl <- 'CO2 Emissions (Mt)'
title <- 'Emissions Uncertainty'
fname <- paste0(fig_base_path, param, '.png')
plot_df <- reorg_prj$`CO2 emissions by sector (no bio)` %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  summarize(value=(44/12)*sum(value)) %>%
  filter(scenario %in% plot_scenarios)
line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max)

# Plot investments/stranded assets across the numerous RDM runs
params <- c("elecCumCapGW", "elecNewCapGW", "elecCumCapCost", "elecNewCapCost", "elecAnnualRetPrematureGW",
            "elecCumRetPrematureGW", "elecAnnualRetPrematureCost", "elecCumRetPrematureCost")
ymin_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = -5, "elecCumRetPrematureGW" = -5,
                  "elecAnnualRetPrematureCost" = -5, "elecCumRetPrematureCost" = -10)
ymax_list <- list("elecCumCapGW" = NULL, "elecNewCapGW" = NULL, "elecCumCapCost" = NULL, "elecNewCapCost" = NULL,
                  "elecAnnualRetPrematureGW" = 1, "elecCumRetPrematureGW" = 1,
                  "elecAnnualRetPrematureCost" = 1, "elecCumRetPrematureCost" = 1)
title <- 'Performance Metric Uncertainty'
for(paramSelect in params){
  fig_path <- paste0(fig_base_path, paramSelect, '.png')
  plot_df_sub <- plot_df %>%
    filter(param==paramSelect) %>%
    filter(scenario %in% c('DDP', 'DDP_Delayed_EndPt'))
  y_lbl <- unique(plot_df_sub$Units)[1]
  line_plot(plot_df_sub, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
            y_min = ymin_list[[paramSelect]], y_max = ymax_list[[paramSelect]], plot_by_XLfac=TRUE)  # title=title, x_lbl=x_lbl,
}

# Plot investments/stranded assets for a select subset of runs.
select_exps <- c('82', '90', '370', '378')  #249
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
for(paramSelect in params){
  title <- paramSelect
  fig_path <- paste0(fig_base_path, paramSelect, "_select", '.png')
  plot_df_sub <- plot_df %>%
    filter(param==paramSelect)
  y_lbl <- unique(plot_df_sub$Units)[1]
  line_plot(plot_df_sub, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
            y_min = ymin_list[[paramSelect]], y_max = ymax_list[[paramSelect]], plot_by_scen=FALSE, plot_by_XLfac=FALSE,
            title=title, plot_by_select_experiment=select_exps, gray_ribbon=TRUE)
}

#-----------------------------------------------------------------------------------------------------------------------
# Produce individual and comparison plots of individual scenarios to evaluate outcomes in more detail.
# Select three representative scenarios (1 Reference, 1 Current Policies, 1 DDP) from the larger prj file
Reference <- prj[['Reference_110']]
ColPol <- prj[['ColPol_110']]
DDP <- prj[['DDP_110']]
# Save a .proj file that only contains only the 3 main scenarios
saveProject()
# Read in all data using readgcam
paramsSelect_i <- c('All')
dataProjPath_i <- paste("C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output") # Path to dataProj file.
dataProj_i <-"05072020.proj"  # Use if gcamdata has been saved as .proj file
RDM_results_for_Metis <- loadProject(paste0(dataProjPath_i, "/", dataProj_i))
scenOrigNames_i <- listScenarios(RDM_results_for_Metis)
regionsSelect_i <- c("Colombia")
dataGCAM<-metis.readgcam(reReadData = F,  # F
                         scenOrigNames = scenOrigNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
                         regionsSelect = regionsSelect_i,
                         paramsSelect=paramsSelect_i)
# Plot
rTable_i <- dataGCAM$data;
paramsSelect_i <- "All"
charts<-metis.chartsProcess(
  rTable=rTable_i, # Default is NULL
  #dataTables=dataTables_i, # Default is NULL
  paramsSelect=paramsSelect_i, # Default is "All"
  regionsSelect=regionsSelect_i, # Default is "All"
  xCompare=c("2010","2030","2050"), # Default is c("2015","2030","2050","2100")
  scenRef="Reference", # Default is NULL
  dirOutputs='C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/metis', # Default is paste(getwd(),"/outputs",sep="")
  regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
  scenarioCompareOnly=0,
  regionCompare=0,
  useNewLabels = 0,
  folderName = "03182020",
  xRange = c(2020, 2030, 2040, 2050),
  colOrder1 = c("Reference", "Current_Policy", "DDP"), #"Original",
  colOrderName1 = "scenario",
  pdfpng='pdf') # Default 0. If set to 1, will only run comparison plots and not individual


scenOrigNames_i = c("Reference", "ColPol", "DDP")
scenNewNames_i = c("Reference", "Current_Policy", "DDP")

#scenNewNames = scenNewNames_i,
#-----------------------------------------------------------------------------------------------------------------------
# EXTRA PLOTS
#-----------------------------------------------------------------------------------------------------------------------
# Co2 rate of change
#-----------------------------------------------------------------------------------------------------------------------
# Processing of outputs
co2_policy_targets <- read.csv(paste0(base_dir, 'CO2_Targets.csv')) %>%
  mutate(rate_change_DDP = if_else(Year>=2010, 100*((DDP/lag(DDP,1))^0.2-1), 0)) %>%
  mutate(rate_change_DelayedDDP = if_else(Year>=2010, 100*((Delayed.DDP/lag(Delayed.DDP,1))^0.2-1), 0)) %>%
  select(-DDP, -Delayed.DDP) %>%
  gather(scenario, value, rate_change_DDP, rate_change_DelayedDDP) %>%
  mutate(scenario=if_else(scenario=='rate_change_DDP', 'DDP', scenario)) %>%
  mutate(scenario=if_else(scenario=='rate_change_DelayedDDP', 'Delayed_DDP', scenario)) %>%
  mutate(experiment = 1) %>%
  dplyr::rename(year=Year)
#Produce Plot
title <- expression(CO[2]~Emissions~Rate~of~Change~('%'))
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/co2_perc_change.png')
x_min <- 2010
x_max <- 2050
plot_scens <- c('DDP', 'Delayed_DDP')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('w/ CCS', 'w/o CCS')
y_lbl <- expression(CO[2]~Emissions~Rate~of~Change~('%'))
line_plot(co2_policy_targets, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title)
#-----------------------------------------------------------------------------------------------------------------------
# Co2 targets (GCAM CO2 constraints)
#-----------------------------------------------------------------------------------------------------------------------
# Processing of outputs
co2_policy_targets <- read.csv(paste0(base_dir, 'CO2_Targets.csv')) %>%
  gather(scenario, value, DDP, Delayed.DDP) %>%
  mutate(experiment = 1) %>%
  dplyr::rename(year=Year) %>%
  mutate(scenario=if_else(scenario=='Delayed.DDP', 'Delayed_DDP', scenario))
#Produce Plot
title <- expression(CO[2]~Emissions)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/co2_emiss.png')
x_min <- 2010
x_max <- 2050
plot_scens <- c('DDP', 'Delayed_DDP')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('w/ CCS', 'w/o CCS')
y_lbl <- expression(CO2~Emissions~(MTCO[2]-Eq))
line_plot(co2_policy_targets, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title)
#-----------------------------------------------------------------------------------------------------------------------
# Power sector investments rate of change
#-----------------------------------------------------------------------------------------------------------------------
# Processing of outputs
power_sector_investments <- read.csv(paste0(base_dir, 'Stranded_Assets.csv')) %>%
  mutate(rate_change_DDP = if_else(Year>=2025, 100*((DDP/lag(DDP,1))^0.2-1), 0)) %>%
  mutate(rate_change_DelayedDDP = if_else(Year>=2025, 100*((Delayed.DDP/lag(Delayed.DDP,1))^0.2-1), 0)) %>%
  select(-DDP, -Delayed.DDP) %>%
  gather(scenario, value, rate_change_DDP, rate_change_DelayedDDP) %>%
  mutate(scenario=if_else(scenario=='rate_change_DDP', 'DDP', scenario)) %>%
  mutate(scenario=if_else(scenario=='rate_change_DelayedDDP', 'Delayed_DDP', scenario)) %>%
  mutate(experiment = 1) %>%
  dplyr::rename(year=Year)
#Produce Plot
title <- expression(Power~Sec.~Investments~Rate~of~Change~('%'))
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/power_invest_perc_change.png')
x_min <- 2025
x_max <- 2050
plot_scens <- c('DDP', 'Delayed_DDP')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('w/ CCS', 'w/o CCS')
y_lbl <- expression(Power~Sec.~Investments~Rate~of~Change~('%'))
line_plot(power_sector_investments, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title)
#-----------------------------------------------------------------------------------------------------------------------
# Power Sector Investments
#-----------------------------------------------------------------------------------------------------------------------
# Processing of outputs
power_sector_investments <- read.csv(paste0(base_dir, 'Stranded_Assets.csv')) %>%
  gather(scenario, value, DDP, Delayed.DDP) %>%
  mutate(experiment = 1) %>%
  dplyr::rename(year=Year) %>%
  mutate(scenario=if_else(scenario=='Delayed.DDP', 'Delayed_DDP', scenario))
#Produce Plot
title <- expression(Power~Sector~Investments)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/power_invest.png')
x_min <- 2025
x_max <- 2050
plot_scens <- c('DDP', 'Delayed_DDP')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('w/ CCS', 'w/o CCS')
y_lbl <- expression(Power~Sector~Investments~(bil.~USD))
line_plot(power_sector_investments, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title)

#-----------------------------------------------------------------------------------------------------------------------
# Produce a plot of power generation metric that singles out certain X/L combinations
qry <- "Electricity generation by aggregate technology.proj"
prj_path <- paste0('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
#********############renewables percentage of power generation. ###############
PowGen<-prj$data$`Electricity generation by aggregate technology`%>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  dplyr::summarize(value=sum(value))
PowGenRenew<-prj$data$`Electricity generation by aggregate technology`%>%
  dplyr::filter(technology %in% c("Biomass","Geothermal","Hydro","Solar","Wind","Biomass w/CCS"))%>%
  dplyr::group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarize(value=sum(value))%>% mutate(Metric = "Renewable Percentage",Units="%")
PowGenRenew$value<-(PowGenRenew$value/PowGen$value)*100
# Join in the X/L factors
DOE_XLRM_DDP_XL <- read_csv('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/DOE_XLRM_DDP_XL.csv') %>%
  mutate(experiment=as.character(experiment))
PowGenRenew <- PowGenRenew %>%
  left_join(DOE_XLRM_DDP_XL, by=c('experiment', 'scenario'))
# Identify four renewable power generation scenarios across four lobes of results
renew_pow_grp1 <- PowGenRenew %>% filter(year==2050, Global_CCS_Cost == 'w/ CCS', Colombia_Nuclear == 'Normal',
                                         Global_RenewableElecTechCost == 'Low', Colombia_Population == 'High',
                                         Colombia_GDP == "High", Colombia_Bldg_ShellApplianceEff=="Low",
                                         Colombia_IndustrialEff=="Low", Colombia_RPS=="Low", Global_EV_Bus=="High") # ID: 82  , value<78
renew_pow_grp2 <- PowGenRenew %>% filter(year==2050, Global_CCS_Cost == 'w/ CCS', Colombia_Nuclear == 'Normal',
                                         Global_RenewableElecTechCost == 'High', Colombia_Population == 'High',
                                         Colombia_GDP == "High", Colombia_Bldg_ShellApplianceEff=="Low",
                                         Colombia_IndustrialEff=="Low", Colombia_RPS=="Low", Global_EV_Bus=="High") # ID: 90  , value<78
renew_pow_grp3 <- PowGenRenew %>% filter(year==2050, Global_CCS_Cost == 'w/o CCS', Colombia_Nuclear == 'Zero',
                                         Global_RenewableElecTechCost == 'Low', Colombia_Population == 'High',
                                         Colombia_GDP == "High", Colombia_Bldg_ShellApplianceEff=="Low",
                                         Colombia_IndustrialEff=="Low", Colombia_RPS=="Low", Global_EV_Bus=="High") # ID: 370  , value<78
renew_pow_grp4 <- PowGenRenew %>% filter(year==2050, Global_CCS_Cost == 'w/o CCS', Colombia_Nuclear == 'Zero',
                                         Global_RenewableElecTechCost == 'High', Colombia_Population == 'High',
                                         Colombia_GDP == "High", Colombia_Bldg_ShellApplianceEff=="Low",
                                         Colombia_IndustrialEff=="Low", Colombia_RPS=="Low", Global_EV_Bus=="High") # ID: 378  , value<78
#Produce Plot (select four experiments colored)
select_exps <- c('82', '90', '370', '378')  #249
title <- expression(Renewable~Power)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/ren_power_select.pdf')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Renewable~Power~Generation~('%'))
line_plot(PowGenRenew, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title, plot_by_select_experiment=select_exps)
#Produce Plot (coloring just CCS=Yes and Nuclear=Yes)
title <- expression(Renewable~Power)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/ren_power_tech.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Renewable~Power~Generation~('%'))
plot_XLfacs <- list('group1' = list('Global_CCS_Cost'='w/ CCS', 'Colombia_Nuclear'='Normal'),
                    'group2' = list('Global_CCS_Cost'='w/o CCS', 'Colombia_Nuclear'='Zero'))
line_plot(PowGenRenew, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=TRUE, title=title)


#Produce Plot (coloring just renewables cost=low and renewables cost = high)
title <- expression(Renewable~Power)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/ren_power_cost.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Renewable~Power~Generation~('%'))
plot_XLfacs <- list('group1' = list('Global_RenewableElecTechCost'='Low'),
                    'group2' = list('Global_RenewableElecTechCost'='High'))
line_plot(PowGenRenew, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=TRUE, title=title)

#-----------------------------------------------------------------------------------------------------------------------
# Wind and solar power
qry <- "Electricity generation by aggregate technology.proj"
prj_path <- paste0('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
PowGenRenewInt<-prj$data$`Electricity generation by aggregate technology`%>%
  dplyr::filter(technology %in% c("Solar","Wind"))%>%
  dplyr::group_by(scenario, region,experiment, old_scen_name, year) %>%
  dplyr::summarize(value=sum(value))%>% mutate(Metric = "Wind and Solar Percentage",Units="%")
PowGenRenewInt$value=(PowGenRenewInt$value/PowGen$value)*100
#Produce Plot (select four experiments colored)
select_exps <- c('82', '90', '370', '378')  #249
title <- expression(Intermittent~Renewables)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/int_ren_power_select.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Intermittent~Generation~('%'))
line_plot(PowGenRenewInt, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title, plot_by_select_experiment=select_exps)

#-----------------------------------------------------------------------------------------------------------------------
# EVs
qry <- c("transport service output by tech.proj")
prj_path <- paste0('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
transbytech<-prj$data$`transport service output by tech`%>%
  dplyr::filter(technology%in% c("Liquids","Cycle","Electric","Walk","BEV","NG","FCEV","Hybrid Liquids","Coal") )%>%
  dplyr::filter(subsector%in% c("Walk","Cycle","Bus","Moped","Motorcycle (50-250cc)","Motorcycle (>250cc)","Compact Car","Large Car and SUV","Mini Car","Subcompact Car"))
VKT<-left_join(transbytech,load_factors, by=c("region","sector", "subsector", "technology", "year")) %>%
  dplyr::mutate(vkt = value/loadFactor)# Million passs-km /pass-veh
VKT$Units<- 'million vehicle-km'
VKT[is.na(VKT)] <- 0
EVVKT<-VKT %>%
  dplyr::filter(technology %in% c("Electric","BEV"))%>%
  dplyr::group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarize(value=sum(vkt))
TotVKT<-VKT %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarize(value=sum(vkt)) %>%
  dplyr::mutate(Metric = "EV VKT")
TotVKT$Units<-"%"
TotVKT$value<-(EVVKT$value/TotVKT$value)*100
# Join in the X/L factors
TotVKT <- TotVKT %>%
  left_join(DOE_XLRM_DDP_XL, by=c('experiment', 'scenario'))
# Identify four renewable power generation scenarios across four lobes of results
#TradBio_grp1 <- TotVKT %>% filter(year==2050, value<75) # ID: 249
#TradBio_grp2 <- TotVKT %>% filter(year==2050, value>99) # ID: 288
#TradBio_grp3 <- TotVKT %>% filter(year==2050, value>85, value<88) # ID: 265


ev_grp1 <- TotVKT %>% filter(year==2050,
                             Global_EV_Bus=="High",
                             Global_CCS_Cost == 'w/ CCS',
                             Colombia_Nuclear == 'Normal',
                             Global_RenewableElecTechCost == 'Low',
                             Colombia_GDP == "High",
                             Colombia_Population == 'High',
                             Colombia_Bldg_ShellApplianceEff=="Low",
                             Colombia_IndustrialEff=="Low",
                             Colombia_RPS=="Low")
ev_grp2 <- TotVKT %>% filter(year==2050,
                             Global_EV_Bus=="Low",
                             Global_CCS_Cost == 'w/ CCS',
                             Colombia_Nuclear == 'Normal',
                             Global_RenewableElecTechCost == 'Low',
                             Colombia_GDP == "High",
                             Colombia_Population == 'High',
                             Colombia_Bldg_ShellApplianceEff=="Low",
                             Colombia_IndustrialEff=="Low",
                             Colombia_RPS=="Low")
#Produce Plot
select_exps <- c('82', '80')
title <- expression(Electric~Vehicles)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/ev_vkt_select.pdf')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(EV~VKT~('%'))
line_plot(TotVKT, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title, plot_by_select_experiment=select_exps)
#-----------------------------------------------------------------------------------------------------------------------
# Share of buses in passenger demand
qry <- c("transport service output by mode.proj")
prj_path <- paste0('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
traindemand<-prj$data$`transport service output by mode` %>%
  dplyr::filter(mode %in% c("HSR","Passenger Rail")) %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarize(value=sum(value))
cycledemand<-prj$data$`transport service output by mode` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  dplyr::summarize(value=sum(value)) %>%
  dplyr::filter(mode %in% c("Cycle"))
airdemand<-prj$data$`transport service output by mode` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  dplyr::summarize(value=sum(value)) %>%
  dplyr::filter(mode %in% c("Domestic Aviation"))
walkdemand<-prj$data$`transport service output by mode` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  dplyr::summarize(value=sum(value)) %>%
  dplyr::filter(mode %in% c("Walk"))
busdemand<-prj$data$`transport service output by mode` %>%
  dplyr::filter(mode %in% c("Bus"))%>%
  dplyr::group_by(scenario, region, experiment, old_scen_name, year) %>%
  dplyr::summarize(value=sum(value)) %>%
  dplyr::mutate(Metric="Bus passenger demand", Units ="%")
cardemand<-prj$data$`transport service output by mode` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  dplyr::summarize(value=sum(value)) %>%
  dplyr::filter(mode %in% c("LDV"))
#*****########Bus percentage########
passdemand<-cardemand$value+traindemand$value+busdemand$value+airdemand$value+cycledemand$value+walkdemand$value
busdemand$value<-(busdemand$value/passdemand)*100
# Produce plot
select_exps <- c('82', '80')
title <- expression(Bus~Passenger~Demand~('%'))
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/bus_perc_vkt_select.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Bus~VKT~('%'))
line_plot(busdemand, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title, plot_by_select_experiment=select_exps)

#-----------------------------------------------------------------------------------------------------------------------
# Traditional Biomass
qry <- c("building final energy by fuel.proj")
prj_path <- paste0('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/juan_manuel/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
bldEnergy<-prj$data$`building final energy by fuel` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name,input, year) %>%
  dplyr::filter(input %in% c("elect_td_bld"))
TotalBuild<-prj$data$`building final energy by fuel` %>%
  dplyr::group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  dplyr::summarize(value=sum(value))
TradBio<-prj$data$`building final energy by fuel`%>%
  filter(input=='traditional biomass')
TradBio$value<-((TradBio$value/TotalBuild$value)*100)
# Join in the X/L factors
TradBio <- TradBio %>%
  left_join(DOE_XLRM_DDP_XL, by=c('experiment', 'scenario'))
# Identify four renewable power generation scenarios across four lobes of results
#TradBio_grp1 <- TradBio %>% filter(year==2050, value<75) # ID: 249
#TradBio_grp2 <- TradBio %>% filter(year==2050, value>99) # ID: 288
#TradBio_grp3 <- TradBio %>% filter(year==2050, value>85, value<88) # ID: 265
#Produce Plot (where select experiments are shown, and everything else is kept gray in background)
select_exps <- c('107', '249', '290', '265')
title <- expression(Residential~Biomass)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/trad_biomass_select.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Building~Trad.~Biomass~('%'))
line_plot(TradBio, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=FALSE, title=title, plot_by_select_experiment=select_exps)
#Produce Plot (coloring by one or two major factors important for biomass)
title <- expression(Residential~Biomass)
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/trad_biomass_colors.png')
x_min <- 2015
x_max <- 2050
plot_scens <- c('DDPXL')  # c('DelayedEndPt', 'DDP', 'DelayedCumEmiss') # c("ColPol", "DDP")
plot_XLfacs <- c('InvalidEntry')
y_lbl <- expression(Building~Trad.~Biomass~('%'))
plot_XLfacs <- list('group1' = list('Colombia_IndustrialEff'=="High"),
                    'group2' = list('Colombia_IndustrialEff'=="Low"))
line_plot(TradBio, fig_path, plot_scens, plot_XLfacs, y_lbl=y_lbl, x_min=x_min, x_max=x_max,
          plot_by_scen=TRUE, plot_by_XLfac=TRUE, title=title)

#-----------------------------------------------------------------------------------------------------------------------
# Dataframe with info about the selected policies
DOE_XLRM_DDP_XL_subset <- DOE_XLRM_DDP_XL %>%
  filter(experiment %in% select_exps)

# Identify policies with desirable characteristics

#-----------------------------------------------------------------------------------------------------------------------
# EXTRA CODE
new <- reorg_dataGCAM_data %>%
  filter(scenario=='Reference', region=='Colombia', param=='elecNewCapCost', experiment==100) %>%
  filter(year>=2020, year<=2050) %>%
  select(scenario, region, param, year, value, Units, experiment, class1) %>%
  group_by(scenario, region, experiment, year) %>%
  summarize(value = sum(value)) %>%
  ungroup()
#-----------------------------------------------------------------------------------------------------------------------
