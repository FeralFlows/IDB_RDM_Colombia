library('rgcam')
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')

# Source plotting function
source('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/RDM_plotting.R')

#-----------------------------------------------------------------------------------------------------------------------
# Importing and Reorganizing data file that contains GCAM query results across the hundreds of RDM GCAM runs.

# Load single file produced from RDM experiment.
base_dir <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/')
output_file <- c('04292020.dat')
prj_path <- paste0(base_dir, output_file)
prj <- loadProject(prj_path)

# Create empty data frame
queries <- listQueries(prj)

# Loop through all data outputs, rename scenario as regular scenario name, and create new "experiment" column with
# corresponding experiment number
for(experiment in names(prj)){
  for(query in queries){
    exp <- prj[[experiment]]
    qry <- exp[[query]]
    qry <- qry %>%
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
reorg_prj <- vector("list", length(queries_relevant))
names(reorg_prj) <- queries_relevant

for(item in names(reorg_prj)){
  reorg_prj[[item]] <- data.frame()
}
for(query in queries_relevant){
  for(experiment in names(prj[short_list])){
    print(experiment)
    #print(reorg_prj)
    reorg_prj[[query]] <- rbind(reorg_prj[[query]], prj[[experiment]][[query]] %>% filter(region=="Colombia"))
  }
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
dataProj_i <-"04292020.proj"  # Use if gcamdata has been saved as .proj file
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
  mutate(experiment=substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
  mutate(old_scen_name=scenario) %>%
  mutate(scenario=substring(scenario, 0, regexpr("_", scenario) - 1)) %>%
  rename(year = x) %>%
  rename(Units = units)
plot_df_append <- reorg_dataGCAM_data %>%
  group_by(scenario, region, experiment, year, param, Units) %>%
  summarize(value = sum(value)) %>%
  ungroup()
plot_df_append <- plot_df_append[, c(1,3,2,6,5,7,4)]
plot_df <- rbind(plot_df, plot_df_append)

#-----------------------------------------------------------------------------------------------------------------------
# Produce uncertainty plots that look across hundreds of Reference, DDP, and Current_Policy scenarios

# General plot assumptions
fig_base_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/')
x_min <- 2025
x_max <- 2050
plot_scens <- c("ColPol", "DDP")
x_lbl <- 'Time'
plot_scenarios <- c('DDP', 'ColPol')

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
    filter(param==paramSelect)
  y_lbl <- unique(plot_df_sub$Units)[1]
  line_plot(plot_df_sub, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max,
            y_min = ymin_list[[paramSelect]], y_max = ymax_list[[paramSelect]])
}
#-----------------------------------------------------------------------------------------------------------------------
# Produce individual and comparison plots of individual scenarios to evaluate outcomes in more detail.

scenOrigNames_i = c("Reference", "ColPol", "DDP")
scenNewNames_i = c("Reference", "Current_Policy", "DDP")
#scenNewNames = scenNewNames_i,
#-----------------------------------------------------------------------------------------------------------------------

new <- reorg_dataGCAM_data %>%
  filter(scenario=='Reference', region=='Colombia', param=='elecNewCapCost', experiment==100) %>%
  filter(year>=2020, year<=2050) %>%
  select(scenario, region, param, year, value, Units, experiment, class1) %>%
  group_by(scenario, region, experiment, year) %>%
  summarize(value = sum(value)) %>%
  ungroup()

#-----------------------------------------------------------------------------------------------------------------------
wheatprice<-price%>%group_by(scenario, region, experiment, old_scen_name,Units, year,market) %>%summarize(value=sum(value))%>%
  filter(market %in% c("globalWheat"))
price2015<-wheatprice%>%  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))%>%  filter(year %in% c("2015"))
for(i in seq(length(wheatprice$value))){
  for(j in seq(length(price2015$value))){
    if(wheatprice$old_scen_name[i]==price2015$old_scen_name[j]){
      wheatprice$value[i]<-(((wheatprice$value[i]-price2015$value[j]))/price2015$value[j])+1
    }
  }
}
wheatprice$Units<-"2015 reference"
wheatprice<-add_column(wheatprice,Metric="Wheat price")
wheatprice<-wheatprice%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%
  summarize(value=sum(value))%>%  filter(year %in% c(2010,2015,2020,2025,2030,2035,2040,2045,2050))
