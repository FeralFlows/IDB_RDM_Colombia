library('rgcam')
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)
# The purpose of this script is to parallelize (on PIC) the process of taking two files that contain outputs
# across all model runs and all queries (1 .proj file and 1 .dat file) and saving one of file of each type 
# (.proj and .dat)  for every single query. These query files can then be downloaded and post-processes on a PC.
# As input this script requires huge .proj and .dat files that store query results for all runs and all queries.
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