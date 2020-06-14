library('rgcam')
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)

produce_query_file <- function(base_dir, output_file, proc_num=NULL, plotting_format=0, metis_format=0){
  # The purpose of this script is to parallelize (on PIC) the process of taking a file that contain outputs
  # across all model runs and all queries (a .proj file) and saving  
  # a .proj file for every single query. These query files can then be downloaded and post-processes on a PC.
  # As input this script requires huge .proj and .dat files that store query results for all runs and all queries.
  
  # Upshot: This script produces files that store results across scenarios for a single query, but the top-level
  # key is "data", and there is just one huge dataframe stored.

  plotting_format <- as.integer(plotting_format)
  metis_format <- as.integer(metis_format)
  # Importing and Reorganizing data file that contains GCAM query results across the hundreds of RDM GCAM runs.
  
  # Load single file produced from RDM experiment.
  
  prj_path <- paste0(base_dir, output_file)
  prj <- loadProject(prj_path)
  # Create empty data frame
  queries <- listQueries(prj)
  
  # Process data for non-Metis-based analysis
  #Eliminate unneeded queries
  if(plotting_format == 1){
    if (is.null(proc_num)){
      queries_relevant <- queries
    }
    else{
      queries_relevant <- queries[as.integer(proc_num)]
    }
  }else{
    queries_relevant <- c("elec gen by gen tech and cooling tech and vintage",
                          "Electricity generation by aggregate technology")
    # Find positions of these queries within the broader prj list
    counter <- 0
    success <- 0
    str_locn <- numeric(length=2)
    for (qry in queries){
      counter <- counter + 1
      if(qry %in% queries_relevant){
        success <- success + 1
        str_locn[success] <- counter
      }
    }
  }
  
  if(plotting_format == 1){
    # Create a single dataframe that stores all all experiments under a single query.
    for (query in queries_relevant){
      prj_isolate_query <- list()
      reorg_prj <- list()
      for (experiment in names(prj)){
        prj_isolate_query[[experiment]] <- prj[[experiment]][[query]] %>% 
          mutate(scenario = gsub('DDP_Delayed_EndPt', 'DelayedEndPt', scenario)) %>% 
          mutate(scenario = gsub('DDP_Delayed_CumEmiss', 'DelayedCumEmiss', scenario)) %>% 
          mutate(experiment=substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
          mutate(old_scen_name=scenario) %>% 
          mutate(scenario=substring(scenario, 0, regexpr("_", scenario)-1))
      }
      # Across all experiments, isolate query, so we can run rbindlist on prj_isolate_query and store it in reorg_prj
      reorg_prj[[query]] <- as.tibble(rbindlist(prj_isolate_query))
      # Save query file as a proj file
      saveProject(list("data"=reorg_prj[query]), file=paste0(base_dir, query, '.proj'))
      print(paste0("Completing ", "query: ", query))
    }
  }
  if(metis_format==1){
    # Upshot: This script produces files that store results across scenarios for a single query, but the top level
    # key is by scenario
    # This does NOT need to be run in parallel. There are only a couple queries we need in this format for metis purposes.
    for (experiment in names(prj)){
      # Reduce prj so that it only contains the query of interest
      #prj[[experiment]] <- prj[[experiment]][query]
      prj[[experiment]] <- prj[[experiment]][str_locn]
    }
    # Save query file as a proj file
    saveProject(prj, file=paste0(base_dir, 'select_queries_metis', '.proj'))
    print(paste0("Completing query for metis format."))
  }
}