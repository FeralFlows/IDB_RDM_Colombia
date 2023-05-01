################################################################################
# Colombia Robust Decisohn Making                                              #
# Author: Mengqi Zhao                                                          #
# Email: mengqiz@umd.edu                                                       #
# Last Update: 2021-05-28                                                      #  
################################################################################

#if("rpart" %in% rownames(installed.packages()) == F){install.packages("rpart")}
library(rpart)
#if("rpart.plot" %in% rownames(installed.packages()) == F){install.packages("rpart.plot")}
library(rpart.plot)
#if("data.table" %in% rownames(installed.packages()) == F){install.packages("data.table")}
library(data.table)
#if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dplyr")}
library(dplyr)
#if("tidyverse" %in% rownames(installed.packages()) == F){install.packages("tidyverse")}
#library(tidyverse)
#if("stringr" %in% rownames(installed.packages()) == F){install.packages("stringr")}
library(stringr)
library(randomForest)
library(magrittr)

rdm_cart <- function(Metrics, exp, export_dir, excl_outliers=NULL, excl_runs=NULL){
  
  if(!is.null(excl_outliers)){
    # Metrics <- Metrics %>% 
    #   dplyr::filter(year %in% 2050) %>% 
    #   dplyr::mutate(lower = max(min(value, na.rm = T), as.numeric(quantile(value, 0.25)) - (IQR(value) * 1.5)),
    #                 upper = min(max(value, na.rm = T), as.numeric(quantile(value, 0.75)) + (IQR(value) * 1.5))) %>% 
    #   dplyr::filter(value < upper, value > lower) %>% 
    #   dplyr::select(-upper, -lower)
    
    Metrics <- Metrics %>% 
      dplyr::filter(year %in% 2050, !old_scen_name %in% excl_runs$old_scen_name) 
  }
  
  cart <- as_tibble(Metrics) %>% 
    left_join(exp, by = c('experiment', 'scenario')) %>% 
    filter(year %in% 2050) %>%
    dplyr::select(-scenario, -region, -experiment, -old_scen_name, -year, -Units, -Metric)
  
  cols <- colnames(cart)[-1]
  cart[cols] <- lapply(cart[cols], factor)
  
  ## Training of the bagged model
  n_model <- 500
  bagged_models <- list()
  for(i in 1:n_model){
    train_index <- sample.int(nrow(cart), size = round(nrow(cart)*0.6), replace = T)
    bagged_models <- c(bagged_models,
                       list(rpart(value~., cart[train_index,], control=rpart.control(minsplit=6))))
  }
  
  ## Getting estimate from the bagged model
  bagged_result <- NULL
  i <- 0
  for(from_bag_model in bagged_models){
    if (is.null(bagged_result))
      bagged_result <- predict(from_bag_model, cart)
    else
      bagged_result <- (i*bagged_result + predict(from_bag_model, cart))/(i+1)
    i <- i + 1
  }
  
  cart %>% select(-value) -> bagdat
  
  bagged <- rpart(bagged_result ~ .,
                  data = bagdat,
                  method = 'anova',
                  control = c(maxdepth = 4),
                  cp = .000001)
  
  summary <- summary(bagged)
  
  # rpart.plot(
  #   bagged,
  #   extra = 101,
  #   box.palette = 'BuRd',
  #   main = paste('Metric:', unique(Metrics$Metric), sep = ' '),
  #   fallen.leaves = T,
  #   branch.type = 5
  # )
  title <- paste('Metric: ', unique(Metrics$Metric), ' (', unique(Metrics$Units), ') in 2050', sep = '')
  save_title <- paste('Metric', unique(Metrics$Metric), sep = '-')
  
  save_folder <- 'cart'
  save_path <- file.path(export_dir, save_folder)
  if(!file.exists(save_path)){
    dir.create(save_path)
  }
  pdf(paste0(save_path, '/', save_title, '.pdf'))
  
  prp(
    bagged,
    type = 2,
    extra = 101,
    split.border.col = 1.2,
    box.palette = 'GnYlRd',
    branch.type = 5,
    faclen = 0,
    fallen.leaves = T,
    varlen = 0,
    main = title
  )
  dev.off()
  rpart.rules(bagged)
}


rdm_random_forest <- function(Metrics, exp, export_dir, excl_outliers = NULL){
  # Import Experiment CSV
  names_old <- c(paste0('Strategy_', seq(1, 6, 1)), paste0('Uncertainty_', seq(1, 7, 1)))
  names_new <- c('Decarbonize_Power', 'Efficiency', 'Electrify_Transporation', 
                 'Public_Transportation', 'AFOLU', 'Diet', 'SocioEconomic',
                 'EV_Cost', 'Intermittent_Cost', 'CCS_Cost', 'Climate_Change_Impacts',
                 'Agricultural_Trade', 'Global_Biomass_Deployment')

  exp <- exp %>%
    dplyr::mutate(scenario = gsub('_', '', scenario)) %>% 
    dplyr::mutate(experiment = as.character(experiment)) %>% 
    dplyr::rename(setNames(names_old, names_new)) %>%
    dplyr::mutate(dplyr::across(.cols = names_new, .fns = as.factor))
  
  year_select <- 2050
  sum_importances = data.frame()
  
  
  for (metrics in Metrics){
    
    metrics <- metrics %>% 
      ungroup() %>% 
      dplyr::mutate(year = as.numeric(year))
    
    # Exclude outliers
    if(!is.null(excl_outliers)){
      metrics <- metrics %>% 
        dplyr::filter(year %in% year_select) %>% 
        dplyr::mutate(lower = max(min(value, na.rm = T), as.numeric(quantile(value, 0.25)) - (IQR(value) * 1.5)),
                      upper = min(max(value, na.rm = T), as.numeric(quantile(value, 0.75)) + (IQR(value) * 1.5))) %>% 
        dplyr::filter(value < upper, value > lower) %>% 
        dplyr::select(-upper, -lower)
    }
    
    df_regr <- metrics %>% 
      dplyr::filter(year %in% year_select) %>% 
      dplyr::left_join(exp, by = c('scenario', 'experiment')) %>% 
      dplyr::select(all_of(c(names_new, 'value')))
    
    #random forest (regression)
    rf_regr <- randomForest(df_regr$value ~ .,
                            data = df_regr,
                            mtry = 13,
                            importance = TRUE,
                            ntree = 500) #bagging (mtry = 7)
    
    #Put variable importances into dataframe
    importance_df <- as.data.frame(rf_regr %>% importance)
    importance_df$varnames <- rownames(importance_df)
    rownames(importance_df) <- NULL  
    importance_df$metric <- unique(metrics$Metric)
    importance_df$year <- 2050
    importance_df$scenario <- 'RDM'
    
    #add to sum_importances storage dataframe
    sum_importances <- rbind(sum_importances,importance_df)
  }
  
  save_folder <- 'random_forest'
  save_path <- file.path(export_dir, save_folder)
  if(!file.exists(save_path)){
    dir.create(save_path)
  }
  write.csv(sum_importances, file.path(save_path,'random_forest.csv'))
}