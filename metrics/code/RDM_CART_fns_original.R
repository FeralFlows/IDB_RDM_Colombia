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

rdm_cart <- function(Metrics, exp, export_dir){
  
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


