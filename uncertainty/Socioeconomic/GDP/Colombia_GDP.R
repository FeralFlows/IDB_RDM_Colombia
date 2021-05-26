library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(gcamdata)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
base_directory <- getwd()
data <- data.table::fread('Colombia_GDP_Mid.csv', stringsAsFactors = FALSE)

basegdp_ref <- data %>% 
  dplyr::slice(6) %>% 
  dplyr::select(-V3) %>% 
  dplyr::mutate(V2 = as.numeric(V2)) %>% 
  dplyr::rename(region = V1,
                baseGDP = V2) %>% 
  tibble::as_tibble()
str(basegdp_ref)

laborforcefillout_ref <- data %>% 
  dplyr::slice(13) %>%
  dplyr::mutate(V2 = as.numeric(V2),
                V3 = as.numeric(V3)) %>% 
  tibble::as_tibble()
names(laborforcefillout_ref) <- c('region', 'year', 'laborforce')
str(laborforcefillout_ref)

productivity_ref <- data %>% 
  dplyr::slice(20:40) %>% 
  dplyr::mutate(V2 = as.numeric(V2),
                V3 = as.numeric(V3)) %>% 
  tibble::as_tibble()
names(productivity_ref) <- c('region', 'year', 'laborproductivity')
str(productivity_ref)

pppconvert_ref <- data %>% 
  dplyr::slice(47) %>%
  dplyr::mutate(V2 = as.numeric(V2),
                V3 = as.numeric(V3)) %>% 
  tibble::as_tibble()
names(pppconvert_ref) <- c('region', 'constRatio', 'PPPConvert')
str(pppconvert_ref)

# productivity medium scenario =================================================
# Auto-produce XML
xmlpath <- paste(base_directory, 'Colombia_GDP_Mid.xml', sep = '/')
mi_header <- 'E:/NEXO-UA/Github/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(basegdp_ref, 'BaseGDP', NULL) %>%
  gcamdata::add_xml_data(laborforcefillout_ref, 'LaborForceFillout', NULL) %>%
  gcamdata::add_xml_data(productivity_ref, 'LaborProductivity', NULL) %>%
  gcamdata::add_xml_data(pppconvert_ref, 'PPPConvert', NULL) %>%
  gcamdata::run_xml_conversion()

# productivity high scenario ===================================================
# add 0.5% to the reference GDP by 2050 
change_2050 <- 0.005
prod_change <- approx(x = c(2020, 2050), y = c(0, change_2050), xout = append(1990, seq(2005, 2100, 5)),
                      method = 'linear', yleft=0, yright=change_2050)
prod_change <- data.frame(matrix(unlist(prod_change), nrow = length(prod_change$x), byrow = FALSE)) %>% 
  dplyr::rename(year = X1,
                change = X2)

productivity_high <- productivity_ref %>% 
  dplyr::left_join(prod_change, by = c('year')) %>% 
  dplyr::mutate(laborproductivity = laborproductivity + change) %>% 
  dplyr::select(-change)

# Auto-produce XML
xmlpath <- paste(base_directory, 'Colombia_GDP_High.xml', sep = '/')
mi_header <- 'E:/NEXO-UA/Github/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(basegdp_ref, 'BaseGDP', NULL) %>% 
  gcamdata::add_xml_data(laborforcefillout_ref, 'LaborForceFillout', NULL) %>%
  gcamdata::add_xml_data(productivity_high, 'LaborProductivity', NULL) %>%
  gcamdata::add_xml_data(pppconvert_ref, 'PPPConvert', NULL) %>%
  gcamdata::run_xml_conversion()


# productivity low scenario ===================================================
# add 0.5% to the reference GDP by 2050 
change_2050 <- -0.005
prod_change <- approx(x = c(2020, 2050), y = c(0, change_2050), xout = append(1990, seq(2005, 2100, 5)),
                      method = 'linear', yleft=0, yright=change_2050)
prod_change <- data.frame(matrix(unlist(prod_change), nrow = length(prod_change$x), byrow = FALSE)) %>% 
  dplyr::rename(year = X1,
                change = X2)

productivity_low <- productivity_ref %>% 
  dplyr::left_join(prod_change, by = c('year')) %>% 
  dplyr::mutate(laborproductivity = laborproductivity + change) %>% 
  dplyr::select(-change)

# Auto-produce XML
xmlpath <- paste(base_directory, 'Colombia_GDP_Low.xml', sep = '/')
mi_header <- 'E:/NEXO-UA/Github/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(basegdp_ref, 'BaseGDP', NULL) %>% 
  gcamdata::add_xml_data(laborforcefillout_ref, 'LaborForceFillout', NULL) %>%
  gcamdata::add_xml_data(productivity_low, 'LaborProductivity', NULL) %>%
  gcamdata::add_xml_data(pppconvert_ref, 'PPPConvert', NULL) %>%
  gcamdata::run_xml_conversion()


# Write out GDP growth rate table for both Low, Medium, and High scenario
gdp_growth <- productivity_low %>% 
  dplyr::left_join(productivity_ref, by = c('region', 'year')) %>% 
  dplyr::left_join(productivity_high, by = c('region', 'year')) %>% 
  dplyr::rename(Low_GDP = laborproductivity.x,
                Mid_GDP = laborproductivity.y,
                High_GDP = laborproductivity)
data.table::fwrite(gdp_growth, file = 'GDP_growth.csv')
