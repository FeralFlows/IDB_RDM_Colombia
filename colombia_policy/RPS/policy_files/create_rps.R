library('dplyr')

# This script creates three .csv files that ultimately need to be combined into a single file and then run through
# the GCAM model interface to create a XML file. Need to figure out if these xml files can actually be kept separate,
# in which case we can automate the XML creation process

regions <- c('Colombia')
supplysectors <- c('electricity')
years_part_1 <- years <- c(1975, 1990)
years_part_2 <- seq(2005, 2100, 5)
years <- c(years_part_1, years_part_2)
# 'hydro' = c('hydro')
subsectors <- list('coal' = c('coal (conv pul CCS)', 'coal (conv pul)', 'coal (IGCC)', 'coal (IGCC CCS)'), 
                   'gas' = c('gas (steam/CT)', 'gas (CC)', 'gas (CC CCS)'), 
                   'biomass' = c('biomass (conv)',  'biomass (conv CCS)', 'biomass (IGCC)', 'biomass (IGCC CCS)'), 
                   'nuclear' = c('Gen_II_LWR', 'Gen_III'), 
                   'wind' = c('wind', 'wind_storage'), 
                   'solar' = c('PV', 'PV_storage', 'CSP', 'CSP_storage'),
                   'geothermal' = c('geothermal'), 
                   'refined liquids' = c('refined liquids (steam/CT)', 'refined liquids (CC)', 'refined liquids (CC CCS)'))

pass_through_stub_techs <- list('coal (conv pul CCS)' = c('coal (conv pul CCS) (once through)', 'coal (conv pul CCS) (seawater)', 'coal (conv pul CCS) (recirculating)', 'coal (conv pul CCS) (dry cooling)'),
                                'coal (conv pul)' = c('coal (conv pul) (once through)', 'coal (conv pul) (seawater)', 'coal (conv pul) (recirculating)', 'coal (conv pul) (dry cooling)', 'coal (conv pul) (cooling pond)'), 
                                'coal (IGCC)' = c('coal (IGCC) (once through)', 'coal (IGCC) (seawater)', 'coal (IGCC) (recirculating)', 'coal (IGCC) (dry cooling)'), 
                                'coal (IGCC CCS)' = c('coal (IGCC CCS) (once through)', 'coal (IGCC CCS) (seawater)', 'coal (IGCC CCS) (recirculating)', 'coal (IGCC CCS) (dry cooling)'), 
                                'gas (steam/CT)' = c('gas (steam/CT) (once through)', 'gas (steam/CT) (seawater)', 'gas (steam/CT) (recirculating)', 'gas (steam/CT) (dry cooling)', 'gas (steam/CT) (cooling pond)'), 
                                'gas (CC)' = c('gas (CC) (once through)', 'gas (CC) (seawater)', 'gas (CC) (recirculating)', 'gas (CC) (dry cooling)', 'gas (CC) (cooling pond)'), 
                                'gas (CC CCS)' = c('gas (CC CCS) (once through)', 'gas (CC CCS) (seawater)', 'gas (CC CCS) (recirculating)', 'gas (CC CCS) (dry cooling)'),  
                                'biomass (conv)' = c('biomass (conv) (once through)', 'biomass (conv) (seawater)', 'biomass (conv) (recirculating)', 'biomass (conv) (dry cooling)', 'biomass (conv) (cooling pond)'),
                                'biomass (conv CCS)' = c('biomass (conv CCS) (once through)', 'biomass (conv CCS) (seawater)', 'biomass (conv CCS) (recirculating)', 'biomass (conv CCS) (dry cooling)'), 
                                'biomass (IGCC)' = c('biomass (IGCC) (once through)', 'biomass (IGCC) (seawater)', 'biomass (IGCC) (recirculating)', 'biomass (IGCC) (dry cooling)'), 
                                'biomass (IGCC CCS)' = c('biomass (IGCC CCS) (once through)', 'biomass (IGCC CCS) (seawater)', 'biomass (IGCC CCS) (recirculating)', 'biomass (IGCC CCS) (dry cooling)'),
                                'Gen_II_LWR' = c('Gen_II_LWR (once through)', 'Gen_II_LWR (seawater)', 'Gen_II_LWR (recirculating)', 'Gen_II_LWR (cooling pond)'), 
                                'Gen_III' = c('Gen_III (once through)', 'Gen_III (seawater)', 'Gen_III (recirculating)', 'Gen_III (cooling pond)'), 
                                'geothermal' = c('geothermal (dry_hybrid)', 'geothermal (recirculating)'), 
                                'refined liquids (steam/CT)' = c('refined liquids (steam/CT) (once through)', 'refined liquids (steam/CT) (seawater)', 'refined liquids (steam/CT) (recirculating)', 'refined liquids (steam/CT) (dry cooling)', 'refined liquids (steam/CT) (cooling pond)'), 
                                'refined liquids (CC)' = c('refined liquids (CC) (once through)', 'refined liquids (CC) (seawater)', 'refined liquids (CC) (recirculating)', 'refined liquids (CC) (dry cooling)'), 
                                'refined liquids (CC CCS)' = c('refined liquids (CC CCS) (once through)', 'refined liquids (CC CCS) (seawater)', 'refined liquids (CC CCS) (recirculating)', 'refined liquids (CC CCS) (dry cooling)'), 
                                'CSP' = c('CSP (dry_hybrid)', 'CSP (recirculating)'),
                                'CSP_storage' = c('CSP_storage (dry_hybrid)', 'CSP_storage (recirculating)'),
                                'wind' = c('wind'), 
                                'wind_storage' = c('wind_storage'),
                                'PV' = c('PV'), 
                                'PV_storage' = c('PV_storage')
                                )

renewables <- c('wind', 'solar', 'geothermal', 'biomass')
subsectors_renewables <- subsectors[renewables]
StubTechRESSecondaryOutput <- data.frame("region" = character(), 'supplysector' = character(), 'subsector' = character(), 
                             'stub.technology' = character(), 'year' = numeric(), 'res.secondary.output' = character(),
                             'output.ratio' = numeric())  # Create base data frame

# Create data for the following file: L2245.StubTechRESSecondaryOutput_RPS.csv
for(region in regions) {
  for(supplysector in supplysectors) {
    for (subsector in names(subsectors_renewables)) {
      for (stubtech in subsectors_renewables[[subsector]]) {
        for (pass_through_tech in pass_through_stub_techs[[stubtech]]) {
          for (year in years){
            temp <- data.frame("region" = region, 'supplysector' = supplysector, 'subsector' = subsector, 
                               'stub.technology' = pass_through_tech, 'year' = year, 'res.secondary.output' = 'ELEC_RPS',
                               'output.ratio' = 1)
            StubTechRESSecondaryOutput <- rbind(StubTechRESSecondaryOutput, temp)
          }
        }
      }        
    }
  }
}

# Write top column labels required to create XML for use in running through model interface with header
output_file <- c('C:/Users/twild/all_git_repositories/IDB_RDM/Uncertainty XML/L2245.StubTechRESSecondaryOutput_RPS.csv')
if(file.exists(output_file)){
  file.remove(output_file)
}
write.table('INPUT_TABLE,,,,,,', 
            output_file,
            append=TRUE,
            sep=',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('Variable ID,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('StubTechRESSecondaryOutput,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table(',,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
# Export data
write.table(StubTechRESSecondaryOutput, output_file, 
            row.names=FALSE,
            sep = ',',
            append=TRUE,
            quote=FALSE)

#----------------------------------------------------------------------------------------------------------------------#
# Create data for the following file: L2245.StubTechAdjCoef_RPS.csv
L2245.StubTechAdjCoef_RPS <- data.frame("region" = character(), 'supplysector' = character(), 'subsector' = character(), 
                                        'stub.technology' = character(), 'year' = numeric(), 
                                        'res.secondary.output' = character(),
                                        'output.ratio' = numeric())  # Create base data frame
model_years <- years
period_years_part_1 <- years <- c(1975, 1990)
period_years_part_2 <- seq(2005, 2100, 5)
period_years <- c(period_years_part_1, period_years_part_2)
level_2020 <- 0.05  # renewables fraction NOT including hydro
level_2050 <- 0.3  # renewables fraction NOT including hydro
for(region in regions) {
  for(supplysector in supplysectors) {
    for (subsector in names(subsectors)) {
      for (stubtech in subsectors[[subsector]]){
        for (pass_through_tech in pass_through_stub_techs[[stubtech]])
          for (period_year in period_years){
            for (model_year in model_years){
              # Calculate RPS coefficient value using simple linear rule
              if(model_year < 2020){
                adjusted.coefficient <- 0
              }else if(model_year <= 2030){
                adjusted.coefficient <- level_2020  
              }else if(model_year <= 2050){
                adjusted.coefficient <- level_2020  + (model_year - 2030)*((level_2050-level_2020)/(2050-2030))
              }else{
                adjusted.coefficient <- level_2050
              }
              # Fill in temp data frame
              temp <- data.frame("region" = region, 'supplysector' = supplysector, 'subsector' = subsector, 
                                 'stub.technology' = pass_through_tech, 'year' = period_year, 'minicam.energy.input' = 'ELEC_RPS',
                                 'market.name' = regions, 'model.year' = model_year, 
                                 'adjusted.coefficient' = adjusted.coefficient, 'CO2' = 'CO2')
              L2245.StubTechAdjCoef_RPS <- rbind(L2245.StubTechAdjCoef_RPS, temp)
            }
          }
      }        
    }
  }
}

# Write top column labels required to create XML for use in running through model interface with header
output_file <- c('C:/Users/twild/all_git_repositories/IDB_RDM/Uncertainty XML/L2245.StubTechAdjCoef_RPS.csv')
if(file.exists(output_file)){
  file.remove(output_file)
}
write.table('INPUT_TABLE,,,,,,,,,', 
            output_file,
            append=TRUE,
            sep=',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('Variable ID,,,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('StubTechAdjCoef,,,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table(',,,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
# Export data
write.table(L2245.StubTechAdjCoef_RPS, output_file, 
            row.names=FALSE,
            sep = ',',
            append=TRUE,
            quote=FALSE)

#----------------------------------------------------------------------------------------------------------------------