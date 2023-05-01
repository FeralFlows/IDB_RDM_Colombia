
setwd("D:/INFEWS/RDM/runs_6_08102021/")
base_dir <- getwd()

PLOT_FOLDER <- "appendix_figs/"
dir.create(PLOT_FOLDER)

#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )


# Diet --------------------------------------------------------------------

qry <- "food consumption by type (specific).proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

FoodConsSpecific <- prj$data$`food consumption by type (specific)`

Meat <- FoodConsSpecific %>%
  filter(experiment %in% c("Policy_Strategy6Low", "Policy_Strategy6High"),
                           year >= 2015 & year <= 2050) %>%
  filter(technology %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat", "OtherMeat_Fish")) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

NonMeat <- FoodConsSpecific %>%
  filter(experiment %in% c("Policy_Strategy6Low", "Policy_Strategy6High"),
                           year >= 2015 & year <= 2050) %>%
  filter(technology %!in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat", "OtherMeat_Fish")) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

plot <- Meat %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy6High" = "High Ambition Diet",
                                "Policy_Strategy6Low" = "Low Ambition Diet"),
                     values = c("Policy_Strategy6High" = "#f58c6c",
                                "Policy_Strategy6Low" = "#327bbe")) +
  labs(title = "Meat consumption", x = "Scenario", y = "Pcal") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"Meat.png"), height = 5, width = 8)

plot <- NonMeat %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy6High" = "High Ambition Diet",
                                "Policy_Strategy6Low" = "Low Ambition Diet"),
                     values = c("Policy_Strategy6High" = "#f58c6c",
                                "Policy_Strategy6Low" = "#327bbe")) +
  labs(title = "Non-meat consumption", x = "Scenario", y = "Pcal") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"NonMeat.png"), height = 5, width = 8)



# Public Transport --------------------------------------------------------


qry <- "transport service output by tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

TrnServTech <- prj$data$`transport service output by tech`

PubTrnServ <- TrnServTech %>%
  filter(Units == "million pass-km", subsector %in% c("Bus", "Passenger Rail"),
         experiment %in% c("Policy_Strategy4High",
                           "Policy_Strategy4Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

PassTrnServ <- TrnServTech %>%
  filter(Units == "million pass-km", subsector %in% c("Bus", "Passenger Rail", "HSR", "2W and 3W",
                                                      "Car", "Large Car and Truck", "Mini Car"),
         experiment %in% c("Policy_Strategy4High",
                           "Policy_Strategy4Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value)) 

PubTrnShare <- PubTrnServ %>%
  left_join(PassTrnServ, by = c("experiment", "year"),
            suffix = c(".PubTrn", ".Total")) %>%
  mutate(share = (value.PubTrn / value.Total) * 100 )

plot <- PubTrnShare %>%
  ggplot(aes(x = year, y = share, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy4High" = "High Public Transport",
                                "Policy_Strategy4Low" = "Low Public Transport"),
                     values = c("Policy_Strategy4High" = "#f58c6c",
                                "Policy_Strategy4Low" = "#327bbe")) +
  labs(title = "Public transport share of passenger vehicle transport (road and rail)", x = "Scenario", y = "%") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"PubTrnShare.png"), height = 5, width = 8)

plot <- PubTrnServ %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy4High" = "High Public Transport",
                                "Policy_Strategy4Low" = "Low Public Transport"),
                     values = c("Policy_Strategy4High" = "#f58c6c",
                                "Policy_Strategy4Low" = "#327bbe")) +
  labs(title = "Public transport service", x = "Scenario", y = "million pass-km") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"PubTrnServ.png"), height = 5, width = 8)



# Electric Transport --------------------------------------------------------


qry <- "transport service output by tech.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

TrnServTech <- prj$data$`transport service output by tech`

PassBEV <- TrnServTech %>%
  filter(Units == "million pass-km", technology %in% c("BEV"),
         experiment %in% c("Policy_Strategy3High",
                           "Policy_Strategy3Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

PassRoad <- TrnServTech %>%
  filter(Units == "million pass-km", subsector %in% c("Bus", "2W and 3W",
                                                      "Car", "Large Car and Truck", "Mini Car"),
         experiment %in% c("Policy_Strategy3High",
                           "Policy_Strategy3Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

PassBEVShare <- PassBEV %>%
  left_join(PassRoad, by = c("experiment", "year"),
            suffix = c(".BEV", ".Total")) %>%
  mutate(share = (value.BEV / value.Total) * 100 )

FreightBEV <- TrnServTech %>%
  filter(Units == "million ton-km", technology %in% c("BEV"),
         experiment %in% c("Policy_Strategy3High",
                           "Policy_Strategy3Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

FreightRoad <- TrnServTech %>%
  filter(Units == "million ton-km", subsector %in% c("Light truck", "Medium truck", "Heavy truck"),
         experiment %in% c("Policy_Strategy3High",
                           "Policy_Strategy3Low"),
         year >= 2015 & year <= 2050) %>%
  group_by(experiment, year) %>%
  dplyr::summarise(value = sum(value))

FreightBEVShare <- FreightBEV %>%
  left_join(FreightRoad, by = c("experiment", "year"),
            suffix = c(".BEV", ".Total")) %>%
  mutate(share = (value.BEV / value.Total) * 100 )


plot <- PassBEV %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy3High" = "High EV Transport",
                                "Policy_Strategy3Low" = "Low EV Transport"),
                     values = c("Policy_Strategy3High" = "#f58c6c",
                                "Policy_Strategy3Low" = "#327bbe")) +
  labs(title = "BEV passenger transport service", x = "Scenario", y = "million pass-km") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"BEVPassTrnServ.png"), height = 5, width = 8)

plot <- FreightBEV %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_manual(labels = c("Policy_Strategy3High" = "High EV Transport",
                                "Policy_Strategy3Low" = "Low EV Transport"),
                     values = c("Policy_Strategy3High" = "#f58c6c",
                                "Policy_Strategy3Low" = "#327bbe")) +
  labs(title = "BEV freight transport service", x = "Scenario", y = "million ton-km") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"BEVFreightTrnServ.png"), height = 5, width = 8)


# CO2 price ---------------------------------------------------------------

qry <- "CO2 prices.proj"
prj_path <- file.path(base_dir, qry)
prj <- loadProject(prj_path)

CO2Price <- prj$data$`CO2 prices`

ROWCO2Price <- CO2Price %>%
  filter(market == "rowCO2", year >= 2015 & year <= 2050)

plot <- ROWCO2Price %>%
  ggplot(aes(x = year, y = value, color = experiment)) +
  geom_line(size = 1, aes( color = experiment )) +
  scale_color_discrete()+
  labs(title = "ROW CO2 price", x = "Scenario", y = "1990$/tC") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER,"CO2 price.png"), height = 5, width = 8)

