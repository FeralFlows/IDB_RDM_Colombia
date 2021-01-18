install.packages('fansi')
install.packages("backports")
install.packages("devtools")
install.packages("sf")
install.packages("magick")
library(devtools)
install_github('JGCRI/rgcam', build_vignettes=TRUE, force = TRUE)
install.packages("rgdal")
install.packages("tmap")

library('tmap')
install_github('JGCRI/metis', build_vignettes=TRUE, force = TRUE)


library(tibble)
library(rgdal)
library('rgcam')
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')
library(reshape)
library(data.table)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021')
# Function to make ghg Anually
Total<-function(Query){
  h<- Query%>%group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value))
  assign(paste("T",deparse(substitute(Query)),sep = ""), h,envir = globalenv())
}
# Function calculate rate from queries
rate<-function(Query){
  g<- Query%>% mutate(Metric=paste(unique(Query$Metric),"Rate"))
  h1<-g[-c(1), ]
  h2<-g[-c(length(g$scenario)), ]
  h<-cbind(h1,h2)%>%mutate(value=(value-value1)/(year-year1))
  h<-h[c(1:8)]%>% mutate(Units="MTCO2/yr")
  assign(paste("rate",deparse(substitute(Query)),sep = ""), h,envir = globalenv())
  
}
# Function to make Cumulative queries
Cumulative<-function(Query){
  h<- Query%>%filter(year %in%(2020:2050))%>% mutate(Metric=paste("Cumulative" ,unique(Query$Metric)))
  h$value<-ave(h$value*5,h$old_scen_name,FUN=cumsum)
  assign(paste("c",deparse(substitute(Query)),sep = ""), h,envir = globalenv())
  
}
# Function to make same starting point
Start2015<-function(Query){
  h<- list()
  for (m in unique(Query$Metric)) {
    f<-Query%>%filter(year %in%(2015),Metric==m)
    g<-min(f$value)
    j<- f%>%mutate(factor=g/value,value=NULL)
    h[[m]]<-Query%>%filter(Metric==m)%>% left_join(j, by=c("scenario","experiment","region","old_scen_name","Metric","Units"))%>%ungroup(year.x) %>%mutate(value=value*factor,factor=NULL,year=year.x,year.x=NULL,year.y=NULL)
  }  
  q <- rbindlist(h)
  assign(paste(deparse(substitute(Query)),sep = ""), q,envir = globalenv())
}

# # Function to make plots
# line_plot <- function(plot_df, fig_path, plot_scens, y_lbl=NULL,
#                       x_lbl=NULL, y_max=NULL, y_min=NULL,
#                       all_same_color=1, title=NULL, legend_on=FALSE, gray_ribbon = TRUE,
#                       plot_var=NULL, x_min=NULL, x_max=NULL){
#   # ggplot2 Theme
#   z_theme <<- theme_bw() +
#     theme(
#       text =                element_text(family = NULL, face = "plain",colour = "black", size = 10 ,hjust = 0.5,
#                                          vjust = 0.5, angle = 0, lineheight = 0.9)
#       , axis.text.x =       element_text(size=8)
#       , axis.text.y =       element_text(size=8)
#       ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
#       ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
#       ,legend.key =         element_blank()
#       ,legend.key.size =    unit(1.5, 'lines')
#       ,legend.text =        element_text(size = 8, colour = "black")
#       ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
#       ,legend.position = "right"
#       ,strip.background =   element_rect(fill = NA, colour = "black")
#       ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
#       ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -4, margin = margin(b=20), size=8)
#     )
#   
#   p <- ggplot()
#   ctr <- 0
#   color_list <- c('dodgerblue3','black','gray','dodgerblue')  #  #de2d26, #fc9272
#   for(plot_scen in plot_scens){
#     ctr <- ctr+1
#     plot_df_filt <- plot_df %>% filter(old_scen_name==plot_scen)#scenario
#     p <- p + geom_line(size=0.5, color=color_list[ctr],
#                        data=plot_df_filt, mapping = aes(x = year, y = value, group=experiment))  # colour=scenario
#     
#   }
#   
#   p <- p + xlab(x_lbl) + ylab(y_lbl)
#   if(!is.null(y_min)){
#     p<-p + scale_y_continuous(limits=c(y_min - 0.1*abs(y_min), 1.1*y_max))
#   }
#   if(!is.null(x_min)){
#     p<-p + scale_x_continuous(limits=c(x_min, x_max))
#   }
#   p <- p + scale_fill_discrete(name="Scenarios",
#                            breaks=c("DDPLeversTest1" ,"DDPLeversTest2" ,"DDPLeversTest3" ,"DDPLeversTest"),
#                            labels=c("DDPLeversTest1" ,"DDPLeversTest2" ,"DDPLeversTest3" ,"DDPLeversTest"))
#   p <- p + guides(color=legend_on)
#   p <- p + ggtitle(title)
#   p <- p + z_theme
#   p
#   ggsave(fig_path, dpi=900, width=2.5, height=2.5, units="in")
# }
# 
# 
# # Function to make plots
# line_plot <- function(plot_df, fig_path, plot_scens, y_lbl=NULL,
#                       x_lbl=NULL, y_max=NULL, y_min=NULL,
#                       all_same_color=1, title=NULL, legend_on=FALSE, gray_ribbon = TRUE,
#                       plot_var=NULL, x_min=NULL, x_max=NULL){
#   # ggplot2 Theme
#   z_theme <<- theme_bw() +
#     theme(
#       text =                element_text(family = NULL, face = "plain",colour = "black", size = 10 ,hjust = 0.5,
#                                          vjust = 0.5, angle = 0, lineheight = 0.9)
#       , axis.text.x =       element_text(size=8)
#       , axis.text.y =       element_text(size=8)
#       ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
#       ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
#       ,legend.key =         element_blank()
#       ,legend.key.size =    unit(1.5, 'lines')
#       ,legend.text =        element_text(size = 5, colour = "black")
#       ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
#       ,legend.position = "right"
#       ,strip.background =   element_rect(fill = NA, colour = "black")
#       ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
#       ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -4, margin = margin(b=20), size=8)
#     )
#   
#   p<-ggplot(plot_df) + geom_line(aes(x=year, y=value, colour=scenario, group=experiment)) +#, group=experiment
#     scale_colour_manual(values=c('dodgerblue3','black','gray','dodgerblue'))
#   p <- p + xlab("") + ylab(paste(plot_df$Metric[1],' (',plot_df$Units[1],')',sep = ""))
#   p<-p + scale_x_continuous(limits=c(x_min, x_max))
#   p <- p + z_theme
#   p <- p + ggtitle(title)
#   p
#   ggsave(fig_path, dpi=900, width=4.5, height=2.5, units="in")
# }

plot_scens <- c("DDPXL")#"Reference",
x_lbl <- 'Time'
title <- ''
x_min <- 2015
x_max <- 2050





#Import Reference tables
GWP<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/GWP.csv', header = TRUE, sep = ",", dec = ".")
#Import Correction factors
CF<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/CorreccionFactor.csv', header = TRUE, sep = ",", dec = ".")
#Import deforestation emission trajectory
ghgtraj<-read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/GHG traj.csv') %>%
  ##BY 12-17-2020
  gather(key = "year", "value", c("X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050"))
#ghgtraj<-melt(ghgtraj, id.vars=c( "Units" ),variable.name = c("year"))
ghgtraj$year<-as.numeric(substring(ghgtraj$year,2))




#####VKT
load_factors <- read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/L254.StubTranTechLoadFactor.csv', skip=1) %>% filter(region == "Colombia")  
##load_factors%>%rename(sector=supplysector, subsector = tranSubsector, technology = stub.technology)
names(load_factors)[2]<-"sector"
names(load_factors)[3]<-"subsector"
names(load_factors)[4]<-"technology"


base_dir <- c('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021/query_proj/')
export_dir <- c('E:/NEXO-UA/Results/RDM_Colombia/runs_512_01_14_2021/output_color/')
if(!file.exists(export_dir)){
  dir.create(export_dir)
}
#Query names in folder
qries<-list.files(base_dir)

source('E:/NEXO-UA/Results/RDM_Colombia/metrics/RDM_plotting.R')
source('E:/NEXO-UA/Results/RDM_Colombia/RDM_CART_fns.R')

#for (qry in qries){
#  prj_path <- paste0(base_dir, qry)
#  prj <- loadProject(prj_path)
  
  qry <- "CO2 emissions by sector (no bio).proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="CO2 emissions by sector (no bio).proj"){
    Co2<-prj$data$`CO2 emissions by sector (no bio)`%>% mutate(value=(44/12)*value,ghg="CO2") 
    co2<-prj$data$`CO2 emissions by sector (no bio)`%>% mutate(Metric="CO2 Emissions",Units="MTCO2") %>% group_by(scenario, region, experiment, old_scen_name, Units,year, Metric) %>%summarize(value=(44/12)*sum(value))
  }
  
  
  qry <- "nonCO2 emissions by resource production.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="nonCO2 emissions by resource production.proj"){
    Non2<-prj$data$`nonCO2 emissions by resource production`%>% mutate(sector=prj$data$`nonCO2 emissions by resource production`$resource,resource=NULL)
    
    Non2 <- Non2 %>%
      group_by(scenario, region, sector, ghg, Units, year, experiment, old_scen_name) %>%
      dplyr::summarise(value = sum(value))
  }
  
  qry <- "nonCO2 emissions by sector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  
  if(qry=="nonCO2 emissions by sector.proj"){
    Non1<-prj$data$`nonCO2 emissions by sector`
    ######NONCO2 emissions##
    Non3<-rbind(Non1,Non2)
    NonCO2emiss<-rbind(Non1,Non2)%>% left_join(GWP, by="ghg") %>%   mutate(Units="MTCO2e",value=value*AR5,SAR=NULL,AR5=NULL,AR4=NULL,SARall=NULL,AR5all=NULL,AR4all=NULL)
    nonco2<-NonCO2emiss%>% group_by(scenario, region, experiment, old_scen_name,Units, year) %>%summarize(value=sum(value))%>%mutate(Metric="Nonco2 Emissions")
    Cumulative(nonco2)
    
    ghg<-rbind(NonCO2emiss,Co2) %>% mutate(Units="MTCO2e")
    
    
    ##########Agriculture GHG emissions############
    CO2eqAg<-ghg%>%filter(sector %in% c("FiberCrop","MiscCrop","OilCrop","OtherGrain","PalmFruit","Corn","Rice","Root_Tuber","SugarCrop","UnmanagedLand","Wheat","biomass","FodderGrass","FodderHerb"))%>% 
      mutate(Metric="Ag. GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)*CF$Ag)
    Total(CO2eqAg)
    Cumulative(TCO2eqAg)
    ############Agriculture(livestock) GHG emissions############
    CO2eqLs<-ghg%>%filter(sector %in% c("Beef","Dairy","Pork","Poultry","SheepGoat"))%>%
      mutate(Metric="Livestock GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)*CF$Ls)
    Total(CO2eqLs)
    Cumulative(TCO2eqLs)
    ############HouseHolding GHG emissions############
    CO2eqHh<-ghg%>%filter(sector %in% c("resid cooling","resid heating","resid others"))%>%
      mutate(Metric="Households GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    Total(CO2eqHh)
    Cumulative(TCO2eqHh)
    ############Commercial GHG emissions############
    CO2eqComm<-ghg%>%filter(sector %in% c("comm cooling","comm heating","comm others"))%>%
      mutate(Metric="Commercial GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    Total(CO2eqComm)
    Cumulative(TCO2eqComm)
    ############Industry of Energy GHG emissions############
    CO2eqIE<-ghg%>%filter(sector %in% c("backup_electricity","csp_backup","district heat","industrial energy use","regional biomassOil","regional corn for ethanol","regional sugar for ethanol"))%>% 
      mutate(Metric="Industry of E. GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)*CF$IE)
    Total(CO2eqIE)
    Cumulative(TCO2eqIE)
    ############Industry Process GHG emissions############
    CO2eqIP<-ghg%>%filter(sector %in% c("N fertilizer","cement","process heat cement","industrial processes","industrial feedstocks"))%>%
      mutate(Metric="I. Process GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    Total(CO2eqIP)
    Cumulative(TCO2eqIP)
    ############Power GHG emissions############
    CO2eqPower<-ghg%>%filter(sector %in% c("elec_biomass (IGCC CCS)","elec_biomass (IGCC)","elec_biomass (conv CCS)","elec_biomass (conv)","elec_coal (IGCC CCS)","elec_coal (IGCC)","elec_coal (conv pul CCS)","elec_coal (conv pul)","elec_gas (CC CCS)","elec_gas (CC)","elec_gas (steam/CT)","elec_refined liquids (CC CCS)","elec_refined liquids (CC)","elec_refined liquids (steam/CT)","electricity","electricity_net_ownuse","gas pipeline"))%>% 
      mutate(Metric="Power GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    Total(CO2eqPower)
    Cumulative(TCO2eqPower)
    ############Fugitive GHG emissions############
    CO2eqFug<-ghg%>%filter(sector %in% c("unconventional oil production","coal","crude oil","natural gas"))%>% 
      mutate(Metric="Fugitive GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)*CF$Fug)
    Total(CO2eqFug)
    Cumulative(TCO2eqFug)
    ############Refining GHG emissions############
    CO2eqRef<-ghg%>%filter(sector %in% c("H2 central production","H2 forecourt production","gas processing","refining","delivered biomass","delivered gas","refined liquids enduse","refined liquids industrial","wholesale gas"))%>%  
      mutate(Metric="Refining GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    CO2eqRef<-CO2eqRef%>%left_join(CO2eqIE,by=c("scenario","region","experiment","old_scen_name","Units","year","ghg"))%>%mutate(value=value.x+((value.y/CF$IE)*(1-CF$IE)),value.x=NULL,value.y=NULL,Metric.y=NULL)
    names(CO2eqRef)[7]<-"Metric"
    Total(CO2eqRef)
    Cumulative(TCO2eqRef)
    ############Waste GHG emissions############
    CO2eqWst<-ghg%>%filter(sector %in% c("urban processes"))%>% 
      mutate(Metric="Waste GHG Emissions")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)*CF$Wst)
    Total(CO2eqWst)
    Cumulative(TCO2eqWst)
    ############Bunker(trn_int) GHG emissions############
    CO2eqBkav<-ghg%>%filter(sector %in% c("trn_aviation_intl"))%>% 
      mutate(Units="MTCO2e")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,ghg) %>%summarize(value=sum(value))
    CO2eqBkshp<-ghg%>%filter(sector %in% c("trn_shipping_intl")) %>% 
      mutate(Units="MTCO2e")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,ghg) %>%summarize(value=sum(value))
    CO2eqBk<-rbind(CO2eqBkav,CO2eqBkshp)%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,ghg) %>%summarize(value=sum(value)*(1-CF$Bk))%>%mutate(Metric="Int. Transp. GHG Emissions")
    Total(CO2eqBk)
    Cumulative(TCO2eqBk)
    ############Freight GHG emissions############
    CO2eqFrght<-ghg%>%filter(sector %in% c("trn_freight","trn_freight_road"))%>% 
      mutate(Metric="Freight Transp. GHG Emissions",Units="MTCO2e")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value)) 
    CO2eqFrght<-CO2eqFrght%>%left_join(CO2eqBkshp,by=c("scenario","region","experiment","old_scen_name","Units", "year","ghg"))
    CO2eqFrght$value.y[is.na(CO2eqFrght$value.y)] <- 0
    CO2eqFrght<-CO2eqFrght%>%mutate(value=value.x+(value.y*CF$Bk),value.x=NULL,value.y=NULL)
    Total(CO2eqFrght)
    Cumulative(TCO2eqFrght)
    ############Passenger GHG emissions############
    CO2eqPass<-ghg%>%filter(sector %in% c("trn_pass","trn_pass_road","trn_pass_road_LDV","trn_pass_road_LDV_2W","trn_pass_road_LDV_4W","trn_pass_road_bus"))%>% 
      mutate(Metric="Passenger GHG Emissions",Units="MTCO2e")%>%
      group_by(scenario, region, experiment, old_scen_name,Units, year,Metric,ghg) %>%summarize(value=sum(value))
    CO2eqPass<-CO2eqPass%>%left_join(CO2eqBkav,by=c("scenario","region","experiment","old_scen_name","Units", "year","ghg"))
    CO2eqPass$value.y[is.na(CO2eqPass$value.y)] <- 0
    CO2eqPass<-CO2eqPass%>%mutate(value=value.x+(value.y*CF$Bk),value.x=NULL,value.y=NULL)
    Total(CO2eqPass)
    Cumulative(TCO2eqPass)
    
    ########## GHG corrected by sector
    ghg1<-rbind(as_tibble(CO2eqAg),as_tibble(CO2eqBk),as_tibble(CO2eqComm),
                as_tibble(CO2eqFrght),as_tibble(CO2eqFug),as_tibble(CO2eqHh),as_tibble(CO2eqIE),
                as_tibble(CO2eqIP),as_tibble(CO2eqLs),as_tibble(CO2eqPass),
                as_tibble(CO2eqPower),as_tibble(CO2eqRef),as_tibble(CO2eqWst))
    
    ############Agriculture+Livestock GHG emissions############
    TCO2eqAgri<-TCO2eqAg
    TCO2eqAgri$Metric<-"Agriculture(Ag+Ls) GHG Emissions"
    TCO2eqAgri$value<-TCO2eqLs$value+TCO2eqAg$value
    
    ##BY 12-17-20: using dplyr methods instead of just directly adding, check if it is the same
    TCO2eqAgri_test <- TCO2eqAg %>%
      left_join(TCO2eqLs, by = c("scenario", "region", "experiment", "old_scen_name", "Units", "year"), suffix = c(".Ag", ".Ls")) %>%
      mutate(value = sum(value.Ag, value.Ls))
    
    ############Transportation GHG emissions############
    TCO2eqTrn <- rbind(TCO2eqPass, TCO2eqFrght) %>%
      group_by(scenario, region, experiment, old_scen_name, year,Units) %>%
      summarize(value=sum(value)) %>%
      mutate(Metric="Trn. GHG Emissions",Units="MTCO2e")
    
    ############FFI GHG emissions############
    CO2eqFFI<-rbind(
      as_tibble(TCO2eqFrght),
      as_tibble(TCO2eqFug),
      as_tibble(TCO2eqIE),
      as_tibble(TCO2eqIP),
      as_tibble(TCO2eqPass),
      as_tibble(TCO2eqPower),
      as_tibble(TCO2eqRef)
    )
    CO2eqFFI <- CO2eqFFI %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
      summarize(value=sum(value)) %>%
      mutate(Metric="FFI GHG Emissions")
    
    rdm_cart(TCO2eqFrght)
    rdm_cart(TCO2eqFug)
    rdm_cart(TCO2eqIE)
    rdm_cart(TCO2eqIP)
    rdm_cart(TCO2eqPass)
    rdm_cart(TCO2eqPower)
    rdm_cart(TCO2eqRef)
    
    ######NOx emissions##
    NOx<-Non3%>%  filter(ghg %in% c("NOx_AGR","NOx_AWB","NOx"))%>%
      group_by(scenario, region, experiment, old_scen_name, Units, year) %>%  summarize(value=sum(value))%>% mutate(Metric="NOx Emissions")
    Cumulative(NOx)
    ######SO2 emissions##
    SO2<-Non3%>%  filter(ghg %in% c("SO2_3","SO2_3_AWB")) %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%  summarize(value=sum(value))%>% mutate(Metric="SO2 Emissions")
    Cumulative(SO2)
    ##########N2O emissions##########
    N2O<-ghg1%>%  filter(ghg %in% c("N2O","N2O_AGR","N2O_AWB"))%>% group_by(scenario, region, experiment, old_scen_name, year,Units) %>%
      summarize(value=sum(value))  %>% mutate(Metric="N2O Emissions")
    Cumulative(N2O)
    ##########CH4 emissions##########
    CH4<-ghg1%>%  filter(ghg %in% c("CH4","CH4_AGR","CH4_AWB"))%>% group_by(scenario, region, experiment, old_scen_name, year,Units) %>%
      summarize(value=sum(value))  %>% mutate(Metric="CH4 Emissions")
    Cumulative(CH4)
    ##########CO2 emissions##########
    CO2<-ghg1%>%  filter(ghg %in% c("CO2"))%>% group_by(scenario, region, experiment, old_scen_name, year,Units) %>%
      summarize(value=sum(value))  %>% mutate(Metric="CO2 Emissions")
    #rate(CO2)
    Cumulative(CO2)
    
    ####AFOLU emissions
    TCO2eqAFOLU<-ghg1%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)) %>% mutate(Units="MTCO2e",Metric="LUC Emissions")%>%left_join(ghgtraj,by=c("year","Units"))%>%mutate(value=value.y-value.x,value.x=NULL,value.y=NULL)
   #####GHG Emissions 
    GHG1<-ghg1%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)) %>% mutate(Units="MTCO2e",Metric="GHG Emissions (no LUC)")
    
    GHG<-rbind(GHG1,TCO2eqAFOLU)%>%
      group_by(scenario, region, experiment, old_scen_name, Units,year) %>%
      summarize(value=sum(value)) %>% mutate(Metric="GHG Emissions")
    Cumulative(GHG)
    
    
    #######Deforested areas
    deforestarea<-TCO2eqAFOLU%>%mutate(value=value*3.332841739,Metric="Deforested area",Units="thous ha") #3.332841739 Kha/MTCO2e
    ############Land use change cost
    LUCcost<-deforestarea%>%mutate(value=value*1000*1131*0.8956*1.35/10**6,Metric="Land use change cost",Units="Mill. 2015$") #1euro=0,8956 dolar #1 dólar2001=1.35 dólares de 2015.
    Cumulative(LUCcost)
   
    
    
    ########## Plot Variables annualy and cumulative
    ##BY 12-17-20 remove rateCO2 START HERE, remove Build, Elecgen
    Metrics <- rbind(as_tibble(CO2),
                     as_tibble(nonco2),
                     as_tibble(GHG),
                     as_tibble(TCO2eqAgri),
                     as_tibble(CO2eqFFI),
                     as_tibble(TCO2eqAg),
                     as_tibble(TCO2eqBk),
                     as_tibble(TCO2eqComm),
                     as_tibble(TCO2eqFrght),
                     as_tibble(TCO2eqFug),
                     as_tibble(TCO2eqHh),
                     as_tibble(TCO2eqIE),
                     as_tibble(TCO2eqIP),
                     as_tibble(TCO2eqLs),
                     as_tibble(TCO2eqPass),
                     as_tibble(TCO2eqPower),
                     as_tibble(TCO2eqRef),
                     as_tibble(TCO2eqWst),
                     as_tibble(N2O),
                     as_tibble(CH4),
                     as_tibble(NOx),
                     as_tibble(SO2),
                     as_tibble(TCO2eqTrn),
                     as_tibble(TCO2eqAFOLU),
                     as_tibble(deforestarea),
                     as_tibble(LUCcost))
    rdm_cart(CO2)
    rdm_cart(nonco2)
    rdm_cart(GHG)
    rdm_cart(TCO2eqAgri)
    rdm_cart(CO2eqFFI)
    rdm_cart(TCO2eqAg)
    # rdm_cart(TCO2eqBk)
    rdm_cart(TCO2eqComm)
    rdm_cart(TCO2eqFrght)
    rdm_cart(TCO2eqFug)
    rdm_cart(TCO2eqHh)
    rdm_cart(TCO2eqIE)
    rdm_cart(TCO2eqIP)
    rdm_cart(TCO2eqLs)
    rdm_cart(TCO2eqPass)
    rdm_cart(TCO2eqPower)
    rdm_cart(TCO2eqRef)
    rdm_cart(TCO2eqWst)
    rdm_cart(N2O)
    rdm_cart(CH4)
    rdm_cart(NOx)
    rdm_cart(SO2)
    rdm_cart(TCO2eqTrn)
    rdm_cart(TCO2eqAFOLU)
    rdm_cart(deforestarea)
    rdm_cart(LUCcost)
    
    select_exps <- c('42', '58')
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      title<-""
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE, plot_by_select_experiment=select_exps)
    }
    Metrics <- rbind(as_tibble(cCO2),
                     as_tibble(cnonco2),
                     as_tibble(cGHG),
                     as_tibble(cTCO2eqAg),
                     as_tibble(cTCO2eqBk),
                     as_tibble(cTCO2eqComm),
                     as_tibble(cTCO2eqFrght),
                     as_tibble(cTCO2eqFug),
                     as_tibble(cTCO2eqHh),
                     as_tibble(cTCO2eqIE),
                     as_tibble(cTCO2eqIP),
                     as_tibble(cTCO2eqLs),
                     as_tibble(cTCO2eqPass),
                     as_tibble(cTCO2eqPower),
                     as_tibble(cTCO2eqRef),
                     as_tibble(cTCO2eqWst),
                     as_tibble(cN2O),
                     as_tibble(cCH4),
                     as_tibble(cNOx),
                     as_tibble(cSO2),
                     as_tibble(cLUCcost))
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      title<-""
      fig_path <- c(paste(export_dir,'Cumulative/',unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
    }
  # rm(NonCO2emiss,co2,nonco2,CO2eqAgri,CO2eqFFI,CO2eqAg,CO2eqBk,CO2eqComm,CO2eqFrght,CO2eqFug,CO2eqHh,CO2eqIE,CO2eqIP,CO2eqLs,CO2eqPass,CO2eqPower,CO2eqRef,CO2eqWst,
  #     N2O,CH4,cco2,cnonco2,cGHG,cTCO2eqAg,cTCO2eqBk,cTCO2eqComm,cTCO2eqFrght,cTCO2eqFug,cTCO2eqHh,cTCO2eqIE,cTCO2eqIP,cTCO2eqLs,cTCO2eqPass,cTCO2eqPower,
  #     cTCO2eqRef,cTCO2eqWst,cN2O,cCH4,cNOx,cSO2)
  }
  
  qry <- "GDP MER by region.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="GDP MER by region.proj"){
    
    GDP<-prj$data$`GDP MER by region`%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)) %>% mutate(Metric="GDP",Units="Million1990US$")
  }
  
  qry <- "Population by region.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="Population by region.proj"){
    
    Pop<-prj$data$`Population by region`%>%group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*1000) %>% mutate(Metric="population",Units="people")
    ghgpop<-prj$data$`Population by region`%>%group_by(scenario, region, experiment, old_scen_name, year)%>%
      summarize(value=sum(value)) %>% mutate(Metric="population",Units="Thous")%>%left_join(GHG,by=c("scenario", "region", "experiment","old_scen_name","year"))%>%mutate(value=(value.y*1000)/(value.x),value.x=NULL,value.y=NULL,Metric.x=NULL,Metric.y=NULL,Units.x=NULL,Units.y=NULL,Metric="GHG Emiss per cap.",Units="TCO2eq")%>%filter(year %in%(2010:2050))
    ghggdp<-GDP%>%left_join(GHG,by=c("scenario", "region", "experiment","old_scen_name","year"))%>%
      mutate(value=(value.y*10**6)/(value.x),value.x=NULL,value.y=NULL,Metric.x=NULL,Metric.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Economy carbon intensity.",Units="gCO2eq/1990US$")%>%filter(year %in%(2010:2050))
    
    Metrics<-rbind(as_tibble(ghgpop),as_tibble(ghggdp))
    rdm_cart(ghgpop)
    rdm_cart(ghggdp)
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
    }
    
  }

  qry <- "aggregated land allocation.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="aggregated land allocation.proj"){
    ######land Allocation######
    biomassland<-prj$data$`aggregated land allocation`%>%  filter(landleaf %in% c("biomass"),year %in% c(2010:2050)) %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
      summarize(value=sum(value))%>% mutate(Metric="Biomass lands")
    cropland<-prj$data$`aggregated land allocation`%>%  filter(landleaf %in% c("crops"),year %in% c(2010:2050)) %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
      summarize(value=sum(value))%>% mutate(Metric="Crop lands")
    biomasscropland<-prj$data$`aggregated land allocation`%>%  filter(landleaf %in% c("biomass","crops"),year %in% c(2010:2050)) %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
      summarize(value=sum(value))%>% mutate(Metric="Agricultural lands")
    Metrics<-rbind(as_tibble(biomassland),as_tibble(cropland),as_tibble(biomasscropland))
    
    rdm_cart(cropland)
    rdm_cart(biomasscropland)
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)}
    rm(biomassland,biomasscropland)  
  }
  
  qry <- "water withdrawals by water mapping source.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="water withdrawals by water mapping source.proj"){
    waterconsump<-prj$data$`water withdrawals by water mapping source`%>%
        group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
        summarize(value=sum(value))%>%  filter(region %in% c("Colombia"))%>%mutate(Metric="Water Demand")
    fig_path <- c(paste(export_dir,waterconsump$Metric[1],'.png',sep = ""))
    y_lbl <- paste(waterconsump$Metric[1],' (',waterconsump$Units[1],')',sep = "")
    line_plot(as_tibble(waterconsump), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
      rm(waterconsump)
  }
  
  qry <- "Electricity generation by aggregate technology.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="Electricity generation by aggregate technology.proj"){
    
    #********############renewables percentage of power generation. ###############
    PowGen<-prj$data$`Electricity generation by aggregate technology`%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Total power gen.",Units="Thous GWh")
    PowGenRenew<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Biomass","Geothermal","Hydro","Solar","Wind","Biomass w/CCS"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Renewable generation",Units="%")
    PowGenRenew$value<-(PowGenRenew$value/PowGen$value)*100
    ####Now in GWh
    PowGenRenewGW<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Biomass","Geothermal","Hydro","Solar","Wind","Biomass w/CCS"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Renewable Power generation",Units="Thous GWh")
    
    #********############intermitent percentage of power generation. ###############
    PowGenRenewInt<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Solar","Wind"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Intermittent generation",Units="%")
    PowGenRenewInt$value=(PowGenRenewInt$value/PowGen$value)*100
    ####Now in GWh
    PowGenRenewIntGW<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Solar","Wind"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Wind and Solar Power generation",Units="Thous GWh")
    #********############Hydro percentage of power generation. ###############
    PowGenHydro<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Hydro"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Hydro Percentage",Units="%")
    PowGenHydro$value=(PowGenHydro$value/PowGen$value)*100
    ####Now in GWh
    PowGenHydroGW<-prj$data$`Electricity generation by aggregate technology`%>%
      filter(technology %in% c("Hydro"))%>%
      group_by(scenario, region,experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Hydro Power generation",Units="Thous GWh")
    
    Metrics <- rbind(as_tibble(PowGen),
                     as_tibble(PowGenHydroGW),
                     as_tibble(PowGenRenewGW),
                     as_tibble(PowGenRenewIntGW),
                     as_tibble(PowGenHydro),
                     as_tibble(PowGenRenew),
                     as_tibble(PowGenRenewInt)
      )
    
    rdm_cart(PowGen)
    rdm_cart(PowGenHydroGW)
    rdm_cart(PowGenRenewGW)
    rdm_cart(PowGenRenewIntGW)
    rdm_cart(PowGenHydro)
    rdm_cart(PowGenRenew)
    rdm_cart(PowGenRenewInt)
    
    select_exps <- c('66', '82', '70', '86')
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps)}
    rm(PowGenHydroGW,PowGenRenewGW,PowGenRenewIntGW,PowGenHydro)
  }
  
  
  qry <- "elec prices by sector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="elec prices by sector.proj"){
    
    ElecPrice<-prj$data$`elec prices by sector`%>%  filter(year %in% c(1990,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2065,2080,2095))
    
    bldPrice<-ElecPrice%>%  filter(fuel %in% c("elect_td_bld")) %>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    trnPrice<-ElecPrice %>%  filter(fuel %in% c("elect_td_trn"))%>%
      group_by(scenario, region, experiment, old_scen_name,  year)%>%
      summarize(value=sum(value)) 
    indPrice<-ElecPrice%>%  filter(fuel %in% c("elect_td_ind")) %>%
      group_by(scenario, region, experiment, old_scen_name, year)%>%
      summarize(value=sum(value)) 
    electricity<-ElecPrice%>%  filter(fuel %in% c("electricity")) %>%
      group_by(scenario, region, experiment, old_scen_name, year)%>%
      summarize(value=sum(value))
    
    #electricity$value<-electricity$value*elecgen$value
    #price2015<-electricity%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
    #  summarize(value=sum(value))
    
    #electricity<-electricity%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
    #  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Electricity Cost",value.x=NULL,value.y=NULL,fuel=NULL)
    
    bldPrice3<-ElecPrice%>%  filter(fuel %in% c("elect_td_bld")) %>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)*(4.52/0.2777))%>%mutate(Metric="Bld. elec. Price",Units="2015$/MWh")
    trnPrice3<-ElecPrice %>%  filter(fuel %in% c("elect_td_trn"))%>%
      group_by(scenario, region, experiment, old_scen_name,  year)%>%
      summarize(value=sum(value)*(4.52/0.2777)) %>%mutate(Metric="Trn. elec. Price",Units="2015$/MWh")
    indPrice3<-ElecPrice%>%  filter(fuel %in% c("elect_td_ind")) %>%
      group_by(scenario, region, experiment, old_scen_name, year)%>%
      summarize(value=sum(value)*(4.52/0.2777)) %>%mutate(Metric="Ind. elec. Price",Units="2015$/MWh")
    electricity3<-ElecPrice%>%  filter(fuel %in% c("electricity")) %>%
      group_by(scenario, region, experiment, old_scen_name, year)%>%
      summarize(value=sum(value)*(4.52/0.2777))%>%mutate(Metric="Marginal Cost of Electicity",Units="2015$/MWh")#4.52*1975$=2015$ ##1GJ=0.2777MWh ##With carbon price
    
    select_exps <- c('42', '58', '46', '62')
    fig_path <- c(paste(export_dir,electricity3$Metric[1],'.png',sep = ""))
    y_lbl <- paste(electricity3$Metric[1],' (',electricity3$Units[1],')',sep = "")
    line_plot(as_tibble(electricity3), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps) # MZ add as_tibble
    
    Metrics<-rbind(as_tibble(electricity3))
    
    rdm_cart(electricity3)
    
    select_exps <- c('42', '58', '46', '62')
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps)}
    rm(PowGenHydroGW,PowGenRenewGW,PowGenRenewIntGW,PowGenHydro)
    
  }
  
  qry <- "prices of all markets.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="prices of all markets.proj"){
    
    
    price<- prj$data$`prices of all markets`%>%filter(market %in% c("globalcrude oil","ColombiaCO2","ColombiaBeef","globalRice","globalWheat","Colombiatrn_freight","Colombiatrn_pass"),year %in% c(2010:2050))
    
    
    #**********#######Elec Carbon Price###########
    Co2price<-price%>%filter(market %in% c("ColombiaCO2"),year %in% c(2010:2050))%>%
      group_by(scenario,  experiment, old_scen_name, year) %>%summarize(value=sum(value)*(12/44)*1.81)%>%mutate(region="Colombia")
    Totaltax<-TCO2eqPower%>%filter(year %in% c(2010:2050))%>%left_join(Co2price,by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x*10**6)*value.y,value.x=NULL,value.y=NULL,Metric="total tax")%>%left_join(PowGen,by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=value.x/(value.y*10**6),value.x=NULL,value.y=NULL,Metric="Elec. carbon price",Metric.x=NULL,Metric.y=NULL,Units="2015$/MWh",Units.y=NULL)
    Totaltax$Units.x<-NULL
    
    ElecPrices<-electricity3%>%filter(year %in% c(2010:2050))%>%left_join(Totaltax,by=c("scenario","region","experiment","old_scen_name","year","Units"))%>%
      mutate(value=value.x-value.y,value.x=NULL,value.y=NULL,Metric="Marginal cost of electricity",Metric.x=NULL,Metric.y=NULL)
    
    #**********#######Oil prices ###########
    oilprice2015<-price%>%filter(market %in% c("globalcrude oil"),year %in% c("2015"))%>%
      group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))
    oilprice<-price%>%filter(market %in% c("globalcrude oil"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(oilprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Oil price",value.x=NULL,value.y=NULL)
    #**********#######Beef prices ###########
    beefprice2015<-price%>%filter(market %in% c("ColombiaBeef"),year %in% c("2015"))%>%
      group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))
    beefprice<-price%>%filter(market %in% c("ColombiaBeef"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(beefprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Beef price",value.x=NULL,value.y=NULL)
    #**********#######Rice prices ###########
    riceprice2015<-price%>%filter(market %in% c("globalRice"),year %in% c("2015"))%>%
      group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))
    riceprice<-price%>%filter(market %in% c("globalRice"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(riceprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Rice price",value.x=NULL,value.y=NULL)
    #**********#######Wheat prices ###########
    wheatprice2015<-price%>%filter(market %in% c("globalWheat"),year %in% c("2015"))%>%
      group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))
    wheatprice<-price%>%filter(market %in% c("globalWheat"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(wheatprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Wheat price",value.x=NULL,value.y=NULL)
    #**********#######Transport prices ###########
    frghtprice<-price%>%filter(market %in% c("Colombiatrn_freight"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))
    passprice<-price%>%filter(market %in% c("Colombiatrn_pass"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))
      
      
      
    Metrics <- rbind(as_tibble(oilprice),
                     as_tibble(beefprice),
                     as_tibble(riceprice),
                     as_tibble(wheatprice),
                     as_tibble(Totaltax),
                     as_tibble(ElecPrices)
    )
    rdm_cart(oilprice)
    rdm_cart(beefprice)
    rdm_cart(Totaltax)
    rdm_cart(ElecPrices)
      
      
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      title<-""
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)}
    rm(oilprice2015)
  }
  
  qry <- "total final energy by aggregate sector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="total final energy by aggregate sector.proj"){
    
    FinalEnergy<- prj$data$`total final energy by aggregate sector` %>%
      group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Final Energy")
    
    rdm_cart(FinalEnergy)
    
    fig_path <- c(paste(export_dir,FinalEnergy$Metric[1],'.png',sep = ""))
    y_lbl <- paste(FinalEnergy$Metric[1],' (',FinalEnergy$Units[1],')',sep = "")
    line_plot(as_tibble(FinalEnergy), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
    rm(FinalEnergy)
  }
  
  qry <- "building final energy by fuel.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="building final energy by fuel.proj"){
    
    bldElec<-prj$data$`building final energy by fuel`%>%
      group_by(scenario, region, experiment, old_scen_name,input, year) %>%
      filter(input %in% c("elect_td_bld"))
    TotalBuild<-prj$data$`building final energy by fuel` %>% group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(value))
    TradBio<-prj$data$`building final energy by fuel`%>%filter(input=='traditional biomass')%>% group_by(scenario, region, experiment, old_scen_name, year) %>% summarize(value=sum(value))%>%
      mutate(Metric="Bld. Trad. biomass",Units="%")
    TradBio$value<-((TradBio$value/TotalBuild$value)*100)
    
    rdm_cart(TradBio)
    
    fig_path <- c(paste(export_dir,TradBio$Metric[1],'.png',sep = ""))
    y_lbl <- paste(TradBio$Metric[1],' (',TradBio$Units[1],')',sep = "")
    line_plot(as_tibble(TradBio), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
        
  }
  
  qry <- "industry final energy by fuel.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="industry final energy by fuel.proj"){
    
    indElec<-prj$data$`industry final energy by fuel` %>%
      group_by(scenario, region, experiment, old_scen_name,input,  year) %>%
      filter(input %in% c("elect_td_ind"))
  }
  
  qry <- "transport final energy by fuel.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="transport final energy by fuel.proj"){
    
    trnElec<-prj$data$`transport final energy by fuel` %>%
      group_by(scenario, region, experiment, old_scen_name,input, year) %>%
      filter(input %in% c("elect_td_trn"))
        
  }

  elecPrice <- bldPrice
  elecPrice$value<-(bldPrice$value*bldElec$value)+(trnPrice$value*trnElec$value)+(indElec$value*indPrice$value)*4.52#4.52*1975$=2015$
  elecPrice<-elecPrice%>%mutate(Metric="Total elec. cost",Units="Thous. Mill. 2015$",fuel=NULL)
  
  price2015<-elecPrice%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
    summarize(value=sum(value))
  
  elecPrice1<-elecPrice%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
    mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Total elec. cost",value.x=NULL,value.y=NULL,fuel=NULL)
  
  rdm_cart(elecPrice1)

  fig_path <- c(paste(export_dir,elecPrice1$Metric[1],'.png',sep = ""))
  y_lbl <- paste(elecPrice1$Metric[1],' (',elecPrice1$Units[1],')',sep = "")
  line_plot(as_tibble(elecPrice1), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
  rm(elecPrice1)

  
  qry <- "elec gen by region (incl CHP).proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="elec gen by region (incl CHP).proj"){
    
    elecgen<-prj$data$`elec gen by region (incl CHP)` %>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))
    
    ###########Power carbon intensity
    Elecgen<-elecgen %>%mutate(Units="MTCO2eq/EJ",Metric="Power carbon intensity")
    Elecgen$value<-TCO2eqPower$value/Elecgen$value
    
    rdm_cart(Elecgen)
    
    fig_path <- c(paste(export_dir,Elecgen$Metric[1],'.png',sep = ""))
    y_lbl <- paste(Elecgen$Metric[1],' (',Elecgen$Units[1],')',sep = "")
    line_plot(as_tibble(Elecgen), fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
    rm(Elecgen)
    
  }
  
  qry <- "transport final energy by mode and fuel.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="transport final energy by mode and fuel.proj"){
    
    #####US$ per liter
    costpollutant <- read.csv('E:/NEXO-UA/Results/RDM_Colombia/metrics/Cost of local pollutants.csv') %>%
    ## BY 12-18-2020
      gather(key = "input", value = "value", c("gasoline", "diesel", "Kerosene", "gas", "coal"))
   ## costpollutant<-melt(costpollutant, id.vars=c("year","units"))%>%mutate(input=variable,variable=NULL)
    
    #********############Cost of local pollutants. ############### LDV=Gasoline HDV=Diesel
    Diesel<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road"),input %in% c("refined liquids enduse")) %>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(input="diesel")%>% left_join(costpollutant, by=c("input","year"))%>%
      mutate(value=(((value.x*(158.98*10**3))/5.67)*value.y)/1000,value.x=NULL,value.y=NULL,units=NULL,Metric="Cost of local pollution",Units="Thous. Millions 2015$")
    Gas<-prj$data$`transport final energy by mode and fuel` %>% filter(input %in% c("delivered gas"),sector %in% c("trn_pass_road","trn_pass_road_LDV_4W")) %>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(input="gas")%>% left_join(costpollutant, by=c("input","year"))%>%
      mutate(value=(value.x*(10**3)*value.y)/1000,value.x=NULL,value.y=NULL,units=NULL,Metric="Cost of local pollution",Units="Thous. Millions 2015$")
    Gasoline<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"),input %in% c("refined liquids enduse")) %>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(input="gasoline")%>% left_join(costpollutant, by=c("input","year"))%>%
      mutate(value=(((value.x*(158.98*10**3))/5.33)*value.y)/1000,value.x=NULL,value.y=NULL,units=NULL,Metric="Cost of local pollution",Units="Thous. Millions 2015$")
    
    ######Cost of Congestion per energy
    Dieselcon<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road"),year=="2015") %>%
      group_by(old_scen_name) %>%  summarize(value=sum(value))%>%
      mutate(factor=((value*(158.98*10**3))/5.67)*0.1,value=NULL)
    Gasolinecon<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"),year=="2015") %>%
      group_by(old_scen_name) %>%  summarize(value=sum(value))%>% 
      mutate(factor=((value*(158.98*10**3))/5.33)*0.12,value=NULL)
    ######Cost of Accidents per energy
    Dieselacc<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road"),year=="2015") %>%
      group_by(old_scen_name) %>%  summarize(value=sum(value))%>% 
      mutate(factor=((value*(158.98*10**3))/5.67)*0.09,value=NULL)
    Gasolineacc<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"),year=="2015") %>%
      group_by(old_scen_name) %>% summarize(value=sum(value))%>% 
      mutate(factor=((value*(158.98*10**3))/5.33)*0.19,value=NULL)
    ######Cost of Road Damage per energy
    Dieseldam<-prj$data$`transport final energy by mode and fuel` %>% filter(sector %in% c("trn_pass_road"),year=="2015") %>%
      group_by( old_scen_name) %>%  summarize(value=sum(value))%>% 
      mutate(factor=((value*(158.98*10**3))/5.67)*0.01,value=NULL)
    ###Total cost of Pollutant
    Pollutantcost<-rbind(Diesel,Gas,Gasoline)%>%  group_by(scenario, region, experiment, old_scen_name, year,Metric,Units) %>%
      summarize(value=sum(value))
    
    
  }
  
  qry <- "transport service output by tech.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="transport service output by tech.proj"){
    
    transbytech<-prj$data$`transport service output by tech`%>% filter(technology%in% c("Liquids","Cycle","Electric","Walk","BEV","NG","FCEV","Hybrid Liquids","Coal"))%>%
      filter(subsector%in% c("Walk","Cycle","Bus","Moped","Motorcycle (50-250cc)","Motorcycle (>250cc)","Compact Car","Large Car and SUV","Mini Car","Subcompact Car"))#"Truck (0-1t)","Truck (6-15t)","Truck (>15t)",
    
    VKT<-left_join(transbytech,load_factors, by=c("region","sector", "subsector", "technology", "year")) %>% mutate(vkt = value/loadFactor)# Million passs-km /pass-veh 
    VKT$Units<- 'million vehicle-km'
    VKT[is.na(VKT)] <- 0
    EVVKT<-VKT %>%filter(technology %in% c("Electric","BEV"))%>% 
      group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(vkt))%>% mutate(Metric = "EV VKT")
    TotVKT<-VKT %>%  group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(vkt))
    EVVKT$Units<-"%"
    EVVKT$value<-(EVVKT$value/TotVKT$value)*100
    ####number of EV they are 140000 for every year (fix)
    #vkt_veh_yr <- read.csv('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/UCD_trn_data_CORE.csv', skip=7) %>% filter(UCD_region == "Latin America",unit=="vkt/veh/yr",mode=="LDV_4W")
    #names(vkt_veh_yr)[4]<-'subsector'
    #vkt_veh_yr2<-melt(vkt_veh_yr, id.vars=c("UCD_region", "UCD_sector", "mode" , "subsector", "UCD_technology", "UCD_fuel","variable","unit" ),variable.name = c("year"))
    #vkt_veh_yr2$year<-as.numeric(substring(vkt_veh_yr2$year,2))
    
    prj$data$`transport service output by tech` %>%  filter(sector == "trn_pass_road_LDV_4W",technology%in% c("BEV"))%>%
      left_join(load_factors, by = c("region","sector", "subsector", "technology", "year")) %>%  mutate(vehicles = (value / loadFactor / 14000)*(10**3),Metric = "Cumulative Elec. vehicles",Units="Thous #")  %>%
      group_by(scenario, experiment, old_scen_name, region, Metric, year, Units) %>% summarise(value = sum(vehicles))%>%  ungroup() -> Evehicles
    Evehicles$value<- ave(Evehicles$value*5,Evehicles$old_scen_name,FUN=cumsum)
    
    ######VKT passenger transport by heavy and light duty vehicles
    HDVvkt<-VKT%>% filter(sector%in% c("trn_pass_road"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(vkt)*10**6)%>%mutate(Units="vehicle-km",Metric="HDV")
    LDVvkt<-VKT%>% filter(sector%in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(vkt)*10**6)%>%mutate(Units="vehicle-km",Metric="LDV")
    gasvkt<-VKT%>% filter(technology%in% c("NG"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(vkt)*10**6)%>%mutate(Units="vehicle-km")
    dieselvkt<-VKT%>% filter(sector%in% c("trn_pass_road"),technology%in% c("Liquids"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(vkt)*10**6)%>%mutate(Units="vehicle-km")
    gasolinevkt<-VKT%>% filter(sector%in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"),technology%in% c("Liquids"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(vkt)*10**6)%>%mutate(Units="vehicle-km")
    
    HDVpass<-transbytech%>% filter(sector%in% c("trn_pass_road"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(value)*10**6)%>%mutate(Units="Pass-km",Metric="HDV")
    LDVpass<-transbytech%>% filter(sector%in% c("trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"))%>%group_by(scenario, experiment, old_scen_name, region, year) %>% summarise(value = sum(value)*10**6)%>%mutate(Units="Pass-km",Metric="LDV")
    
    
    
    ######VKT by heavy and light duty vehicles value.y=2015
    HDVvkt<-HDVvkt%>%left_join(HDVvkt%>% filter(year%in% c("2015"))%>%mutate(year=NULL), by=c("scenario","experiment","old_scen_name","region","Units"))
    LDVvkt<-LDVvkt%>%left_join(LDVvkt%>% filter(year%in% c("2015"))%>%mutate(year=NULL), by=c("scenario","experiment","old_scen_name","region","Units"))
    ######PKT by heavy and light duty vehicles value.y=2015
    HDVpass<-HDVpass%>%left_join(HDVpass%>% filter(year%in% c("2015"))%>%mutate(year=NULL), by=c("scenario","experiment","old_scen_name","region","Units"))
    LDVpass<-LDVpass%>%left_join(LDVpass%>% filter(year%in% c("2015"))%>%mutate(year=NULL), by=c("scenario","experiment","old_scen_name","region","Units"))
    ######Cost of Pollution /VKT
    Diesel$value<-(Diesel$value*(10**6))/dieselvkt$value
    Gas$value<-(Gas$value*(10**6))/gasvkt$value
    Gasoline$value<-(Gasoline$value*(10**6))/gasolinevkt$value
    ###Total cost of Pollutant/VKT
    Pollutantcostvkt<-rbind(Diesel,Gas,Gasoline)%>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Cost of Pollution per vkt",Units="US$ 2015")%>%filter(year %in% c(2015:2050))
    ######Cost of Congestion 
    Dieselcon1<-HDVvkt%>%left_join(Dieselcon,by=c("old_scen_name"))%>%mutate(value=(factor*value.x)/value.y)
    Gasolinecon1<-LDVvkt%>%left_join(Gasolinecon,by=c("old_scen_name"))%>%mutate(value=(factor*value.x)/value.y)
    ###TOtal Cost of Congestion 
    Congestioncost<-rbind(Dieselcon1,Gasolinecon1)%>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)/1000)%>%mutate(Metric="Cost of Congestion",Units="Thous. Millions 2015$")
    Congestioncostvkt<-Congestioncost%>%mutate(Metric="Cost of Congestion per vkt",Units="US$ 2015")
    Congestioncostvkt$value<-(Congestioncostvkt$value*10**6)/(LDVvkt$value.x+HDVvkt$value.x)
    ######Cost of accidents 
    Dieselacc1<-HDVpass%>%left_join(Dieselacc,by=c("old_scen_name"))%>%mutate(value=(factor*value.x)/value.y)
    Gasolineacc1<-LDVpass%>%left_join(Gasolineacc,by=c("old_scen_name"))%>%mutate(value=(factor*value.x)/value.y)
    ###TOtal Cost of accidents 
    Accidentalcost<-rbind(Dieselacc1,Gasolineacc1)%>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)/1000)%>%mutate(Metric="Cost of Accidents",Units="Thous. Millions 2015$")
    Accidentalcostpkt<-Accidentalcost%>%mutate(Metric="Cost of Accidents per pass.",Units="US$ 2015")
    Accidentalcostpkt$value<-(Accidentalcostpkt$value*10**6)/(LDVpass$value.x+HDVpass$value.x)
    ######Cost of road damage
    Dieseldam1<-HDVvkt%>%left_join(Dieseldam,by=c("old_scen_name"))%>%mutate(value=(factor*value.x)/value.y)
    ###TOtal Cost of road damage 
    Damagecost<-Dieseldam1%>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value)/1000)%>%mutate(Metric="Road Damage",Units="Thous. Millions 2015$")
    Damagecostvkt<-Damagecost%>%mutate(Metric="Cost of R. Damage per vkt",Units="US$ 2015")
    Damagecostvkt$value<-(Damagecostvkt$value*10**6)/(LDVvkt$value.x+HDVvkt$value.x)
    ###TOtal Cost of Transport externalities
    TrnExternalities<-rbind(as_tibble(Congestioncost),as_tibble(Accidentalcost),as_tibble(Damagecost),as_tibble(Pollutantcost))%>%  
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Trn. Externalities",Units="Thous. Millions 2015$")
    #TrnExternalitiesvkt<-rbind(as_tibble(Congestioncostvkt),as_tibble(Damagecostvkt),as_tibble(Pollutantcostvkt))%>%group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%mutate(Metric="Cost of Trn. Ext. per vkt",Units="US$ 2015")
    
    select_exps <- c('10', '26', '74', '90', "11", "27", "75", "91")
    Metrics <-
      rbind(
        as_tibble(EVVKT),
        as_tibble(Evehicles),
        as_tibble(Congestioncost),
        as_tibble(Accidentalcost),
        as_tibble(Damagecost),
        as_tibble(Pollutantcost),
        as_tibble(TrnExternalities),
        as_tibble(Congestioncostvkt),
        as_tibble(Accidentalcostpkt),
        as_tibble(Damagecostvkt),
        as_tibble(Pollutantcostvkt)
      )
    
    rdm_cart(EVVKT)
    # rdm_cart(Evehicles)
    rdm_cart(Congestioncost)
    rdm_cart(Accidentalcost)
    rdm_cart(Damagecost)
    rdm_cart(Pollutantcost)
    rdm_cart(TrnExternalities)
    rdm_cart(Congestioncostvkt)
    rdm_cart(Accidentalcostpkt)
    rdm_cart(Damagecostvkt)
    rdm_cart(Pollutantcostvkt)

    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE, plot_by_select_experiment=select_exps)}
    rm(TotVKT,Evehicles)  
  }
  
  qry <- "final energy consumption by sector and fuel.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="final energy consumption by sector and fuel.proj"){
    
    vv<-prj$data$`final energy consumption by sector and fuel`
    
    #####Industry
    indEnergy<-prj$data$`final energy consumption by sector and fuel` %>%filter(sector %in% c("N fertilizer","cement","industrial energy use","industrial feedstocks","process heat cement"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    indEnergy$input <- gsub("biomass", "delivered biomass", indEnergy$input)
    indEnergy$input <- gsub("coal", "delivered coal", indEnergy$input)
    indEnergy$input <- gsub("electricity", "elect_td_ind", indEnergy$input)
    indEnergy$input <- gsub("gas", "delivered gas", indEnergy$input)
    indEnergy$input <- gsub("hydrogen", "H2 enduse", indEnergy$input)
    indEnergy$input <- gsub("refined liquids", "refined liquids industrial", indEnergy$input)
    #####Building
    buildEnergy<-prj$data$`final energy consumption by sector and fuel`  %>%filter(sector %in% c("comm cooling","comm heating","comm others","resid cooling","resid heating","resid others"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    buildEnergy$input <- gsub("biomass", "delivered biomass", buildEnergy$input)
    buildEnergy$input <- gsub("coal", "delivered coal", buildEnergy$input)
    buildEnergy$input <- gsub("electricity", "elect_td_bld", buildEnergy$input)
    buildEnergy$input <- gsub("gas", "delivered gas", buildEnergy$input)
    buildEnergy$input <- gsub("hydrogen", "H2 enduse", buildEnergy$input)
    buildEnergy$input <- gsub("refined liquids", "refined liquids enduse", buildEnergy$input)
    #####aviation transport
    avEnergy<-prj$data$`final energy consumption by sector and fuel` %>%filter(sector %in% c("trn_aviation_intl"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value)*CF$Bk)
    avEnergy$input <- gsub("refined liquids", "refined liquids enduse", avEnergy$input)
    #####Pass transport
    passEnergy<-prj$data$`final energy consumption by sector and fuel`  %>%filter(sector %in% c("trn_pass","trn_pass_road","trn_pass_road_LDV_2W","trn_pass_road_LDV_4W"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    passEnergy$input <- gsub("electricity", "elect_td_trn", passEnergy$input)
    passEnergy$input <- gsub("gas", "delivered gas", passEnergy$input)
    passEnergy$input <- gsub("hydrogen", "H2 enduse", passEnergy$input)
    passEnergy$input <- gsub("refined liquids", "refined liquids enduse", passEnergy$input)
    ###****************
    passEnergy<-rbind(avEnergy,passEnergy)%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    #####shipping transport
    shpEnergy<-prj$data$`final energy consumption by sector and fuel` %>%filter(sector %in% c("trn_shipping_intl"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value)*CF$Bk)
    shpEnergy$input <- gsub("refined liquids", "refined liquids enduse", shpEnergy$input)
    #####frght transport
    frghtEnergy<-prj$data$`final energy consumption by sector and fuel`  %>%filter(sector %in% c("ttrn_freight","trn_freight_road"))%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    frghtEnergy$input <- gsub("electricity", "elect_td_trn", frghtEnergy$input)
    frghtEnergy$input <- gsub("gas", "delivered gas", frghtEnergy$input)
    frghtEnergy$input <- gsub("refined liquids", "refined liquids enduse", frghtEnergy$input)
    ###****************
    frghtEnergy<-rbind(shpEnergy,frghtEnergy)%>%
      group_by(scenario, region, input,experiment, old_scen_name,  year) %>%  summarize(value=sum(value))%>%mutate(Metric="Frght. Trn Energy use",Units="EJ")
    
  }
  
  
 
  qry <- "transport service output by mode.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="transport service output by mode.proj"){
    #********############Bus percentage of passenger demand. ###############
    traindemand<-prj$data$`transport service output by mode` %>% filter(mode %in% c("HSR","Passenger Rail")) %>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))%>%  mutate(Metric="Train passenger demand", Units ="million pass-km")
    cycledemand<-prj$data$`transport service output by mode`%>%  filter(mode %in% c("Cycle")) %>%
      group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
      summarize(value=sum(value))
    airdemand<-prj$data$`transport service output by mode` %>%  filter(mode %in% c("Domestic Aviation"))%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))%>%  mutate(Metric="airplane passenger demand", Units ="million pass-km")
    walkdemand<-prj$data$`transport service output by mode` %>%  filter(mode %in% c("Walk"))%>%
      group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
      summarize(value=sum(value))
    bussdemand<-prj$data$`transport service output by mode` %>%filter(mode %in% c("Bus"))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%  mutate(Metric="Bus passenger demand", Units ="million pass-km")
    busdemand<-prj$data$`transport service output by mode` %>%filter(mode %in% c("Bus"))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%  mutate(Metric="Bus passenger demand", Units ="%")
    cardemand<-prj$data$`transport service output by mode` %>%  filter(mode %in% c("LDV"))%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))%>%  mutate(Metric="Car passenger demand", Units ="million pass-km")
    #*****########Bus percentage########
    passdemand<-cardemand$value+traindemand$value+busdemand$value+airdemand$value+cycledemand$value+walkdemand$value
    busdemand$value<-(busdemand$value/passdemand)*100

    #*****########Bus percentage########
    #passdemand<-cardemand$value+busdemand$value
    #cardemand$value<-(cardemand$value/passdemand)*100
    trnpasss<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_pass"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Total Pass. service cost",Units="Thous. Mill. 2015$")
    trnpasss$value<-(passprice$value*(trnpasss$value)*1.81)/1000#1.81 *1990$=2015$
    price2015<-trnpasss%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    trnpass<-trnpasss%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL)
    
    trnfrghts<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_freight"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Total Freight service cost",Units="Mill. 2015$")
    trnfrghts$value<-frghtprice$value*(trnfrghts$value)*1.81#1.81 *1990$=2015$
    price2015<-trnfrghts%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    trnfrght<-trnfrghts%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL)
    
    trn<-rbind(trnpass,trnfrght)%>%group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Total trn. service cost")
    price2015<-trn%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    trn<-trn%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL)
    
    Trnpass<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_pass"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Pass.trn. carbon intensity",Units="gCO2eq/Pass-km")
    TCO2eqPass<-TCO2eqPass%>%  filter(year %in% c(2010:2050))
    Trnpass$value<-(TCO2eqPass$value/Trnpass$value)*10**6
    
    Trnfrght<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_freight"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Frght.trn. carbon intensity",Units="gCO2eq/Ton-km")
    TCO2eqFrght<-TCO2eqFrght%>%  filter(year %in% c(2010:2050))
    Trnfrght$value<-(TCO2eqFrght$value/Trnfrght$value)*10**6
    
    
    Trnpass1<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_pass"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Pass.trn. energy efficiency",Units="MJ/Pass-km")
    passEnergy1<-passEnergy%>%  filter(year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    Trnpass1$value<-(passEnergy1$value/Trnpass1$value)*10**6
    
    Trnfrght1<-prj$data$`transport service output by mode`%>%  filter(sector %in% c("trn_freight"),year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Frght.trn. energy efficiency",Units="MJ/Ton-km")
    frghtEnergy1<-frghtEnergy%>%  filter(year %in% c(2010:2050))%>%
      group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    Trnfrght1$value<-(frghtEnergy1$value/Trnfrght1$value)*10**6
    
    select_exps <- c('10', '26', '74', '90', "11", "27", "75", "91")
    Metrics <-
      rbind(
        as_tibble(busdemand),
        as_tibble(trnpass),
        as_tibble(trnfrght),
        as_tibble(trn),
        as_tibble(trnpasss),
        as_tibble(trnfrghts),
        as_tibble(Trnpass1),
        as_tibble(Trnfrght1)
      )
    
    rdm_cart(busdemand)
    rdm_cart(trnpass)
    rdm_cart(trnfrght)
    rdm_cart(trn)
    rdm_cart(trnpasss)
    rdm_cart(trnfrghts)
    rdm_cart(Trnpass1)
    rdm_cart(Trnfrght1)
 
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE, plot_by_select_experiment=select_exps)}
    
    rm(traindemand,cycledemand,airdemand,walkdemand,cardemand,passdemand)
  }
 
  qry <- "final energy prices.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="final energy prices.proj"){
    
    names(prj$data$`final energy prices`)[3]<-"input"
    #####Industry
    indEnergycost<-indEnergy%>%left_join(prj$data$`final energy prices`,by=c("scenario", "region", "input","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x*10**6)*value.y,value.x=NULL,value.y=NULL)%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-indEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    indEnergycost1<-indEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Ind. final energy cost")
    #####Building
    buildEnergycost<-buildEnergy%>%left_join(prj$data$`final energy prices`,by=c("scenario", "region", "input","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x*10**6)*value.y,value.x=NULL,value.y=NULL)%>%
      filter(input%in% c("delivered biomass","delivered coal","elect_td_bld","delivered gas","H2 enduse","refined liquids enduse"))%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-buildEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    buildEnergycost1<-buildEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Bld. final energy cost")
    #####Pass transport
    passEnergycost<-passEnergy%>%left_join(prj$data$`final energy prices`,by=c("scenario", "region", "input","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x*10**6)*value.y,value.x=NULL,value.y=NULL)%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-passEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    passEnergycost1<-passEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Pass.trn. final energy cost")
    #####frght transport
    frghtEnergycost<-frghtEnergy%>%left_join(prj$data$`final energy prices`,by=c("scenario", "region", "input","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x*10**6)*value.y,value.x=NULL,value.y=NULL)%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-frghtEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    frghtEnergycost1<-frghtEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Frght.trn. final energy cost")
    #####transport
    trnEnergycost<-rbind(passEnergycost,frghtEnergycost)%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-trnEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    trnEnergycost1<-trnEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="trn. final energy cost")
    #####Total
    totalEnergycost<-rbind(passEnergycost,frghtEnergycost,indEnergycost,buildEnergycost)%>%
      group_by(scenario, region, experiment, old_scen_name,  year) %>%  summarize(value=sum(value))
    price2015<-totalEnergycost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    totalEnergycost1<-totalEnergycost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Total final energy cost")
    
    Metrics <-
      rbind(
        as_tibble(indEnergycost1),
        as_tibble(buildEnergycost1),
        as_tibble(passEnergycost1),
        as_tibble(frghtEnergycost1),
        as_tibble(trnEnergycost1),
        as_tibble(totalEnergycost1)
      )
    
    rdm_cart(indEnergycost1)
    rdm_cart(buildEnergycost1)
    rdm_cart(passEnergycost1)
    rdm_cart(frghtEnergycost1)
    rdm_cart(trnEnergycost1)
    rdm_cart(totalEnergycost1)
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)}
    
    #rm(indEnergycost1,indEnergycost,buildEnergycost1,buildEnergycost,passEnergycost1,passEnergycost,frghtEnergycost1,frghtEnergycost,
       #trnEnergycost1,trnEnergycost,totalEnergycost1,totalEnergycost1,price2015)
  }
  
  qry <- "primary energy consumption by region (avg fossil efficiency).proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="primary energy consumption by region (avg fossil efficiency).proj"){
    #####Energy consumpt
    all<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%  group_by(scenario, region, experiment, old_scen_name,Units,year) %>%
      summarize(value=sum(value))%>%mutate(Metric="Total primary energy consumption")%>%filter(year%in% c(2010:2050))
    oil<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("a oil"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    oilp<-oil%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Oil primary energy consumption")%>%filter(year%in% c(2010:2050))
    gas<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("b natural gas"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050)) %>%
      mutate(Units="EJ",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Gas primary energy consumption")%>%filter(year%in% c(2010:2050))
    gasp<-gas%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Gas primary energy consumption")%>%filter(year%in% c(2010:2050))
    coal<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("c coal"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    coalp<-coal%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Coal primary energy consumption")%>%filter(year%in% c(2010:2050))
    renewables<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("g wind","h solar","i geothermal"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    renewablesp<-renewables%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Renew. primary energy consumption")%>%filter(year%in% c(2010:2050))
    biomass<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("d biomass"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    biomassp<-biomass%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Biomass primary energy consumption")%>%filter(year%in% c(2010:2050))
    hydro<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("f hydro"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    hydrop<-hydro%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Hydro primary energy consumption")%>%filter(year%in% c(2010:2050))
    tradbio<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("j traditional biomass"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    tradbiop<-tradbio%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Trad.Bio. primary energy consumption")%>%filter(year%in% c(2010:2050))
    nuclear<-prj$data$`primary energy consumption by region (avg fossil efficiency)`%>%filter(fuel %in% c("e nuclear"))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    nuclearp<-nuclear%>%left_join(all, by=c("scenario","region","experiment","old_scen_name","year"))%>%
      mutate(value=(value.x/value.y)*100,Units="%",value.x=NULL,value.y=NULL,Units.x=NULL,Units.y=NULL,Metric="Nuclear primary energy consumption")%>%filter(year%in% c(2010:2050))
    
    pengdp<-GDP%>%left_join(all,by=c("scenario", "region", "experiment","old_scen_name","year"))%>%
      mutate(value=(value.y*10**9)/(value.x),value.x=NULL,value.y=NULL,Metric.x=NULL,Metric.y=NULL,Units.x=NULL,Units.y=NULL,Metric="P. Energy per GDP",Units="GJ/Mill.1990US$")%>%filter(year %in%(2010:2050))
    penpop<-Pop%>%left_join(all,by=c("scenario", "region", "experiment","old_scen_name","year"))%>%
      mutate(value=(value.y*10**9)/(value.x),value.x=NULL,value.y=NULL,Metric.x=NULL,Metric.y=NULL,Units.x=NULL,Units.y=NULL,Metric="P. Energy per capita",Units="GJ")%>%filter(year %in%(2010:2050))
    
    
    Metrics <-
      rbind(
        as_tibble(all),
        as_tibble(oilp),
        as_tibble(gas),
        as_tibble(coalp),
        as_tibble(renewablesp),
        as_tibble(biomassp),
        as_tibble(hydrop),
        as_tibble(tradbiop),
        as_tibble(nuclearp),
        as_tibble(pengdp),
        as_tibble(penpop)
      )
    
    rdm_cart(all)
    rdm_cart(oilp)
    rdm_cart(gas)
    rdm_cart(coalp)
    rdm_cart(renewablesp)
    rdm_cart(biomassp)
    rdm_cart(hydrop)
    rdm_cart(tradbiop)
    rdm_cart(nuclearp)
    rdm_cart(pengdp)
    rdm_cart(penpop)
    
    select_exps <- c("42", "58", "43", "59", "46", "62", "47", "63")
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps)}
    rm(all,oilp,gasp,coalp,renewablesp,biomassp,hydrop,tradbiop,nuclearp)  
  }
  
  qry <- "regional primary energy prices.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="regional primary energy prices.proj"){
    nuc<-prj$data$`regional primary energy prices`%>%filter(fuel=="nuclearFuelGenIII")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    nuc$value<-nuc$value*nuclear$value*10**6
    bio<-prj$data$`regional primary energy prices`%>%filter(fuel=="regional biomass")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    bio$value<-bio$value*biomass$value*10**6
    c<-prj$data$`regional primary energy prices`%>%filter(fuel=="regional coal")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    c$value<-c$value*coal$value*10**6
    ngas<-prj$data$`regional primary energy prices`%>%filter(fuel=="regional natural gas")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    ngas$value<-ngas$value*gas$value*10**6
    roil<-prj$data$`regional primary energy prices`%>%filter(fuel=="regional oil")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))
    roil$value<-roil$value*oil$value*10**6
    totalPcost<-rbind(nuc,bio,c,ngas,roil)%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))
    price2015<-totalPcost%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    totalPcost1<-totalPcost%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Total primary energy cost")
    
    Metrics<-rbind(as_tibble(totalPcost1))
    
    rdm_cart(totalPcost1)
    
    select_exps <- c("42", "58", "43", "59", "46", "62", "47", "63")
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps)}
    rm(nuc,bio,c,ngas,roil,price2015,totalPcost,totalPcost1) 
  }
  
  qry <- "refined liquids production by subsector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="refined liquids production by subsector.proj"){
    bioliquid<-prj$data$`refined liquids production by subsector`%>%filter(subsector=="biomass liquids")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))%>%mutate(Units="EJ",Metric="Biofuels Energy")
    
    bioliquid1<-prj$data$`refined liquids production by subsector`%>%filter(subsector=="biomass liquids")%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))%>%mutate(Units="%",Metric="Biofuels production")
    refliquid<-prj$data$`refined liquids production by subsector`%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))%>%filter(year%in% c(2010:2050))%>%mutate(Units="EJ",Metric="Total liquid production")
    bioliquid1$value<-(bioliquid1$value/refliquid$value)*100
    Metrics <- rbind(as_tibble(bioliquid), as_tibble(bioliquid1))
    
    rdm_cart(bioliquid)
    rdm_cart(bioliquid1)
    
    select_exps <- c("42", "58", "43", "59", "46", "62", "47", "63")
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE,  plot_by_select_experiment=select_exps)}
    
  }
  
  qry <- "building service costs.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="building service costs.proj"){
    
    bldsercost <-
      prj$data$`building service costs` %>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value = sum(value)) %>%  filter(year %in% c(2010:2050))
  }
  
  qry <- "building service output by service.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="building service output by service.proj"){
    
    bldoutput<-prj$data$`building service output by service`%>%  group_by(scenario, region, experiment, old_scen_name, year) %>% summarize(value=sum(value))
    bldoutput$value<-TotalBuild$value/bldoutput$value
    output2015<-bldoutput%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    bldoutput1<-bldoutput%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="bld. energy efficiency")
    
    bldoutput2<-prj$data$`building service output by service` %>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))%>%  filter(year %in% c(2010:2050))
    bldoutput2$value<-bldsercost$value*bldoutput2$value*10**6
    output2015<-bldoutput2%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    bldoutput3<-bldoutput2%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="bld. energy cost")
    
    Bldoutput<-prj$data$`building service output by service` %>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    
    #######Building carbon intensity..... query  -> building service output by service  required
    Build<-rbind(TCO2eqHh,TCO2eqComm)%>% group_by(scenario, region, experiment, old_scen_name, year,Units) %>%
      summarize(value=sum(value))  %>% mutate(Metric="Bld. carbon intensity",Units="MTCO2eq/EJ")
    Build$value<-Build$value/Bldoutput$value
    
    Metrics <-
      rbind(as_tibble(bldoutput1),
            as_tibble(bldoutput3),
            as_tibble(Build))
    
    rdm_cart(bldoutput1)
    rdm_cart(bldoutput3)
    rdm_cart(Build)
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)}
  }
  #if(qry=="fuel prices to industry.proj"){
  #  
  #  indsercost<-prj$data$`fuel prices to industry`  %>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
  #    summarize(value=sum(value))%>%  filter(year %in% c(2010:2050))
  #}
  
  qry <- "industry primary output by sector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="industry primary output by sector.proj"){
    
    nfert<-prj$data$`industry primary output by sector`%>%  filter(year %in% c(2010:2050),sector%in% c("N fertilizer")) %>% group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))
    
    
    cement<-prj$data$`industry primary output by sector`%>%  filter(year %in% c(2010:2050),sector%in% c("cement")) %>% group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))
    
    industry<-prj$data$`industry primary output by sector`%>%  filter(year %in% c(2010:2050),sector%in% c("industry")) %>% group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    
    
    #indoutput2<-prj$data$`industry primary output by sector` %>%  group_by(scenario, region, experiment, old_scen_name, year) %>%
    #  summarize(value=sum(value))%>%  filter(year %in% c(2010:2050))
    #indoutput2$value<-indsercost$value*indoutput2$value*10**6
    #output2015<-indoutput2%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
    #  summarize(value=sum(value))
    #indoutput3<-indoutput2%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
    #  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Ind. energy cost")
    
    #Metrics<-rbind(as_tibble(indoutput3))
    #for (i in seq(length(unique(Metrics$Metric)))) {
    #  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
    #  plot_df <- M
    #  fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
    #  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
    #  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)}
  }
  
  qry <- "industry total final energy by service.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="industry total final energy by service.proj"){
    Nfert<-prj$data$`industry total final energy by service`%>%  filter(year %in% c(2010:2050),sector%in% c("N fertilizer")) %>% group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))
    Cement<-prj$data$`industry total final energy by service`%>%  filter(year %in% c(2010:2050),sector%in% c("cement")) %>% group_by(scenario, region, experiment, old_scen_name,  year) %>%
      summarize(value=sum(value))
    Industry<-prj$data$`industry total final energy by service`%>%  filter(year %in% c(2010:2050),sector%in% c("industrial energy use","industrial feedstocks")) %>% group_by(scenario, region, experiment, old_scen_name, year) %>%
      summarize(value=sum(value))
    
    Nfert$value<-nfert$value/Nfert$value
    output2015<-Nfert%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    Nfert1<-Nfert%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="N fert. energy efficiency")
    Cement$value<-cement$value/Cement$value
    output2015<-Cement%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    Cement1<-Cement%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Cement energy efficiency")
    Industry$value<-industry$value/Industry$value
    output2015<-Industry%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    Industry1<-Industry%>%left_join(output2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Industry energy efficiency")
    
    Metrics <- rbind(as_tibble(Industry1),
                     as_tibble(Nfert1),
                     as_tibble(Cement1))
    
    rdm_cart(Industry1)
    rdm_cart(Nfert1)
    rdm_cart(Cement1)
    
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=NULL, x_min=x_min, x_max=x_max, legend_on=FALSE, gray_ribbon = TRUE)
      }
  }
 
  qry <- "refined liquids costs by subsector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="refined liquids costs by subsector.proj"){
    
    lcost<-prj$data$`refined liquids costs by subsector`%>%  filter(year %in% c(2010:2050))%>%  group_by(scenario, region, experiment,subsector, old_scen_name, year) %>%
      summarize(value=sum(value))
  }
  
  qry <- "refined liquids production by subsector.proj"
  prj_path <- paste0(base_dir, qry)
  prj <- loadProject(prj_path)
  if(qry=="refined liquids production by subsector.proj"){
    
    lprod<-prj$data$`refined liquids production by subsector`%>%  filter(year %in% c(2010:2050))%>%  group_by(scenario, region, experiment,subsector, old_scen_name, year) %>%
      summarize(value=sum(value))%>%left_join(lcost,by=c("scenario", "region", "experiment","subsector","old_scen_name", "year"))%>%mutate(value=value.x*value.y,value.x=NULL,value.y=NULL)
    lprod<-lprod%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))
    lprod1<-lprod%>%mutate(Metric="liquids carbon intensity",Units="MTCO2eq/EJ")
    cost2015<-lprod%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
      summarize(value=sum(value))
    lprod<-lprod%>%left_join(cost2015, by=c("scenario","region","experiment","old_scen_name"))%>%
      mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",value.x=NULL,value.y=NULL,Metric="Total liquids cost")
    
    lqemiss<-Co2%>%  filter(sector %in% c("refined liquids enduse","refined liquids industrial","refining"),year %in% c(2010:2050))%>%  group_by(scenario, region, experiment, old_scen_name,year) %>%
      summarize(value=sum(value))
    lprod1$value<-lqemiss$value/lprod1$value
    
    Metrics <- rbind(as_tibble(lprod), as_tibble(lprod1))
    
    rdm_cart(lprod)
    rdm_cart(lprod1)
    
    for (i in seq(length(unique(Metrics$Metric)))) {
      M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
      plot_df <- M
      fig_path <- c(paste(export_dir,unique(Metrics$Metric)[i],'.png',sep = ""))
      plot_XLfacs <- c('InvalidEntry')
      y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
      line_plot(plot_df, fig_path, plot_scens, plotXLfacs, y_lbl=y_lbl, x_lbl=x_lbl, x_min=x_min, x_max=x_max,title=NULL, legend_on=FALSE, gray_ribbon = TRUE)}
  }
#}

#}

  
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
  source('E:/NEXO-UA/Results/RDM_Colombia/metrics/RDM_plotting.R')
  setwd('E:/NEXO-UA/Results/RDM_Colombia/runs_126_12_22_2020')
  
  # Create dataFrame that will store all uncertainty results to be plotted
  plot_df <- data.frame(scenario = character(), experiment = integer(), region = character(), param = character(),
                        year = integer(), value = numeric(), Units = integer())
  
  #-----------------------------------------------------------------------------------------------------------------------
  # Use Metis to process certain complex queries that (1) require significant post-processing, and (2) for which
  # specific Metis functions/handing have already been developed. For example, this includes electricity investments. We
  # produce a single ".proj" file on PIC that stores the GCAM queries for hundreds or thousands of GCAM runs. This
  # ".proj" file is already in the format required by Metis, so very little pre-processing is required below. However,
  # some post-processing of metis-results is required to get the Metis outputs formatted for plotting.
  
  # Select relevant parameters that we want to process in Metis. The rest (which are simpler) will be processed here in
  # this script, not in Metis.
  paramsSelect_i <- c("elecCumCapGW", "elecNewCapGW", "elecCumCapCost", "elecNewCapCost", "elecAnnualRetPrematureGW",
                      "elecCumRetPrematureGW", "elecAnnualRetPrematureCost", "elecCumRetPrematureCost")   # c('emissCO2BySectorNoBio')  # c('All')
  
  dataProj_i <-"E:/NEXO-UA/Results/RDM_Colombia/runs_126_12_22_2020/query_proj_12_22_2020/select_queries_metis.proj"  # Use if gcamdata has been saved as .proj file
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
    dplyr::summarize(value = sum(value)) %>%
    ungroup()
  plot_df_append <- plot_df_append[, c(1,3,2,6,5,7,4)]
  plot_df <- rbind(plot_df, plot_df_append)
  
  
  fig_base_path <- "E:/NEXO-UA/Results/RDM_Colombia/runs_126_12_22_2020/output_color/"
  dir.create(fig_base_path)
  
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
    fig_path <- paste0(fig_base_path, paramSelect, "_select", '.png')
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
  