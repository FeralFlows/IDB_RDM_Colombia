library('rgcam')
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')

# Load single file produced from RDM experiment.
base_dir <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/')
output_file <- c('04022020.dat')
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
prj_copy <- prj  # Stores the original prj, before we cut extra stuff out of it.

num_runs <- 180
short_list <- c()
scens <- c("Reference", "ColPol", "DDP")
for(scen in scens){
  for(run in seq(1:num_runs)){
    if(paste0(scen, "_", run-1) %in% names(prj)){
      short_list <- append(short_list, paste0(scen, "_", run-1))
    }
  }
}
prj <- prj[short_list]  # Eliminate any extra/unnecessary runs stored in the .dat file from PIC.
# Save the new file so it can then be imported in the way Metis expects
saveRDS(prj, file='C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/04022020_Trim.dat')
#Import file to Metis
scenOrigNames_i = c("Reference", "ColPol", "DDP")
scenNewNames_i = c("Reference", "Current_Policy", "DDP")
#paramsSelect_i <- c('All')
paramsSelect_i <- c('emissCO2BySectorNoBio')
# Connect to gcam database or project
dataProjPath_i <- paste("C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output") # Path to dataProj file.
dataProj_i <-"04022020.dat"  # Use if gcamdata has been saved as .proj file

# LOOK INTO ERROR WITH TRIM FILE...corrupted?

#queriesSelect_i <- c("emissCO2BySectorNoBio")  # c("All")
queriesSelect_i <- c("CO2 emissions by sector (no bio)") # c("All")
regionsSelect_i <- c("Colombia")
dataGCAM<-metis.readgcam(reReadData = F,  # F
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
                         regionsSelect = regionsSelect_i,
                         paramsSelect=paramsSelect_i,
                         queriesSelect=queriesSelect_i)



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

# Save this R data object to avoid having to go through this import and transformation process again.
saveRDS(reorg_prj, file='C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/Reorg_allResults.proj')
# Plot CO2 emissions across the 540 scenarios.
fig_path <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/post_processing/figures/CO2.png')
y_lbl <- 'CO2 Emissions (Mt)'
x_lbl <- 'Time'
title <- 'Emissions Uncertainty'
plot_df <- reorg_prj$`CO2 emissions by sector (no bio)` %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  summarize(value=(44/12)*sum(value)) %>%
  filter(scenario %in% c("DDP", "ColPol"))
x_min <- 2010
x_max <- 2050
plot_scens <- c("ColPol", "DDP")
line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max)
# Function to make plots
line_plot <- function(plot_df, fig_path, plot_scens, y_lbl=NULL,
                      x_lbl=NULL, y_max=NULL, y_min=NULL,
                      all_same_color=1, title=NULL, legend_on=TRUE,
                      plot_var=NULL, x_min=NULL, x_max=NULL){
  # ggplot2 Theme
  z_theme <<- theme_bw() +
    theme(
      text =                element_text(family = NULL, face = "plain",colour = "black", size = 10 ,hjust = 0.5,
                                         vjust = 0.5, angle = 0, lineheight = 0.9)
      , axis.text.x =       element_text(size=8)
      , axis.text.y =       element_text(size=8)
      ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
      ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
      ,legend.key =         element_blank()
      ,legend.key.size =    unit(1.5, 'lines')
      ,legend.text =        element_text(size = 8, colour = "black")
      ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
      ,strip.background =   element_rect(fill = NA, colour = "black")
      ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
      ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -4, margin = margin(b=20), size=8)
      ,legend.position =    c(0.95, 0.95)
    )

#  p <- ggplot(data=plot_df, mapping = aes(x = year, y = value, colour=scenario, fill=scenario))
  p <- ggplot()
  ctr <- 0
  color_list <- c('black', 'dodgerblue3')  #  #de2d26, #fc9272
  for(plot_scen in plot_scens){
    ctr <- ctr+1
    plot_df_filt <- plot_df %>% filter(scenario==plot_scen)
    p <- p + geom_line(size=0.5, color=color_list[ctr],
                       data=plot_df_filt, mapping = aes(x = year, y = value, group=experiment))  # colour=scenario

  }

  p <- p + xlab(x_lbl) + ylab(y_lbl)
  if(!is.null(y_min)){
    p<-p + scale_y_continuous(limits=c(y_min - 0.1*abs(y_min), 1.1*y_max))
  }
  if(!is.null(x_min)){
    p<-p + scale_x_continuous(limits=c(x_min, x_max))
  }
  if(legend_on==TRUE){
    p <- p + guides(color=legend_on)
  }
  p <- p + ggtitle(title)
  p <- p + z_theme
  p
  ggsave(fig_path, dpi=900, width=2.5, height=2.5, units="in")
}

#-----------------------------------------------------------------------------------------------------------------------
# Select three scenarios for which you want to produce individual/comparison plots.
Reference <- prj[['Reference_110']]
ColPol <- prj[['ColPol_110']]
DDP <- prj[['DDP_110']]
saveRDS(Reference, file = "Reference_110.rds")
saveRDS(ColPol, file = "ColPol_110.rds")
saveRDS(DDP, file = "DDP_110.rds")
scenarios <- c('Reference', 'ColPol', 'DDP')
#-----------------------------------------------------------------------------------------------------------------------
