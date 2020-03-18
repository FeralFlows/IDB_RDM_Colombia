library('rgcam')

# Load single file produced from RDM experiment.
base_dir <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/output/')
output_file <- c('allResults.proj')
prj <- loadProject(paste0(base_dir, output_file))
prj_copy <- prj
# Select three scenarios for which you want to produce individual/comparison plots.

Reference <- prj[['Reference_110']]
ColPol <- prj[['ColPol_110']]
DDP <- prj[['DDP_110']]
saveRDS(Reference, file = "Reference_110.rds")
saveRDS(ColPol, file = "ColPol_110.rds")
saveRDS(DDP, file = "DDP_110.rds")
scenarios <- c('Reference', 'ColPol', 'DDP')

# Create empty data frame
queries <- listQueries(prj)
output <- data.frame()
output <- setNames(data.frame(matrix(ncol = length(queries), nrow = 0)), queries)

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

# Create a single dataframe that stores all all experiments under a single query.
names(reorg_prj) <- queries
reorg_prj <- vector("list", length(queries))
names(reorg_prj) <- queries

for(item in names(reorg_prj)){
  reorg_prj[[item]] <- data.frame()
}
for(query in queries){
  for(experiment in names(prj)){
    reorg_prj[[query]] <- rbind(reorg_prj[[query]], prj[[experiment]][[query]])
  }
}

# Plot CO2 emissions across the 540 scenarios.

# Function to make plots
line_plot_hist_proj <- function(plot_df, plot_df_hist, fig_name, gcm_names, rcp_names, rolling=0, y_lbl=NULL,
                                x_lbl=NULL, y_max=NULL, y_min=NULL, trendline=1, all_same_color=1, title=NULL, legend_on=TRUE,
                                plot_var=NULL, plot_hist=TRUE, x_min=NULL, x_max=NULL, plot_reference=NULL,
                                gcm_list=NULL, rcp_list=NULL){

  line_colors<-get(plot_df$FillPalette)

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
      #,plot.margin=grid::unit(c(0,0,0,0), "mm")
    )

  p <- ggplot(data=plot_df_hist, mapping = aes(x = year, y = rolling_mean, colour=gcm, fill=gcm))


  if(all_same_color==1){
    color_var = 'grey70' # "#153E7E"
  }else{
    color_var = NULL
  }

  for(gcm1 in gcm_names){
    for(rcp1 in rcp_names){
      filtered_df <- plot_df_orig %>% filter(rcp==rcp1, gcm==gcm1)
      if(rolling==1){
        if(all_same_color==1){
          p <- p + geom_line(size=0.5, color = color_var, data=filtered_df, mapping = aes(x = year, y = rolling_mean))
        }else{
          p <- p + geom_line(size=0.5, data=filtered_df, mapping = aes(x = year, y = rolling_mean,
                                                                       colour=gcm))
        }
      }else if(rolling==2){
        if(all_same_color==1){
          p <- p + geom_line(size=0.5, color = color_var, data=filtered_df, mapping = aes(x = year, y = smoothedY))
        }else{
          p <- p + geom_line(size=0.5, data=filtered_df, mapping = aes(x = year, y = smoothedY,
                                                                       colour=gcm))
        }

      }else{
        if(all_same_color==1){
          p <- p + geom_line(size=0.5, color = color_var, data=filtered_df, mapping = aes(x = year, y = value))
        }else{
          p <- p + geom_line(size=0.5, data=filtered_df, mapping = aes(x = year, y = value, colour=gcm))
        }
      }

    }
  }

  # Plot reference scenario
  if(!is.null(plot_reference)){
    p <- p + geom_line(size=0.5, linetype=1, color = 'black', data=filtered_df, mapping = aes(x = year, y = reference))
  }

  # SO far, only adding this colored gcm/rcp plots for smoothedY
  # Plot select subset of gcms. Maximum of two lines, or will generate error
  if(rolling==2){
    ctr <- 0
    color_list <- c('dodgerblue3', '#fc9272')  #  #de2d26
    if(!is.null(gcm_list)){
      for(model in gcm_list){
        for(forc in rcp_list){
          ctr <- ctr + 1
          if(ctr>2){
            Print("error: currently can only plot 2 lines of individual gcms/rcps as one color")
          }
          plot_df_orig_2 <- plot_df_orig %>% filter(gcm == model, rcp==forc)  # , rcp == c('rcp2p6')
          plot_df_orig_2$gcm <- as.character(plot_df_orig_2$gcm)
          plot_df_orig_2$rcp <- as.character(plot_df_orig_2$rcp)
          p <- p + geom_line(size=0.5, linetype=1, color = color_list[ctr],
                             data=plot_df_orig_2, mapping = aes(x = year, y = smoothedY))  # linetype=2
        }
      }
    }
  }

  p <- p + xlab(x_lbl) + ylab(y_lbl)
  if(!is.null(y_min)){
    p<-p + scale_y_continuous(limits=c(y_min - 0.1*abs(y_min), 1.1*y_max))
  }
  if(!is.null(x_min)){
    p<-p + scale_x_continuous(limits=c(x_min, x_max))
  }
  p<-p + scale_color_manual(values=line_colors, name = "Time Scale")
  if(!is.null(plot_df_hist)){
    p<-p + scale_color_manual(values=line_colors_hist)
  }
  if(legend_on==FALSE){
    p <- p + guides(color=legend_on)
  }
  p <- p + ggtitle(title)
  p <- p + z_theme
  p
  ggsave(fig_name, dpi=900, width=2.5, height=2.5, units="in")
}
