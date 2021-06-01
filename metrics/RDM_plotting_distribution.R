if('ggdist' %in% rownames(installed.packages()) == F){install.packages('ggdist')}
library(ggdist)

# Function to make plots
line_plot <- function(plot_df, fig_path, plot_scens, plot_XLfacs, y_lbl=NULL,
                      x_lbl=NULL, y_max=NULL, y_min=NULL,
                      all_same_color=1, title=NULL, legend_on=FALSE,
                      plot_var=NULL, x_min=NULL, x_max=NULL, plot_by_scen=FALSE,
                      plot_by_XLfac=FALSE, plot_by_select_experiment=FALSE,
                      gray_ribbon=NULL, distribution=NULL){
  # ggplot2 Theme
  z_theme <<- theme_bw() +
    theme(
       text =                element_text(family = NULL, face = "plain",colour = "black", size = 8 ,hjust = 0.5,
                                         vjust = 0.5, angle = 0, lineheight = 0.9)
      ,axis.text.x =        element_text(size=8)
      ,axis.text.y =        element_text(size=8)
      # ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
      ,axis.title.x =       element_blank()
      ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
      ,legend.key =         element_blank()
      ,legend.key.size =    unit(1.0, 'lines')
      ,legend.text =        element_text(size = 6, colour = "black")
      ,legend.title =       element_text(size = rel(1), face = NULL, hjust = 0, colour = "black")
      ,strip.background =   element_rect(fill = NA, colour = "black")
      ,plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines")
      # ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -1, margin = margin(b=20), size=8)
      ,plot.title=          element_blank()
      ,legend.position =    c('right')  # c(0.95, 0.95)
    )

  p <- ggplot()  # create initial plot

  # Place gray ribbon on background
  if(!is.null(gray_ribbon)){
    year_list <- seq(x_min, x_max, 5)
    if(!is.null(distribution)){
      smry_final <- plot_df %>% 
        filter(year %in% year_list) %>% 
        group_by(year) %>% 
        median_qi(value, .width = c(.1, .25, .5, .75, .9, 1))
      
      p <- smry_final %>%
        ggplot(aes(x = year, y = value, ymin = .lower, ymax = .upper), size = 0.1) +
        geom_lineribbon() +
        scale_fill_brewer(labels = c('100%', '90%', '75%', '50%', '25%', '10%'))
      
    }else{
      smry_final <- tibble("max"=double(), "min"=double(), "year"=integer())
      for(yr in year_list){
        smry <- plot_df %>%
          filter(year==yr) %>%
          summarize(max=max(value), min=min(value), year=yr)
        smry_final <- rbind(smry_final, smry)
      }
      plot_df <- plot_df %>% left_join(smry_final, by=c('year'))
      p <- p + geom_ribbon(data=plot_df , aes(x = year, ymin=min, ymax=max), fill="grey85")
    }
  }

  #  p <- ggplot(data=plot_df, mapping = aes(x = year, y = value, colour=scenario, fill=scenario))
  ctr <- 0
  color_list <- c('dodgerblue3', 'black', 'grey85')  #  #de2d26, #fc9272
  if (plot_by_scen == TRUE){
    for(plot_scen in plot_scens){
      ctr <- ctr+1
      plot_df_filt <- plot_df %>% filter(scenario==plot_scen)
      p <- p + geom_line(size=0.5, color=color_list[ctr],
                         data=plot_df_filt, mapping = aes(x = year, y = value, group=experiment))  # colour=scenario,
    }
  }

  ctr <- 0
  color_list <- c('darkred', 'forestgreen')  #  #de2d26, #fc9272
  if (plot_by_XLfac == TRUE){
    num_facs=length(names(plot_XLfacs))
    for(filter_group in names(plot_XLfacs)){
      #Each filter group will be brought on with a different color. A filter group can consist of a combination of XL factors.
      XLfactor_names <- names(plot_XLfacs[[filter_group]])
      plot_df_filt <- plot_df
      for(var_name in XLfactor_names){
        plot_df_filt <- plot_df_filt %>%
          filter(!!as.symbol(var_name)==plot_XLfacs[[filter_group]][[var_name]])
      }
      ctr <- ctr + 1
      p <- p + geom_line(size=0.5, color=color_list[ctr],
                         data=plot_df_filt, mapping = aes(x = year, y = value, group=experiment))
    }
  }
  if (plot_by_select_experiment != FALSE){
    ctr <- 0
    color_list <- c('darkred', 'darkred', 'forestgreen', 'forestgreen')
    line_list <- c("solid", "dashed", "solid", "dashed")
    #color_list <- c('darkred', 'forestgreen', 'black', 'darkorange2')
    for(XL_exp in plot_by_select_experiment){
      ctr <- ctr+1
      plot_df_filt <- plot_df %>% filter(experiment==XL_exp)
      p <- p + geom_line(size=0.5, color=color_list[ctr], linetype=line_list[ctr],
                         data=plot_df_filt, mapping = aes(x = year, y = value))  # colour=scenario, color=color_list[ctr],
    }
  }
  p <- p +
    # xlab(x_lbl) +
    ylab(y_lbl)
  
  if(!is.null(y_min)){
    p <- p + scale_y_continuous(limits=c(y_min - 0.1*abs(y_min), 1.1*y_max))
  }
  if(!is.null(x_min)){
    p <- p + scale_x_continuous(limits=c(x_min, x_max))
  }
  if(legend_on==TRUE){
    p <- p + guide_legend(color=legend_on)
  }
  p <- p + ggtitle(title)
  p <- p + z_theme
  p
  ggsave(fig_path, dpi = 300, width = 4, height = 3.4, units = "in")
}
