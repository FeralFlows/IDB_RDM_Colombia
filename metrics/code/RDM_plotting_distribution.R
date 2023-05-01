if('ggdist' %in% rownames(installed.packages()) == F){install.packages('ggdist')}
library(ggdist)

# Function to make plots
line_plot <- function(plot_df, fig_path, plot_scens, plot_XLfacs, y_lbl=NULL,
                      x_lbl=NULL, y_max=NULL, y_min=NULL,
                      all_same_color=1, title=NULL, legend_on=FALSE,
                      plot_var=NULL, x_min=NULL, x_max=NULL, plot_by_scen=FALSE,
                      plot_by_XLfac=FALSE, plot_by_select_experiment=FALSE,
                      gray_ribbon=NULL, distribution=NULL, excl_outliers=NULL,
                      impact_ribbon=NULL, impact_factor=NULL, legend_name=NULL,
                      exp=NULL, percentile_upper=NULL, percentile_lower=NULL){
  # ggplot2 Theme
  z_theme <<- theme_bw() +
    theme(
       text =               element_text(family = NULL, face = "plain",colour = "black", size = 8 ,hjust = 0.5,
                                         vjust = 0.5, angle = 0, lineheight = 0.9)
      ,axis.text.x =        element_text(size=8)
      ,axis.text.y =        element_text(size=8)
      # ,axis.title.x =       element_text(vjust = -1, margin=ggplot2::margin(t=1,unit="line"))
      ,axis.title.x =       element_blank()
      ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=ggplot2::margin(r=1,unit="line"))
      ,legend.key =         element_blank()
      ,legend.key.size =    ggplot2::unit(1.0, 'lines')
      ,legend.text =        element_text(size = 6, colour = "black")
      ,legend.title =       element_text(size = rel(1), face = NULL, hjust = 0, colour = "black")
      ,strip.background =   element_rect(fill = NA, colour = "black")
      ,plot.margin =        ggplot2::unit(c(0.2, 0.2, 0.2, 0.2), "lines")
      # ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -1, margin = ggplot2::margin(b=20), size=8)
      ,plot.title=          element_blank()
      ,legend.position =    c('right')  # c(0.95, 0.95)
    )

  p <- ggplot()  # create initial plot
  year_list <- seq(x_min, x_max, 5)
  # Exclude outliers
  if(!is.null(excl_outliers)){
    plot_df_2015 <- plot_df %>% 
      dplyr::filter(year %in% 2015)
    plot_df_temp <- plot_df %>% 
      dplyr::filter(year %in% year_list) %>% 
      dplyr::group_by(year) %>% 
      dplyr::mutate(lower = max(min(value, na.rm = T), as.numeric(quantile(value, 0.25)) - (IQR(value) * 1.5)),
                    upper = min(max(value, na.rm = T), as.numeric(quantile(value, 0.75)) + (IQR(value) * 1.5))) %>% 
      dplyr::filter(value < upper, value > lower) %>% 
      dplyr::select(-upper, -lower)
    plot_df <- rbind(plot_df_2015, plot_df_temp)
  }

  # Place gray ribbon on background
  if(!is.null(gray_ribbon)){
    if(!is.null(distribution)){
      smry_final <- plot_df %>% 
        filter(year %in% year_list) %>% 
        group_by(year) %>% 
      # median_qi(value, .width = c(.1, .25, .5, .75, .9, 1)) %>%
      # 0.5 means the band width from median, so it extend 0.25 quantile on each side
        median_qi(value, .width = c(.5, .8, 1)) 
      
      # smry_final <- plot_df %>%
      #   dplyr::filter(year %in% year_list) %>%
      #   dplyr::group_by(year) %>%
      #   dplyr::summarize(quantile = scales::percent(c(0, 0.1, 0.25, 0.75, 0.9, 1)),
      #             value = quantile(value, c(0, 0.1, 0.25,  0.75, 0.9, 1))) %>%
      #   dplyr::mutate(.width = case_when(quantile %in% c('0%', '100%') ~ 1,
      #                             quantile %in% c('10%', '90%') ~ 0.8,
      #                             quantile %in% c('25%', '75%') ~ 0.5),
      #          bound = case_when(quantile %in% c('0%', '10%', '25%') ~ '.lower',
      #                            quantile %in% c('75%', '90%', '100%') ~ '.upper'),
      #          .point = 'median',
      #          .interval = 'qi') %>%
      #   dplyr::ungroup() %>%
      #   dplyr::select(-quantile) %>%
      #   tidyr::spread(value = value, key = c('bound')) %>%
      #   dplyr::arrange(.width) %>%
      #   dplyr::left_join(plot_df %>%
      #               filter(year %in% year_list) %>%
      #               group_by(year) %>%
      #               summarize(value = quantile(value, c(0.5))), by = 'year') %>%
      #   dplyr::select(year, value, .lower, .upper, .width, .point, .interval)
      
      
      p <- smry_final %>%
        ggplot(aes(x = year, y = value, ymin = .lower, ymax = .upper), size = 0.1) +
        geom_lineribbon() +
        # scale_fill_brewer(labels = c('0-100th Percentile', '10-90th Percentile', '25-75th Percentile'),
        #                   palette = 'Blues')
        scale_fill_manual(labels = c('0-100th Percentile', '10-90th Percentile', '25-75th Percentile'),
                          values = c("#BDD7E7", "#6BAED6", "#3182BD"))
                          # values = c("#C0C0C0", "#736F6E", "grey20"))
      
      # p <- plot_df %>%
      #   filter(year %in% year_list) %>%
      #   ggplot(aes(x = year, y = value)) +
      #   stat_lineribbon(aes(fill=stat(.width)), .width = ppoints(50)) +
      #   scale_fill_distiller()

    }else if(!is.null(impact_ribbon)){
      
      df <- plot_df %>%
        left_join(exp, by = c('experiment', 'scenario')) %>%
        ungroup() %>% 
        select(year, {{impact_factor}}, value) %>% 
        dplyr::rename(group = {{impact_factor}})
      
      smry_final <- df %>% 
        group_by(year, group) %>% 
        summarize(max = max(value),
                  min = min(value))
      plot_impact <- df %>% 
        left_join(smry_final, by=c('year', 'group')) %>% 
        filter(year %in% year_list)
      
      if(!is.null(percentile_upper) & !is.null(percentile_lower)){
        plot_impact <- plot_impact %>% 
          group_by(year) %>% 
          mutate(percentile_upper = quantile(value, probs = percentile_upper),
                 percentile_lower = quantile(value, probs = percentile_lower))
        plot_impact$percentile_upper <- unique(plot_impact$percentile_upper[plot_impact$year == 2050])
        plot_impact$percentile_lower <- unique(plot_impact$percentile_lower[plot_impact$year == 2050])
      }
      
      p <- ggplot(plot_impact, aes(x = year, y = value, colour = group, fill = group)) +
        geom_ribbon(aes(ymin = min, ymax = max), alpha = .4) +
        scale_fill_manual(values = c("skyblue", "coral"),
                          aesthetics = c("colour", "fill"),
                          name = legend_name) +
        geom_line(aes(y = percentile_upper), color = 'red3', linetype = 'dashed', size = 1) +
        geom_line(aes(y = percentile_lower), color = 'olivedrab3', linetype = 'dashed', size = 1)
      
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


# Plot Metrics under Factor Impact =============================================
impact_plots <- function(M, path, exp, excl_outliers=NULL, 
                         impact_factor, legend_name, percentile_upper, percentile_lower){
  fig_path <- file.path(path, paste(unique(M$Metric), '_impact.png', sep = ""))
  y_lbl <- paste(M$Metric[1], ' (', M$Units[1], ')', sep = "")
  line_plot(
    M,
    fig_path,
    plot_scens,
    y_lbl = y_lbl,
    x_lbl = x_lbl,
    title = NULL,
    x_min = x_min,
    x_max = x_max,
    legend_on = FALSE,
    gray_ribbon = TRUE,
    plot_by_select_experiment = FALSE,
    distribution = NULL,
    excl_outliers = excl_outliers,
    impact_ribbon = TRUE,
    impact_factor = impact_factor,
    legend_name = legend_name,
    exp = exp,
    percentile_upper = percentile_upper,
    percentile_lower = percentile_lower)
}