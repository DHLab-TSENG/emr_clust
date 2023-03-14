#' Title
#'
#' @param clustering_output \code{emr_clust} output
#' @param plot_feature a vector indicating the indexes(colnames) of plot features
#' @param plot_value a value want to choose for plot
#' @param cluster_color_set for ggplot color set
#' @param proportion_score remove exam items in all clusters are less than some percentage (default: 0.7)
#'
#'
#' @return wind & ros plot
#' @export
#'
#' @examples
wind_rose_plot<-function(clustering_output,
                         plot_feature = NULL,
                         plot_value,
                         proportion_score = 0.7,
                         cluster_color_set = NULL
){
  PC_data<- clustering_output[["statistics_table"]][["PC_data"]]


  if(length(which(sapply(PC_data, is.factor)))<2){
    stop("You should convert the data into a factor")
  }

  if(is.null(plot_feature)){
    plot_feature <-clustering_output$statistics_table$use_table$Ori_Variable%>%unique()%>%na.exclude()
  }

  profile_status_by_item_data <-
    copy(PC_data) %>%
    # Subset data by column names
    .[,.SD,.SDcols = c("Cluster",plot_feature)] %>%
    # Transform data set into long format for further data portraying
    melt(.,
         measure.vars = plot_feature,
         variable.name = "plot_feature",
         value.name = "Status")  %>%
    # Tabulation
    .[,.N,by = c("Cluster","plot_feature","Status")]


  profile_status_by_item_data[-grep(plot_value, profile_status_by_item_data$Status),3]<-2
  profile_status_by_item_data[grep(plot_value, profile_status_by_item_data$Status),3]<-1

  if(is.null(cluster_color_set)){
    cluster_color_set<-c(pal_nejm(alpha = 0.8)(n_distinct(PC_data$Cluster))[-6],"grey10")
  }


  profile_status_by_item_data<- profile_status_by_item_data %>%
    # Transform data set for tabulating every possible permutation
    dcast.data.table(.,... ~ Status,value.var = "N",fill = 0) %>%
    melt.data.table(.,id.vars = c("Cluster","plot_feature"),variable.name = "Status",value.name = "N") %>%
    # Calculation for proportions
    .[,proportion := round(N/sum(N),3),by = c("Cluster","plot_feature")] %>%
    # Subset data set for those rows of Abnormal Status
    .[Status %in% 1,] %>%
    # Data ordering
    .[order(Cluster,-proportion),]

  if(is.null(plot_feature)){
    radar_plot_item_order <-
      split(profile_status_by_item_data,by = "Cluster") %>%
      purrr::map(., ~ .x[order(-proportion),] %>%
                   .[proportion > 0.5,as.character(plot_feature)] ) %>%
      flatten_chr(.) %>%
      unique(.) %>%
      # set the above variables in the front of other variables
      { c(.,str_subset(levels(profile_status_by_item_data$plot_feature),
                       paste(.,collapse = "|"),
                       negate = TRUE)) } %>%
      str_subset(.,
                 split(profile_status_by_item_data,by = "plot_feature") %>%
                   # remove exam items in all clusters are less than 10%
                   purrr::map(., ~ .x[all(proportion<=proportion_score),unique(as.character(plot_feature))]) %>%
                   flatten_chr(.) %>%
                   paste(.,collapse = "|"),
                 negate = TRUE)

    customised_rose_plot_item_order <-radar_plot_item_order
  }else{
    customised_rose_plot_item_order <-plot_feature
  }


  profile_status_by_item_plot_data <-
    copy(profile_status_by_item_data) %>%
    .[,.SD,.SDcols = c("Cluster","plot_feature","proportion")] %>%
    .[plot_feature %in% customised_rose_plot_item_order,] %>%
    .[,plot_feature := factor(plot_feature,levels = customised_rose_plot_item_order)] %>%
    .[,Cluster := as.factor(Cluster)] %>%
    .[order(Cluster,plot_feature),] %>%
    dcast.data.table(.,... ~ plot_feature,value.var = "proportion")


  t_col <-
    function(color, percent = percent, name = NULL) {
      ## Get RGB values for named colour
      rgb.val <- col2rgb(color)

      ## Make new colour using input colour as base and alpha set by transparency
      t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                   max = 255,
                   alpha = (100 - percent) * 255 / 100,
                   names = name)

      ## Save the colour
      invisible(t.col)
    }

  profile_status_by_item_overlapped_plot <-
    ggRadar2(data = profile_status_by_item_plot_data,
             aes(group = Cluster),
             alpha = 0.05,
             size = .1,
             rescale = FALSE) +
    scale_color_manual(values = map_chr(cluster_color_set, ~ t_col(.x,percent = 40)),
                       labels = c(profile_status_by_item_plot_data[,str_replace_all(Cluster,"Cluster\\s","")])) +
    scale_fill_manual(values = map_chr(cluster_color_set, ~ t_col(.x,percent = 40)),
                      labels = c(profile_status_by_item_plot_data[,str_replace_all(Cluster,"Cluster\\s","")])) +
    scale_y_continuous(breaks = seq(0,1,by = .25),
                       labels = paste0(seq(0,100,by = 25),"%"),
                       limits = seq(0,1)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(size = 0.5,linetype = 2),
          legend.position = "bottom",
          legend.title = element_text(face = "bold",size = rel(1.2)),
          legend.text = element_text(face = "bold",size = rel(1.1)),
          axis.text.x = element_text(face = "bold",size = rel(1.1),margin = margin(10,0,0,0)),
          axis.text.y = element_text(face = "bold",size = rel(1.1),margin = margin(0,10,0,0)),
          axis.ticks.y.left = element_blank(),
          plot.background = element_rect(fill = "white", color = "white")) +
    guides(color = guide_legend(nrow = 1,byrow = TRUE))




  geom_bar_test_wind<- function(..., wind.params=list()) {
    params <- list(...)
    wind.params<- modifyList(params, wind.params)
    bar5 <- do.call("geom_bar", modifyList(
      list(stat = "identity", colour = "grey20", size = .3),
      wind.params)
    )
    list(bar5)
  }



  profile_status_by_item_roseplot <-
    copy(profile_status_by_item_plot_data) %>%
    melt.data.table(data = .,
                    id.vars = "Cluster",
                    variable.name = "Biomarker",
                    variable.factor = TRUE,
                    value.name = "Proportion") %>%
    { ggplot(data = .) +
        geom_bar(aes(x = Biomarker, y = Proportion, fill = Cluster), stat = "identity", colour = "grey20", size = .3) +
        scale_fill_manual(values = map_chr(cluster_color_set, ~ t_col(.x,percent = 40))) +
        scale_y_continuous(breaks = seq(0,1,by = .25),
                           labels = paste0(seq(0,100,by = 25),"%"),
                           limits = seq(0,1)) +
        theme_bw() +
        coord_polar(theta = "x", start = -pi/45, clip = "off") +
        theme(panel.border = element_blank(),
              panel.background = element_rect(fill = "white"),
              panel.grid = element_line(size = 0.5,linetype = 2),
              # panel.spacing = unit(.2, "lines"),
              legend.position = "none",
              axis.title = element_blank(),
              axis.text.x = element_text(face = "bold",size = rel(1),margin = margin(10,0,0,0)),
              axis.text.y = element_text(face = "bold",size = rel(1),margin = margin(0,10,0,0)),
              axis.ticks.y.left = element_blank(),
              strip.text = element_text(face = "bold",size = rel(1.2)),
              strip.background = element_blank(),
              plot.background = element_rect(fill = "white", color = "white")) +
        facet_wrap(Cluster ~ .,nrow = 2)
    }
  out<-list(overlapped=profile_status_by_item_overlapped_plot ,seperated=profile_status_by_item_roseplot,customised_rose_plot_item_order=customised_rose_plot_item_order)
  return(out)
}

