#' Title
#' @import dplyr
#' @import yardstick
#' @param clustering_output \code{emr_clust} output
#' @param cluster_color_set for ggplot color set
#' @param plot_feature a vector indicating the indexes(colnames) of plot features
#'
#' @return sen_spe_plot
#' @export
#'
#' @examples
sen_spe<-function(clustering_output=emr,
                  cluster_color_set = NULL,
                  plot_feature = NULL){

  if(is.null(plot_feature)){
    plot_feature <-clustering_output$statistics_table$use_table$Ori_Variable%>%unique()%>%na.exclude()
  }

  PC_data<- clustering_output[["statistics_table"]][["PC_data"]]
  if(length(which(sapply(PC_data, is.factor)))<2){
    stop("You should convert the data into a factor")
  }
  if(is.null(cluster_color_set)){
    cluster_color_set<-c(pal_nejm(alpha = 0.8)(n_distinct(PC_data$Cluster))[-6],"grey10")
  }
  sen_spe_data_prep <-
  copy(clustering_output$statistics_table$PC_data)%>%
  .[ , (which(sapply(., is.factor))) := lapply(.SD, as.numeric), .SDcols = which(sapply(., is.factor))]

  sen_spe_data_prep <-
    sen_spe_data_prep %>%
    .[,c(plot_feature) := purrr::map(.SD,
                                   ~ str_split(.x,"\\.",simplify = TRUE)[,1] %>%
                                     factor(.)),
      .SDcols = plot_feature]

  sen_spe_data_prep<-sen_spe_data_prep[,-"Group"]


  sen_spe_data <-
    PC_data[,unique(Cluster) %>% seq_along(.)] %>%
    purrr::map(.,
        ~ copy(sen_spe_data_prep) %>%
          .[,Truth := ifelse(Cluster==.x,1,2) %>% factor(.,levels = c(1,2))] %>%
          .[,.SD,.SDcols = c("Truth",plot_feature)] %>%
          melt.data.table(., id.vars = "Truth", variable.factor = FALSE, value.name = "Test" ,value.factor = TRUE)
    )

  # --------------------------------------------------------------------------------------------------------------
  sens_result_data <-
    purrr::map(sen_spe_data,
        ~ copy(.x) %>%
          .[,yardstick::sens_vec(truth = Truth, estimate = Test, event_level = "second"), by = "variable"] %>%
          .[,.metric := "sens"] %>%
          setnames(.,"V1",".estimate")
    )

  spec_result_data <-
    purrr::map(sen_spe_data,
        ~ .x[,yardstick::spec_vec(truth = Truth, estimate = Test, event_level = "second"), by = "variable"] %>%
          .[,.metric := "spec"] %>%
          setnames(.,"V1",".estimate")
    )

  sens_spec_result_data <-
    map2(sens_result_data,
         spec_result_data,
         ~ rbind(.x,.y) %>%
           setDT(.) %>%
           .[,.metric := factor(.metric, levels = c("spec","sens"), labels = c("Specificity","Sensitivity"))] %>%
           .[, cumulative_estimate := cumsum(.estimate), by = c("variable")]) %>%
    setNames(.,paste(PC_data[,unique(Cluster) %>% seq_along(.)]) ) %>%
    rbindlist(.,idcol = "Cluster") %>%
    .[variable %in% plot_feature,] %>%
    .[,variable := factor(variable,levels = rev(plot_feature))] %>%
    .[,Cluster := factor(Cluster,levels = PC_data[,levels(Cluster) %>% rev(.)])] %>%
    .[order(Cluster, .metric),]

  #Profile for the sum of sensitivity and specificity
  sens_spec_result_plot <-
    copy(sens_spec_result_data) %>%
    { ggplot(.,
             aes(x = variable, y = cumulative_estimate, fill = Cluster)) +
        geom_col(data = .[.metric=="Specificity"], width = .65, position = position_dodge(width = .7), alpha = .5) +
        geom_col(data = .[.metric=="Sensitivity"], width = .65, position = position_dodge(width = .7), alpha = 1) +
        geom_tile(aes(x = variable, y = NA_integer_, alpha = .metric)) +
        geom_hline(yintercept = 1,   size = .5, color = "grey40", linetype = 2) +
        geom_hline(yintercept = 1.5, size = .5, color = "grey20", linetype = 2) +
        scale_x_discrete(name = "Immunomarkers") +
        scale_alpha_discrete(name = "Metric", breaks = c("Sensitivity","Specificity")) +
        scale_y_continuous(name = "Sensitivity + Specificity",expand = c(0,0),limits = c(0,2)) +
        scale_fill_manual(values = rev(cluster_color_set)) +
        theme_bw()  +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme(axis.title.x = element_text(face = "bold",size = rel(1.2),margin = margin(20,0,0,0)),
              axis.title.y = element_text(face = "bold",size = rel(1.2),margin = margin(0,20,0,0)),
              axis.text = element_text(face = "bold",size = rel(1)),
              legend.title = element_text(face = "bold",size = rel(1.2)),
              legend.text = element_text(face = "bold",size = rel(1))) +
        coord_flip() }
    n<-unique(sens_spec_result_plot$data$variable)
    a<-summary(PC_data)%>%as.data.frame()
    a<-a[grep(paste(n,collapse = "|"),a$Var2),3]%>%na.omit()
    a<- strsplit(a, ":", fixed = TRUE)
    a<-as.data.frame(a)
    a<-a[1,]%>%gather()
    a<-a[,-1]
    level<-data.frame(label=rep(c(1,2),length(unique(sens_spec_result_plot$data$variable))),value=a)
    out<-list(sens_spec_result_plot=sens_spec_result_plot,level=level)
  return(out)
}


