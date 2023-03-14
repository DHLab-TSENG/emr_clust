clust_biplot<-function(PC_data,
                       attr_data,
                       pc.params,
                       attr.params,
                       text.params,
                       cluster_color_set,
                       bar.params
){

  if(is.null(bar.params)){
    bar.params<-list(stat = "identity",position = "stack",alpha = 0.8)
  }
  variable_list <-
    purrr::map(c(sum(str_detect(names(PC_data),"^PC")):2),
               ~ paste0("`","PC ",c(.x-1,.x),"`") %>%
                 c(.,"Variable","Cluster","Group")) %>%
    rev(.)

  if(is.null(cluster_color_set)){
    cluster_color_set<-c(pal_nejm(alpha = 0.8)(n_distinct(PC_data$Cluster))[-6],"grey10")
  }


  geom_point_test <- function(..., v,point_attr.params = list() , point_pc.params=list(), text.params=list()) {
    params <- list(...)
    point_attr.params <- modifyList(params, point_attr.params)
    point_pc.params <- modifyList(params, point_pc.params)
    text.params <- modifyList(params, text.params)
    bar <- do.call("geom_point", modifyList(
      list(data = attr_data,
           mapping=aes_string(x = variable_list[[1]][1],y = variable_list[[1]][2]),
           shape = "cross",
           color = "black",
           alpha = 0.3),
      point_attr.params)
    )
    bar2 <- do.call("geom_point", modifyList(
      list(data = PC_data,
           mapping=aes_string(x = variable_list[[1]][1],y = variable_list[[1]][2],color = variable_list[[1]][4],shape = variable_list[[1]][5]),
           position = position_jitter(height = 0.001,width = 0.001),
           size = 1.5,
           alpha = 0.5),
      point_pc.params)
    )
    list(bar,bar2)
  }
  geom_text_test<- function(..., text.params=list()) {
    params <- list(...)
    text.params <- modifyList(params, text.params)
    bar3 <- do.call("geom_text_repel", modifyList(
      list(data = attr_data,
           aes_string(x =variable_list[[1]][1],y = variable_list[[1]][2],label =variable_list[[1]][3]),
           size = rel(3),
           alpha = 0.2,
           min.segment.length = 5,
           box.padding = .3,
           max.overlaps = 100),
      text.params)
    )
    list(bar3)
  }
  clustering_biplots <-purrr::map(variable_list,
                                  ~ggplot()+
                                    geom_point_test(
                                      point_pc.params=c(pc.params,list(mapping=aes_string(x = .x[1],y = .x[2],color = .x[4],shape = .x[5]))),
                                      point_attr.params=c(attr.params,list(mapping=aes_string(x = .x[1],y = .x[2]))))+
                                    geom_text_test(
                                      text.params=c(text.params,list(aes_string(x = .x[1],y = .x[2],label = .x[4]))))+
                                    geom_vline(xintercept = 0,color = "black",alpha = 0.4) +
                                    geom_hline(yintercept = 0,color = "black",alpha = 0.4) +
                                    scale_colour_manual(name = "Cluster",values = cluster_color_set) +
                                    scale_fill_manual(name = "Cluster",values = cluster_color_set)+theme_bw()
  )


  cluster_size_string <-
    copy(PC_data) %>%
    .[,.N,by = "Cluster"] %>%
    .[,strings := label_comma()(N)] %>%
    .[order(Cluster),]

  profile_by_cluster_data <-
    copy(PC_data) %>%
    .[,.N,by = c("Cluster","Group")] %>%
    .[order(Cluster,Group),] %>%
    .[,Cluster_label := factor(Cluster,
                               levels = cluster_size_string$Cluster,
                               labels = cluster_size_string$strings)] %>%
    .[,Cluster_size       := sum(N),by = c("Cluster")] %>%
    .[,Cluster_proportion := sqrt(round(Cluster_size/sum(N),3))*0.95] %>%
    .[,Group_proportion   := round(N/Cluster_size,3)] %>%
    .[,Group_position   := cumsum(Group_proportion)-0.5*Group_proportion,by = c("Cluster")] #%>%
  #.[,Group := factor(Group,
  #                  levels = selected_disease_groups,
  #                 labels = map_chr(selected_disease_groups,~ str_split(.x,"\\s") %>%
  #                                   map_chr(., ~ str_sub(.x,1,1) %>%
  ##                                            str_to_upper(.) %>%
  #                                          str_c(.,collapse = ""))))]



  profile_by_cluster_holistic_data <-
    merge(profile_by_cluster_data,
          { copy(profile_by_cluster_data) %>%
              .[,unique(.SD),.SDcols = c("Cluster","Cluster_proportion")] %>%
              .[,x_position := round(cumsum(Cluster_proportion/sum(Cluster_proportion))-2.2*Cluster_proportion/sum(Cluster_proportion),3)] %>%
              .[,.SD,.SDcols = -c("Cluster_proportion")] },
          by = "Cluster",
          all.x = TRUE
    ) %>%
    .[,Cluster_label := factor(Cluster,
                               levels = sort(unique(cluster_size_string$Cluster)),
                               labels = cluster_size_string$strings)]


  geom_bar_test<- function(..., bar.params=list()) {
    params <- list(...)
    bar.params <- modifyList(params, bar.params)
    bar4 <- do.call("geom_bar", modifyList(
      list(stat = "identity",position = "stack",alpha = 0.8),
      bar.params)
    )
    list(bar4)
  }


  profile_by_cluster_integrated <-
    ggplot(profile_by_cluster_holistic_data,aes(x = x_position,y = Group_proportion,fill = Group,width = Cluster_proportion)) +
    geom_bar_test(bar.params=list(bar.params)) +
    scale_x_continuous(name = "Cluster\n(n)",
                       breaks = unique(profile_by_cluster_holistic_data$x_position),
                       labels = unique(profile_by_cluster_holistic_data$Cluster),
                       expand = c(0.001,0.001)) +
    scale_y_continuous(name = "Percentage",
                       breaks = seq(0,1,by = 0.2),
                       labels = seq(0,100,by = 20),
                       expand = c(0,0)) +
    scale_fill_manual(name = "Group",values = pal_jama()(7)) +
    facet_grid(~ Cluster+Cluster_label, space = "free_x", scales = "free_x",switch = "x",labeller = "label_value")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.background = element_blank(),
          panel.border = element_blank(),
    )


  cluster_size_string <-
    copy(PC_data) %>%
    .[,.N,by = "Group"] %>%
    .[order(-N),] %>%
    .[,Group_abr := factor(Group,
                           levels = unique(Group),
                           labels = map_chr(Group,~ str_split(.x,"\\s") %>%
                                              map_chr(., ~ str_sub(.x,1,2) %>%
                                                        str_to_upper(.) %>%
                                                        str_c(.,collapse = ""))))] %>%
    .[,strings := label_comma()(N)]

  profile_by_group_data <-
    copy(PC_data) %>%
    .[,.N,by = c("Cluster","Group")] %>%
    .[order(Group,-Cluster),] %>%
    .[,Group_label := factor(Group,
                             levels = cluster_size_string$Group,
                             labels = cluster_size_string$strings)] %>%
    .[,Group_size := sum(N),by = "Group"] %>%
    .[,Cluster_proportion_within_group := round(N/Group_size,4)] %>%
    .[,Cluster_proportion_within_group_position := cumsum(Cluster_proportion_within_group)-0.5*Cluster_proportion_within_group,by = c("Group")] %>%
    .[,Group := factor(Group,
                       levels = cluster_size_string$Group,
                       labels = cluster_size_string$Group_abr)]

  profile_by_group_holistic_data <-
    merge(profile_by_group_data,
          { copy(profile_by_group_data) %>%
              .[,unique(.SD),.SDcols = c("Group","Group_size")] %>%
              .[,Group_proportion := round(Group_size/sum(Group_size),3),] %>%
              .[,x_position := round(cumsum(Group_proportion/sum(Group_proportion))-0.5*Group_proportion/sum(Group_proportion),3)] %>%
              .[,.SD,.SDcols = -c("Group_size")] },
          by = "Group",
          all.x = TRUE
    )

  profile_by_group_integrated <-
    ggplot(profile_by_group_holistic_data,aes(x = x_position,y = Cluster_proportion_within_group,fill = Cluster,width = Group_proportion)) +
    geom_bar_test(bar.params=list(bar.params)) +
    scale_x_continuous(name = "Groups\n(n)",
                       breaks = unique(profile_by_group_holistic_data$x_position),
                       labels = unique(profile_by_group_holistic_data$Group),
                       expand = c(0.001,0.001)) +
    scale_y_continuous(name = "Percentage",
                       breaks = seq(0,1,by = 0.2),
                       labels = seq(0,100,by = 20),
                       expand = c(0,0)) +
    scale_fill_manual(values = cluster_color_set) +
    facet_grid(~ Group+Group_label, space = "free_x", scales = "free_x",switch = "x",labeller ="label_value")+
    theme_bw() +
    theme(legend.title = element_text(face = "bold",size = rel(1)),
          legend.text = element_text(face = "bold",size = rel(.9)),
          legend.position = "bottom",
          legend.key.size = unit(.6,"line"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.border = element_blank()) +
    guides(fill = guide_legend(nrow=1,byrow=TRUE))

  bioplot<-c(list(clustering_biplots=clustering_biplots,profile_by_cluster_integrated=profile_by_cluster_integrated ,profile_by_group_integrated=profile_by_group_integrated))
}

