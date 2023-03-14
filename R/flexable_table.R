
flexable_table<-function(data,
                         clust_output,
                         group,
                         choose_group
){

  PC_data <-#合併pc跟clust
    # Retrieve the best result object
    clust_output[[1]] %>%
    as.data.table(.) %>%
    # Set names
    { setnames(.,c(paste("PC",seq(1,ncol(.)-1),sep = " "),"Cluster")) } %>%
    # Add Group column
    cbind(.,
          data)%>%
    .[,':=' (Cluster = factor(Cluster,
                              levels = seq(min(Cluster),max(Cluster),by = 1)))]
  PC_data <-PC_data %>%
    .[,Group:= .[,group,with=F][[1]]%>%as.vector()%>%factor(.,
                                                            levels = names(table(PC_data [,group,with=F])))]
  if(!is.null(choose_group)){
    PC_data<-PC_data[Group %in% choose_group,]
  }

  attr_data <-
    # Retrieve best result object
    clust_output[[2]]%>%
    as.data.table(.,keep.rownames = T) %>%
    { setnames(.,c("Variable",paste("PC",seq(1,ncol(.)-1),sep = " "))) } #%>%
  # Modify values in the attribute column and create the status column for plotting purposes
  #.[,Attr_mdf := str_sub(Attr,
  #                      str_locate(Attr,"\\.AbN|\\.N")[,"start"]) %>%
  #  str_replace(.,"\\.","")] %>%
  #.[,Status   := str_split(Attr_mdf,"_",simplify = TRUE)[,1] %>%
  #   factor(.,levels = c("N","AbN"))]

  auto_antibody_string <- unique(attr_data$Variable)


  numAct <-   copy(PC_data) %>%
    { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] } %>%
    sapply(., is.numeric)%>%which()
  catAct <-   copy(PC_data) %>%
    { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] }

  catAct <-which(!sapply(catAct, is.numeric))

  cat_table_variable<-names(catAct)[-length(names(catAct))]
  num_table_variable<-names(numAct)

  if(length(numAct)==0){
    table_one_by_diseases <-
      # data preps
      copy(PC_data) %>%
      { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] } %>%
      #.[,SEX := factor(SEX,levels = c("M","F"),labels = c("Male","Female"))] %>%
      #[,c(auto_antibody_string) := purrr::map(.SD, ~ sapply(str_split(.x,"_"),`[`,1)),.SDcols = auto_antibody_string] %>%
      #[,c(auto_antibody_string) := purrr::map(.SD, ~ factor(.x,levels = c("N","AbN"))),.SDcols = auto_antibody_string] %>%
      # create the table one
      CreateTableOne(data = .,
                     strata = group,
                     factorVars = cat_table_variable,
                     test = TRUE,
                     testExact = TRUE,
                     smd = FALSE,
                     addOverall = FALSE)
  }else if(length(catAct)==0){
    table_one_by_diseases <-
      # data preps
      copy(PC_data) %>%
      { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] } %>%
      #.[,SEX := factor(SEX,levels = c("M","F"),labels = c("Male","Female"))] %>%
      #[,c(auto_antibody_string) := purrr::map(.SD, ~ sapply(str_split(.x,"_"),`[`,1)),.SDcols = auto_antibody_string] %>%
      #[,c(auto_antibody_string) := purrr::map(.SD, ~ factor(.x,levels = c("N","AbN"))),.SDcols = auto_antibody_string] %>%
      # create the table one
      CreateTableOne(data = .,
                     strata = group,
                     vars = num_table_variable,
                     test = TRUE,
                     testExact = TRUE,
                     smd = FALSE,
                     addOverall = FALSE)
  }else{
    table_one_by_diseases <-
      # data preps
      copy(PC_data) %>%
      { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] } %>%
      #.[,SEX := factor(SEX,levels = c("M","F"),labels = c("Male","Female"))] %>%
      #.[,c(auto_antibody_string) := purrr::map(.SD, ~ sapply(str_split(.x,"_"),`[`,1)),.SDcols = auto_antibody_string] %>%
      #.[,c(auto_antibody_string) := purrr::map(.SD, ~ factor(.x,levels = c("N","AbN"))),.SDcols = auto_antibody_string] %>%
      # create the table one
      CreateTableOne(data = .,
                     strata = group,
                     vars = num_table_variable,
                     factorVars = cat_table_variable ,
                     test = TRUE,
                     testExact = TRUE,
                     smd = FALSE,
                     addOverall = FALSE)
  }

  p_value_table <-
    purrr::map(list(table_one_by_diseases$ContTable,
                    table_one_by_diseases$CatTable) %>%
                 compact(.),
               ~ attr(.x,"pValues") %>%
                 as.data.table(.,keep.rownames = TRUE) %>%
                 .[,-3] %>%
                 setnames(.,c("Variable","p value")) %>%
                 .[,"p value" := ifelse(`p value`<0.001,"<0.001",sprintf("%.3f",`p value`))]) %>%
    rbindlist(.)

    if(length(grep(paste0(group,"|Group"),p_value_table$Variable))!=0){
      p_value_table<-p_value_table[-grep(paste0(group,"|Group"),p_value_table$Variable),]
    }

  catAct <-   copy(PC_data) %>%
    { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"),"Group",group)] }

  catAct <-which(!sapply(catAct, is.numeric))

  if(any(numAct)){
    if(length(numAct)!=1){
      table_one_continuous_var<-purrr::map(table_one_by_diseases$ContTable,
                                           ~ .x[,c("n","mean","sd")] %>%
                                             as.data.table(x = .,keep.rownames =T)%>%
                                             setnames(.,".","value",skip_absent=TRUE))%>%
        rbindlist(.,use.names = TRUE,idcol = "Group")%>%
        reshape2::melt(., id.vars = c("Group","rn"))
      names(table_one_continuous_var)<-c("Group","Variable","Statistics","value")

      table_one_continuous_var<-table_one_continuous_var%>% as.data.table(.)%>%
        # retrieve necessary data and modify values for an easy-to-read purpose
        dcast.data.table(., ... ~ Statistics,value.var = "value")%>%
        .[,':=' (n    = comma(n),
                 mean = sprintf("%.1f",mean),
                 sd   = sprintf("%.1f",sd))] %>%
        .[,"mean (sd)" := paste0(sprintf("%3s ",mean),"(",sprintf("%4s",sd),")")] %>%
        # transform dataset for the tabulation purpose
        melt.data.table(.,id.vars = c("Group","Variable"),variable.name = "Statistics",value.name = "value") %>%
        dcast.data.table(., ... ~ Group,value.var = "value") %>%
        .[Statistics %in% "mean (sd)",] %>%
        .[,':=' (Ori_Variable = Variable)]%>%
        merge(.,
              p_value_table,
              by.x = "Variable",
              by.y = "Variable")
    }else{
      table_one_continuous_var <-
        # extract desired statistics and merge them into one data.table by disease group
        purrr::map(table_one_by_diseases_1$ContTable,
                   ~ .x[,c("n","mean","sd")] %>%
                     as.data.table(x = .,keep.rownames = "Statistics") %>%
                     setnames(.,".","value")) %>%
        rbindlist(.,use.names = TRUE,idcol = "Group") %>%
        # retrieve necessary data and modify values for an easy-to-read purpose
        dcast.data.table(., ... ~ Statistics,value.var = "value") %>%
        .[,':=' (n    = comma(n),
                 mean = sprintf("%.1f",mean),
                 sd   = sprintf("%.1f",sd))] %>%
        .[,"mean (sd)" := paste0(sprintf("%3s ",mean),"(",sprintf("%4s",sd),")")] %>%
        # transform dataset for the tabulation purpose
        melt.data.table(.,id.vars = "Group",variable.name = "Statistics",value.name = "value") %>%
        dcast.data.table(., ... ~ Group,value.var = "value") %>%
        .[Statistics %in% "mean (sd)",] %>%
        .[,':=' (Ori_Variable = names(numAct),
                 Variable     = names(numAct))] %>%
        merge(.,
              p_value_table,
              by.x = "Ori_Variable",
              by.y = "Variable")
    }
  }


  if(any(catAct)){
    table_one_categorical_var <-
      # extract desired statistics and merge them into one data.table by disease group
      purrr::map(table_one_by_diseases$CatTable,
                 ~ purrr::map(.x, ~ .x[,c("level","freq","percent")]) %>%
                   rbindlist(.,idcol = "Ori_Variable")) %>%
      rbindlist(.,idcol = "Group") %>%
      # retrieve necessary data and modify values for an easy-to-read purpose
      .[!level %in% "N",] %>%
      .[,':=' (freq    = as.integer(freq) %>% comma(.,accuracy = 1),
               percent = sprintf("%.2f",percent) %>% str_replace_all(.,"0.00","0"))] %>%
      .[,"n (%)" := sprintf("%3s (%s)",freq,percent)] %>%
      .[,Variable := level] %>%
      .[is.na(Variable), Variable := Ori_Variable] %>%
      # transform dataset for the tabulation purpose
      .[,.SD,.SDcols = -c("level")] %>%
      dcast.data.table(.,Variable + Ori_Variable ~ Group, drop = TRUE,value.var = "n (%)") %>%
      .[,"Statistics" := "n (%)"] %>%
      merge(.,
            p_value_table,
            by.x = "Ori_Variable",
            by.y = "Variable")}

  table_lable <-
    copy(PC_data) %>%
    { .[,.SD,.SDcols = -c(str_subset(names(.),"^PC|^Cluster"))] }%>%
    as.data.frame()%>%
    .[,grep("Group",colnames(.))]%>%
    table()%>%as.data.table()

  table_lable<-paste0(table_lable$.,"\n","(n = ",comma(table_lable$N),")")

  if(length(catAct)!=0){
    table_one_cat_template <-
      purrr::map(list(table_one_categorical_var),
                 ~ .x[is.na(Variable),Variable := Ori_Variable]) %>%
      rbindlist(.,use.names = TRUE) %>%
      setkey(Variable) %>%
      .[,.SD,.SDcols = -c("Statistics")] %>%
      setnames(.,c(names(table_one_by_diseases$CatTable)),table_lable)
    table_one_cat_template<-table_one_cat_template%>%.[order(.[,1]),]
    table_one_cat_template[duplicated(table_one_cat_template[,1]),1]<-NA
  }else{
    table_one_cat_template<-data.frame()
  }

  if(length(numAct)!=0){
    table_one_continue_template <-
      purrr::map(list(table_one_continuous_var),
                 ~ .x[is.na(Variable),Variable := Ori_Variable]) %>%
      rbindlist(.,use.names = TRUE) %>%
      setkey(Variable) %>%
      .[,.SD,.SDcols = -c("Statistics")] %>%
      setnames(.,c(names(table_one_by_diseases$ContTable)),table_lable)
    table_one_continue_template<-table_one_continue_template%>%.[order(.[,1]),]
    table_one_continue_template$Variable<-NA
  }else{
    table_one_continue_template<-data.frame()
  }
  # Set output formats
  set_flextable_defaults(font.family = "Calibri",
                         font.size = 10)

  table_one_template<-rbind(table_one_continue_template,table_one_cat_template)
  if(sum(is.na(table_one_template$Ori_Variable))>0){
    table_one_template[is.na(table_one_template$Ori_Variable),length(table_one_template)]<-NA
  }

  # Table one generation


    table_one_flextable <-
      flextable(table_one_template,
                col_keys = c("Ori_Variable","Variable",table_lable,"p value"),
                cwidth =c(1.45,1,1,1),
                theme_fun =
      ) %>%
      align(.,j = c(table_lable,"p value"),align = "right",part = "all") %>%
      autofit(.)

  table_one_template<-setcolorder(x = table_one_template,neworder = c( "Ori_Variable","Variable",table_lable,"p value" ))
  tab_out<-list(PC_data=PC_data,attr_data=attr_data,table_one_flextable =table_one_flextable,use_table=table_one_template)
  return( tab_out)
}
