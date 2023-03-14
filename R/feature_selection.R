
feature_selection<-function(data,
                            redundant_iter_vars,
                            kmoCutoff,
                            MSAi_cutoff,
                            anynum,
                            anyfact,
                            numAct ,
                            facAct,
                            center,
                            scale
){

  if(anynum){
    repeat {
      # Construct the correlation coefficient matrix (remove redundant variables in advance)person
      correlation_dataset <-
        if (length(redundant_iter_vars)>0) {
          as.matrix(data[, numAct , drop = FALSE, with=FALSE]) %>%
            scale(center=center,scale=scale)%>%
            as.data.table()%>%
            .[,.SD,.SDcols = -(redundant_iter_vars)] %>%
            cor(method = "pearson")
        } else {
          as.matrix(data[, numAct , drop = FALSE,with=FALSE]) %>%
            scale(center=center,scale=scale)%>%
            as.data.table()%>%
            cor(method = "pearson")
        }

      colnames(correlation_dataset)<-row.names(correlation_dataset)
      # KMO test
      KMO_iter_results  <- KMO(correlation_dataset)

      # iterate till overall MSA is over the specified cutoff value
      if (KMO_iter_results$MSA>=kmoCutoff) break

      if (all(KMO_iter_results$MSAi <= MSAi_cutoff))
        stop("\nkmoCutoff is too high. There's no parameter lower than MSAi_cutoff\n")

      # Find the variable with the smallest MSAi value to remove
      min_MSAi_var <- KMO_iter_results$MSAi[KMO_iter_results$MSAi <= MSAi_cutoff] %>%
        which.min(.) %>%
        names(.)

      redundant_iter_vars <- append(redundant_iter_vars,min_MSAi_var)
    }
  }else{#anyfact=T
    repeat {
      # Construct the correlation coefficient matrix (remove redundant variables in advance)
      correlation_dataset <-
        if (length(redundant_iter_vars)>0) {
          copy(data[, facAct , drop = FALSE,with=FALSE]) %>%
            as.data.table()%>%
            .[,.SD,.SDcols = -(redundant_iter_vars)] %>%
            cramerV_matrix(dataset = .,
                           vars = colnames(.))
        } else {
          copy(data[, facAct , drop = FALSE,with=FALSE]) %>%
            as.data.table()%>%
            .[,.SD,.SDcols = c(1:length(.))] %>%
            cramerV_matrix(dataset = .,
                           vars = colnames(.))
        }
      colnames(correlation_dataset)<-row.names(correlation_dataset)
      # KMO test
      KMO_iter_results  <- KMO(correlation_dataset)


      # iterate till overall MSA is over the specified cutoff value
      if (KMO_iter_results$MSA>=kmoCutoff) break

      if (all(KMO_iter_results$MSAi >= MSAi_cutoff))
        stop("\nkmoCutoff is too high. There's no parameter lower than MSAi_cutoff\n")

      # Find the variable with the smallest MSAi value to remove
      min_MSAi_var <- KMO_iter_results$MSAi[KMO_iter_results$MSAi <= MSAi_cutoff] %>%
        which.min(.) %>%
        names(.)
      redundant_iter_vars <- append(redundant_iter_vars,min_MSAi_var)
    }
  }
  return(list(correlation_dataset,redundant_iter_vars))
}
