dimensio_clust<-function(ready_data,
                         method,
                         anynum,
                         anyfact,
                         nclus,
                         ndim,
                         criterion,
                         dst,
                         alpha,
                         center,
                         scale,
                         alphak,
                         rotation,
                         nstart,
                         seed,
                         distMethod,
                         perplexity,#tsne
                         theta){
  seed_vector   <- 365 # for MAC OS: 11, 83, 365, 3155; on Linux OS, use 20211111 instead.
  result_list_name <- paste("seed =",seed_vector)
  if (length(nclus)>1){
    if(length(method)==1){
      if(anynum){
        temp_list<-tuneclus(ready_data,nclus=nclus,ndim=ndim,criterion = criterion, dst = dst,alpha=alpha,method=method, center = center, scale = scale,alphak=alphak, rotation=rotation, nstart=nstart,  seed=seed)
        out<-list(pluck(temp_list,"clusobjbest") %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"obscoord"),
                            pluck(.,"cluster") ) },
                  pluck(temp_list,"clusobjbest") %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"attcoord")) },
                  pluck(temp_list,"critgrid"))
        names(out)<-c("PC_Clust","attr","tune_table")
        names(out$tune_table)<-gsub("X","Dim",names(out$tune_table))
      }else{
        temp_list<-tuneclus(ready_data,nclus=nclus,ndim=ndim,method=method,criterion = criterion, dst = dst,nstart=nstart,alphak=alphak,seed=seed)
        out<-list(pluck(temp_list,"clusobjbest") %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"obscoord"),
                            pluck(.,"cluster"))},
                  pluck(temp_list,"clusobjbest") %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"attcoord")) },
                  pluck(temp_list,"critgrid"))
        names(out)<-c("PC_Clust","attr","tune_table")
        names(out$tune_table)<-gsub("X","Dim",names(out$tune_table))
      }
    }else{
      si_num<-0
      tune_table<-data.frame()
      row=0
      for(cl in 1:length(nclus)){
        row=row+1
        col=0
        for (di in  1:length(ndim)){
          col=col+1
          if(nclus[cl]>ndim[di]){
            dimention_temp<-dimention_redu(ready_data,
                                           center = center,
                                           scale = scale,
                                           ndim = ndim[di] ,
                                           nstart = nstart,#初始化
                                           rotation=rotation,
                                           method=method[1],
                                           perplexity = perplexity,#tsne
                                           theta = theta,#tsne
                                           distMethod = distMethod,
                                           seed=seed,
                                           anynum=anynum,
                                           anyfact=anyfact)
            clust_temp<-clust(dimention_temp[[1]],
                              nclus=nclus[cl],
                              nstart=nstart,
                              method=method[2],
                              distMethod = distMethod ,
                              seed=seed,
                              anynum=anynum,
                              anyfact=anyfact)
            si<-silhouette(clust_temp$cluster, dist(dimention_temp[[1]]))
            tune_table[row,col]<-mean(si[, 3])
            row.names(tune_table)[row]<-nclus[cl]
            if(mean(si[, 3])>si_num){
              si_num<-mean(si[, 3])
              need_cl<-nclus[cl]
              need_dim<-ndim[di]
            }
          }
          colnames(tune_table)[col]<-paste0("Dim",ndim[di])
        }
      }
      dimention_temp<-dimention_redu(ready_data,
                                     center = center,
                                     scale = scale,
                                     ndim = need_dim ,
                                     nstart = nstart,#初始化
                                     rotation=rotation,
                                     method=method[1],
                                     perplexity = perplexity,#tsne
                                     theta = theta,#tsne
                                     distMethod = distMethod,
                                     seed=seed,
                                     anynum=anynum,
                                     anyfact=anyfact)
      clust_temp<-clust(dimention_temp[[1]],
                        nclus=need_cl,
                        nstart=nstart,
                        method=method[2],
                        distMethod =  distMethod ,
                        seed=seed,
                        anynum=anynum,
                        anyfact=anyfact)
      dimention_temp[[1]]<-cbind(dimention_temp[[1]],clust_temp$cluster)
      dimention_temp[[3]]<-tune_table
      names(dimention_temp)<-c("PC_Clust","attr","tune_table")
      out<-dimention_temp
    }
  }else{
    if(length(method)==1){
      if(anynum){
        temp_list<-cluspca(ready_data,nclus=nclus,ndim=ndim,alpha=NULL,method=method, center = center, scale = scale, rotation=rotation, nstart=nstart, smartStart=NULL, seed=seed)
        out<-list(temp_list %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"obscoord"),
                            pluck(.,"cluster") ) },
                  temp_list %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"attcoord")) })
        names(out)<-c("PC_Clust","attr")
      }else{
        temp_list<-clusmca(ready_data,nclus=nclus,ndim=ndim,method=method,alphak = alphak,nstart=nstart,smartStart=NULL,gamma = TRUE,inboot=FALSE,seed=seed)
        out<-list(temp_list %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"obscoord"),
                            pluck(.,"cluster") ) },
                  temp_list %>%
                    # Retrieve PC coordinate and cluster data
                    { cbind(pluck(.,"attcoord")) })
        names(out)<-c("PC_Clust","attr")
      }
    }else{
      dimention_temp<-dimention_redu(ready_data,
                                     center = center,
                                     scale = scale,
                                     ndim = ndim ,
                                     nstart = nstart,#初始化
                                     rotation=rotation,
                                     method=method[1],
                                     perplexity = perplexity,#tsne
                                     theta = theta,#tsne
                                     distMethod = distMethod,
                                     seed=seed,
                                     anynum=anynum,
                                     anyfact=anyfact)
      clust_temp<-clust(dimention_temp[[1]],
                        nclus=nclus,
                        nstart=nstart,
                        method=method[2],
                        distMethod =  distMethod ,
                        seed=NULL,
                        anynum=anynum,
                        anyfact=anyfact)
      dimention_temp[[1]]<-cbind(dimention_temp[[1]],clust_temp$clustering)
      out<-dimention_temp
      names(out)<-c("PC_Clust","attr")
    }
  }
return(out)
}
