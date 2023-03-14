
dimention_redu<- function(data,
                          center,
                          scale ,
                          ndim  ,
                          nstart ,#初始化
                          rotation,
                          method,
                          perplexity ,#tsne
                          theta ,#tsne
                          distMethod,
                          seed,
                          anynum,
                          anyfact
){


  if(method=="MCA"){
    dimention_table<-copy(data)%>%
      MCA( ncp = ndim, graph = F)
    dimention_table<-list(dimention_table$ind$cos2%>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) },
                          dimention_table$var$coord%>%
                            as.data.table(keep.rownames = "T")%>%
                            { setnames(.,c("Variable",paste("PC",seq(1,ncol(.)-1),sep = " "))) }
    )
  }

  if(method=="PCA"){
    dimention_table<-copy(data)%>%
      prcomp( scale = scale)
    dimention_table<-list(dimention_table$x[,c(1:ndim)]%>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) },
                          a<-dimention_table[["rotation"]]%>%
                            as.data.table(keep.rownames = "T")%>%
                            { setnames(.,c("Variable",paste("PC",seq(1,ncol(.)-1),sep = " "))) }

    )
  }

  if(method=="SVD"){
    dimention_table<-copy(data)%>%
      scale(center = center, scale = scale)%>%
      svd()
    dimention_table<-list(dimention_table$u[,c(1:ndim)]%>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) },
                          dimention_table$v %>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) }
    )
    row.names(dimention_table[[1]])<-row.names(data)
    dimention_table[[2]]<-dimention_table[[2]]%>%as.data.table()%>% .[,Variable := colnames(data)]%>%as.data.frame%>%.[,c(length(.),1:(length(.)-1))]
  }


  if(method=="TSNE"){
    dimention_table<-copy(data)%>%
      as.matrix()%>%
      Rtsne(pca = TRUE,
            perplexity=perplexity, theta=theta, dims=ndim,check_duplicates=F)
    dimention_table<-list(dimention_table$Y%>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) })
    row.names(dimention_table[[1]])<-row.names(data)
  }

  if(method=="MDS"){
    dimention_table<-copy(data)%>%
      dist(method=distMethod)%>%
      cmdscale(eig=T, k=ndim)
    dimention_table<-list(dimention_table$points%>%
                            as.data.table()%>%
                            { setnames(.,c(paste("PC",seq(1,ncol(.)),sep = " "))) })
    row.names(dimention_table[[1]])<-row.names(data)
  }

  dimention_table

}
