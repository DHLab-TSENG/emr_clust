
clust<-function(data,
                nclus,
                nstart,
                method,
                distMethod ,
                seed,
                anynum,
                anyfact
){


  if(method=="Kmean"){
    clust_table<-copy(data)%>%
      kmeans(centers=nclus, nstart = nstart)
  }

  if(method=="Kmedoid"){
    clust_table<-copy(data)%>%
      pam(k=nclus, metric = distMethod, stand = T)
  }

  return(clust_table)
}

