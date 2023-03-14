#' Feature selection + dimention reduction + clustering
#'@import data.table
#'@import dplyr
#'@import magrittr
#'@import purrr
#'@import lubridate
#'@import scales
#'@import tidyr
#'@import cluster
#'@import FactoMineR
#'@import Rtsne
#'@import kohonen
#'@import devtools
#'@import ggplot2
#'@import ggsci
#'@import ggrepel
#'@import ggforce
#'@import cowplot
#'@import tableone
#'@import flextable
#'@import stringr
#'@import clustrd
#'@importFrom psych KMO
#'@importFrom psych cortest.bartlett
#'@importFrom vcd assocstats
#'
#' @param data Dataset with metric variables.
#' @param group to choose data gruup.
#' @param method 	Specifies the method. Combine methods \code{RKM} for reduced K-means, \code{FKM} for factorial K-means, \code{MCAk} for MCA K-means, \code{iFCB} for Iterative Factorial Clustering of Binary variables and \code{clusCA} for Cluster Correspondence Analysis. Dimension reduction methods \code{PCA} for Principal Component Analysis, \code{SVD} for Singular Value Decomposition, \code{TSNE} for t-distributed Stochastic Neighbor Embedding, \code{MDS} for Multidimensional Scaling and \code{MCA} for Multiple correspondence analysis. Clustering methods \code{Kmedoid}, \code{Kmean}).
#' @param nclus Number of clusters .
#' @param ndim Dimensionality of the solution.
#' @param center a logical value indicating whether the variables should be shifted to be zero centered. Alternately, a vector of length equal the number of columns of x can be supplied. The value is passed to \code{scale} (default = TRUE).
#' @param scale a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied (default = TRUE).
#' @param kmoCutoff for kmo test.
#' @param nstart Number of starts (default = 50).
#' @param rotation Specifies the method used to rotate the factors.
#' @param alphak Non-negative scalar to adjust for the relative importance of MCA (alphak = 1) and K-means (alphak = 0) in the solution (default = .5). Works only in combination with method = \code{MCAk}.
#' @param criterion One of \code{asw},\code{ch}, \code{crit}. Determines whether average silhouette width, Calinski-Harabasz index ,criterion value or objective value of the selected method is used (default = \code{asw}).
#' @param dst Specifies the data used to compute the distances between objects. Options are \code{full} for the original data (after possible scaling) and \code{low} for the object scores in the low-dimensional space (default = \code{full}).
#' @param perplexity for dimension reduction method \code{TSNE}; numeric; Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1, see details for interpretation).
#' @param theta for dimension reduction method \code{TSNE}; numeric; Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact TSNE (default: 0.5).
#' @param distMethod the distance measure to be used. This must be one of \code{euclidean},\code{manhattan}. Any unambiguous substring can be given.
#' @param seed An integer that is used as argument by \code{set.seed()} for offsetting the random number generator.
#' @param pc.params to adjust ggplot clustering_biplots.
#' @param attr.params to adjust ggplot clustering_biplots.
#' @param text.params to adjust ggplot clustering_biplots.
#' @param cluster_color_set to adjust ggplot biplot.
#' @param bar.params to adjust ggplot profile_by_cluster_integrated & profile_by_group_integrated.
#' @param choose_group to adjust ggplot only show the parameter want to see.
#'
#' @return clust data
#' @export
#'

#' @examples

#'
emr_clust<- function(data,
                     group,
                     center = TRUE,
                     scale = TRUE,
                     kmoCutoff ,
                     method = c("RKM","FKM","clusCA","iFCB","MCAk","PCA","SVD","TSNE","MDS","MCA","Kmedoid","Kmean"),
                     nclus,#Using gap statistic(fviz_nbclust)
                     ndim,
                     nstart=10,#初始化
                     rotation="none",
                     alphak = 0.5,
                     criterion = c("asw","ch","crit"),
                     dst = c("full","low"),
                     perplexity = 30,#tsne
                     theta = 0.4,#tsne
                     distMethod =c("euclidean","manhattan"),#MDS
                     seed=NULL,
                     pc.params=NULL,
                     attr.params=NULL,
                     text.params=NULL,
                     cluster_color_set=NULL,
                     bar.params=NULL,
                     choose_group=NULL

){
  ori_data<-data

  data<-data%>%as.data.table()%>%.[,.SD,.SDcols = -group]%>%as.data.frame()

  if(all(method%in%c("RKM","FKM","clusCA","iFCB","MCAk"))){
    if(length(method)>1)
      stop("\nYou choose combine methods don't need other methods\n")
  }else if(length(method)==1){
    stop("\nYou should give dimention reduction and clustering methods\n")
  }else if(all(method%in%c("PCA","SVD","TSNE","MDS","SOM","MCA"))){
    stop("\nYou should give clustering method\n")
  }

  if(length(method)>1){
    if(method[2]%in%c("PCA","SVD","TSNE","MDS","SOM","MCA")){
      method<-append(method[2],method[1])
    }
  }
  numvars <- sapply(data, is.numeric)
  anynum <- any(numvars)
  catvars <- sapply(data, is.factor)
  anyfact <- any(catvars)
  numAct <- which(sapply(data, is.numeric))
  facAct <- which(!sapply(data, is.numeric))

  if (anynum&anyfact)
    stop("\nMixed data! Use Mix_emr_clust \n")
  numobs = nrow(data)


  redundant_vars<-copy(data) %>%
    as.data.table()%>%
    .[,purrr::map(.SD, ~ uniqueN(.x))] %>%
    { names(.)[str_which(.,"1")] }


  MSAi_cutoff <- 0.5
  if(identical(redundant_vars, character(0))){
    redundant_iter_vars<-c()
    data<-as.data.table(data)
  }else{
    redundant_iter_vars<-redundant_vars
    data <-  as.data.table(data)
  }

  temp_list<-feature_selection(data,
                               redundant_iter_vars=redundant_iter_vars,
                               kmoCutoff=kmoCutoff,
                               MSAi_cutoff=MSAi_cutoff,
                               anynum=anynum,
                               anyfact=anyfact,
                               numAct =numAct,
                               facAct=facAct,
                               center=center,
                               scale=scale
  )
  correlation_dataset<-temp_list[[1]]
  redundant_iter_vars<-temp_list[[2]]

  Bart_results <-
    cortest.bartlett(correlation_dataset,
                     # n = sqrt(nrow(clustering_analysis_data)))
                     n = nrow(data)/100)



  if (Bart_results$p.value>0.05){
    cat("\nbartlett_results isn't significant \n")
  }


  if(length(redundant_iter_vars)>length(redundant_vars)){
    ready_data <-
      copy(data) %>%
      as.data.table()%>%
      .[,.SD,.SDcols = -c(redundant_iter_vars[grep(FALSE,redundant_iter_vars%in%redundant_vars)])]
  }else{
    ready_data<-data
  }
  out<-dimensio_clust(ready_data,
                      nclus=nclus,
                      ndim=ndim,
                      anyfact=anyfact,
                      anynum=anynum,
                      criterion = criterion,
                      dst = dst,
                      alpha=NULL,
                      method=method,
                      center = center,
                      scale = scale,
                      alphak=alphak,
                      rotation=rotation,
                      nstart=nstart,
                      seed=seed,
                      distMethod = distMethod ,
                      perplexity = perplexity,#tsne
                      theta = theta)

  statistics_table<-flexable_table(data=ori_data,
                                   clust_output=out,
                                   group=group,
                                   choose_group=choose_group)
  biplot<-clust_biplot(PC_data=statistics_table[[1]],
                       attr_data=statistics_table[[2]],
                       pc.params=pc.params,
                       attr.params=attr.params,
                       text.params=text.params,
                       cluster_color_set=cluster_color_set,
                       bar.params=bar.params)
  out<-list(clust_out=out,statistics_table=statistics_table,biplot=biplot)
  return(out)
}


