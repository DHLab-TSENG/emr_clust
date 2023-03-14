#' Feature selection + dimention reduction + clustering
#'
#' These function can handle numerical and categorical data
#'
#'
#' \code{emr_clust} can be used for doing feature selection + dimention reduction + clustering with data Visualization.
#' the data input should be a data frame object in R, and contain at least one cluster coloumn.
#' Also, if inputting categorical dataset make sure it has already been converted into Binary factor datasets.
#'
#'
#' @name emr_clust
#' @param data Dataset with metric variables.
#' @param group to choose data gruup.
#' @param method 	Specifies the method. Combine methods: \code{RKM} for reduced K-means, \code{FKM} for factorial K-means, \code{MCAk} for MCA K-means, \code{iFCB} for Iterative Factorial Clustering of Binary variables and \code{clusCA} for Cluster Correspondence Analysis.Seperated methods: Dimension reduction methods \code{PCA} for Principal Component Analysis, \code{SVD} for Singular Value Decomposition, \code{TSNE} for t-distributed Stochastic Neighbor Embedding, \code{MDS} for Multidimensional Scaling and \code{MCA} for Multiple correspondence analysis. Clustering methods \code{Kmedoid}, \code{Kmean}.
#' @param nclus Number of clusters .
#' @param ndim Dimensionality of the solution.
#' @param center a logical value indicating whether the variables should be shifted to be zero centered. Alternately, a vector of length equal the number of columns of x can be supplied. The value is passed to \code{scale} (default = TRUE).
#' @param scale a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied (default = TRUE).
#' @param kmoCutoff KMO test(Kaiser–Meyer–Olkin test).
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
#' @import data.table
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import lubridate
#' @import scales
#' @import tidyr
#' @import cluster
#' @import FactoMineR
#' @import Rtsne
#' @import kohonen
#' @import devtools
#' @import ggplot2
#' @import ggsci
#' @import ggrepel
#' @import ggforce
#' @import cowplot
#' @import tableone
#' @import flextable
#' @import stringr
#' @import clustrd
#' @importFrom psych KMO
#' @importFrom psych cortest.bartlett
#' @importFrom vcd assocstats
#'
#' @return \code{list}.
#' 1) \code{clust_out}: Simple output data table including tuning output table.
#'
#' 2) \code{statistic_table}: Complete dimention reduction and clustering table with Descriptive statistics table.
#'
#' 3) \code{biplot}: Result presentation for clustering results.Including Biplot of the first and second principal components,the number and proportion of case in each cluster and the number and proportion of case in each group.
#' @examples
#'#for categorical dataset with combine dimention reduction & clustering method example
#' library(FactoMineR)
#' data(tea)
#' # I. Data doing feature selection + dimention reduction + clustering with result presentation for distribution of variables
#' emr_cat<-emr_clust(tea[,1:13],
#'                    method =c("MCAk"),
#'                    kmoCutoff = 0.5,
#'                    nclus=c(4:5),
#'                    ndim=c(2:3),
#'                    group="Tea",
#'                    nstart=50)
#' #show the tuning output table
#' knitr::kable(head(emr_cat$clust_out$tune_table),"pipe")
#' #show the statistic table
#' knitr::kable(head(emr_cat$statistics_table$PC_data),"pipe")
#' knitr::kable(head(emr_cat$statistics_table$attr_data),"pipe")
#' knitr::kable(head(emr_cat$statistics_table$use_table),"pipe")
#' #Biplot of the first and second principal components
#' emr_cat$biplot$clustering_biplots
#' #Profile the number and proportion of case in each cluster
#' emr_cat$biplot$profile_by_cluster_integrated
#' #Profile the number and proportion of case in each group
#' emr_cat$biplot$profile_by_group_integrated
NULL


#' Clustering output Visualization using Wind Rose plot
#'
#'
#' \code{emr_clust} can be used for doing feature selection + dimention reduction + clustering with data Visualization
#'
#'
#' @name wind_rose_plot
#' @import FactoMineR
#' @param clustering_output \code{emr_clust} output
#' @param plot_feature a vector indicating the indexes(colnames) of plot features
#' @param plot_value a value want to choose for plot
#' @param cluster_color_set for ggplot color set
#' @param proportion_score remove \code{plot_feature} in all clusters are less than some percentage (default: 0.7)
#'
#' @return \code{list}. 1) \code{overlapped}: Profile by cluster - overlapped. 2) \code{seperated}: Profile by cluster - separated
#' @examples
#' library(FactoMineR)
#' data(tea)
#'
#' emr_cat<-emr_clust(tea[,1:13],
#'                    method =c("MCAk"),
#'                    kmoCutoff = 0.5,
#'                    nclus=c(4:5),
#'                    ndim=c(2:3),
#'                    group="Tea",
#'                    nstart=50)
#'
#' pic<-wind_rose_plot(clustering_output = emr_cat, plot_value = "Not" )
#' pic$overlapped
#' pic$seperated
#'
#' in_parameters<-names(tea[,1:7])
#' pic<-wind_rose_plot(clustering_output = emr_cat, plot_value = "Not" ,plot_feature = in_parameters)
#' pic$overlapped
#' pic$seperated

NULL

#' Clustering output sensitivity + specificity
#'
#'
#'
#' @name sen_spe
#' @import FactoMineR
#' @import dplyr
#' @import yardstick
#' @param clustering_output \code{emr_clust} output
#' @param cluster_color_set for ggplot color set
#' @param plot_feature a vector indicating the indexes(colnames) of plot features
#'
#' @return \code{plot}. Profile for the sum of sensitivity and specificity.
#' @examples
#' library(FactoMineR)
#' data(tea)
#'
#' emr_cat<-emr_clust(tea[,1:13],
#'                    method =c("MCAk"),
#'                    kmoCutoff = 0.5,
#'                    nclus=c(4:5),
#'                    ndim=c(2:3),
#'                    group="Tea",
#'                    nstart=50)
#'
#' sen_spe_out<-sen_spe(clustering_output = emr_cat )
#' sen_spe_out
#' sen_spe_out$level
#' in_parameters<-names(tea[,1:7])
#' sen_spe_out<-sen_spe(clustering_output = emr_cat, plot_feature = in_parameters)
#' sen_spe_out
NULL
