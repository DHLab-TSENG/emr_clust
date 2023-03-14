## ---- include = FALSE---------------------------------------------------------

library(TEST)
library(FactoMineR)
library(tidyverse)
library(reshape2)
library(data.table)
library(knitr)
library(mlbench)

## ----setup, echo = FALSE, message = FALSE-------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  error = T
)

options(tibble.print_min = 4L, tibble.print_max = 4L, knitr.kable.NA = '',rmarkdown.html_vignette.check_title = FALSE)


## ---- message = FALSE , warning = TRUE ,include=FALSE-------------------------
data(tea)
emr_iFCB<-emr_clust(tea[,1:13],
                   method = c("iFCB"),
                   kmoCutoff = 0.5,
                   nclus = c(4:6),
                   ndim = c(2:3),
                   group = "Tea",
                   nstart = 50,
                   criterion = "ch",
                   dst = "low")

## ----example, echo=TRUE, warning=FALSE,message = FALSE------------------------
#show the tuning output table
knitr::kable(head(emr_iFCB$clust_out$tune_table),"html")
#show the statistic table
knitr::kable(head(emr_iFCB$statistics_table$PC_data),"html")
knitr::kable(head(emr_iFCB$statistics_table$attr_data),"html")
knitr::kable(head(emr_iFCB$statistics_table$use_table),"html")

## ---- message = F, warning = TRUE,include=FALSE-------------------------------
data(tea)

emr_MCA<-emr_clust(tea[,1:13],
                   method = c("MCA","Kmedoid"),
                   kmoCutoff = 0.5,
                   nclus = c(4:5),
                   ndim = c(2:3),
                   group = "Tea",
                   nstart = 50)

## ---- echo=TRUE, warning=FALSE,message = FALSE--------------------------------
#Biplot of the first and second principal components
emr_iFCB$biplot$clustering_biplots[[1]]
#Profile the number and proportion of case in each cluster
emr_iFCB$biplot$profile_by_cluster_integrated
#Profile the number and proportion of case in each group
emr_iFCB$biplot$profile_by_group_integrated

## ---- echo=TRUE, warning=FALSE,message = FALSE--------------------------------
#Biplot of the first and second principal components
emr_MCA$biplot$clustering_biplots[[1]]
#Profile the number and proportion of case in each cluster
emr_MCA$biplot$profile_by_cluster_integrated
#Profile the number and proportion of case in each group
emr_MCA$biplot$profile_by_group_integrated

## ---- message = TRUE, warning = TRUE,include=FALSE----------------------------

emr_iris<-emr_clust(iris,
                    method = c("FKM"),
                    kmoCutoff = 0.5,
                    nclus = c(4:5),
                    ndim = c(2:3),
                    group = "Species",
                    nstart = 50)

## ---- echo=TRUE, warning=FALSE,message = FALSE--------------------------------
#show the tuning output table
knitr::kable(head(emr_iris$clust_out$tune_table),"html")
#show the statistic table
knitr::kable(head(emr_iris$statistics_table$PC_data),"html")
knitr::kable(head(emr_iris$statistics_table$attr_data),"html")
knitr::kable(head(emr_iris$statistics_table$use_table),"html")

#Biplot of the first and second principal components
emr_iris$biplot$clustering_biplots[[1]]
#Profile the number and proportion of case in each cluster
emr_iris$biplot$profile_by_cluster_integrated
#Profile the number and proportion of case in each group
emr_iris$biplot$profile_by_group_integrated


## ---- message = TRUE, warning = TRUE------------------------------------------
data(PimaIndiansDiabetes)
knitr::kable(head(PimaIndiansDiabetes),"html")

## ---- message = TRUE, warning = TRUE ,include=FALSE---------------------------
emr_diabetes<-emr_clust(PimaIndiansDiabetes,
                    method = c("FKM"),
                    kmoCutoff = 0.5,
                    nclus = c(4:5),
                    ndim = c(2:3),
                    group = "diabetes",
                    nstart = 50,
                    attr.params=list(shape =18, fill="blue", color="darkred"),
                    text.params=list(alpha = 0.9))

## ---- echo=TRUE, warning=FALSE,message = FALSE--------------------------------
#Biplot of the first and second principal components
emr_diabetes$biplot$clustering_biplots[[1]]

## ---- echo=TRUE, warning=FALSE,message = FALSE--------------------------------
#Profile the number and proportion of case in each cluster
emr_diabetes$biplot$profile_by_cluster_integrated + coord_flip()
#Profile the number and proportion of case in each group
emr_diabetes$biplot$profile_by_group_integrated + labs(title = "profile_by_group_integrated")

## ----  warning = FALSE--------------------------------------------------------
#Using combined method iFCB
pic_iFCB <- wind_rose_plot(clustering_output = emr_iFCB , plot_value = "Not" )
pic_iFCB$overlapped
pic_iFCB$seperated

## ----  warning = FALSE--------------------------------------------------------
#using separate method MCA + Kmedoid
pic_MCA<-wind_rose_plot(clustering_output = emr_MCA , plot_value = "Not" )
pic_MCA$overlapped
pic_MCA$seperated


## ---- message = FALSE, warning = FALSE----------------------------------------
#setting a vector with specific feature column names
in_parameters<-names(tea[,1:6])

pic_iFCB<-wind_rose_plot(clustering_output = emr_iFCB , plot_value = "Not" , plot_feature = in_parameters)
pic_iFCB$overlapped
pic_iFCB$seperated


## ---- message = FALSE, warning = FALSE----------------------------------------
sen_spe_iFCB<-sen_spe(clustering_output = emr_iFCB)
sen_spe_iFCB

## ---- message = FALSE, warning = FALSE----------------------------------------
sen_spe_MCA<-sen_spe(clustering_output = emr_MCA)
sen_spe_MCA

## ---- message = FALSE, warning = FALSE----------------------------------------
#setting a vector with specific feature column names
in_parameters<-names(tea[,1:6])

sen_spe_iFCB<-sen_spe(clustering_output = emr_iFCB , plot_feature = in_parameters)
sen_spe_iFCB

