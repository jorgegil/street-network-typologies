# Statistical analysis of street network centrality results
# cities typologies study
# authors: Jorge Gil, Miguel Serra


##
#### import required libraries ####
##
# data manipulation functions, including joins
library("dplyr")
library("memisc")
# for plotting data
library("ggplot2")
# for plotting good quality images
library("Cairo")
# restructure data
library("reshape2")
# for descriptive statistics
library("psych")
# Excel read/write
library("xlsx")
# for clustering
library("cluster")
library("fpc")
# frequencies
library("gmodels")
# mosaic plots
library("vcd")


##
##### 0. useful functions ####
##
# scale given object's values between 0 and 1
scale01 <- function(x, na = TRUE){(x-min(x, na.rm=na))/(max(x, na.rm=na)-min(x, na.rm=na))}
# calculate outlier limits based on iqr
outliersIQR <- function(x){
    q <- quantile(x, probs = c(25,75)/100, type=8)
    iqr <- q[2]-q[1]
    low <- q[1]-(1.5*iqr)
    high <- q[2]+(1.5*iqr)
    return (c(low=low, high=high))
}

##
#### 0. data setup ####
##
# import Amsterdam data
city_data <- list()
city_data$ams <- read.csv(paste0(getwd(),"/gis/ams_results.csv"))
# import Gothenburg data
city_data$got <- read.csv(paste0(getwd(),"/gis/got_results.csv"))
# import Stockholm data
city_data$sto <- read.csv(paste0(getwd(),"/gis/sto_results.csv"))
# import London data
city_data$lon <- read.csv(paste0(getwd(),"/gis/lon_results.csv"))
# study cities names for reuse in automation
cities <- c('Amsterdam','Gothenburg','Stockholm','London')
cities_short <- c('ams','got','sto','lon')
results_cols <- list(    
    "betweenness" = colnames(city_data$ams[, grepl( "NBe_walk", names(city_data$ams))])
)


##
#### 1. descriptive statistics ####
##

## 1.1 raw and scaled data ####
city_stats <- list()
for (i in cities_short){
    city_stats[[i]]$stats <- describe(city_data[[i]][,results_cols$"betweenness"],IQR=TRUE,fast=FALSE)
}
# scale data and calculate z-scores
city_data_scaled <- list()
city_data_zscore <- list()
for (i in cities_short){
    city_data_scaled[[i]] <- data.frame(apply(city_data[[i]][,results_cols$"betweenness"], 2, scale01))
    city_data_zscore[[i]] <- data.frame(apply(city_data[[i]][,results_cols$"betweenness"], 2, scale, center = TRUE, scale = TRUE))
}
for (i in cities_short){
    city_stats[[i]]$scaled <- describe(city_data_scaled[[i]][,results_cols$"betweenness"],IQR=TRUE,fast=FALSE)
    city_stats[[i]]$zscore <- describe(city_data_zscore[[i]][,results_cols$"betweenness"],IQR=TRUE,fast=FALSE)
}

## 1.2 log data dropped ####

## 1.3 save statistics to excel ####
for (i in c(1:length(cities))){
    write.xlsx2(city_stats[[cities_short[i]]]$stats, paste0(getwd(),"/tables/descriptive_stats_by_city.xlsx"),
                sheetName=cities[i], append=TRUE)
    write.xlsx2(city_stats[[cities_short[i]]]$scaled, paste0(getwd(),"/tables/descriptive_stats_by_city.xlsx"),
                sheetName=paste0(cities[i],'_scaled'), append=TRUE)
    write.xlsx2(city_stats[[cities_short[i]]]$zscore, paste0(getwd(),"/tables/descriptive_stats_by_city.xlsx"),
                sheetName=paste0(cities[i],'_zscore'), append=TRUE)
}

## 1.4 plot the different scaled stats ####
## comparative line plot function
comparative_lineplot <- function(data, save_path, width=250, height=180,
                                 nrow=NULL, ncol=NULL, scales="free_y",
                                 title="Betweenness descriptive statistics"){
    line_plot <- ggplot(data=data, aes(x=Var1, y=value, group=city, colour=city)) +
        geom_line() +
        scale_color_manual(values=c(
            rgb(230,97,1,max = 255),
            rgb(178,171,210,max = 255),
            rgb(253,184,99,max = 255),
            rgb(94,60,153,max = 255))) +
        facet_wrap(~Var2, nrow=nrow, ncol=ncol, scales=scales) +
        theme(
            title = element_text(size=10, face = "bold"),
            axis.text = element_text(size=7),
            axis.text.x = element_text(angle = 90, hjust = 1),
            strip.background = element_rect(fill=NA, color=NA),
            strip.text = element_text(size=10),
            legend.text = element_text(size=9)
        ) +
        labs(x="Radius (m)", y="", title=title)
    ggsave(line_plot,filename=paste0(getwd(),save_path),width=width,height=height,units='mm',dpi=300)
}
# plot the scaled stats
stats_long <- melt(as.matrix(city_stats$ams$scaled))
stats_long$city <- "Amsterdam"
plot_data <- subset(stats_long, 
            stats_long$Var1 %in% c(results_cols$betweenness) & 
            !(stats_long$Var2 %in% c('vars','n','min','max','range')))
for (i in c(2:length(cities))){
    stats_long <- melt(as.matrix(city_stats[[cities_short[i]]]$scaled))
    stats_long$city <- cities[i]
    plot_data <- rbind(plot_data, subset(stats_long, 
            stats_long$Var1 %in% c(results_cols$betweenness) & 
            !(stats_long$Var2 %in% c('vars','n','min','max','range'))))
}
comparative_lineplot(plot_data,"/images/charts/1_4_scaled_stats.png")
# plot the zscore data
stats_long <- melt(as.matrix(city_stats$ams$zscore[,c('median','mad','skew','min','max','IQR')]))
stats_long$city <- "Amsterdam"
plot_data <- subset(stats_long,stats_long$Var1 %in% c(results_cols$betweenness))
for (i in c(2:length(cities))){
    stats_long <- melt(as.matrix(city_stats[[cities_short[i]]]$
                                     zscore[,c('median','mad','skew','min','max','IQR')]))
    stats_long$city <- cities[i]
    plot_data <- rbind(plot_data, subset(stats_long, 
            stats_long$Var1 %in% c(results_cols$betweenness)))
}
# rename var1 for x axis
mylevels <- sub("NBe_walk_", "", results_cols$betweenness)
plot_data$Var1 <- factor(sub("NBe_walk_", "", plot_data$Var1), levels = mylevels)
comparative_lineplot(plot_data,"/images/charts/1_4_zscore_stats.png",width=250, height=130)
# clean up 
rm(stats_long)

## 1.5 export zscore results for mapping ####
for (i in cities_short){
    write.csv(data.frame(city=i, ID=city_data[[i]]$ID, city_data_scaled[[i]]), paste0(getwd(),"/gis/",i,"_scaled.csv"))
    write.csv(data.frame(city=i, ID=city_data[[i]]$ID, city_data_zscore[[i]]), paste0(getwd(),"/gis/",i,"_zscore.csv"))
}


##
#### 2. PCA, following Serra 2013 ####
##

## 2.1 making some tests with cor and cov ####
# PCA for zscore, log zscore, and the same with covariance matrix
pca_tests <- list()
for (i in cities_short){
    pca_tests[[i]]$data <- principal(cor(city_data[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=FALSE)
    pca_tests[[i]]$scaled <- principal(cor(city_data_scaled[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=FALSE)
    pca_tests[[i]]$zscore <- principal(cor(city_data_zscore[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=FALSE)
    pca_tests[[i]]$log_zscore <- principal(cor(city_data_log_zscore[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=FALSE)
    pca_tests[[i]]$zscore_cov <- principal(cov(city_data_zscore[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=TRUE)
    pca_tests[[i]]$log_zscore_cov <- principal(cov(city_data_log_zscore[[i]][,results_cols$betweenness]), 
        nfactors = length(results_cols$betweenness), rotate="none", scores=FALSE, covar=TRUE)
}

## 2.2 plot PCA scree plots ####
## scree plot function
pca_scree_plot <- function(data, title, save_path, width=200, height=100){
    scree_bet_plot <- ggplot(data=data, aes(x=factor(Var1), y=value, group=Var2, colour=Var2)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept=1) +
        labs(x="Components", y="Eigen value", title=title, colour = "City")
    ggsave(scree_bet_plot,filename=paste0(getwd(),save_path),width=width,height=height,units='mm',dpi=300)
}
# make scree plot for each set of PCA results
# raw
plot_data <- data.frame(pca_tests$ams$data$values)
colnames(plot_data) <- 'Amsterdam'
for (i in c(2:length(cities))){
    plot_data[,cities[i]] <- pca_tests[[cities_short[i]]]$data$values
}
plot_data_long <- melt(as.matrix(plot_data))
pca_scree_plot(plot_data_long,"data PCA scree plot","/images/charts/2_2_data_PCA_scree_plot.png")
# scaled
plot_data <- data.frame(pca_tests$ams$scaled$values)
colnames(plot_data) <- 'Amsterdam'
for (i in c(2:length(cities))){
    plot_data[,cities[i]] <- pca_tests[[cities_short[i]]]$scaled$values
}
plot_data_long <- melt(as.matrix(plot_data))
pca_scree_plot(plot_data_long,"scaled PCA scree plot","/images/charts/2_2_scaled_PCA_scree_plot.png")
# zscore
plot_data <- data.frame(pca_tests$ams$zscore$values)
colnames(plot_data) <- 'Amsterdam'
for (i in c(2:length(cities))){
    plot_data[,cities[i]] <- pca_tests[[cities_short[i]]]$zscore$values
}
plot_data_long <- melt(as.matrix(plot_data))
pca_scree_plot(plot_data_long,"zscore PCA scree plot","/images/charts/2_2_zscore_PCA_scree_plot.png")

## 2.3 calculating PCA with 3 rotated components ####
# PCA scores
city_pca <- list()
for (i in cities_short){
    city_pca[[i]] <- principal(city_data_zscore[[i]][,results_cols$betweenness],
            nfactors = 3, rotate="varimax",
            scores=TRUE,normalize=FALSE,eps=1e-14)
}
# chi statistic value didn't converge warning
# betweenness PCA variance
city_pca$ams$loadings
pca_variance <- data.frame("City"="Amsterdam", "PC1"=0.519, "PC2"= 0.243, "PC3"= 0.178, "Total"= 0.940)
city_pca$got$loadings
pca_variance <- rbind(pca_variance,data.frame("City"="Gothenburg", "PC1"=0.486, "PC2"= 0.256, "PC3"= 0.219, "Total"= 0.961))
city_pca$lon$loadings
pca_variance <- rbind(pca_variance,data.frame("City"="London", "PC1"=0.482, "PC2"= 0.274, "PC3"= 0.213, "Total"= 0.969))
city_pca$sto$loadings
pca_variance <- rbind(pca_variance,data.frame("City"="Stockholm", "PC1"=0.432, "PC2"= 0.272, "PC3"= 0.269, "Total"= 0.973))
write.csv(pca_variance, paste0(getwd(),"/tables/2_3_pca_variance.csv"))

## 2.4 plotting the 3 RC (rotated components) loadings ####
plot_data <- melt(as.matrix(city_pca$ams$loadings[,]))
plot_data$city <- "Amsterdam"
for (i in c(2:length(cities))){
    pca_long <- melt(as.matrix(city_pca[[cities_short[i]]]$loadings[,]))
    pca_long$city <- cities[i]
    plot_data <- rbind(plot_data,pca_long)
}
# rename var1 for x axis
plot_data$Var1 <- factor(sub("NBe_walk_", "", plot_data$Var1), levels = mylevels)
comparative_lineplot(plot_data,"/images/charts/2_4_RC_loadings.png", 
                     nrow=1, ncol=3, scales="fixed", width=300, height=100,
                     title="Betweenness PCA Rotated Components loadings")
# clean up
rm(pca_long)

## 2.5 write pca scores for mapping ####
for (i in cities_short){
    pca_scores <- data.frame(id=city_data[[i]]$ID,city_pca[[i]]$scores)
    write.csv(pca_scores, paste0(getwd(),"/gis/",i,"_pca.csv"))
}
# clean up
rm(pca_scores)

##
#### 3. Clustering ####
##

## 3.0 combine scores into one data set ####
combined_pca_scores <- data.frame(city="ams", ID=city_data$ams$ID, city_pca$ams$scores)
combined_pca_scores <- rbind(combined_pca_scores, data.frame(city="got", ID=city_data$got$ID, city_pca$got$scores))
combined_pca_scores <- rbind(combined_pca_scores, data.frame(city="lon", ID=city_data$lon$ID, city_pca$lon$scores))
combined_pca_scores <- rbind(combined_pca_scores, data.frame(city="sto", ID=city_data$sto$ID, city_pca$sto$scores))
# identify dead ends (zeros)
city_zeros <- list()
for (i in cities_short){
    city_zeros[[i]] <- city_data[[i]][rowSums(abs(city_data[[i]][,results_cols$"betweenness"]))==0,1]
    write.csv(city_zeros[[i]], paste0(getwd(),"/gis/",i,"_zeros.csv"))
}
# combined scores without segments with zero result
temp_data <- data.frame(city="ams", ID=city_data$ams$ID, city_pca$ams$scores)
nozero_pca_scores <- temp_data[!(temp_data$ID %in% city_zeros$ams),]
for (i in c(2:length(cities))){
    temp_data <- data.frame(city=cities_short[i], 
            ID=city_data[[cities_short[i]]]$ID, 
            city_pca[[cities_short[i]]]$scores)
    nozero_pca_scores <- rbind(nozero_pca_scores, 
            temp_data[!(temp_data$ID %in% city_zeros[[cities_short[i]]]),])
}

## 3.1 identify outliers ####
# calculate limits
outlier_limits <- list()
outlier_limits$RC1 <- outliersIQR(combined_pca_scores$RC1)
outlier_limits$RC2 <- outliersIQR(combined_pca_scores$RC2)
outlier_limits$RC3 <- outliersIQR(combined_pca_scores$RC3)
# identify combined outliers
combined_outliers <- list()
combined_outliers$RC1_high <- subset(combined_pca_scores, RC1 > outlier_limits$RC1[2])
combined_outliers$RC1_low <- subset(combined_pca_scores, RC1 < outlier_limits$RC1[1])
combined_outliers$RC2_high <- subset(combined_pca_scores, RC2 > outlier_limits$RC2[2])
combined_outliers$RC2_low <- subset(combined_pca_scores, RC2 < outlier_limits$RC2[1])
combined_outliers$RC3_high <- subset(combined_pca_scores, RC3 > outlier_limits$RC3[2])
combined_outliers$RC3_low <- subset(combined_pca_scores, RC3 < outlier_limits$RC3[1])

## 3.2 write outlier ids for mapping - dropped ####

## 3.3 create a core data set without outliers - dropped ####

## 3.4 hierarchical clustering and dendogram of 10% sample ####
set.seed(123)
combined_sample_scores <- sample(combined_pca_scores, 25000)
nozero_sample_scores <- sample(nozero_pca_scores, 25000)
# for the full
pca_hc <- hclust(dist(combined_sample_scores[,3:5]),method = "ward.D")
plot.new()
png(paste0(getwd(),"/images/charts/3_4_combined_full_dendrogram.png"), width = 300,height = 200, units = "mm", res = 300)
plot(pca_hc, main = "Cities clustering full PCA scores", xlab = "sample segments", sub = "", hang = -1)
dev.off()
# for the no zeros
pca_hc <- hclust(dist(nozero_sample_scores[,3:5]),method = "ward.D")
plot.new()
png(paste0(getwd(),"/images/charts/3_4_combined_nozero_dendrogram.png"), width = 300,height = 200, units = "mm", res = 300)
plot(pca_hc, main = "Cities clustering no zeros PCA scores", xlab = "sample segments", sub = "", hang = -1)
dev.off()
# clean up
rm(pca_hc)

## 3.5 scree plot and silhouete plot ####
scree_plot <- function(data, title, save_path, width=200,height=100){
    data_long <- melt(as.matrix(data))
    data_plot <- ggplot(data=data_long, aes(x=factor(Var1), y=value, group = 1)) +
        geom_line() + geom_point() +
        labs(x="Number of Clusters", y="Within groups sum of squares", 
        title=paste0(title," clusters scree plots"))
    ggsave(data_plot,filename=paste0(getwd(),save_path),width=width,height=height,units='mm',dpi=300)
}
# plot scree plots 
cluster_res <- c(1:25)
for (i in 1:25) cluster_res[i] <- sum(kmeans(combined_pca_scores[,c(3:5)], centers=i)$withinss)
scree_plot(cluster_res,"Combined full","/images/charts/3_5_combined_full_scree_plot.png")
for (i in 1:25) cluster_res[i] <- sum(kmeans(nozero_pca_scores[,c(3:5)], centers=i)$withinss)
scree_plot(cluster_res, "Combined no zero","/images/charts/3_5_combined_nozero_scree_plot.png")
# test k number using clara and the silhouete width
cluster_res <- c(0:24)
for (i in 2:25) cluster_res[i] <- 
    clara(combined_sample_scores[,c(3:5)], i, metric = "euclidean", medoids.x = FALSE, 
          keep.data = FALSE, stand = FALSE, samples = 1000, rngR = TRUE)$silinfo$avg.width
sil_cluster_data <- data.frame('Combined full'=cluster_res)
combined_full_best <- which.max(cluster_res)
for (i in 2:25) cluster_res[i] <- 
    clara(nozero_sample_scores[,c(3:5)], i, metric = "euclidean", medoids.x = FALSE, 
          keep.data = FALSE, stand = FALSE, samples = 1000, rngR = TRUE)$silinfo$avg.width
sil_cluster_data$'Combined nozero' <- cluster_res
combined_nozero_best <- which.max(cluster_res)
# make plot
data_long <- melt(as.matrix(sil_cluster_data))
sil_cluster_plot <- ggplot(data=data_long, aes(x=factor(Var1), y=value, group=Var2, colour=Var2)) +
    geom_line() +
    geom_point() +
    labs(x="Number of Clusters", y="Average silhouette width", title="Combined cities clusters silhouette plot")
ggsave(sil_cluster_plot,filename=paste0(getwd(),"/images/charts/3_5_combined_clusters_silhouette_plot.png"),width=200,height=100,units='mm',dpi=300)

## 3.6 calculating other cluster indices ####
# unfortunately this only works for small data sets
# calculating the full distance matrix d1 is too expensive in terms of memory

## 3.7 calculate cluster solutions ####
# combined full: getting 3, 4, 5, 6, 7, 8, 9 clusters
set.seed(123)
combined_clusters <- list()
for (i in c(3:9)){
    combined_clusters[[paste0('k',i)]] <- clara(combined_pca_scores[,c(3:5)], i, metric = "euclidean", stand = FALSE, samples = 10000, rngR = TRUE)
}
# combined nozero: getting 4, 7, 8, 9, 10 clusters
set.seed(123)
nozero_clusters <- list()
for (i in c(4, 7, 8, 9, 10)){
    nozero_clusters[[paste0('k',i)]] <- clara(nozero_pca_scores[,c(3:5)], i, metric = "euclidean", stand = FALSE, samples = 10000, rngR = TRUE)
}
# get clusters info
clusters_info <- list()
for (i in names(combined_clusters)){
    clusters_info$combined[[i]] <- combined_clusters[[i]]$clusinfo[,1]
}
for (i in names(nozero_clusters)){
    clusters_info$nozero[[i]] <- nozero_clusters[[i]]$clusinfo[,1]
}
# extract cluster ids
for (i in names(combined_clusters)){
    combined_pca_scores[[i]] <- combined_clusters[[i]]$clustering
}
write.csv(combined_pca_scores, paste0(getwd(),"/gis/combined_clusters.csv"))
for (i in names(nozero_clusters)){
    nozero_pca_scores[[i]] <- nozero_clusters[[i]]$clustering
}
write.csv(nozero_pca_scores, paste0(getwd(),"/gis/nozero_clusters.csv"))

## 3.8 merge zscore and cluster solutions ####
# combined centrality data
combined_data_zscore <- data.frame(city="ams", ID=city_data$ams$ID, city_data_zscore$ams)
combined_data_zscore <- rbind(combined_data_zscore, data.frame(city="got", ID=city_data$got$ID, city_data_zscore$got))
combined_data_zscore <- rbind(combined_data_zscore, data.frame(city="lon", ID=city_data$lon$ID, city_data_zscore$lon))
combined_data_zscore <- rbind(combined_data_zscore, data.frame(city="sto", ID=city_data$sto$ID, city_data_zscore$sto))
# merge cluster solutions
for (i in names(combined_clusters)){
    combined_data_zscore[[i]] <- combined_clusters[[i]]$clustering
}
# do the same for nozero data
temp_data <- data.frame(city="ams", ID=city_data$ams$ID, city_data_zscore$ams)
nozero_data_zscore <- temp_data[!(temp_data$ID %in% city_zeros$ams),]
for (i in c(2:length(cities))){
    temp_data <- data.frame(city=cities_short[i], 
                            ID=city_data[[cities_short[i]]]$ID, 
                            city_data_zscore[[cities_short[i]]])
    nozero_data_zscore <- rbind(nozero_data_zscore, 
                                 temp_data[!(temp_data$ID %in% city_zeros[[cities_short[i]]]),])
}
# merge cluster solutions
for (i in names(nozero_clusters)){
    nozero_data_zscore[[i]] <- nozero_clusters[[i]]$clustering
}

## 3.9 plot cluster solutions ####
## cluster line plots
cluster_line_plots <- function(x,label,mylevels){
    cluster_data_long <- melt(as.matrix(x[,c(2:ncol(x))]))
    cluster_data_long$Var2 <- factor(sub("NBe_walk_", "",
                cluster_data_long$Var2), levels = mylevels)
        cluster_profiles <- ggplot(data=cluster_data_long, aes(x=Var2, y=value, group=1)) +
        geom_line() +
        facet_wrap(~Var1) +
        theme(
            title = element_text(size=14, face = "bold"),
            axis.text = element_text(size=12),
            axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text.x = element_text(size=14, face="bold"),
            legend.text = element_text(size=14)
        ) +
        labs(x="Radius (m)", y=paste0("mean values (zscore)"), 
             title=paste0(label," clusters centrality profile"))
    ggsave(cluster_profiles,filename=paste0(getwd(),
            "/images/charts/3_8_cluster_",label,"_profiles.png"),
           width=250,height=200,units='mm',dpi=300)
}
## cluster box-plots
cluster_box_plots <- function(x,label,mylevels){
    cluster_data_long <- melt(x, id = c(1))
    cluster_data_long$variable <- factor(sub("NBe_walk_", "",
                cluster_data_long$variable), levels = mylevels)
    cluster_profiles <- ggplot(data=cluster_data_long, aes(x=variable, y=value)) +
        geom_boxplot(outlier.size=1) + #, aes(fill = cluster)) +
        facet_wrap(~cluster) +
        theme(
            title = element_text(size=14, face = "bold"),
            axis.text = element_text(size=12),
            axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text.x = element_text(size=14, face="bold"),
            legend.text = element_text(size=14)
        ) +
        labs(x="Radius (m)", y=paste0("mean values (zscore)"), 
             title=paste0(label," clusters centrality profile"))
    ggsave(cluster_profiles,filename=paste0(getwd(),
            "/images/charts/3_8_cluster_",label,"_boxplot_profiles.png"),
           width=400,height=300,units='mm',dpi=300)
}
# make line plots
for (i in names(combined_clusters)){
    x <- aggregate(combined_data_zscore[,c(results_cols$betweenness)],
                   by=list(combined_data_zscore[[i]]), FUN=mean, na.rm=TRUE)
    cluster_line_plots(x,paste0('combined_',i),mylevels)
}
for (i in names(nozero_clusters)){
    x <- aggregate(nozero_data_zscore[,c(results_cols$betweenness)],
                   by=list(nozero_data_zscore[[i]]), FUN=mean, na.rm=TRUE)
    cluster_line_plots(x,paste0('nozero_',i),mylevels)
}
# make box plots
for (i in names(combined_clusters)){
    x <- combined_data_zscore[,c(i,results_cols$betweenness)]
    colnames(x)[1] <- 'cluster'
    x$cluster <- sub("^","cluster ",x$cluster)
    cluster_box_plots(x,paste0('combined_',i),mylevels)
}
for (i in names(nozero_clusters)){
    x <- nozero_data_zscore[,c(i,results_cols$betweenness)]
    colnames(x)[1] <- 'cluster'
    x$cluster <- sub("^","cluster ",x$cluster)
    cluster_box_plots(x,paste0('No zero ',i,' '),mylevels)
}
# line plots of RC values
mylevels_pca <- c("RC1","RC2","RC3")
for (i in names(nozero_clusters)){
    x <- aggregate(nozero_pca_scores[,mylevels_pca],
                   by=list(nozero_pca_scores[[i]]), FUN=mean, na.rm=TRUE)
    label <- paste0('nozero_',i)
    cluster_data_long <- melt(as.matrix(x[,c(2:ncol(x))]))
    cluster_data_long$Var2 <- factor(sub("RC_", "",
                                         cluster_data_long$Var2), levels = mylevels_pca)
    cluster_profiles <- ggplot(data=cluster_data_long, aes(x=Var2, y=value, group=1)) +
        geom_line() +
        facet_wrap(~Var1) +
        theme(
            title = element_text(size=14, face = "bold"),
            axis.text = element_text(size=12),
            axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text.x = element_text(size=14, face="bold"),
            legend.text = element_text(size=14)
        ) +
        labs(x="Rotated component", y=paste0("RC values"), 
             title=paste0(label," clusters PCA profile"))
    ggsave(cluster_profiles,filename=paste0(getwd(),
                                            "/images/charts/3_8_cluster_",label,"_pca_profiles.png"),
           width=250,height=200,units='mm',dpi=300)
}
# boxplots of RC values
for (i in names(nozero_clusters)){
    x <- nozero_pca_scores[,c(i,mylevels_pca)]
    colnames(x)[1] <- 'cluster'
    x$cluster <- sub("^","cluster ",x$cluster)
    label <- paste0('nozero_',i)
    cluster_data_long <- melt(x, id = c(1))
    cluster_data_long$variable <- factor(sub("RC_", "",
                                             cluster_data_long$variable), levels = mylevels_pca)
    cluster_profiles <- ggplot(data=cluster_data_long, aes(x=variable, y=value)) +
        geom_boxplot(outlier.size=1) + #, aes(fill = cluster)) +
        facet_wrap(~cluster) +
        theme(
            title = element_text(size=14, face = "bold"),
            axis.text = element_text(size=12),
            axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text.x = element_text(size=14, face="bold"),
            legend.text = element_text(size=14)
        ) +
        labs(x="Rotated component", y=paste0("RC values"), 
             title=paste0(label," clusters PCA profile"))
    ggsave(cluster_profiles,filename=paste0(getwd(),
                                            "/images/charts/3_8_cluster_",label,"_pca_boxplot_profiles.png"),
           width=400,height=300,units='mm',dpi=300)
}
# contingency boxplots for result
plot.new()
png(paste0(getwd(),"/images/charts/4_1_contingency_city_nozero_",i,".png"), width = 6,height = 6, units = "in", res = 600)
association <- assoc(contingency[[1]], shade=TRUE,
                     labeling_args=list(gp_labels=gpar(fontsize=9),gp_varnames=gpar(fontsize=10)),
                     legend_args=list(fontsize=9), main_gp = gpar(fontsize = 12),
                     main="Association between City (y) and Betweenness (x) Types")
dev.off()



##
#### 4. Comparing cities ####
##
# contingency for full combined table
for (i in names(combined_clusters)){
    #freq <- table(combined_data_zscore[[i]],combined_data_zscore$"city")
    contingency <- CrossTable(combined_data_zscore[[i]], combined_data_zscore$city)
    write.xlsx2(contingency[[1]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_combined.xlsx"), 
                sheetName=paste0(i,"_count"), append=TRUE)
    write.xlsx2(contingency[[2]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_combined.xlsx"), 
                sheetName=paste0(i,"_share_by_row"), append=TRUE)
    write.xlsx2(contingency[[3]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_combined.xlsx"), 
                sheetName=paste0(i,"_share_by_column"), append=TRUE)
    plot.new()
    png(paste0(getwd(),"/images/charts/4_1_contingency_city_combined_",i,".png"), width = 6,height = 6, units = "in", res = 600)
    association <- assoc(contingency[[1]], shade=TRUE,
                labeling_args=list(gp_labels=gpar(fontsize=9),gp_varnames=gpar(fontsize=10)),
                legend_args=list(fontsize=9), main_gp = gpar(fontsize = 12),
                main="Association between City (y) and Betweenness (x) Types")
    dev.off()
}
# contingency for nozero combined table
for (i in names(nozero_clusters)){
    #freq <- table(combined_data_zscore[[i]],combined_data_zscore$"city")
    contingency <- CrossTable(nozero_data_zscore[[i]], nozero_data_zscore$city)
    write.xlsx2(contingency[[1]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_nozero.xlsx"), 
                sheetName=paste0(i,"_count"), append=TRUE)
    write.xlsx2(contingency[[2]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_nozero.xlsx"), 
                sheetName=paste0(i,"_share_by_row"), append=TRUE)
    write.xlsx2(contingency[[3]], paste0(getwd(),"/tables/4_1_contingency_city_cluster_nozero.xlsx"), 
                sheetName=paste0(i,"_share_by_column"), append=TRUE)
    plot.new()
    png(paste0(getwd(),"/images/charts/4_1_contingency_city_nozero_",i,".png"), width = 6,height = 6, units = "in", res = 600)
    association <- assoc(contingency[[1]], shade=TRUE,
                         labeling_args=list(gp_labels=gpar(fontsize=9),gp_varnames=gpar(fontsize=10)),
                         legend_args=list(fontsize=9), main_gp = gpar(fontsize = 12),
                         main="Association between City (y) and Betweenness (x) Types")
    dev.off()
}



##
## clean up
rm(plot_data)
rm(data_long)
rm(stats_long)
rm(data_plot)
rm(pca_variance)
rm(sil_cluster_data)
rm(sil_cluster_plot)
rm(association)
rm(x)
rm(i)
rm(contingency)