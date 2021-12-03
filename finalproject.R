library("mlr")           #
library("tidyverse")     #
library("DataExplorer")  #
library("factoextra")    #
library("dendextend")    #
library("reshape2")      #
library("ggforce")       #
library("cluster")       #
library("dplyr")         #
library("corrplot")
library("cluster")
library("NbClust")
library("gridExtra")
library("GGally")

cc_data <- read.csv("CC GENERAL.csv", 
                    stringsAsFactors = F,
                    na.strings = c(" "))
View((cc_data))
View(summary(cc_data))

#all variables are numeric other than CUST_ID which will be removed for clustering.

str(cc_data)
sum(is.na(cc_data))   #Trying to find NA values.

#Where is my missing NA values?
plot_missing(cc_data)

# missing values are to be replaced with 0.

cc_data$MINIMUM_PAYMENTS[which(is.na(cc_data$MINIMUM_PAYMENTS))]<- 0
summary(cc_data)

#In banking system, CREDIT LIMIT can not be 0, so we give it a mean value.

cc_data$CREDIT_LIMIT[which(is.na(cc_data$CREDIT_LIMIT))] <- mean(cc_data$CREDIT_LIMIT,
                                                                 na.rm=TRUE) 
summary(cc_data)
#recheck
plot_missing(cc_data)
#There is no missing values in the dataset


cc_data <- cc_data[, -1] #Removing cust_id 
View(cc_data)

# Historgram for each attribute
cc_data %>% 
  gather(Attributes, value, 1:17) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency",
       title="Credit Cards Attributes - Histograms") +
  theme_bw()
plot_histogram(cc_data)
#correlation Matrix

corrplot(cor(cc_data), diag = FALSE, type = "upper", order = "hclust",
         tl.col = "black", tl.pos = "td", tl.cex = 0.9, method = "circle")

# Relationship between Balance and Cash Advance
ggplot(cc_data, aes(x=BALANCE, y=CASH_ADVANCE)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Credit Cards Attributes",
       subtitle="Relationship between Balance and Cash Advance") +
  theme_bw()

# Normalization
cc_scaled <- as.data.frame(scale(cc_data)) #It helps us to normalise the data within a particular range. (Also scaling gives us fast processing.)

# Original data
p1 <- ggplot(cc_data, aes(x=CREDIT_LIMIT, y=BALANCE)) +
  geom_point() +
  labs(title="Original data") +
  theme_bw()

# Normalized data 
p2 <- ggplot(cc_scaled, aes(x=CREDIT_LIMIT, y=BALANCE)) +
  geom_point() +
  labs(title="Normalized data") +
  theme_bw()

# Subplot
grid.arrange(p1, p2, ncol=2)

###Kmeans
par(mar=c(1,1,1,1))
set.seed(96743)        # because starting assignments are random
k <- kmeans(cc_scaled, centers=4, nstart = 25)

k$center
clusplot(cc_data, k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")
#cluster size
k$size

#Determining Optimal Clusters
#There are three popular methods to find optimal clusters:
  
#Elbow Method Silhouette Method GAP Statistics
#Elbow
wss <- (nrow(cc_data)-1)*sum(apply(cc_data,2,var))

for (i in 2:15) {
  wss[i] <- sum(kmeans(cc_data, centers=i)$tot.withinss)
}

plot(1:15, wss, type="b", pch = 19, frame = FALSE,
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fviz_cluster(seg.k, data = cc_scaled)
fviz_nbclust(cc_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette

avg_sil <- function(k) {
  km.res <- kmeans(cc_scaled, centers = k, nstart = 25, iter.max = 50)
  ss <- silhouette(km.res$cluster, dist(cc_scaled))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(cc_scaled, kmeans, method = "silhouette")

#Gap

#gapK<- clusGap(cc_scaled, kmeans, nstart=50, iter.max=50,
               #d.power=2, K.max = 20, B =150) # Observe d.power
#plot(gapK, main="gap K-means")
#set.seed(123)
# Compute the gap statistic
#gap_stat <- clusGap(cc_scaled, FUN = kmeans, nstart = 25, 
                 #   iter.max = 50, K.max = 10, B = 100) 
# Plot the result
fviz_gap_stat(gap_stat)
NbClust(data = cc_scaled, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")
set.seed(1234)

seg.k7 <- kmeans(cc_scaled, centers=7)

# Mean values of each cluster
aggregate(cc_data, by=list(seg.k7$cluster), mean)
seg.k7$size
# Clustering 
ggpairs(cbind(cc_data, Cluster=as.factor(seg.k7$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()
seg.k7
fviz_cluster(seg.k7, data = cc_scaled)
cc_withclusters<- mutate(cc_data, cc_hclusters)
cc_withclusters
count(cc_withclusters, cc_hclusters)
