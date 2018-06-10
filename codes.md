# k-means-clustering

---
title: "K-means Clustering"
author: "alok"
date: "May 18, 2018"
output: html_document
odi<- read.csv("E:/odi-batting.csv")

---
```{r}
library(dplyr)
```

```{r}
odi$century=ifelse(odi$Runs>99,1,0)
odi$ducks=ifelse(odi$Runs==0,1,0)
odi$above_150=ifelse(odi$Runs>149,1,0)
odi$fifties=ifelse(odi$Runs>49&odi$Runs<100,1,0)
odi$missed_centuries=ifelse(odi$Runs>90&odi$Runs<100,1,0)

```
# Create KPI metrics for each player

```{r}
players_summary=odi%>%group_by(Player)%>%
  summarise(matches=n(),
  total_runs=sum(Runs,na.rm = T),
  avg_runs=mean(Runs,na.rm = T),
  centuries=sum(century,na.rm = T),
  ducks=sum(ducks,na.rm = T),
  fifties=sum(ducks,na.rm = T),
  above_150=sum(above_150,na.rm = T),
  missed_centuries=sum(missed_centuries,na.rm = T)
            )
  View(players_summary)

```
```{r}
library(BBmisc)
```

```{r}
top_players=players_summary%>%arrange(-total_runs)%>%head(100)
data_means=top_players%>%select(-Player)
data_norm=normalize(data_means,range=c(0,1),method = 'range')
model_kmeans=kmeans(data_norm,centers = 10)
top_players$cluster=model_kmeans$cluster
barplot(table(top_players$cluster),col = "violet",xlab = "cluster",ylab = "kmeans")
```
# between square sums
```{r}
model_kmeans$betweenss
```
# within square sums
```{r}
model_kmeans$withinss
```
# total withinss
```{r}
model_kmeans$tot.withinss
```
# plot total withinss
```{r}
barplot()
```




```{r}
hr_subset=hr%>%select(Age,MonthlyIncome)
hr_subset$Age=as.numeric(hr_subset$Age)
hr_subset_norm=normalize(hr_subset,method='range',range=c(0,1))
model_hr=kmeans(hr_subset_norm,centers = 2)
hr_subset$cluster=model_hr$cluster



```
## Hierarchical Clustering
```{r}
names(data_norm)
data_norm$cluster<-model_kmeans$cluster
hclust_model=hclust(dist(data_norm%>%select(-cluster)))
plot(hclust_model)

```
# Get cluster labels/

```{r}
library(ggplot2)
```

```{r}
data_norm$cluster<-model_kmeans$cluster
data_norm$cluster=cutree(hclust_model,k=4)
data_norm_2d=cmdscale(dist(data_norm%>%select(-cluster)))
data_norm_2d=as.data.frame(data_norm_2d)
data_norm_2d$cluster=as.factor(data_norm$cluster)
ggplot(data_norm_2d,aes(x=v1,y=v2,color=cluster))+geom_point()
```
```{r}
library(corrplot)
corr_players=cor(t(data_norm%>%head(10)%>%select(-cluster)))
corrplot(corr_players,order = "hclust",addrect = 2)
```




