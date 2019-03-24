states = row.names(USArrests) # the rows constain states
states

names(USArrests) # columns contain variables like "Murder", "Assault","Rape" and " UrbanPop"
apply(USArrests,2,mean) # variables have vastly different means grouped by column

## variances of the variables
apply(USArrests,2,var)

# standardising the variables
pr.out = prcomp(USArrests,scale = TRUE) # by default prcomp standardises the variable to have mean 0 and sd = 1 by chosing scale = TRUE
names(pr.out)

pr.out$center# provides means
pr.out$scale # provides standard deviations
pr.out$rotation # matrix provides loadings 

## using prcomp we need not multiply loadings with the data,x gives us the multiplication matrix
dim(pr.out$x)

biplot(pr.out,scale = 0)

## sign change doesnot alter the solution of principal components
## to produce its mirror image as discussed in the book

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out,scale = 0)

# standard deviation 
pr.out$sdev

## variance 
pr.var = pr.out$sdev^2

## proportion of variance explained 
pve = pr.var/sum(pr.var)
pve

## PC1 explains around 62% variance

plot(pve,xlab = "Principal Component",ylab = "Proportion of variance explained",ylim = c(0,1),type = 'b')
## gives elbow shaped plot 

## plotting cumulative PVE
plot(cumsum(pve),xlab = "Principal Component",ylab = "Cumulative Proportion of variance explained",ylim = c(0,1),type = 'b')
#cumsum is used for calculating cumulative sum

a = c(1,2,8,-3)
cumsum(a)

################## Clustering #########################
### K means clustering #############

set.seed(2)
x = matrix(rnorm(50*2),ncol = 2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2] = x[1:25,2]-4

km.out = kmeans(x,2,nstart = 20)## k =2 , with 20 random sets

km.out$cluster

plot(x, col = (km.out$cluster+1),main = "Kmeans clustering results with K =2",pch=20,cex =2)

## But in the above example we knew that we have 2 variables and so k =2 was chosen,but in practical we are unaware of the number of clusters to be taken 

## let us take k =3 in the above example

set.seed(4)
km.out = kmeans(x,3,nstart = 1)
km.out

km.out$withinss ## within cluster sum of squares

## nstart >1 means K means clustering will be performed using multiple random assignemnts and out of those the best assignment is chosen
## other wise local optimum will be obtained


set.seed(4)
km.out = kmeans(x,3,nstart = 20)
km.out

############## Hierarchical clustering #############################################
## USing Euclidean distance as the dissimilarity measure
hc.complete = hclust(dist(x),method = "complete")

hc.average = hclust(dist(x),method = "average")
hc.single = hclust(dist(x),method = "single")

## plotting dendograms
par(mfrow=c(1,3))
plot(hc.complete,main = "Complete Linkage",xlab = "",sub = "",cex = 0.9)
plot(hc.average,main = "Average Linkage",xlab = "",sub = "",cex = 0.9)
plot(hc.single,main = "Single Linkage",xlab = "",sub = "",cex = 0.9)

#single ,complete and average clustering with 2 clusters
cutree(hc.complete,2)

cutree(hc.average,2)

cutree(hc.single,2)
# Single linkage identifies one point belonging to its own cluster .So choosing 4 clusters
cutree(hc.single,4)

### Scaling the features

xsc = scale(x)
plot(hclust(dist(xsc),method = "complete"),main = "Hierarchical clustering with scaled features")
## correaltion based distance 
## minimum 3 features are required for correlation based distance
x = matrix(rnorm(30*3),ncol = 3)
dd = as.dist(1-cor(t(x)))
plot(hclust(dd,method = "complete"),main = "Complete linkage with Correlation Based Distance ",xlab = "", sub = "")


##################### NC160  data 
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

dim(nci.data)
nci.labs[1:4]

table(nci.labs)

### PCA on the data after scaling the variables to have standard deviation =1 ,although it is not advisable to scale gene data
pr.out = prcomp(nci.data,scale = TRUE)


## We try to plot first few principal components to visualise the data 
## However,cell lines belonging to a given cancer type will be plotted in the same color 
## nci.labs has 14 factor levels
COls = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))
plot(pr.out$x[,1:2],col = COls(nci.labs),pch = 19,xlab = "Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)],col = COls(nci.labs),pch = 19,xlab = "Z1",ylab = "Z3")

summary(pr.out)

plot(pr.out)

pve = 100*pr.out$dev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve , type = "o",ylab = "PVE",xlab = "Principal COmponent" , col = "blue")
plot(cumsum(pve),type = "o",ylab = "Cumulative PVE" , xlab = "Principal Component",col = "brown3")



## clustering the observatio 
sd.data = scale(nci.data)
par(mfrow = c(1,3))
data.dist = dist(sd.data)
plot(hclust(data.dist),labels = nci.labs,main = "Complete Linkage",xlab = "",ylab = "",sub = "")
plot(hclust(data.dist,method = "average"),labels = nci.labs,main = "Average Linkage",xlab = "",ylab = "",sub = "")
plot(hclust(data.dist,method = "single"),labels = nci.labs,main = "Single Linkage",xlab = "",ylab = "",sub = "")


#analysing complete linkage hierarchical cluster 

hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out,4)
table(hc.clusters,nci.labs)

par(mfrow = c(1,1))
plot(hc.out,labels = nci.labs)
abline(h = 139,col = "red")
hc.out 

set.seed(2)
km.out = kmeans(sd.data,4,nstart = 20)
km.clusters = km.out$cluster
table(km.clusters,hc.clusters)

hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels = nci.labs,main = "Hierarchical Clsut on first 5 score vectors")
table(cutree(hc.out,4),nci.labs)