#Let's load necessary packages
library(FactoMineR)
library(SensoMineR)


## Appearance


#Let’s import the data using read command.
appearance <- read.csv("C:/Users/BSL CTCRI/Downloads/SD.csv")

appearance

appearance$consumer <- as.factor(appearance$consumer)

appearance$product <- as.factor(appearance$product)


### This data set consists of 10 product types assessed by 15 consumers, each row corresponding to the assessment of one product by one consumer.

#Representation of the hedonic scores per product, using the boxplot function


boxplot(appearance~product,data=appearance,col="lightgray")


#Summary of the Data

summary(appearance)

#As shown by the outputs of the summary function, this data set needs to
#be restructured.

#The new Matrix data would be :
  
  
consumer <- levels(appearance$consumer)

nbconso <- length(consumer)

product <- levels(appearance$product)

nbprod <- length(product)

newmat <- matrix(0,nbconso,0)

rownames(newmat) <- consumer
for (p in 1:nbprod){
  data.p <- appearance[appearance$product==product[p],]
  data.add <- as.matrix(data.p[,3])
  rownames(data.add) <- data.p[,1]
  newmat <- cbind(newmat,data.add[rownames(newmat),])
}

colnames(newmat) <- product

newmat

#Let’s represent the consumers according to the first point of view. we apply a PCA on this table. Let us remind that for this PCA, two consumers are all the more close that they gave the same scores to the products. For instance, if we consider the following consumers 12, 13, and 4, we expect to find the first two really close on the first factorial plane, and very far from the third one.


newmat[c("12","13","4"),]


#Applying and Visualising PCA


raw.pca <- PCA(newmat,graph=FALSE)
plot.PCA(raw.pca,choix="ind",title="When appearance is not centered")



#From this analysis, the most surprising results are provided by the variable representation which highlights the size effect.


plot.PCA(raw.pca,choix="var",title="When appearance is not centered")


#This size effect highlights the fact that the variables of the data set are all positively correlated. In terms of consumers and liking, it means that some consumers like everything, and some others dislike everything.

#Let’s now consider the shape effect for which the data set is mean centered by rows, i.e., the data we are dealing with are not pure liking scores anymore, but preferences.


newmat2 <- cbind(newmat,as.matrix(apply(newmat,1,mean)))

colnames(newmat2)[11] <- "Average"

raw.pca2 <- PCA(newmat2,quanti.sup=11,graph=FALSE)

plot.PCA(raw.pca2,choix="var")


#Centering the data by Rows


means <- apply(newmat,1,mean)

round(means,2)[1:15]

newmat_centered <- sweep(newmat,1,means,FUN="-")

round(apply(newmat_centered,1,mean))[1:15]


#Applying and Visualising PCA


center.pca <- PCA(newmat_centered,graph=FALSE)

plot.PCA(center.pca,choix="var",title="When appearance is centered")



#To identify the best product, two different approaches are considered: a univariate and a multivariate one which uses Fisher’s LSD and MDPref respectively.

## Lets consider univariate point of view.

#After changing the contrasts in R using the options function, let’s run a two-way ANOVA on the data set


options(contrasts=c("contr.sum", "contr.sum"))

model <- lm(appearance~product+consumer,data=appearance)

anova(model)


Summary of the ANOVA


summary(model)


#Lets see how the levels of the Product effect are organized.


levels(appearance$product)


#According to the previous outputs, **we can say that T10 and T2 were the most appreciated products**.

## Performing posteriori tests

The LSD.test function returns 5 main outputs


options(contrasts=c("contr.sum","contr.sum"))

model <- aov(appearance~product+consumer,data=appearance)

library(agricolae)

res.LSD <- LSD.test(model,"product",p.adj="none",group=TRUE,
                    main="Results of the Fisher LSD")

names(res.LSD)



#Analysing the Statistics output


res.LSD$statistics


#the last value provided is called LSD. This corresponds to the least significant difference required between two products to be significantly different. Since here, the LSD value is of 1.19, every pair of products with a difference in the average liking score larger than 1.19 is significant at 5%.


res.LSD$groups

#In this example, the results of the Fisher’s LSD test show that T10, T2, T3, T6, T7 and T5 are the significantly most liked products, whereas T9, T8, T1 and T4 are significantly the least liked products.

#Graphical representation of the result.


bar.group(res.LSD$groups,ylim=c(0,10),density=4,border="black",cex.names=0.7)


## Lets consider multivariate point of view.

#The idea of MDPref, is to get a representation of the products based on the hedonic scores: two products are all the more close that they are liked similarly.


data.mdpref <- t(newmat)

mdpref <- PCA(data.mdpref,ncp=Inf)


#The first dimension of the PCA opposes T5 and T8 to T10. In other words, consumers who preferred T5 tend to also like more T8, and tend to appreciate less T10, and
#vice versa. T5 and T8 seem to have been preferred similarly,but very differently from T10.

#Obtaining groups of products similar in terms of the way they have been preferred 


prod.hcpc <- HCPC(mdpref, nb.clust=-1)


#Determining the best product within homogeneous segments of consumers.


center.pca <- PCA(newmat_centered,graph=FALSE)

cons.hcpc <- HCPC(center.pca, nb.clust=-1)


#Description of the groups by both the individuals and the variables of the data set.


names(cons.hcpc)


#The number of consumers belonging to each group


summary(cons.hcpc$data.clust[,1:11])


#According to this output, there are eight groups of consumers, a first one with 1 consumer, a second one with 1 consumers, a third one with 2 consumers and so on.

#Description of the first cluster of consumers with respect to the quantitative variables of the data set


round(cons.hcpc$desc.var$quanti$"1",3)

Consumers in cluster 1 significantly preferred product such as T5


cons.hcpc$desc.ind


#a description of the clusters with respect to the individuals of the data set, from the most typical to the least typical.


clusters <- as.matrix(cons.hcpc$data.clust$clust)

rownames(clusters) <- rownames(cons.hcpc$data.clust)

colnames(clusters) <- "Cluster"

cons.clust <- merge(x=newmat,y=clusters,by=0)

rownames(cons.clust) <- cons.clust[,1]

cons.clust <- cons.clust[,-1]

#The average score by product for each cluster of consumers

cluster.mean <- aggregate(cons.clust[,1:10],by=list(cons.clust[,11]),
                          FUN="mean")[,-1]
round(cluster.mean,2)

#Representing graphically the differences in terms of liking between clusters.

plot(1:10,cluster.mean[1,],ylim=c(1,15),xlab="",ylab="Average appearance Scores",
     main="Mean by clusters",col=1,lwd=2,type="l",axes=FALSE,cex.lab=0.7)

lines(1:10,cluster.mean[2,],lwd=2,col=2)
lines(1:10,cluster.mean[3,],lwd=2,col=3)
lines(1:10,cluster.mean[4,],lwd=2,col=4)
lines(1:10,cluster.mean[5,],lwd=2,col=5)
lines(1:10,cluster.mean[6,],lwd=2,col=6)
lines(1:10,cluster.mean[7,],lwd=2,col=7)
lines(1:10,cluster.mean[8,],lwd=2,col=8)

axis(1,at=1:10,label=colnames(cluster.mean),cex.axis=0.55,las=2)

axis(2,cex.axis=0.7)

legend("topright",legend=paste("Cluster",1:8),bty="n",col=1:8,lwd=2,cex=0.7)


#Preffered product of Cluster 1: T2

#Preffered product of Cluster 2: T5

#Preffered product of Cluster 3: T10

#Preffered product of Cluster 4: T10

#Preffered product of Cluster 5: T10

#Preffered product of Cluster 6: T2

#Preffered product of Cluster 7: T10

#Preffered product of Cluster 8: T5