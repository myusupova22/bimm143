url <- "https://bioboot.github.io/bimm143_W18/class-material/WisconsinCancer.csv"
wisc.df <-read.csv(url)
wisc.data <- as.matrix(wisc.df[,3:ncol(wisc.df)])

#How many M's do we have?
table(wisc.df$diagnosis)

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id
#head(wisc.data)

#Make a numeric classification vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

## Q1: How many observations in the dataset (observations= number of patients, so nrow function)
#should give this result
nrow(wisc.data)
# A1: 569

## Q2: How many variables/features in the data are suffixed with _mean?
# find the word "mean" in the colnames of the wisc.data; adding invert=TRUE would
# give the opposite output)
mean_things <- grep("mean", colnames(wisc.data), value = TRUE)
#[1] "radius_mean"            "texture_mean"           "perimeter_mean"        
#[4] "area_mean"              "smoothness_mean"        "compactness_mean"      
#[7] "concavity_mean"         "concave.points_mean"    "symmetry_mean"         
#[10] "fractal_dimension_mean"

length(mean_things)
# Length = 10

#Q3. How many of the observations have a malignant diagnosis?
diagnosis
sum(diagnosis)

#-----------------------------------------------------------------------------
# Check column means and standard deviations
colMeans(wisc.data)

apply(wisc.data,2,sd)

#remove the last column from wisc.data because it has "NA" answers 
wisc.data <- as.matrix(wisc.df[,3:32])
#OR, redefine old wisc.data <- as.matrix(wisc.df[,3:ncol(wisc.df)])
#wisc.data <- wisc.data[,-31]

# Perform PCA on wisc.data by completing the following code
wisc.pr <-prcomp(wisc.data, scale. = TRUE)

# Look at summary of results
summary(wisc.pr)

##Q4. From your results, what proportion of the original variance is captured by the 
#first principal components (PC1)?
#44.3%

##Q5. How many principal components (PCs) are required to describe at least 70% of 
#the original variance in the data?
#The first 3 PCs

##Q6. How many principal components (PCs) are required to describe at least 90% of
#the original variance in the data?
#The first 7 PCs

#-----------------------------------------------------------------------------
## Plot PCA results

#Create a biplot of the wisc.pr using the biplot() function.
biplot(wisc.pr)

#Q7. What stands out to you about this plot? Is it easy or difficult to understand? Why?
# It is very difficult to understand (setting scale = TRUE didn't help)

#What is in our PCA output
attributes(wisc.pr)

#Scatter plot observations by components 1 and 2
plot(wisc.pr$x[,1], wisc.pr$x[,2], col = (diagnosis +1), 
     xlab = "PC1", ylab = "PC2")

#Q8. Repeat the same for principal components 1 and 3. 
#What do you notice about these plots?
plot(wisc.pr$x[,2], wisc.pr$x[,3], col = diagnosis, 
     xlab = "PC2", ylab = "PC3")
#the below function (col = (diagnosis + 1) colored it red
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")


## Variance explained Scree-plot
#Calculate vairance of each component
pr.var <- wisc.pr$sdev^2
head(pr.var)

## Proportion of variance
pve <- pr.var / sum(pr.var)

#Plot variance explained for each principal component
par(mfrow=c(1, 2))
plot( pve , xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")


# Alternative scree plot of the same data, note data driven y-axis
#mfrow organizes plots by rows (versus by colums); specifcy the c(nrows, ncols)
barplot(pve, ylab = "Precent of Variance Explained",
        names.arg=paste0("PC",1:length(pve)), las=2, axes = FALSE)

axis(2, at=pve, labels=round(pve,2)*100 )

#-----------------------------------------------------------------------------
## Selecting number of clusters

#scale
data.scale <- scale(wisc.data)
data.dist <- dist(data.scale)
#hclust
wisc.hclust <- hclust(dist(data.scale))

plot(wisc.hclust)
abline(h=20, col="blue")

wisc.hclust.clusters <-cutree(wisc.hclust, k = 4)
wisc.hclust.clusters
table(wisc.hclust.clusters)

table(wisc.hclust.clusters, diagnosis)

# K-means clustering
wisc.km <- kmeans(data.dist, centers= 2, nstart= 20)








