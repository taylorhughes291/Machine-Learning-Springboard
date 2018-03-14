movies <- read.table("Clustering and K Means/movieLens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action",
                      "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama",
                      "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi",
                      "Thriller", "War", "Western")
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward.D")
plot(clusterMovies)

clusterGroups <- cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title == "Men in Black (1997)")
clusterGroups[257]
cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

#--------------------------------------------------------

flower <- read.csv("Clustering and K Means/flower.csv", header = FALSE)
str(flower)
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 <- as.vector(flower)
str(flowerVector2)

distance <- dist(flowerVector, method = "euclidean")
clusterIntensity <- hclust(distance, method = "ward.D")
plot(clusterIntensity)

# Stopped at Hierarchy Clustering
## -------------------------------------------------------

healthy <- read.csv("Clustering and K Means/healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))
healthyVector <- as.vector(healthyMatrix)


k = 5
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyClusters <- KMC$cluster
KMC$centers[2]

dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))


tumor <- read.csv("Clustering and K Means/tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)
library(flexclust)

KMC.kcca <- as.kcca(KMC, healthyVector)
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col = rainbow(k))

































