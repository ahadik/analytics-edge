library(dplyr)
library(tidyverse)
library(tm) # Will use to create corpus and modify text therein.
library(SnowballC) # Will use for "stemming." 
library(rpart) # Will use to construct a CART model.
library(rpart.plot) # Will use to plot CART tree.
library(caret)
library(glmnet)
library(ModelMetrics)
library(ggplot2)
library(knitr)
library(ggcorrplot)

artworks = read.csv("artworks.csv")
subject_ids = read.csv("id_details.csv")

artworks_wv = as.data.frame(matrix(0, ncol = nrow(subject_ids), nrow = nrow(artworks)))

#id_remap = data.frame(id=integer(), index=integer())
id_remap = c()

for (i in 1:nrow(subject_ids)) {
  id_remap[[subject_ids[i,]$id]] = i
}


artworks_wv$id = artworks$id
artworks_wv = artworks_wv %>% select(id, everything())

for (r in 1:nrow(artworks)) {
  subject_row = artworks[r,]
  subjects = strsplit(subject_row$subjects, ',')
  if (length(subjects[[1]]) > 0) {
    for (i in 1:length(subjects[[1]])) {
      artworks_wv[r, id_remap[[as.numeric(subjects[[1]][i])]]+1] = 1
    }
  }
}

km <- kmeans(subset(artworks_wv, select=-c(id)), centers = 50, iter.max=100)

km.artworks.50.cluster = artworks
km.artworks.50.cluster$cluster = km$cluster
cluster_count = count(group_by(km.artworks.50.cluster, cluster))

get_photos = function(dataset) {
  photo_info = data.frame(cluster=integer(), title=character(), url=character(), artist=character())
  
  for (c in 1:50) {
    cluster = filter(dataset, cluster == c & url != "" & artist != "Joseph Mallord William Turner")
    for (x in 1:10) {
      dir.create(paste('samples/c', c, sep=""))
      index = floor(runif(1)*nrow(cluster))
      image_url = str_replace(cluster[index,]$url, "_8", "_9")
      image_url_alt = cluster[index,]$url
      file_path = paste('samples/c', c, '/', x, '.jpg', sep="")
      tryCatch(download.file(image_url, file_path, mode="wb"),
               error = function(e) {
                 tryCatch(download.file(image_url_alt, file_path, mode="wb"),
                          error = function(e) {
                            print(e)
                          })
               })
      photo_info = rbind(photo_info, c(c, cluster[index,]$title, cluster[index,]$url, cluster[index,]$artist))
    }
  }
  
  photo_info
}

p = get_photos(filter(km.artworks.50.cluster,  artist != "Joseph Mallord William Turner"))









# d <- dist(subset(artworks_wv, select=-c(id)))    # method = "euclidean"
# class(d)
# 
# # Creates the Hierarchical clustering
# hclust.mod <- hclust(d, method="ward.D2")
# 
# h.50.clusters <- cutree(hclust.mod, 50)

#rm(artworks)


# id_details = read.csv('id_details.csv')
# 
# hist(id_details$count, breaks=10)
# 
# bigger_counts = filter(id_details, count > 10)
# 
# hist(log(bigger_counts$count), breaks=10)

# We see from this that there are tons of ids used only a few times. We can reduce the number of ids dramatically and still keep a high representation.











