# ------------------------------------------------------------------------------
# Merge hand and algorithm measurements
# 7 August 2017; updated 24 April 2018
# ------------------------------------------------------------------------------

# this script merges hand measurements with image-based measurements based on
# image name

library(Hmisc)
library(reshape2)
library(dplyr)

setwd("~/Dropbox/carrot-image-analysis/data")

# read in hand measured data
hand <- read.csv("hand_measurements.csv", header = TRUE)

# read in and combine image extracted measurements
# source: https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y)}, datalist)
}

image <- multmerge("~/Dropbox/carrot-image-analysis/data/merge")

# convert px to cm for appropriate traits
# based on pixel equivalent for measurements of a known size (50.8px/cm)
# traits: bounding box width/length, perimeter, petiole length/width
names(image[,c(5,9:10,14,18:19,23,27:29,31)])
image[,c(5,9:10,14,18:19,23,27:29,31)] <- image[,c(5,9:10,14,18:19,23,27:29,31)]/50.8 

# add .NEF to image name for merging
image$image_name <- paste(image$image_name, ".NEF", sep="")

write.csv(image, "image_data.csv", row.names = FALSE)

# merge hand and image measurements
merged <- merge(hand, image, by = "image_name")

# export image ids for images that did not run successfully
failed <- anti_join(hand, image, by="image_name")
sum(failed$spr, na.rm=TRUE)

# log transform biomass
merged$flw <- log(merged$flw)
merged$dlw <- log(merged$dlw)
merged$frw <- log(merged$frw)
merged$drw <- log(merged$drw)
merged$shootBio <- log(merged$shootBio)
merged$rootBio <- log(merged$rootBio)

# add principal components for shoot and root
shoot_pcs <- read.csv("~/Dropbox/carrot-image-analysis/data/shoot_pcs.csv", header=TRUE)
colnames(shoot_pcs) <- c("image_name", "S_PC1", "S_PC2", "S_PC3")
str(shoot_pcs)
shoot_pcs$image_name <- paste0(shoot_pcs$image_name, ".NEF")

root_pcs <- read.csv("~/Dropbox/carrot-image-analysis/data/root_pcs_normalized.csv", header=TRUE)
colnames(root_pcs) <- c("image_name", "R_PC1", "R_PC2", "R_PC3")
root_pcs$image_name <- paste0(root_pcs$image_name, ".NEF")

merged_data <- merge(merged, shoot_pcs, by="image_name")
merged_data <- merge(merged_data, root_pcs, by="image_name")

# export merged data
write.csv(merged_data, "merged_data.csv", row.names = FALSE)
