# ------------------------------------------------------------------------------
#   Process and combine data for F2 and diallel 
#   S. Turner 
#   27 February 2017; updated 24 April 2018
# ------------------------------------------------------------------------------
# this script combines hand measurements for a carrot diallel and F2 mapping
# population, along with corresponding image names.

library(splitstackshape)
library(stringr)

setwd("~/Dropbox/carrot-image-analysis/data")

# ------------------------------------------------------------------------------
# F2 data
# ------------------------------------------------------------------------------

f2data <- read.csv("F2RawDataUnprocessed.csv", header = TRUE)

# calculate shoot biomass
f2data$flw <- f2data$leafBagFresh - f2data$leafBag
f2data$dlw <- f2data$leafBagDry - f2data$leafBag

# calculate root biomass
f2data$frw_samp <- f2data$rootSampleFresh - f2data$rootBag
f2data$drw_samp <- f2data$rootSampleDry - f2data$rootBag
f2data$pedigree <- as.factor(f2data$source)
f2data$plot <- as.factor(f2data$plot)
f2data$subsample <- as.factor(f2data$subsample)

# calculate percent dry matter in root sample
f2data$pdm <- f2data$drw_samp/f2data$frw_samp

# estimate root dry weight for whole root
f2data$drw <- f2data$rootWt*f2data$pdm

f2data$frw <- f2data$rootWt

# add population identifier
f2data$pop <- "f2"

# subset for final data 
f2data <- subset(f2data, select = c("pedigree", "plot", "subsample", "height", "tapRoot", 
                                    "leafNum", "flw", "dlw", "frw", "drw", "spr", "pop"))

write.csv(f2data, "f2data.csv", row.names=FALSE)

# ------------------------------------------------------------------------------
# diallel data
# ------------------------------------------------------------------------------
dialleldata <- read.csv("diallelUnprocessedRaw.csv", header = TRUE)

# calculate shoot biomass
dialleldata$flw <- dialleldata$leafBagFresh - dialleldata$leafBag
dialleldata$dlw <- dialleldata$leafBagDry - dialleldata$leafBag

# calculate root biomass
dialleldata$frw <- dialleldata$rootBagFresh - dialleldata$rootBag
dialleldata$drw <- dialleldata$rootBagDry - dialleldata$rootBag

# add population identifier
dialleldata$pop <- "diallel"
dialleldata$plot <- as.factor(dialleldata$plot)
dialleldata$subsample <- as.factor(dialleldata$subsample)

# subset for final data
dialleldata1 <- subset(dialleldata, select = c("pedigree", "plot", "subsample", "height", "tapRoot", 
                                    "leafNum", "flw", "dlw", "frw", "drw", "spr", "pop"))

write.csv(dialleldata1, "diallelData.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# combine F2 and diallel data & add image names
# ------------------------------------------------------------------------------
hand_measurements <- rbind(dialleldata1, f2data)

# get image names
# alternative: can use list.files() in the image directory
names <- read.csv("image_names.csv", header=FALSE, stringsAsFactors=TRUE)
ids <- str_split(names$V1, "_", simplify=T)
colnames(ids) <- c("plot", "subsample", "time")
image_names <- cbind(names, as.data.frame(ids))
image_names$subsample <- gsub("rep*", "", image_names$subsample)
image_names <- image_names[,1:3]
colnames(image_names) <- c("image_name", "plot", "subsample")

# combine hand measurements with image names
combinedData <- merge(hand_measurements, image_names, by = c("plot", "subsample"))

# export data
write.csv(combinedData, "hand_measurements.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# format diallel data for repeatability calculations
# ------------------------------------------------------------------------------
# only diallel data w/ rep information
dialleldata2 <- subset(dialleldata, select = c("pedigree", "plot", "year", "rep", 
                                               "subsample", "height", "tapRoot", 
                                               "leafNum", "flw", "dlw", "frw", 
                                               "drw", "spr", "pop"))

diallel2 <- merge(dialleldata2, image_names, by = c("plot", "subsample"))

write.csv(diallel2, "diallel_repeatability.csv", row.names = FALSE)
