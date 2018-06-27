# ------------------------------------------------------------------------------
#   Create r/qtl input file
# ------------------------------------------------------------------------------

# this script creates an input file for r/qtl
# each row is a unique individual
# the first set of columns are phenotypes
# the rest of the columns are SNPs at individual loci

# ------------------------------------------------------------------------------
# step 1: combine linkage maps for each chromosome

setwd("~/Dropbox/carrot-image-analysis/qtl_mapping/")

# read in linkage map for individual chromosomes and combine into a list
files <- lapply(list.files(path="./linkage_map", pattern="*.csv", full=TRUE), FUN=read.csv)

# merge chromosomes by individual name
merged_map <- Reduce(function(x,y) merge (x, y, by=c("Nr", "Individual")), files)

# write out complete linkage map (all environments)
write.csv(merged_map, "./data/full_linkage_map.csv", row.names=FALSE)

# reduce set to include only CA2016 environment

CA15_removed <- merged_map[!grepl("3263", merged_map$Individual),]
CA16 <- CA15_removed[!grepl("6097", CA15_removed$Individual),]

# write out reduced data set
write.csv(CA16, "./data/CA16_linkage_map.csv", row.names=FALSE)

# ------------------------------------------------------------------------------
# step 2: merge manual and image measurements

# want to keep missing values 
# phenotypes <- read.csv("./data/merged_data.csv", header=TRUE)
phenotypes <- read.csv("./data/merged_data2.csv", header=TRUE)

library(stringr)
phenotypes$subsample <- str_pad(phenotypes$subsample, 3, pad="0")

phenotypes$Individual <- paste(phenotypes$plot, phenotypes$subsample, sep="-")

rqtl_input <- merge(phenotypes, CA16, by="Individual")

# remove unwanted columns
# image_name, plot, subsample, pedigree, spr, pop, whole_BoundingBox,
# whole_BoundingBox.1, top_BoundingBox, top_BoundingBox.1, 
# bottom_BoundingBox, bottom_BoundingBox.1, Nr
rqtl_input <- rqtl_input[,-c(2:5,13:14,20:21,29:30,38:39,53)]

write.csv(rqtl_input, "./data/input2.csv", row.names=FALSE)
# need to fix header, etc. in excel
