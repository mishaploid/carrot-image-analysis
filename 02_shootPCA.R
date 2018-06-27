# ------------------------------------------------------------------------------
#   Carrot Image Analysis - Principal Component Analysis for Shoot Profile
#   S. Turner & N. Miller
#   12 July 2016; updated 24 April 2018
# ------------------------------------------------------------------------------

# this script runs a principal components analysis on the shoot profile output
# from the algorithm

setwd("~/Dropbox/carrot-image-analysis/data")

shootProfile <- read.csv("shootProfile.csv", row.names=1, header=FALSE)
shootProfile <- as.matrix(shootProfile)

# ------------------------------------------------------------------------------
# run PCA and plot results

pcShoot <- prcomp(shootProfile)
shoot_pcs <- as.data.frame(pcShoot$x[,1:3]) # extract first three principal components
write.csv(shoot_pcs, "shoot_pcs.csv", row.names=TRUE)

# screeplot and pairwise correlations among PCs
pdf("~/Dropbox/carrot-image-analysis/results/shoot_pca_diagnostics.pdf")
plot(pcShoot, type="l")
pairs(shoot_pcs)
dev.off()

# summarize % variation explained for PCs
summary(pcShoot)$importance[, 1:4]

# find example images showing the range of variation in PCs 1 & 2
plot(shoot_pcs$PC2 ~ shoot_pcs$PC1)
grid(nx=20, col="gray", lty=1)

# point and click to find equidistant/representative examples
example_image_numbers <- identify(shoot_pcs$PC1, shoot_pcs$PC2, col="blue")
example_image_numbers
example_images <- shoot_pcs[example_image_numbers,] # get image names
write.csv(example_images, "~/Dropbox/carrot-image-analysis/results/example_shoot_images.csv")

# images selected for Figure 5
# text(shoot_pcs[919,1:2], labels = "919", pos = 1, col="blue")
# text(shoot_pcs[361,1:2], labels = "361", pos = 1, col="blue")
# text(shoot_pcs[517,1:2], labels = "517", pos = 1, col="blue")
# text(shoot_pcs[600,1:2], labels = "600", pos = 1, col="blue")
# text(shoot_pcs[318,1:2], labels = "318", pos = 1, col="blue")
# text(shoot_pcs[146,1:2], labels = "146", pos = 1, col="blue")
# text(shoot_pcs[359,1:2], labels = "359", pos = 1, col="blue")
# text(shoot_pcs[88,1:2], labels = "88", pos = 1, col="blue")
# text(shoot_pcs[36,1:2], labels = "36", pos = 1, col="blue")

# alternatively, choose representative images based on range of values for PC1/PC2
n <- 5
ideal <- seq(min(shoot_pcs$PC1),max(shoot_pcs$PC1),(max(shoot_pcs$PC1)-min(shoot_pcs$PC1))/(n-1))
result <- sapply(ideal, function(x) shoot_pcs$PC1[which.min(abs(shoot_pcs$PC1-x))])