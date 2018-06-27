# ------------------------------------------------------------------------------
#   Carrot Image Analysis - Principal Component Analysis for Root Profile
#   S. Turner & N. Miller
#   12 July 2016; updated 24 April 2018
# ------------------------------------------------------------------------------

# this script runs a principal components analysis on the root profile output
# from the algorithm; output includes the raw PCA results as well as results after
# normalization for length & width

setwd("~/Dropbox/carrot-image-analysis/data")

rootProfile <- read.csv("rootProfile.csv", row.names=1, header=FALSE)
str(rootProfile)
rootProfile <- as.matrix(rootProfile)

# ------------------------------------------------------------------------------
# run PCA and plot results

pcRoot <- prcomp(rootProfile)
root_pcs <- as.data.frame(pcRoot$x[,1:3]) # extract first three principal components
write.csv(root_pcs, "root_pcs_raw.csv", row.names=TRUE)

# screeplot and pairwise correlations among PCs
pdf("~/Dropbox/carrot-image-analysis/results/root_pca_diagnostics.pdf")
plot(pcRoot, type="l")
pairs(root_pcs)
dev.off()

# summarize % variation explained for PCs
summary(pcRoot)$importance[, 1:4]

# ------------------------------------------------------------------------------
# function 'plotRoot' to plot eigenvectors for PCs
# x = principal component, y = matrix used for PCA, z = title of graph

plotRoot <- function(pc_object, x, y, z){
  sweep <- seq(from = -pc_object$sdev[x], to = pc_object$sdev[x], by = 2*pc_object$sdev[x]/2)
  cl <- c("lightgray", "gray50", "black")
  matplot(0, 0, ylim = c(0, 3200), xlim = c(-200, 200), type = "n", xlab = "", ylab = "",
          main = print(z), cex.axis = 1)
  for (i in 1:length(sweep)) {
    root <- sweep[i] * pc_object$rotation[,x] + colMeans(y)
    r <- as.numeric(t(length(root):1))
    matlines(root, r, lwd = 3, col = cl[i])
    matlines(-root, r, lwd = 3, col = cl[i])
  }
}

# plot eigenvectors for non-normalized PCs
pdf("root_pcs_raw.pdf", height=7, width=4.5)
plotRoot(pcRoot, 1, rootProfile, "PC1 - 71.6% variation explained")
plotRoot(pcRoot, 2, rootProfile, "PC2 - 13.3% variation explained")
plotRoot(pcRoot, 3, rootProfile, "PC3 - 6.3% variation explained")
dev.off()

# ------------------------------------------------------------------------------
# normalize root profile by length and width to view differences in shape

N = 1000
rootProfileNormalized <- array(0,c(dim(rootProfile)[1],N)) # create empty array

# populate array with values normalized by length
for(i in 1:dim(rootProfile)[1]) {
  f <- rootProfile[i,] # f = specific genotype
  ind <- which(f == 0) # obs that equal 0
  last <- ind[1]-1
  ind <- seq(from = 1, to = ind[1],1)
  f <- f[ind]
  xi <- seq(from = 1,to = last, by = (last-1)/(N-1))
  fi <- approx(ind, f, xi)
  rootProfileNormalized[i,] <- fi$y
}

# normalize values by width
rootProfileNorm <- t(apply(rootProfileNormalized, 1, 
                           function(x)(x-min(x))/(max(x)-min(x))))

# get rownames (image ids) from original matrix
row.names(rootProfileNorm) <- row.names(rootProfile)

# ------------------------------------------------------------------------------
# re-run PCA and plot results

pcNorm <- prcomp(rootProfileNorm)
norm_pcs <- as.data.frame(pcNorm$x[,1:3]) # extract first three principal components
write.csv(norm_pcs, "root_pcs_normalized.csv", row.names=TRUE)

# screeplot and pairwise correlations among PCs
pdf("root_norm_pca_diagnostics.pdf")
plot(pcNorm, type="l")
pairs(norm_pcs)
dev.off()

# summarize % variation explained for PCs
summary(pcNorm)$importance[, 1:4]

# ------------------------------------------------------------------------------
# function to plot eigenvectors - change ylim for normalized data

plotRoot_norm <- function(pc_object, x, y, z){
  sweep <- seq(from = -pc_object$sdev[x], to = pc_object$sdev[x], by = 2*pc_object$sdev[x]/4)
  cl <- gray.colors(5)
  matplot(0, 0, ylim = c(0, 1000), xlim = c(-1, 1), type = "n", xlab = "", ylab = "",
          main = print(z), cex.axis = 1)
  for (i in 1:length(sweep)) {
    root <- sweep[i] * pc_object$rotation[,x] + colMeans(y)
    r <- as.numeric(t(length(root):1))
    matlines(root, r, lwd = 3, col = cl[i])
    matlines(-root, r, lwd = 3, col = cl[i])
  }
}

# plot eigenvectors for non-normalized PCs
pdf("root_pcs_normalized.pdf", height=7, width=4.5)
plotRoot_norm(pcNorm, 1, rootProfileNorm, "PC1 - 66.4% variation explained")
plotRoot_norm(pcNorm, 2, rootProfileNorm, "PC2 - 16.6% variation explained")
plotRoot_norm(pcNorm, 3, rootProfileNorm, "PC3 - 5.55% variation explained")
dev.off()

# export EPS files
setEPS()
postscript("root_pcs_norm.eps", height=4, width=10)
par(mfrow=c(1,3))
plotRoot_norm(pcNorm, 1, rootProfileNorm, "PC1 - 66.4% variation explained")
plotRoot_norm(pcNorm, 2, rootProfileNorm, "PC2 - 16.6% variation explained")
plotRoot_norm(pcNorm, 3, rootProfileNorm, "PC3 - 5.55% variation explained")
dev.off()

# find example images for the range of variation captured by PC1
plot(norm_pcs$PC2 ~ norm_pcs$PC1)
grid(nx=20, col="gray", lty=1)
# point and click to find equidistant/representative examples
# example_image_numbers <- identify(norm_pcs$PC1, norm_pcs$PC2, col="blue")
# example_image_numbers
# example_images <- norm_pcs[example_image_numbers,]
# write.csv(example_images, "example_root_images.csv")

# plot root profiles
# plot(rootProfile[917,], type = "l", lwd = 2, ylim = c(0, 200))
# lines(rootProfile[2,], lwd = 2, col = "red")
# lines(rootProfile[120,], lwd = 2, col = "blue")

# alternatively, choose representative images based on range of values for PC1/PC2
n <- 5

# for pc3, major outliers - remove for image id
PC3_v2 <- norm_pcs[norm_pcs$PC3 < 5, 3]
ideal <- seq(min(norm_pcs$PC3_v2),max(norm_pcs$PC3_v2),(max(norm_pcs$PC3_v2)-min(norm_pcs$PC3_v2))/(n-1))
result <- sapply(ideal, function(x) norm_pcs$PC1[which.min(abs(norm_pcs$PC1-x))])
