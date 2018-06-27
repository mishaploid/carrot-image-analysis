# ------------------------------------------------------------------------------
# Image analysis correlations
# S. Turner
# 13 September 2017; updated 24 April 2018
# ------------------------------------------------------------------------------

# this script plots correlations between hand and image-measured traits

library(Hmisc)
library(corrplot)
library(reshape2)
library(ggplot2)
library(GGally)
library(ggthemes)
library(car)
library(gplots)
library(viridis)
library(psych)
library(cowplot)


setwd("~/Dropbox/carrot-image-analysis/data")

# load functions for plotting
source("~/Dropbox/carrot-image-analysis/scripts/correlation_functions.R")

merged <- read.csv("merged_data.csv", header=TRUE)

# quick look at correlations among all measurements
ggcorr(merged[,c(5:11, 14:33)], method = c("complete.obs", "pearson"))

# create a nice heatmap
# subset data
merged2 <- merged[,c(1,5,7:9,6,10:11, 23:27, 31, 30, 32:36, 40, 39, 41:43, 45,44)]
str(merged2)

# matrix of correlations
merged_mat <- cor(merged2[,2:27], use="complete.obs", method="pearson")

# draw heatmap
cols <- c(rep("black",7), rep("grey50",14), rep("grey75",5))
text_col <- c(rep("#009E73",4), rep("#D55E00",3), rep("#009E73",7),
              rep("#D55E00",7), rep("#009E73",4), rep("#D55E00",1))

pdf("~/Dropbox/carrot-image-analysis/draft figures/FigS1.pdf", height=12, width=12)
heatmap.2(merged_mat, Rowv=NA, Colv=NA, dendrogram="none", symm=TRUE,
          trace="none", col=rev(viridis(100)), rowsep=c(7,21), colsep=c(7,21),
          RowSideColors=cols, ColSideColors=cols, colRow=text_col, colCol=text_col,
          key=TRUE, density.info="none", margins=c(10,10), cellnote=combinedMat)
# legend("top", legend = c("hand", "classic image", "carrot algorithm"), 
#        col = c("black", "grey50", "grey75"), lty=1, lwd=10, bty="n", cex=1)
dev.off()

#-------------------------------------------------------------------------------
# shoot height, root length, and biomass (Figure 3)

merged_sub <- subset(merged, select = c("image_name", "height", "top_BoundingBox.3",
                                        "flw", "shootBio", 
                                        "tapRoot", "bottom_BoundingBox.3",
                                        "frw", "rootBio"))
# simple version
ggpairs(data = merged_sub[,2:9])

## add new variable names for pretty plotting
str(merged_sub)
colnames(merged_sub) <- c("image_name", "S_height", "S_BB", "S_biomass", "S_area", 
                          "R_length","R_BB", "R_biomass", "R_area")

g <- ggpairs(data = merged_sub[2:9], 
             upper = list(continuous = my_custom_cor),
             lower = list(continuous = my_fn)) + 
  theme_few() + theme(panel.spacing = unit(0.1, "lines"))
g

ggsave("~/Dropbox/carrot-image-analysis/figures/Fig3_part1.eps", 
       width = 10, height = 8, units = "in")

#-------------------------------------------------------------------------------
# leaf number, petiole width, & petiole length

leaf_sub <- subset(merged, select = c("leafNum", "Petiole_Count", "Petiole_Width",
                                      "Petiole_Length"))

# read in ground truth measurements from ImageJ (n=100)
pet <- read.csv("petiole_imageJ.csv", header = TRUE)
# pet$image_name <- gsub(".NEF", "", pet$image_name)
# convert pixels to cm (50.8 px/cm)
pet[,2:3] <- pet[,2:3]/50.8 

petioles <- merge(pet[,c(1:3)], merged[,c(1,7,41:43)], all=TRUE, by="image_name")

# leaf count
n <- qplot(leafNum, Petiole_Count, data=petioles,
           xlab = "Leaf number (algorithm)", 
           ylab = "Leaf Number (manual)",
           xlim = c(0,35), ylim = c(0,35),
           geom="point", shape="b") +
  scale_shape_manual(values=16) +
  geom_smooth(method="lm", col="#009e73") +
  theme(legend.position="none")

# test correlations
cor.test(petioles$leafNum, petioles$Petiole_Count, method = "pearson")
# 0.7701123
# p-value < 2.2e-16
cor.test(petioles$leafNum, petioles$Petiole_Count, method = "spearman")
# rho = 0.8416443
# p-value < 2.2e-16

# petiole length
l <- qplot(pet_length, Petiole_Length, data=petioles,
           xlab = "Petiole length (algorithm; cm)", ylab = "Petiole length (manual; cm)",
           xlim=c(5,45), ylim=c(5,45), geom="point", shape="b") +
  scale_shape_manual(values=17) + 
  geom_smooth(method="lm", col="#009e73") +
  theme(legend.position="none")

cor.test(petioles$pet_length, petioles$Petiole_Length, method = "pearson")
# 0.9013005
# p-value < 2.2e-16
cor.test(petioles$pet_length, petioles$Petiole_Length, method = "spearman")
# 0.9065107
# p-value < 2.2e-16

# petiole width
w <- qplot(pet_width, Petiole_Width, data=petioles,
           xlab = "Petiole width (algorithm; cm)", ylab = "Petiole width (manual; cm)",
           xlim = c(0.25,0.6), ylim = c(0.25, 0.6), geom="point", shape="b") +
  scale_shape_manual(values=15) +
  geom_smooth(method="lm", col="#009e73") +
  theme(legend.position="none")

cor.test(petioles$pet_width, petioles$Petiole_Width, method = "pearson")
# 0.8459182
# p-value < 2.2e-16
cor.test(petioles$pet_width, petioles$Petiole_Width, method = "spearman")
# 0.8640864
# p-value < 2.2e-16

# combine them! 
theme_set(theme_cowplot(font_size=10))
plot_grid(n, l, w, ncol=3)
ggsave("~/Dropbox/carrot-image-analysis/draft figures/Fig4.eps", 
       height=60, width = 180, units = "mm")

### correlation of pcs with shoot and root shape
cor.test(merged$S_PC1, merged$flw)
detach("package:cowplot", unload=TRUE)

qplot(S_PC1, flw, data=merged,
      xlab = "Shoot PC1", ylab = "Shoot biomass (g)",
      geom="point", xlim=c(-50000, 25000)) +
  geom_smooth(method="lm", col="#009e73") +
  theme(legend.position="none", 
        text=element_text(family="Arial", size=12),
        axis.text=element_text(family="Arial", size=10))

ggsave("~/Dropbox/carrot-image-analysis/figures/spc1_flw.eps", 
       height=80, width = 85, units = "mm")

qplot(S_PC2, height, data=merged,
      xlab = "Shoot PC2", ylab = "Height (cm)",
      geom="point") +
  geom_smooth(method="lm", col="#009e73") +
  theme(legend.position="none", 
        text=element_text(family="Arial", size=12),
        axis.text=element_text(family="Arial", size=10))

ggsave("~/Dropbox/carrot-image-analysis/figures/spc2_height.eps", 
       height=80, width = 85, units = "mm")


cor.test(merged$S_PC1, merged$flw, method="pearson")
cor.test(merged$S_PC1, merged$flw, method="spearman")
# r=-0.74
# p=-0.79
# P < 2.2e-16

cor.test(merged$S_PC2, merged$height, method="pearson")
cor.test(merged$S_PC2, merged$height, method="spearman")
# r=-0.72
# p=-0.79
# P < 2.2e-16