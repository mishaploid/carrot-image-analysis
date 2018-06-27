library(qtl)

# read in mqm results
load("~/Dropbox/carrot-image-analysis/qtl_mapping/results/mqm_results.RData")

# read in permutations
# note: need to add 0 before single digit result names
setwd("~/Dropbox/carrot-image-analysis/qtl_mapping/")
filelist <- list.files(path="./osg_results", pattern = "perm_combined*", 
                       recursive=TRUE, full.names=TRUE)
filelist <- sort(filelist)
df_list <- lapply(filelist, function(x) source(x))

# assign trait names
traits <- read.csv("trait_ids.csv")
names(df_list) <- traits$traits

cbPalette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## plot hand measurements
setEPS()
postscript("./results/manual_qtl.eps", height=4.5, width=6)

plot(mqm_backw[[1]], mqm_backw[[4]], mqm_backw[[3]], col=cbPalette[c(1,3,4)], bandcol="#EBEBEB",
     ylab="LOD")
plot(mqm_backw[[2]], mqm_backw[[6]], add=TRUE, col=cbPalette[c(2,7)])
add.threshold(mqm_backw[[1]], perms = df_list[[1]][[1]], alpha = 0.05, col = cbPalette[1])
add.threshold(mqm_backw[[4]], perms = df_list[[4]][[1]], alpha = 0.05, col = cbPalette[3])
add.threshold(mqm_backw[[3]], perms = df_list[[3]][[1]], alpha = 0.05, col = cbPalette[4])
add.threshold(mqm_backw[[2]], perms = df_list[[2]][[1]], alpha = 0.05, col = cbPalette[2])
add.threshold(mqm_backw[[6]], perms = df_list[[6]][[1]], alpha = 0.05, col = cbPalette[7])
add.threshold(mqm_backw[[1]], perms = df_list[[1]][[1]], alpha = 0.01, col = cbPalette[1], lty=2)
add.threshold(mqm_backw[[4]], perms = df_list[[4]][[1]], alpha = 0.01, col = cbPalette[3], lty=2)
add.threshold(mqm_backw[[3]], perms = df_list[[3]][[1]], alpha = 0.01, col = cbPalette[4], lty=2)
add.threshold(mqm_backw[[2]], perms = df_list[[2]][[1]], alpha = 0.01, col = cbPalette[2], lty=2)
add.threshold(mqm_backw[[6]], perms = df_list[[6]][[1]], alpha = 0.01, col = cbPalette[7], lty=2)

legend("topright", c("shoot height", "shoot biomass", "leaf number", "root length", "root biomass"),
       col=cbPalette[c(1,3,4,2,7)], lwd=2, ncol=1, bg="white", cex=0.75)

dev.off()

## plot image measurements with manual analogs
setEPS()
postscript("./results/image_qtl.eps", height=4.5, width=6)

plot(mqm_backw[[21]], mqm_backw[[33]], mqm_backw[[30]], col=cbPalette[c(1,3,4)], bandcol="#EBEBEB",
     ylab="LOD")
plot(mqm_backw[[28]], mqm_backw[[32]], add=TRUE, col=cbPalette[c(2,7)])
add.threshold(mqm_backw[[21]], perms = df_list[[21]][[1]], alpha = 0.05, col = cbPalette[1])
add.threshold(mqm_backw[[33]], perms = df_list[[33]][[1]], alpha = 0.05, col = cbPalette[3])
add.threshold(mqm_backw[[30]], perms = df_list[[30]][[1]], alpha = 0.05, col = cbPalette[4])
add.threshold(mqm_backw[[28]], perms = df_list[[28]][[1]], alpha = 0.05, col = cbPalette[2])
add.threshold(mqm_backw[[32]], perms = df_list[[32]][[1]], alpha = 0.05, col = cbPalette[7])
add.threshold(mqm_backw[[21]], perms = df_list[[21]][[1]], alpha = 0.01, col = cbPalette[1], lty=2)
add.threshold(mqm_backw[[33]], perms = df_list[[33]][[1]], alpha = 0.01, col = cbPalette[3], lty=2)
add.threshold(mqm_backw[[30]], perms = df_list[[30]][[1]], alpha = 0.01, col = cbPalette[4], lty=2)
add.threshold(mqm_backw[[28]], perms = df_list[[28]][[1]], alpha = 0.01, col = cbPalette[2], lty=2)
add.threshold(mqm_backw[[32]], perms = df_list[[32]][[1]], alpha = 0.01, col = cbPalette[7], lty=2)

legend("topright", c("shoot height", "shoot area", "leaf number", "root length", "root area"),
       col=cbPalette[c(1,3,4,2,7)], lwd=2, ncol=1, bg="white", cex=0.75)

dev.off()

## plot predictions from image extracted traits
setEPS()
postscript("./results/predicted_qtl.eps", height=4.5, width=6)

plot(mqm_backw[[31]], mqm_backw[[29]], mqm_backw[[34]], col=cbPalette[c(1,3,4)], bandcol="#EBEBEB",
     ylab="LOD", ylim=c(0,22))
plot(mqm_backw[[35]], mqm_backw[[27]], mqm_backw[[37]], add=TRUE, col=cbPalette[c(6,8,2)])
plot(mqm_backw[[38]], mqm_backw[[39]], add=TRUE, col=cbPalette[c(7,5)])

add.threshold(mqm_backw[[31]], perms = df_list[[31]][[1]], alpha = 0.05, col = cbPalette[1])
add.threshold(mqm_backw[[29]], perms = df_list[[29]][[1]], alpha = 0.05, col = cbPalette[3])
add.threshold(mqm_backw[[34]], perms = df_list[[34]][[1]], alpha = 0.05, col = cbPalette[4])
add.threshold(mqm_backw[[35]], perms = df_list[[35]][[1]], alpha = 0.05, col = cbPalette[6])
add.threshold(mqm_backw[[27]], perms = df_list[[27]][[1]], alpha = 0.05, col = cbPalette[8])
add.threshold(mqm_backw[[37]], perms = df_list[[37]][[1]], alpha = 0.05, col = cbPalette[2])
add.threshold(mqm_backw[[38]], perms = df_list[[38]][[1]], alpha = 0.05, col = cbPalette[7])
add.threshold(mqm_backw[[39]], perms = df_list[[39]][[1]], alpha = 0.05, col = cbPalette[5])
add.threshold(mqm_backw[[29]], perms = df_list[[29]][[1]], alpha = 0.01, col = cbPalette[1], lty=2)
add.threshold(mqm_backw[[31]], perms = df_list[[31]][[1]], alpha = 0.01, col = cbPalette[3], lty=2)
add.threshold(mqm_backw[[34]], perms = df_list[[34]][[1]], alpha = 0.01, col = cbPalette[4], lty=2)
add.threshold(mqm_backw[[35]], perms = df_list[[35]][[1]], alpha = 0.01, col = cbPalette[6], lty=2)
add.threshold(mqm_backw[[27]], perms = df_list[[27]][[1]], alpha = 0.01, col = cbPalette[8], lty=2)
add.threshold(mqm_backw[[37]], perms = df_list[[37]][[1]], alpha = 0.01, col = cbPalette[2], lty=2)
add.threshold(mqm_backw[[38]], perms = df_list[[38]][[1]], alpha = 0.01, col = cbPalette[7], lty=2)
add.threshold(mqm_backw[[39]], perms = df_list[[39]][[1]], alpha = 0.01, col = cbPalette[5], lty=2)

legend("topright", c("petiole length", "petiole width", "shoot PC1", "shoot PC2", 
                     "root width", "root PC1", "root PC2", "root PC3"),
       col=cbPalette[c(1,3,4,6,8,2,7,5)], lwd=2, ncol=2, bg="white", cex=0.75)

dev.off()

# plot extra traits - #8-26
# exclude BB traits - 13 & 14, 20 & 21, 27 & 28
# 8 - whole eccentricity
# 9 - whole_Equivalent diameter
# 10 - whole Euler number
# 11 - whole perimeter
# 12 - solidity
# 15 - top eccentricity
# 16 - top equiv diameter
# 17 - top Euler number
# 18 - top perimeter
# 19 - top solidity
# 22 - bottom eccentricity
# 23 - bottom equiv diameter
# 24 - bottom Euler number
# 25 - bottom perimeter
# 26 - bottom solidity 
plot(mqm_backw[[8]], mqm_backw[[9]], mqm_backw[[10]])

# ------------------------------------------------------------------------------
# fetch significant markers and LOD intervals
# ------------------------------------------------------------------------------
# significant markers
sig.markers <- list()

for (i in 1:39){
  tryCatch({
  sig.markers[[i]] <- mqmfind.marker(topsize.fill, mqmscan = mqm_backw[[i]], 
                                     perm = df_list[[i]][[1]], alpha = 0.01)
  }, error=function(e){cat("ERROR:", conditionMessage(e), "\n")})
}

# add filler for RPC3
sig.markers[[39]] <- "NULL"

names(sig.markers) <- traits$traits

sig.markers <- lapply(sig.markers, as.data.frame)

result2 <- NULL 
out <- list() 

for (i in 1:39){
  tryCatch({
    result <- mqmextractmarkers(mqm_backw[[i]])
    for (j in 1:length(sig.markers[[i]]$chr)){
      chr <- sig.markers[[i]]$chr[j]
      lodint.result <- lodint(result, paste(chr), drop=1.5)
      result2 <- rbind(result2, lodint.result)
    }
    out[[i]] <- result2
    result2 <- NULL
  }, error=function(e){cat("ERROR:", conditionMessage(e), "\n")})
}

rm(topsize)
rm(topsize.fill)

library(data.table)
out <- lapply(out, as.data.frame)
test <- rbindlist(out, idcol=TRUE)


# ------------------------------------------------------------------------------
# additive and dominance effects
# ------------------------------------------------------------------------------
library(qtl)

setwd("~/Dropbox/carrot-image-analysis/qtl_mapping/data/")

# ------------------------------------------------------------------------------
# read in data using read.cross function
# ------------------------------------------------------------------------------
topsize <- read.cross(file="input2.csv", format="csv", estimate.map=FALSE,
                      genotypes = c("a", "h", "b"), alleles = c("a", "b"),
                      na.strings = c("-", "NA"))  

# jitter marker positions
topsize <- jittermap(topsize)

# calculate probabilities of true underlying genotypes given marker data
# allows for genotyping errors
topsize <- calc.genoprob(topsize, step = 1, map.function = "kosambi", error.prob = 0.001) 

topsize <- sim.geno(topsize, step=1, map.function="kosambi", error.prob=0.001)
topsize <- calc.genoprob(topsize, step=1, error.prob=0.001, map.function="kosambi")

topsize.fill <- fill.geno(topsize, method = "argmax")
topsize.fill <- calc.genoprob(topsize.fill, step = 1, error.prob = 0.001) # recalculate genoprob
topsize.fill<- sim.geno(topsize.fill, step=1, map.function="kosambi", error.prob=0.001)


effects_ht <- effectscan(cross=topsize.fill, pheno.col=c("height"))
effects[c("S2_43085743", "S7_20387007"),]



# 
# # ------------------------------------------------------------------------------
# # MQM effect plots
# # ------------------------------------------------------------------------------
# 
# # plot LOD curves with indication of sign of estimated QTL effects
# # create directed QTL plot for each trait 
# for(i in 1:8){
#   dirresults[[i]] <- mqmplot.directedqtl(topsize.fill, mqm_backw[[i]], 
#                                          pheno.col = i+1)
# }
# 
# real_markers <- mqmextractmarkers(mqm_backw[[1]])
# max(mqm_backw[[1]])
# mname <- find.marker(topsize.fill, 2, 135)
# 
# 
# # marker effect plots
# # height
# png("height_ch2.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S2_42514453", pheno.col = "height", 
#         ylab = "Height (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S2_42514453", pheno.col = "height")
# aggregate(t$pheno ~ t$S2_42514453, FUN = mean)
# 
# png("height_ch7.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S7_34233365", pheno.col = "height",
#         ylab = "Height (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S7_34233365", pheno.col = "height")
# aggregate(t$pheno ~ t$S7_34233365, FUN = mean)
# 
# # root length
# png("rootLength_ch1.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S1_36873754", pheno.col = "storageRoot",
#         ylab = "Root length (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S1_36873754", pheno.col = "storageRoot")
# aggregate(t$pheno ~ t$S1_36873754, FUN = mean)
# 
# png("rootLength_ch2.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S2_42514453", pheno.col = "storageRoot",
#         ylab = "Root length (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S2_42514453", pheno.col = "storageRoot")
# aggregate(t$pheno ~ t$S2_42514453, FUN = mean)
# 
# png("rootLength_ch3.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S3_39522852", pheno.col = "storageRoot",
#         ylab = "Root length (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S3_39522852", pheno.col = "storageRoot")
# aggregate(t$pheno ~ t$S3_39522852, FUN = mean)
# 
# png("rootLength_ch4.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S4_13990251", pheno.col = "storageRoot",
#         ylab = "Root length (cm)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S4_13990251", pheno.col = "storageRoot")
# aggregate(t$pheno ~ t$S4_13990251, FUN = mean)
# 
# # leaf number
# png("leafNumber_ch2.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S2_42354283", pheno.col = "leafNum",
#         ylab = "Leaf number")
# dev.off()
# t <- plot.pxg(topsize.fill, "S2_42354283", pheno.col = "leafNum")
# aggregate(t$pheno ~ t$S2_42354283, FUN = mean)
# 
# # shoot biomass
# png("shootBio_ch1", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S1_51097991", pheno.col = "flw",
#         ylab = "Shoot biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S1_51097991", pheno.col = "flw")
# aggregate(t$pheno ~ t$S1_51097991, FUN = mean)
# 
# png("shootBio_ch2.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S2_42514453", pheno.col = "flw",
#         ylab = "Shoot biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S2_42514453", pheno.col = "flw")
# aggregate(t$pheno ~ t$S2_42514453, FUN = mean)
# 
# png("shootBio_ch3.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S3_31275436", pheno.col = "flw",
#         ylab = "Shoot biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S3_31275436", pheno.col = "flw")
# aggregate(t$pheno ~ t$S3_31275436, FUN = mean)
# 
# png("shootBio_ch7.png", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S7_29080627", pheno.col = "flw",
#         ylab = "Shoot biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S7_29080627", pheno.col = "flw")
# aggregate(t$pheno ~ t$S7_29080627, FUN = mean)
# 
# # root biomass
# png("rootBio_ch2", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S2_13447591", pheno.col = "frw",
#         ylab = "Root biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S2_13447591", pheno.col = "frw")
# aggregate(t$pheno ~ t$S2_13447591, FUN = mean)
# 
# png("rootBio_ch7", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S7_29080627", pheno.col = "frw",
#         ylab = "Root biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S7_29080627", pheno.col = "frw")
# aggregate(t$pheno ~ t$S7_29080627, FUN = mean)
# 
# png("rootBio_ch8", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S8_1708467", pheno.col = "frw",
#         ylab = "Root biomass (g)")
# dev.off()
# t <- plot.pxg(topsize.fill, "S8_1708467", pheno.col = "frw")
# aggregate(t$pheno ~ t$S8_1708467, FUN = mean)
# 
# # color
# png("color_ch4", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S4_13990251", pheno.col = "skinColor",
#         ylab = "Color")
# dev.off()
# t <- plot.pxg(topsize.fill, "S4_13990251", pheno.col = "skinColor")
# aggregate(t$pheno ~ t$S4_13990251, FUN = mean)
# 
# png("color_ch7", height = 5, width = 5, units = "in", res = 600)
# plotPXG(topsize.fill, "S7_33957928", pheno.col = "skinColor",
#         ylab = "Color")
# dev.off()
# t <- plot.pxg(topsize.fill, "S7_33957928", pheno.col = "skinColor")
# aggregate(t$pheno ~ t$S7_33957928, FUN = mean)
# 
# 
# # investigate possible epistatic interactions
# effectplot(topsize.fill, mname1 = "S2_42354283", mname2 = "S7_34233365",
#            pheno.col = "height")
# 
# 
# # ------------------------------------------------------------------------------
# # try a few more traits?
# # ------------------------------------------------------------------------------
# 
# mqm_imp5 <- mqmscan(topsize.fill, pheno.col = 1:5, n.cluster = 2)
# 
# mqmplot.multitrait(mqm_imp5, type = "image")
# 
# cofactorlist <- mqmsetcofactors(topsize.fill, 3) # set auto cofactors every third marker
# 
# # this does take some time
# mqm_imp5 <- mqmscan(topsize.fill, pheno.col = 2:5, cofactors = cofactorlist,
#                     n.cluster = 2)
# 
# mqmplot.multitrait(mqm_imp5, type = "lines")
# 
# # circle plot
# mqmplot.circle(topsize, mqm_imp5)
# 
# # highlight second trait
# mqmplot.circle(topsize, mqm_imp5, highlight = 1)
# 
# 
# # interval estimates
# lodint(mqm_backw_low, chr=2)
# 
# lodint(test$value, chr=4, drop=1.5, expandtomarkers=TRUE)
# 
# lodint(mqm_auto[[8]], chr = 7)
# 
# 
# # marker summary
# mapthis <- pull.map(topsize.fill)
# summaryMap(mapthis)
