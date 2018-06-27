# ------------------------------------------------------------------------------
# MQM for carrot top size traits
# S. Turner
# 21 March 2017 - updated 29 November 2017
# ------------------------------------------------------------------------------

library(qtl)

setwd("~/Dropbox/carrot-image-analysis/qtl_mapping/data/")

# ------------------------------------------------------------------------------
# read in data using read.cross function
# ------------------------------------------------------------------------------
topsize <- read.cross(file="input2.csv", format="csv", estimate.map=FALSE,
                      genotypes = c("a", "h", "b"), alleles = c("a", "b"),
                      na.strings = c("-", "NA"))  

# summary of linkage map
map_summary <- summaryMap(topsize)
write.csv(map_summary, "../results/map_summary.csv")

plotMap(topsize)

# jitter marker positions
topsize <- jittermap(topsize)

# calculate probabilities of true underlying genotypes given marker data
# allows for genotyping errors
topsize <- calc.genoprob(topsize, step = 1, map.function = "kosambi", error.prob = 0.001) 

# visualize missing genotype data & number of heterozygotes/homozygotes
geno.image(topsize) # aa = red, ab = blue, bb = green, white = missing 

# ------------------------------------------------------------------------------
# fill in missing data using 'argmax' 
# for each individual, most probable sequence of genotypes, given observed data
# is used
# ------------------------------------------------------------------------------
topsize <- sim.geno(topsize, step=1, map.function="kosambi", error.prob=0.001)
topsize <- calc.genoprob(topsize, step=1, error.prob=0.001, map.function="kosambi")

topsize.fill <- fill.geno(topsize, method = "argmax")
topsize.fill <- calc.genoprob(topsize.fill, step = 1, error.prob = 0.001) # recalculate genoprob

geno.image(topsize.fill) # check genotypes

# plot recombination fraction
png("~/Dropbox/carrot-image-analysis/qtl_mapping/rf_plot.png", height=5, width=6,
    units="in", res=300)
plotRF(topsize.fill) # check recombination fraction
dev.off()

# plot missing genotypes, genetic map, missing individuals, and trait distributions
# e.g. shoot height
plot(topsize.fill, pheno.col=2) 

# ------------------------------------------------------------------------------
# build model by unsupervised backward elimination
# (selects cofactors automatically)
# ------------------------------------------------------------------------------
# create a list of cofactors for backward elimination (mqmautocofactors)
# accounts for underlying marker density (for data with few individuals)
# sets cofactors at every 5th marker (mqmsetcofactors)
# places additional cofactor at chr 1

# set list of cofactors, 50 throughout map
autocofactors <- mqmautocofactors(topsize.fill, 50)

# run mqm for auto cofactors
mqm_auto <- mqmscan(topsize.fill, autocofactors, pheno.col = 2:40)

# set cofactors ever 5 cm
setcofactors <- mqmsetcofactors(topsize.fill, 5)

# run backwards selection, add high significance threshold
mqm_backw <- mqmscan(topsize.fill, setcofactors, pheno.col = 2:40,
                      cofactor.significance = 0.001) # can change significance 

# mqm_backw <- mqmscan(topsize.fill, setcofactors, pheno.col = "height",
#                      cofactor.significance = 0.001) # can change significance 

save(mqm_auto, mqm_backw, file="~/Dropbox/carrot-image-analysis/qtl_mapping/results/mqm_results.RData")

# visual inspection of initial models
par(mfrow = c(2,1))
mqmplot.cofactors(topsize.fill, autocofactors, justdots = TRUE, 
                  main = "Genetic map: auto cofactors")
mqmplot.cofactors(topsize.fill, setcofactors, justdots = TRUE,
                  main = "Genetic map: cofactor every 5 cM")

# plot results for marker locations
par(mfrow = c(2,1))
plot(mqmgetmodel(mqm_auto[[1]]))
plot(mqmgetmodel(mqm_backw[[1]])) # fewer significant markers, may go with this



