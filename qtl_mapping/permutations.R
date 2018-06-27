# ------------------------------------------------------------------------------
# Permutation tests for MQM
# S. Turner
# 22 March 2017 - updated on 29 November 2017
# ------------------------------------------------------------------------------

library(qtl)
setwd("~/Dropbox/carrot-image-analysis/qtl_mapping/data/")

# ------------------------------------------------------------------------------
# read in data using read.cross function
# ------------------------------------------------------------------------------
cross <- read.cross(file="input.csv", format="csv", estimate.map=FALSE,
                      genotypes = c("a", "h", "b"), alleles = c("a", "b"),
                      na.strings = c("-", "NA"))  


cross <- jittermap(cross)
cross <- calc.genoprob(cross, step = 1, error.prob = 0.001,
                       map.function="kosambi") 

# ------------------------------------------------------------------------------
# fill in missing data using 'argmax' 
# for each individual, most probably sequence of genotypes, given observed data
# is used
# ------------------------------------------------------------------------------
cross <- est.rf(cross)
cross.hk <- fill.geno(cross, method = "argmax")
cross.hk <- calc.genoprob(cross.hk, step = 1, error.prob=0.001,
                          map.function="kosambi") # recalculate genoprob

# ------------------------------------------------------------------------------
# build model by unsupervised backward elimination
# (selects cofactors automatically)
# ------------------------------------------------------------------------------
# create a list of cofactors for backward elimination (mqmautocofactors)
# accounts for underlying marker density (for data with few individuals)
# sets cofactors at every 5th marker (mqmsetcofactors)
# places additional cofactor at chr 1
# run mqm for auto cofactors
# set cofactors ever 5 cm
setcofactors <- mqmsetcofactors(cross.hk, 5)

# ------------------------------------------------------------------------------
# QTL significance - permutation tests!
# ------------------------------------------------------------------------------
# set to 10000 permutations for publication

results <- mqmpermutation(cross.hk, scanfunction = mqmscan, 
                          cofactors = setcofactors, n.perm = 10, 
                          pheno.col = 2,
                          verbose = FALSE)
saveRDS(results, "perm.rds")
