#load libraries
library(qtl)

args <- commandArgs(TRUE)
#create constants
pheno.col <- as.integer(args[1])
n.draws <- 64
inputcmd <- paste("tar -xzf ", "perm_combined_", pheno.col, ".tar.gz", sep="")
system(inputcmd)

###########################
#processimppermresults
input.filenames <- list.files(pattern="^perm_part_[0-9]+_[0-9]+\\.Rdat$")
# We need to read in the first results into the vector before assigning a
# class.  Once the vector has the correct class, the proper c() function will
# be dispatched for the remaining results.
all.results <- c(dget(input.filenames[1]))
class(all.results) <- c("scanoneperm", "list")
if (length(input.filenames) > 1) {
    for (i in input.filenames[2:length(input.filenames)]) {
        results <- dget(i)
        all.results <- c(all.results, results)
    }
}
output.filename <- paste("perm_combined_", pheno.col, ".Rdat", sep="")
dput(all.results, output.filename)
output.filename <- paste("perm_summary_", pheno.col, ".txt", sep="")
sink(output.filename)
print(summary(all.results, alpha=c(0.01, 0.05, 0.10)))
# print(calc.penalties(all.results, alpha=c(0.05, 0.10, 0.20)))
sink()


##################
#simgeno
# Load the R/qtl cross data file.  These first few steps are the same as those
# in run_impperm_test.
cross <- read.cross("csv", file="input.csv", estimate.map=FALSE, genotypes=c("a","h","b"), alleles=c("a","b"), na.strings=c("-","NA"))

# Some markers are at the same coordinates, so apply a jittermap
cross <- jittermap(cross)

# Uses HMMs to calculate the probabilities of the underlying genotypes given
# the observed multipoint marker data, with allowance for genotyping errors.
cross <- calc.genoprob(cross, step=1, map.function="kosambi", error.prob=0.001)

# Create a grid of the recombination fractions for all pairs of markers, or of
# the LOD scores between markers.
# cross <- est.rf(cross)

# Simulate the imputed genotypes
results <- sim.geno(cross, map.function="kosambi", step=1, n.draws=n.draws, error.prob=0.001)
results <- calc.genoprob(results, step=1, error.prob=0.001, map.function="kosambi")

# fill missing data with 'argmax'
# for each individual, most probable sequence of genotypes is used
results <- fill.geno(cross, method="argmax")
results <- calc.genoprob(results, step=1, error.prob=0.001, map.function="kosambi")

output.filename <- paste("fill_geno_results_", pheno.col, ".Rdat", sep="")
dput(results, output.filename)


##################
#findmodel
# Load the imputation permutation test results
input.filename <- paste("perm_combined_", pheno.col, ".Rdat", sep="")
perm.results <- dget(input.filename)

# Load the simulated imputed genotypes results
input.filename <- paste("fill_geno_results_", pheno.col, ".Rdat", sep="")
fill.geno.results <- dget(input.filename)

# Calculate the pLOD
# penalties <- calc.penalties(perm.results)

# Look for the best model
# results <- stepwiseqtl(fill.geno.results, pheno.col=pheno.col, method="hk",
#                        penalties=penalties, keeplodprofile=TRUE, keeptrace=TRUE)

# set cofactors every 5 cm
setcofactors <- mqmsetcofactors(results, 5)

# run MQM
results <- mqmscan(fill.geno.results, setcofactors, pheno.col=pheno.col, cofactor.significance=0.001)

output.filename <- paste("find_model_results_", pheno.col, ".Rdat", sep="")
dput(results, output.filename)

#### save workspace image
save.image(file=paste("workspace_image_", pheno.col, ".RData", sep=""))

##################
#makeqtl
# Load the imputation permutation test results
input.filename <- paste("fill_geno_results_", pheno.col, ".Rdat", sep="")
fill.geno.results <- dget(input.filename)
input.filename <- paste("perm_combined_", pheno.col, ".Rdat", sep="")
perm.results <- dget(input.filename)

# Load the results from finding the best QTL model
input.filename <- paste("find_model_results_", pheno.col, ".Rdat", sep="")
find.model.results <- dget(input.filename)
#CHECK LENGTH, if 0 then no qtl...quit!!!!!
if(length(find.model.results) < 1) {
	quit(210)
}

# Make the QTL object
results <- makeqtl(fill.geno.results, chr=find.model.results$chr,
                   pos=find.model.results$pos, what="draws")

markers <- mqmfind.marker(fill.geno.results, mqmscan=results, perm=perm.results, alpha=0.01)
output.filename <- paste("qtl_", pheno.col, ".Rdat", sep="")
dput(markers, output.filename)

##################
#refineqtl
# Load the imputation permutation test results
input.filename <- paste("fill_geno_results_", pheno.col, ".Rdat", sep="")
fill.geno.results <- dget(input.filename)

# Load the results from finding the best QTL model, and extract its formula.
input.filename <- paste("find_model_results_", pheno.col, ".Rdat", sep="")
find.model.results <- dget(input.filename )
formula <- mqmgetmodel(find.model.results)

# Load the QTL object
input.filename <- paste("qtl_", pheno.col, ".Rdat", sep="")
qtl.results <- dget(input.filename)

# Refine positions of the QTL
results <- refineqtl(sim.geno.results, pheno.col=pheno.col, qtl=qtl.results,
                     verbose=FALSE, method="hk", formula=formula)
output.filename <- paste("refined_qtl_", pheno.col, ".Rdat", sep="")
dput(results, output.filename)

output.filename <- paste("refined_qtl_summary_", pheno.col, ".txt", sep="")
sink(output.filename)
for (i in 1:(results$n.qtl)) {
    cat("Bayesian credible interval for QTL", i, "\n")
    print(bayesint(results=results, qtl.index=i, prob=0.95, expandtomarkers=TRUE))
    cat("\n")
}
sink()

lod.profile <- attr(results, "lodprofile")
dir.create("lod_profiles")
for (i in names(lod.profile)) {
    filename <- paste("lod_profiles/", i, ".csv", sep="")
    write.csv(lod.profile[[i]], filename)
}




##################
#fitqtl
# Load the imputation permutation test results
input.filename <- paste("sim_geno_results_", pheno.col, ".Rdat", sep="")
sim.geno.results <- dget(input.filename)

# Load the results from finding the best QTL model, and extract its formula.
input.filename <- paste("find_model_results_", pheno.col, ".Rdat", sep="")
find.model.results <- dget(input.filename)
formula <- formula(find.model.results)

# Load the QTL object
input.filename <- paste("refined_qtl_", pheno.col, ".Rdat", sep="")
refined.qtl.results <- dget(input.filename)

# Refine positions of the QTL
results <- fitqtl(sim.geno.results, pheno.col=pheno.col,
                  qtl=refined.qtl.results, formula=formula, method="hk", get.ests=TRUE)
output.filename <- paste("fit_qtl_results_", pheno.col, ".Rdat", sep="")
dput(results, output.filename )


##################
#create graph...
