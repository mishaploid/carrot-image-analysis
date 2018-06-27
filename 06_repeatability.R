# ------------------------------------------------------------------------------
#   Calculate repeatability
#   S. Turner 
#   05 March 2017; updated 24 April 2018
# ------------------------------------------------------------------------------
library(lme4)

setwd("~/Dropbox/carrot-image-analysis/data")

# read in diallel data
dialleldata <- read.csv("diallel_repeatability.csv", header = TRUE)
str(dialleldata)
dialleldata$plot <- as.factor(dialleldata$plot)
dialleldata$year <- as.factor(dialleldata$year)
dialleldata$rep <- as.factor(dialleldata$rep)
dialleldata$subsample <- as.factor(dialleldata$subsample)

# read in image data
imagedata <- read.csv("image_data.csv", header = TRUE)

# read in shoot and root PCs
shoot_pcs <- read.csv("shoot_pcs.csv", header=TRUE)
colnames(shoot_pcs) <- c("image_name", "S_PC1", "S_PC2", "S_PC3")
shoot_pcs$image_name <- paste0(shoot_pcs$image_name, ".NEF")
root_pcs <- read.csv("root_pcs_normalized.csv", header=TRUE)
colnames(root_pcs) <- c("image_name", "R_PC1", "R_PC2", "R_PC3")
root_pcs$image_name <- paste0(root_pcs$image_name, ".NEF")

# combine hand and image data
repeatData <- merge(dialleldata, imagedata, by = "image_name", all.x=TRUE)
repeatData <- merge(repeatData, shoot_pcs, by="image_name", all.x=TRUE)
repeatData <- merge(repeatData, root_pcs, by="image_name", all.x=TRUE)

repeatData$plot <- as.factor(repeatData$plot)
repeatData$year <- as.factor(repeatData$year)
repeatData$rep <- as.factor(repeatData$rep)
repeatData$leafNum <- as.numeric(repeatData$leafNum)

# clean data/select columns of interest
repeatData <- repeatData[,c(1:13,25:29,33,32,34:38,42,41,43:53)]
str(repeatData)

# view number of observations by year/rep
table(repeatData$pedigree, repeatData$year:repeatData$rep)

### average across subsamples
newdata <- aggregate(repeatData[,7:38], by=list(repeatData$pedigree, repeatData$year, repeatData$rep), 
                     FUN=mean)
newdata <- as.data.frame(newdata)
colnames(newdata)[1:3] <- c("pedigree", "year", "rep")

# rm <- c("7262A x P0159B", "L7550A x Nbh2189B")
# repeatData <- repeatData[!repeatData$pedigree %in% rm,]
# repeatData <- droplevels(repeatData)

# function to calculate min, max, and repeatability for selected traits
# repeatability <- function(x, df){
repeat_out <- apply(newdata[,4:35], 2, FUN=function(x){
  # temp table with pedigree and trait
  tmp <- as.data.frame(cbind(newdata$pedigree, newdata$year, newdata$rep, x))
  colnames(tmp) <- c("pedigree", "year", "rep", "trait")
  # remove pedigrees with missing phenotypes
  na_omit <- tmp[complete.cases(tmp),]
  # count number of test environments for each line
  n_year <- as.data.frame(table(na_omit$pedigree:na_omit$rep))
  # count reps for each line
  n_rep <- as.data.frame(table(na_omit$pedigree:na_omit$year))
  # note: values '1' and '2' are used for years '2' and '3', respectively
  # because variable is numeric
  n_rep.y2 <- as.data.frame(table(na_omit[na_omit$year=="1",]$pedigree))
  n_rep.y3 <- as.data.frame(table(na_omit[na_omit$year=="2",]$pedigree))
  
  # harmonic means (ignore zeros)
  y_mean <- 1/mean(1/n_year$Freq[n_year$Freq>0])
  r_mean <- 1/mean(1/n_rep$Freq[n_rep$Freq>0])
  r_mean.y2 <- 1/mean(1/n_rep.y2$Freq[n_rep.y2$Freq>0])
  r_mean.y3 <- 1/mean(1/n_rep.y3$Freq[n_rep.y3$Freq>0])
  
  # run the model
  #  formula <- as.formula(paste(x, "~ (1|pedigree) + (1|year) + (1|rep:year) + (1|year:pedigree)"))
  #  r.lm <- lmer(formula, data = df, REML=TRUE)
  # across years
  model <- lmer(x ~ (1|pedigree) + (1|year) + (1|rep:year) + (1|year:pedigree), 
                data=newdata, REML=TRUE)
  # for year 2 (WI2015)
  model.y2 <- lmer(x[newdata$year=="2"] ~ (1|pedigree), data=newdata[newdata$year=="2",], REML=TRUE)
  # for year 3 (CA2016)
  model.y3 <- lmer(x[newdata$year=="3"] ~ (1|pedigree), data=newdata[newdata$year=="3",], REML=TRUE)
  
  # extract variance components
  summ <- as.data.frame(summary(model)$varcor)
  summ.y2 <- as.data.frame(summary(model.y2)$varcor)
  summ.y3 <- as.data.frame(summary(model.y3)$varcor)
  
  # residual variance
  resid_var <- summ$vcov[5]
  resid_var.y2 <- summ.y2$vcov[2]
  resid_var.y3 <- summ.y3$vcov[2]
  
  # genotype variance
  geno_var <- summ$vcov[2]
  geno_var.y2 <- summ.y2$vcov[1]
  geno_var.y3 <- summ.y3$vcov[1]

  # gxe variance
  gxe_var <- summ$vcov[1]
  
  # calculate repeatability
  r <- geno_var/(geno_var + (gxe_var/y_mean) + (resid_var/(y_mean*r_mean)))
  r.y2 <- geno_var.y2/(geno_var.y2 + (resid_var.y2/r_mean.y2))
  r.y3 <- geno_var.y3/(geno_var.y3 + (resid_var.y3/r_mean.y3))
  
  # min and max genotypes
  # REMOVED b/c doesn't average across all reps
  # min.val <- min(x, na.rm=TRUE)
  # min.geno <- newdata[which.min(x),]["pedigree"][1,1]
  # max.val <- max(x, na.rm=TRUE)
  # max.geno <- newdata[which.max(x),]["pedigree"][1,1]
  
  # combine values
  print(r)
  # r_out <- cbind(colnames(x), paste(min.geno), min.val, paste(max.geno), max.val, r, r.y2, r.y3)
  r_out <- cbind(colnames(x), r, r.y2, r.y3)
  }
)

repeat_out2 <- as.data.frame(t(repeat_out), stringsAsFactors = FALSE)
# colnames(repeat_out2) <- c("min_geno", "min_val", "max_geno", "max_val", "overall", "WI2015", "CA2016")
colnames(repeat_out2) <- c("overall", "WI2015", "CA2016")
# repeat_out2[,c(2,4:7)] <- as.numeric(unlist(repeat_out2[,c(2,4:7)]))
# repeat_out2[,c(2,4:7)] <- round(repeat_out2[,c(2,4:7)], 2)
repeat_out2[,c(1:3)] <- round(repeat_out2[,c(1:3)], 2)

write.csv(repeat_out2, "~/Dropbox/carrot-image-analysis/results/repeatability_table.csv")

# min and max values
rm <- c("7262A x P0159B", "L7550A x Nbh2189B")
repeatData <- repeatData[!repeatData$pedigree %in% rm,]
repeatData <- droplevels(repeatData)

j <- apply(repeatData[,7:38], 2, FUN = function(x){ 
  t <- aggregate(x ~ pedigree, data = repeatData, FUN = function(x) c(mu = mean(x, na.rm=TRUE),
                                                            se = sd(x, na.rm=TRUE)/sqrt(length(x))
                                                            ))
  
    t.df <- cbind(t[,1:2], t[,2])
    t.df <- t.df[order(t.df$mu),]
    min <- head(t.df,1)[,c(1,3:4)]
    max <- tail(t.df,1)[,c(1,3:4)]
    ranges <- rbind(min, max)
}
)

library(data.table)
minmax <- rbindlist(j, idcol=TRUE)
minmax[,3:4] <- round(minmax[,3:4], 2)

write.csv(minmax, "~/Dropbox/carrot-image-analysis/results/minmax_values.csv")

