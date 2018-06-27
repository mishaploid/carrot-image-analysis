# ------------------------------------------------------------------------------
# function to calculate significance of correlations
# source: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# ------------------------------------------------------------------------------
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# ------------------------------------------------------------------------------
# export matrix of p-values
# ------------------------------------------------------------------------------
p.mat <- cor.mtest(merged2[,2:27])

# ------------------------------------------------------------------------------
# create matrix for significance notations 
# * = P < 0.05, ** = P < 0.01, *** = P < 0.001
# ------------------------------------------------------------------------------
sigCodes <- ifelse(p.mat[[1]] > 0.05, "NS", 
                   ifelse(p.mat[[1]] > 0.01, "*", 
                          ifelse(p.mat[[1]] > 0.001, "**", "***")))

# set upper triangle to NA
sigCodes[upper.tri(sigCodes)] <- NA

# combine significance and correlations
combinedMat <- lowerUpper(upper = sigCodes, lower = round(merged_mat, 1), 
                          diff = FALSE)
diag(combinedMat) <- ""

# ------------------------------------------------------------------------------
# functions to format ggpairs
# adapted from: https://github.com/ggobi/ggally/issues/139
# ------------------------------------------------------------------------------

my_custom_cor <- function(data, mapping, color = I("black"), sizeRange = c(1, 5), ...) {
  
  # get the x and y data to use the other code
  x <- eval(mapping$x, data)
  y <- eval(mapping$y, data)
  
  ct <- cor.test(x,y)
  pt <- cor.test(x,y, method="spearman")
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  p <- unname(pt$estimate)
  pt <- format(p, digits=2)[1]
  
  # plot the cor value
  ggally_text(
    paste("r=", rt, "\n", "p=", pt, sep=""), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = 5,
    color = color,
    ...
  ) 
  # # add the sig stars
  # geom_text(
  #   aes_string(
  #     x = 0.8,
  #     y = 0.8
  #   ),
  #   label = sig,
  #   size = I(cex),
  #   color = color,
  #   ...
  # ) +
  # remove all the background stuff and wrap it with a dashed line
  # theme_classic() + 
  #   theme(
  #     panel.background = element_rect(
  #       color = color, 
  #       linetype = "longdash"
  #     ), 
  #     axis.line = element_blank(), 
  #     axis.ticks = element_blank(), 
  #     axis.text.y = element_blank(), 
  #     axis.text.x = element_blank()
  #   )
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 0.5) + 
    geom_smooth(method=lm, color="blue", size = 0.5)
  p
}