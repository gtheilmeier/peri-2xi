### R code of possible solutions of section 2

## Data "import"
Baseline <- c( 23289, 24352, 15788, 42998, 29228, 6050, 40960, 53548,
               149776, 134763, 119067, 33276)
MetoHD <- c( 7510, 0, 27972, 38500, 14632, 0, 0, 79388, 96158, 0, 0,
             42137, 52612, 23477)


## Numerical EDA
BSummary <- c( summary( Baseline),
               "St. Dev." = signif( sd( Baseline), digits = 4))
MHDSummary <- c( summary( MetoHD),
                 "St. Dev." = signif( sd( MetoHD), digits = 4))


## Graphical EDA
pdf( file = "EDA2.pdf", paper = "a4r", width = 0, height = 0)

boxplot( list( Baseline = Baseline, "Meto-HD" = MetoHD), boxwex = 0.5,
         horizontal = TRUE)

set.seed( 42)
stripchart( list( Baseline = Baseline, "Meto-HD" = MetoHD), pch = c( 3, 1),
            col = c( "blue", "red"), method = "jitter",
            vertical = FALSE, add = TRUE)

ypos <- c( 1, 2) - 0.4
BMmeans <- c( mean( Baseline), mean( MetoHD))
delta <- c( sd( Baseline), sd( MetoHD))

points( y = ypos, x = BMmeans, pch = 18,
        col = "orange", cex = 2.5)

arrows( y0 = ypos, x0 = BMmeans - delta,
        y1 = ypos, x1 = BMmeans + delta,
        code = 3, col = "orange", angle = 90, length = 0.1)

title( main = paste( "Baseline versus Metoprolol-HD:\nboxplots,",
                     "strip charts, mean +/- 1 st. dev."),
       sub = "Raw values and graphical summarizing information",
       ylab = "Group", xlab = "Average plaque size [micrometer^2]")
dev.off()



## Normal q-q plots
library( car)
pdf( file = "EDA2_qqplots.pdf", paper = "a4r", width = 0, height = 0)
par( mfrow = c( 1, 2), oma = c( 0, 0, 2, 0))

qqPlot( Baseline, main = "Baseline")
qqPlot( MetoHD, main = "Metoprolol-HD")

title( main = paste( "Normal q-q plots with reference lines\n",
                     "and pointwise 95 % confidence intervals"),
       outer = TRUE)
dev.off()



 ### Parametric inference assuming normal distributions: ###

 # (a) Testing for a difference in population variability
 #     (= homo- vs. heteroscedasticity):

var.test( Baseline, MetoHD)

    # Alternatives: Bartlett's test (for k >= 2)
    # or Levene's test (package car) (for k >= 2)


 # (b1) Testing for a difference in population means (= location
 #      shift) assuming homoscedasticity:

t.test( Baseline, MetoHD, var.equal = TRUE)


 # (b2) Testing for a difference in population means (= location
 #      shift) allowing heteroscedasticity:

t.test( Baseline, MetoHD)


 ### Non-parametric inference assuming (only) conti- ###
 ### nuous distributions of the same shape:          ###

 # (a) Testing for a difference in population variability
 #     (= homo- vs. heteroscedasticity): Fligner-Killeen (rank)
 #     test of homogeneity of variances

fligner.test( list( B = Baseline, MHD = MetoHD))

    # Alternatives: Mood's (rank) test ("there exist more useful
    # tests") or Ansari-Bradley (rank) test
    # (Note: Ansari-Bradley considers s = scale_1 / scale_2 = 1
    #        while Fligner-Killeen considers s^2 = 1!)


 # (b) Testing for a difference in population locations
 #     assuming homoscedasticity: Wilcoxon's rank sum test

wilcox.test( Baseline, MetoHD)

   # Alternative: v. d. Waerden's test


 # (c) Testing for an arbitrary in difference in distri-
 #     butions: Kolmogorov-Smirnov test (an "omnibus test")

ks.test( Baseline, MetoHD)



 ### Non-parametric inference for arbitrary distributions: ###

 # Testing for stochastic tendency between distributions:
 # ANOVA-type rank test by Brunner & Munzel for relative
 # treatment effects

library( nparcomp)

  # Need a data frame to store the data because the testing
  # function npar.t.test() accepts only a so-called model
  # formula describing the data structure:

(P2 <- data.frame( Values = c( Baseline, MetoHD),
                   Group = rep( c( "B", "MHD"),
                                times = c( length( Baseline),
                                           length( MetoHD)) )
                 ) )
str( P2)

summary( npar.t.test( Values ~ Group, data = P2))
