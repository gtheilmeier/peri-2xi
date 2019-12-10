### R code of section 3

P3.df <- read.csv2( file = "Data/RWSData_Meto_LONG.csv")

str( P3.df)
P3.df

head( P3.df, n = 3)   # The first 3 rows (default is n = 6).
tail( P3.df, n = 2)   # The last 2 rows.

summary( P3.df)



(groupsizes <- table( P3.df$Group))  # For the meaning of $ see below.

barplot( groupsizes, main = "Group sizes", sub = "'Treatments'",
         col = c( "blue", "green", "red", "orange", "black"), las = 1,
         cex.axis = 2, cex.names = 0.8, cex.main = 2.5, cex.sub = 2)



P3.df$Value

P3.df[ c( 1, 3)]

P3.df[ 3, 2]   # The element in row no. 3 and column no. 2.

  # The elements in rows 9 and 5 of columns 2 and 1 (in this order):
P3.df[ c( 9, 5), c( 2, 1)]  

P3.df[ 3, ]   # The complete row no. 3 (as a data frame of one row).

P3.df[ c( 4, 12, 7), ]   # Rows 4, 12 & 7 as a data frame of 3 rows.


P3.df <- P3.df[ -2]   # All columns except for the second.
str( P3.df)   # To check what has happened.


by( P3.df$Value, P3.df$Group,
     FUN = function( x) {
            c( summary( x), "St. Dev." = signif( sd( x), 4))
            }
   )


with( P3.df,
      by( Value, Group,
          FUN = function( x) {
                 c( summary( x), "St. Dev." = signif( sd( x), 4))
                 }
        )
    )




boxplot( Value ~ Group, data = P3.df,   # Boxplots with smaller
         cex.axis = 0.8)                # axis annotation.
axis( side = 3,   # Extra axis above the plot (side = 3) with tick-
      at = 1:5,   # marks *at* designated positions (1:k = c(1,...,k))
      labels = paste( "n =",      # and labels composed of a constant
                      groupsizes)) # ("n =") and varying values.

title( main = "Project 3: groupwise boxplots",  # Main and sub-title
       sub = "Treatment groups",                # 2.5 lines away from
       line = 2.5)                              # the plotting region.
set.seed( 42)   # To make the jittering reproducible.
stripchart( Value ~ Group, data = P3.df, method = "jitter", pch = 1:5,
            col = c( "blue", "green", "red", "orange", "black"),
            vertical = TRUE, add = TRUE)
Vmeans <- with( P3.df, by( Value, Group, mean))  # Compute Value-means
                                                 # by groups and
points( x = 1:5, y = Vmeans,        # add at their x- and y-positions
        pch = 18, col = "orange",   # diamonds in orange with
        cex = 2)                    # "character expansion" 2.
legend( "top", legend = "Mean",   # Add at the top center a legend
        pch = 18, pt.cex = 2,     # with respective text and plot-
        col = "orange")           # ting symbol information.





summary( P3.aov <- aov( Value ~ Group, data = P3.df))


library( car)     # A special version of qqPlot() (of package car)
set.seed( 4711)   # for linear models automatically extracts the
qqPlot( P3.aov,   # studentized residuals, computes the pointwise
        reps = 500,  # 95 %-confidence intervals based on simula-
        line = "quartiles")  # tions (here with 500 replications),
                             # and determines the reference line.


bartlett.test( Value ~ Group, data = P3.df)


oneway.test( Value ~ Group, data = P3.df)


with( P3.df, pairwise.t.test( Value, Group))


with( P3.df, pairwise.t.test( Value, Group, pool.sd = FALSE))


with( P3.df, pairwise.t.test( Value, Group, pool.sd = FALSE,
      p.adjust.method = "none"))



TukeyHSD( P3.aov)

par( mar = c( 5,6,4,0)+0.1, cex = 1.2, las = 1)
plot( TukeyHSD( P3.aov))

groupsizes




fligner.test( Value ~ Group, data = P3.df)


kruskal.test( Value ~ Group, data = P3.df)


with( P3.df, pairwise.wilcox.test( Value, Group))


library( nparcomp)     # attach the relevant package (if it is installed)
summary( nparcomp( Value ~ Group, data = P3.df, "Tukey"))
