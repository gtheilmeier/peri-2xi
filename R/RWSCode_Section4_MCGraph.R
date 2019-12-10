### R code of section4 for multiple comparisons graph

P4.df <- read.csv2( "Data/RWSData_Section4.csv")
str( P4.df)
head( P4.df, n = 10)
tail( P4.df, n = 10)


 # Open a pdf graphics device, 7 inches wide, 7 inches high (default):
pdf( "MultCompGraph.pdf", width = 7, height = 7)

 # Change the layout of the device's figure region so that
 # + there are smaller margins around the plot region on sides 1 (bot-
 #   tom), 2 (left) and 4 (right), but a much larger margin (of eight
 #   lines) on side 3 (top), achieved by the value for mar;
 # + the distances of the axis titles and axis labels are reduced,
 #   achieved by the first two elements of the value for mgp;
 # + the orientation of the axis label is always horizontal, achieved
 #   by the value for las;
 # + the number of tickmarks on the vertical axis is increased,
 #   achieved by the first element of the value for lab;
 # + it is allowed to "draw" outside the plot region (i.e., in the
 #   margins), achieved by the value for xpd:
par( mar = c( 4.6, 3.1, 8.7, 0.1)+0.1, mgp = c( 2, 0.7, 0), las = 1, 
     lab = c( 10, 7, 4), xpd = TRUE)

 # Groupwise boxplots with variable box widths that are proportional
 # to the square root of their pertaining group size:
boxplot( Value ~ Treatment, data = P4.df, varwidth = TRUE,
         ylab = "Measurement scale", xlab = "Treatment groups")

 # An extra axis with 4 tickmarks (at positions 1, ..., 4 (here)) is
 # placed at the top of the plot region with labels constructed from
 # the character string "n =" and the elements of groupsizes by
 # pasting them together:
groupsizes <- table( P4.df$Treatment)
axis( side = 3, at = seq( groupsizes),
      labels = paste( "n =", groupsizes))

 # Compute Value-means by Treatment-groups:
Vmeans <- with( P4.df, by( Value, Treatment, mean))

 # Add points at (x[i], y[i]) for i = 1, ..., 4 (here) to the plot
 # using plot character 18 (a diamond) and orange color, and with
 # character expansion of 2 (i.e. larger). Here, x[i] = i and y[i]
 # = Vmeans[i]:
points( x = seq( Vmeans), y = Vmeans, col = "orange", pch = 18, cex = 2)

 # Add a legend in the topleft corner of the plot:
legend( "topleft", legend = "Means", pch = 18, col = "orange")

 # Drawing "arrows" 
 # + whose starting and ending coordinates are (x0[i], y0[i]) and
 #   (x1[i], y1[i]) for i = 1, ..., 4 (here), respectively;
 # + which have arrow heads on _both_ sides, achieved by code = 3,
 #   with a rectangular angle between shaft and edge of each arrow
 #   head, achieved by angle = 90, with small arrow head edges
 # (length = 0.04), and in red color (col = "tomato3"):
y.coordinates <- 0:3 * 4 + 106   # y-coordinates determined by trial
                                 # and error.
arrows( x0 = c( 1, 1, 2, 3), y0 = y.coordinates, # x-coordinates chosen
        x1 = c( 2, 3, 3, 4), y1 = y.coordinates, # according to the sig-
                                                 # nificance results.
        code = 3, angle = 90, length = 0.04, col = "tomato3")

 # Add text in the margins of the plot (achieved because the y-coordi-
 # nates are so large that they extend over the upper limit of the
 # y-axis)
 # + centered at the coordinates (x[i], y[i]) for i = 1, ..., 4 (here);
 # + specified by the elements of labels;
 # + with a character expansion of 0.8 (i.e. smaller) and the color
 #   "tomato":
text( x = c( 1.5, 2, 2.5, 3.5), y = y.coordinates + 1,
      labels = c( "p = 0.0017",
                  rep( "p << 0.001", 3)),   # rep() repeats things.
      cex = 0.8, col = "tomato")

 # A main title far away from the upper edge of the plot region, and 
 # a sub title no too far away from the lower edge of the plot region,
 # both with individual (and pretty inappropriate) colors:
title( main = paste( "Holm-adjusted p-values of all pairwise",
                     "comparisons\n (by t-tests with non-pooled",
                     "standard deviations)"),
       line = 6.5, col.main = "aquamarine3")
title( sub = "An additional sub-title could be placed here",
       line = 3.5, col.sub = "darkorchid")

dev.off()
