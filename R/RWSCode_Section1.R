### R code of section 1

## 2. Data "import"

(Control <- c( 39252, 77997, 78702, 16127, 18930, 32400, 15806, 63667))
(MetoLD <- c( 0, 35822, 50930, 25666, 39457, 5694, 9804, 53048, 49192))




## 3. Numerical EDA

summary( MetoLD)


 ## 3(d)
(MLDSummary <- c( summary( MetoLD),
                  "St. Dev." = signif( sd( MetoLD), digits = 4)))

(CSummary <- ....)   # to be completed!





## 4. Indexing vectors

MLDSummary[ 4];     MLDSummary[ "3rd Qu."]

MLDSummary[ c( 1, 6)];     MLDSummary[ c( "1st Qu.", "3rd Qu.")]

MLDSummary[ -c( 2, 5)]

MLDSummary[ c( TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)]





## 5. Graphical EDA:

 ## 5(a)
boxplot( list( C = Control, MLD = MetoLD),
         main = "Control in comparison to Metoprolol-LD: boxplots")


 ## 5(b)
stripchart( list( C = Control, MLD = MetoLD),
            pch = c( 3, 1),         # Types of plotting characters
            col = c( "blue", "red"),   # and their colors for 1st
                                       # and 2nd strip chart.
            method = "jitter",   # Jitter plotting symbols verti-
                                 # cally to reduce overplotting.
            ylim = c( 0.5, 2.5),   # Wider limits of y-axis.
            main = "Control versus Metoprolol-LD: strip charts")


 ## 5(c)
boxplot( list( C = Control, MLD = MetoLD),
         main = "Control vs. Metoprolol-LD: boxplots & strip charts")
stripchart( list( C = Control, MLD = MetoLD), pch = c( 3, 1),
            col = c( "blue", "red"), method = "jitter",
            vertical = TRUE,   # Upright orientation of chart.
            add = TRUE)        # Add to existing (!) plot.


 ## 5(d)
boxplot( list( C = Control, MLD = MetoLD),
    boxwex = 0.5,   # Reduce box widths to 50 % of default size.
    main = "C vs. MLD: boxplots, strip charts & mean +/- 1 st. dev.",
    sub = "Raw values and summarizing information")
set.seed( 42)   # Set the starting value of the RNG so
                # that the random jittering is the same
                # each time this *all* is executed.
stripchart( list( C = Control, MLD = MetoLD), pch = c( 3, 1),
            col = c( "blue", "red"), method = "jitter",
            vertical = TRUE, add = TRUE)
xpos <- c( 1, 2) - 0.4  # x-positions of the markers of the means
                        # in the plot, because k vertical boxplots
                        # are always placed at x = 1, ..., k.
CMmeans <- c( CSummary[ "Mean"],    # Concatenate the pre-com-
              MLDSummary[ "Mean"])  # puted mean values and mark
points( x = xpos,                   # them in the plot at desig-
        y = CMmeans,                # nated x- and y-positions
        pch = 18, col = "orange",   # as diamonds in orange, and
        cex = 2.5)                  # 2.5-times larger than usual.
CMstdevs <- c( CSummary[ "St. Dev."],   # Concatenate the pre-com-
               MLDSummary[ "St. Dev."]) # puted st. deviations and
arrows( x0 = xpos, y0 = CMmeans - CMstdevs,  # draw the error bars
        x1 = xpos, y1 = CMmeans + CMstdevs,  # of "+/- 1 st. dev."
        code = 3, col = "orange",  # as two-headed, orange arrows 
        angle = 90,                # with orthogonal arrow-heads
        length = 0.1)              # of length 0.1 (x-axis units).




## 6. After the break

objects()     # or ls(), or ls.str() for more details.




## 7. Error bars with errbar()

#install.packages( "Hmisc") # Download & install package Hmisc; DIESER BEFEHL IST MIT # LAHMGELEGT!!!!!!
                           # this is necessary only once!
                           # Note the compulsory quotation marks!

library( Hmisc)  # Attach package Hmisc (if it is installed); this
                 # is necessary every time you (re-)start R!
                 # Note: no quotation marks needed here!
Y <- c( mean( Control), mean( MetoLD)) # Compute vectors of the means
delta <- c( sd( Control), sd( MetoLD)) # and the standard deviations.
errbar( x = c( 1, 2), y = Y,    # Provide the center coordinates of
        yplus = Y + delta,      # the bars and their upper and lower
        yminus = Y - delta,     # endpoints.
        xlab = "", ylab = "",   # Use empty (= no) axes labels, and
        xaxt = "n",             # do not draw an x-axis, but use
        xlim = c( 0.5, 2.5))    # wider x-axis limits (than usual).
axis( side = 1,               # axis below the plot (side = 1) with 
      at = c( 1, 2),          # tickmarks *at* designated positions  
      tick= FALSE,				#keine Striche an der Achsenbenennung      
      labels = c( "Control", "Meto.-LD"))   # and their labels. 
      text(x=0.5,y=60000,"Phil`s text", pos=4)     
text(x=1,y=mean(Control), labels= "mean of control", pos=4)



## 8. Assessing normality

par( mfrow = c( 1, 2))       # Set up a 1 x 2 multiple plot frame.
qqnorm( Control, main = "C: normal q-q plot\nwith reference line")
                 # The 'escape character' \n inserts a line break.
qqline( Control)  # The reference line for the existing(!) q-q plot.
qqnorm( MetoLD, main = "MLD: normal q-q plot\nwith reference line")
qqline( MetoLD)




library( car)  # Attach package car (if it is installed); this
               # is necessary after every (re-)start of R!
               # Remember: no quotation marks needed here!

par( mfrow = c( 1, 2),      # Set up a 1 x 2 multiple plot frame
     oma = c( 0, 0, 2, 0))  # with empty space of the height of 2
                            # text lines in the _o_uter _mar_gin
                            # on the top side of the graph.
qqPlot( Control, main = "Control")       # qqPlot() from package car
qqPlot( MetoLD, main = "Metoprolol-LD")  # does not need an additio-
                                         # nal function for the refe-
                                         # rence line.
title( main = paste( "Normal q-q plots with",   # paste() "glues" its
                     "reference lines\n",       # arguments to one (or
                     "and pointwise 95 %",      # several) character
                     "confidence intervals"),   # string(s) together.
       outer = TRUE)   # Place a main title in the _outer_ margin.




## 9. PDF-output

pdf( file = "EDA.pdf", paper = "a4r", width = 0, height = 0)

par( mfrow = c( 1, 3),      # Form a 1 x 3 multiple plot frame therein
     oma = c( 0, 0, 3, 0),  # with empty space of three text lines in
                            # the outer margin on the top of the graph,
     mar = c( 5, 4, 2, 1),  # with (partially) reduced inner margins
                            # to have less "white space" around each
     cex = 1.2)             # graph, and with increased size of text
                            # and most symbols by a factor of 1.2.

boxplot( list( C = Control, MLD = MetoLD), boxwex = 0.5,
         sub = "Raw values & summarizing information")
set.seed( 42)
stripchart( list( C = Control, MLD = MetoLD), pch = c( 3, 1),
            col = c( "blue", "red"), method = "jitter",
            vertical = TRUE, add = TRUE)

xpos <- c( 1, 2) - 0.4
CMmeans <- c( mean( Control), mean( MetoLD))
delta <- c( sd( Control), sd( MetoLD))

points( x = xpos, y = CMmeans, pch = 18,
        col = "orange", cex = 2.5)
arrows( x0 = xpos, y0 = CMmeans - delta,
        x1 = xpos, y1 = CMmeans + delta,
        code = 3, col = "orange", angle = 90, length = 0.1)

qqPlot( Control, main = "Control")
qqPlot( MetoLD, main = "Metoprolol-LD")

title( main = paste( "Control vs. Metoprolol-LD: boxplots,",
                     "strip charts, mean +/- 1 st. dev.,\nand",
                     "normal q-q plots with reference lines",
                     "and pointwise 95 % confidence intervals"),
       outer = TRUE, line = 1)


dev.off() # IMPORTANT: complete and close the graphics device properly!




## 10. Inferential analysis

 ## 10(i) Parametric inference assuming normal distributions

  ## 10(i)(a) Homo- vs. heteroscedasticity
var.test( MetoLD, Control)


  ## 10(i)(b) Location shift?
t.test( MetoLD, Control, var.equal = TRUE)


t.test( MetoLD, Control)




## 11. sink() & source()

sink( "Project1Results.txt")   # Redirect all following text output to
                               # file "Project1Results.txt".
source( "Exercise1.R")  # Read all code from file "Exercise1.R"
                        # and execute it directly.
sink()           # End redirecting text output and close
                 # the previously opened sink-file. 
