### ~/Theilmeier/Workshop/Tropo_Data_Analysis.R
### R 3.0.2, 5.10.2013
###***************************************************************

 ##****************************
 ## "Analysis" of Troponin data
 ##*****************************

 ## Now, the actual analysis commences: Fitting a risk pre-
 ## diction model (i.e., a logistic regression model) for
 ## MACE as outcome (= response / dependent) variable and
 ## various others as covariables (= explanatory / prediction /
 ## prognostic / regressor variables).
 ## We are going to use package PredictABEL which is a useful
 ## collection of functions from various other packages.

# install.packages( "PredictABEL") # if not yet installed.

library( PredictABEL) # Attach package to search path so that its
                      # contents is available in current session.
                      # For a link to some documentation type
                      # ??PredictABEL (with two question marks).

 ## Step 1a:
 ##
 ## We will fit a logistic regression model for MACE on RCRI
 ## and other potential risk predictors (almost) according to
 ## PredictABEL's description:



Tropo$MACE <- match( Tropo$MACE, c( "neg", "pos")) - 1


 # Specify column number of outcome/response variable:
(colOutcome <- which( names( Tropo) == "MACE"))

 # Specify column numbers of non-genetic predictors:
(colNonGenPred <- match( c( "RCRI", "TR_PRE", "TR_PO",
                            "ABS_TRO", "RE_TROA"),
                         names( Tropo)))

    # We do neither specify column numbers of non-genetic
    # categorical predictors nor of any genetic predictors.

 # Fitting the logistic regression model using PredictABEL's
 # fct. fitLogRegModel():
riskmodel <- fitLogRegModel( data = Tropo,
                             cOutcome = colOutcome, 
                             cNonGenPreds = colNonGenPred)

 # Show summary details for the fitted risk model:
summary( riskmodel)


    ## Step 1b:
    ##
    ## ALTERNATIVE: It is, of course, possible to "directly" fit
    ## logistic regression models for MACE on RCRI and other po-
    ## tential risk predictors using Rs "standard" function glm()
    ## (for Generalized Linear Models) with the logit link fct.
    ## (from the binomial family of link functions):

    # Here is no specification of genetic, non-genetic, catego-
    # rical or non-categorical variables necessary.
    # And for the binomial family the response can be specified
    # as a factor (when its first level denotes failure and all
    # others success).
    # Check the order of MACE's levels:
  levels( Tropo$MACE)

    # We start with a logistic model with RCRI as the single
    # predictor for MACE, i.e., we consider the model
    #    logit( P( MACE = "neg" | RCRI))
    #     = log(  P( MACE = "neg" | RCRI) /
    #           (1 - P( MACE = "neg" | RCRI)) )
    #     = beta0 + beta1 * RCRI
    #
    # This is equivalent to
    #    P( MACE = "neg" | RCRI)
    #     = 1 / ( 1 + exp( -(beta0 + beta1 * RCRI)) )
    #
    # The fitting:
   summary( riskmodel1 <- glm( MACE ~ RCRI, data = Tropo,
                               family = binomial( "logit")))

    # Now, we are going to update the model in that we add a 
    # second predictor variable. The dot (.) means "take what
    # is there", and the plus (+) means "add", but not in its
    # arithmetic sense. The model will be
    #    logit( P( MACE = "neg" | RCRI, TR_PRE))
    #     = log(  P( MACE = "neg" | RCRI, TR_PRE) /
    #           (1 - P( MACE = "neg" | RCRI, TR_PRE)) )
    #     = beta0 + beta1 * RCRI + beta2 * TR_PRE
    #
    # This is equivalent to
    #  P( MACE = "neg" | RCRI, TR_PRE)
    #   = 1 / (1 + exp( -(beta0 + beta1 * RCRI + beta2 * TR_PRE)))
    #
    # The fitting:
   summary( riskmodel2 <- update( riskmodel1,
                                  formula = . ~ . + TR_PRE))

    # We can add several (further) predictors at once:
   summary( riskmodel3 <- update( riskmodel2,
                                  formula = . ~ . + TR_PO
                                                  + ABS_TRO
                                                  + RE_TROA))

      # Side note: the last model could have also been fitted at
      # once:
     summary( riskmodel3 <- glm( MACE ~ RCRI + TR_PRE + TR_PO +
                                        ABS_TRO + RE_TROA,
                                 data = Tropo,
                                 family = binomial( "logit")))

      # Side note: comparison of the three nested/hierarchical
      # models (using a Chi-squared test) testing hypotheses of
      # equality of "adjacent" models:
     anova( riskmodel1, riskmodel2, riskmodel3, test = "Chisq")



 ## Step 2a:
 ##
 ## Next, we obtain predicted risks (= fitted/estimated prob-
 ## abilities of MACE = "neg" for all rows of the data frame
 ## used for fitting the model, i.e., for each patient in Tropo: 
predRisk1 <- predRisk( riskmodel1)
predRisk2 <- predRisk( riskmodel2)
predRisk3 <- predRisk( riskmodel3)

    ## Step 2b:
    ##
    ## ALTERNATIVE: It is again possible to directly obtain
    ## fitted probabilities from a glm-object using a base
    ## R function, here: predict():
   predRisk1 <- predict( riskmodel1, type = "response")
   predRisk2 <- predict( riskmodel2, type = "response")
   predRisk3 <- predict( riskmodel3, type = "response")

  

 ## Step 3a:
 ##
 ## Production of a plot of the ROC-curves for the fitted models
 ## augmented with a legend, and computation of each AUC plus its
 ## 95 % confidence interval:

 # Specify labels of the ROC curves (by hand):
labels <- paste( "RCRI", c( "", "+ hs-cTnT pre-op",
                                "+ all"))

    # Side note: specify labels of the ROC curves automatically:
   labels <- sapply( list( riskmodel1, riskmodel2, riskmodel3),
                     function( x) deparse( formula( x)[[3]]))

 # Produce the ROC curves and compute the AUC-values:
plotROC( data = Tropo, cOutcome = colOutcome,
         predrisk = cbind( predRisk1, predRisk2, predRisk3),
         labels = labels)




# specify cutoff values for risk categories
cutoff <- c( 0, 0.10, 0.30, 1)

# compute reclassification measures
reclassification( data = Tropo, cOutcome = colOutcome,
                  predrisk1 = predRisk1, predrisk2 = predRisk2,
                  cutoff)


# specify labels of the predictiveness curves
labels <- paste( "RCRI", c( "", "+ hs-cTnT pre-op"))

# produce predictiveness curves
plotPredictivenessCurve( predrisk = cbind( predRisk1, predRisk2),
                         rangeyaxis = c( 0, 1), labels = labels)













library( OptimalCutpoints)

# Youden Index Method ("Youden"):

# Default method
optcutp.Youden <- optimal.cutpoints( X = "TR_PRE", status = "MACE",
                                     tag.healthy = 0, methods = "Youden",
                                     data = Tropo)
summary( optcutp.Youden)
plot( optcutp.Youden, which = 1)


# Formula method
optcutp.Youden <- optimal.cutpoints( X = TR_PRE ~ MACE, tag.healthy = 0, 
                                     methods = "Youden", data = Tropo)
summary( optcutp.Youden)

