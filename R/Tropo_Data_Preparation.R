### ~/Theilmeier/Workshop/Tropo_Data_Preparation.R
### R 3.0.2, 5.10.2013
###***************************************************************

 ##*************************
 ## Import of Troponin data
 ##*************************

 ## Troponin data: decimal sign: , (comma).
 ## NOTE: there are several semi-cola (;) in the original xls-file
 ## so that it could not be exported as csv-file, but had to be
 ## exported tab-delimited. Thus, read.delim2() is the most con-
 ## venient fct. for importing the data into a data frame:

Tropo <- read.delim2( file = "Data/RWSData_Tropo_delim2.csv",
                      as.is = "OPTEXT")
                      # Prevents column OPTEXT from conversion to
                      # a factor (and leaves it as character).


 ## Checking structure and contents of the (imported) data frame:

names( Tropo)     # Names of columns/variables of data frame.

str( Tropo)       # Structure information (column names, their
                  # data type and their first few elements).

head( Tropo, 3)   # The first 3 rows and
tail( Tropo, 2)   # the last 2 rows.

summary( Tropo)   # Column-wise summaries.


 ## Conversion of the contents of some of the imported numerical
 ## (= metric scale) data columns -- which contain in fact nume-
 ## rical codings -- to factor variables (= nominal scale) or
 ## ordered factor variables (= ordinal scale), if necesssary,
 ## depending on their actual meaning:

Tropo$ID <- factor( Tropo$ID)         # Convert columns ID and
Tropo$FALLNR <- factor( Tropo$FALLNR) # FALLNR of data frame
                                      # Tropo into factors.

    # By the way: checking uniqueness reveals a doublette in
    # vector FALLNR:
   any( duplicated( Tropo$FALLNR)) # "Are there any duplicated
                                   # values in column FALLNR?"

    # Which is the number of the duplicated element?
   (duplnum <- which( duplicated( Tropo$FALLNR)))

    # Show the whole row in which the duplicated element is:
   Tropo[ duplnum, ]

    # Extract the value of the doublette and ...
   (duplval <- Tropo$FALLNR[ duplnum])

    # ... use it to show all (!) rows in which the duplicated
    # value in column FALLNR of Tropo occurs:
   Tropo[ Tropo$FALLNR == duplval, ]  # Note the double "=" for
                                      # testing for quality!


  # Convert GESCHL into a factor such that its current levels 0
  # and 1 are turned into levels with new labels "female" and
  # "male", respectively.
  # Note that values that do not occur in the set given to
  # levels are converted to NA (= not available)!
Tropo$GESCHL <- factor( Tropo$GESCHL, levels = c( 0, 1),
                        labels = c( "female", "male"))

  # Convert KLEMME into a factor such that its current levels
  # 0, 1, 2 and 3 are turned into levels with new labels:
Tropo$KLEMME <- factor( Tropo$KLEMME, levels = 0:3,
                        labels = c( "keine Aorten-OP",
                                    "AK infrarenal",
                                    "AK perirenal",
                                    "AK suprarenal"))

  # Convert MACE into a factor such that its current levels 0
  # and 1 are turned into levels with labels "neg" and "pos":
Tropo$MACE <- factor( Tropo$MACE, levels = 0:1,
                      labels = c( "neg", "pos"))

 ## Check what has happened so far:
str( Tropo[ 1:10])



 ## Continued conversion:
Tropo$OP_VERF <- factor( Tropo$OP_VERF, levels = 0:4,
                         labels = c( "Rohrprothese", "Y-Prothese",
                                     "infrarenal", "suprarenal",
                                     "thorakal"))

Tropo$OP_TYPE <- factor( Tropo$OP_TYPE, levels = 1:4,
                         labels = c( "Carotis-OP", "Aorten-OP",
                                     "Periph. Gef??eingriff",
                                     "Thorak. Aorteneingriff"))

for( v in c( "OP_TYPE1", "OP_TYPE2", "OP_TYPE3", "CSTENSYM",
             "ART_HTN", "HERZINS2", "DIABETES", "NIERENIN",
             "KHK_MI2", "STROKE_T", "COPD", "ASTHMA", "COPDASTH",
             "TRPREC", "TRPOC", "DELTROC", "ABSTROC", "RELTROC",
             "RELTROAC", "ANV", "STROKE", "ICU_COMP", "COMP1NOR",
             "COMP2NOR")) {
 Tropo[[ v]] <- factor( Tropo[[ v]], levels = 0:1,
                        labels = c( "nein", "ja"))
 }

Tropo$RAUCHER <- factor( Tropo$RAUCHER, levels = 0:2,
                         labels = c( "nein", "ja", "Ex"))

Tropo$RCRI_C <- factor( Tropo$RCRI_C, levels = 0:3,
                        labels = c( 0:2, "3+"))

Tropo$ANV_KRIT <- factor( Tropo$ANV_KRIT, levels = 1:5,
                          labels = c( paste( "Anstieg", 1:3),
                                      "Dial. neu", "Dial. preop"))

Tropo$MACE_KRI <- factor( Tropo$MACE_KRI, levels = 1:7,
                          labels = c( "TnT", "EKG", "TnT+EKG",
                                      "AP+TnT", "TnT+AP+EKG",
                                      "AP+EKG", "CV death"))

Tropo$MACE_GR <- factor( Tropo$MACE_GR, levels = 0:2,
                          labels = c( "none", "early", "late"))

Tropo$SEPSIS <- factor( Tropo$SEPSIS, levels = 0:4,
                          labels = c( "kein", "SIRS",
                                      "Sepsis OP-Gebiet",
                                      "Sepsis woanders",
                                      "Inf. ohne SIRS/Sepsis"))

Tropo$SEPSIS_K <- factor( Tropo$SEPSIS_K, levels = 1:5,
                          labels = c( "SIRS", "Sepsis OP-Gebiet",
                                      "Sepsis woanders",
                                      "Inf. au?erhalb ohne SIRS",
                                      "Inf. OP-Gebiet ohne SIRS"))

Tropo$COMPL1 <- factor( Tropo$COMPL1, levels = 1:3,
                        labels = c( "ANV", "Stroke",
                                    "Sepsis/SIRS/Infekt"))

Tropo$COMPL2 <- factor( Tropo$COMPL2, levels = 1:4,
                        labels = c( "ANV", "Stroke",
                                    "Sepsis/SIRS/Infekt", "MACE"))


 ## Not converted because either truly numeric or unknown
 ## meanings of numeric coding:
 # ASA_KAT, OPRISIKO, RCRI, RCRI_C1, RCRI_C2, RCRI_C3, SURGRISK,
 # DECREASE, B_BLOCKE, STATIN, SARTAN, ACE_HEM, CA_ANTAG,
 # DIURETIK, KREA_PRE, GFR_MDRD, TR_PRE, TR_PRE2, TR_PO, TR_PO2,
 # DEL_TRO, ABS_TRO, ABS_TRO2, ABSTROC2, REL_TRO, RE_TROA,
 # RE_TROA2, V61_A, BREATHD, ICU_1, ICU_D, HOSP_D, MORTALIT,
 # ANV_D, MACE_D, STROKE_D, SEPSIS_D, COMPL1_D, COMPL2_D,
 # MACE_LH1, MACE_LH2, MACE_LH3, MACE_LH4, MACE_LH5, MACE_LH6,
 # MACE_LH7, MACE_LH8, MACE_LH9


 ## Check what has happened altogether:
str( Tropo)

head( Tropo, 3)   # The first 3 rows and
tail( Tropo, 2)   # the last 2 rows.

summary( Tropo)   # Note the difference: many more frequency
                  # tables instead of five-numbers-summaries.
