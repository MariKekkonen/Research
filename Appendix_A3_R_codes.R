#R codes for statistical analyses
#NOTE! This work is still under preparation and the code unfinished.

#Packages

library("stats", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("boot", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

#nDNA Dataset
#-------------------

#Download dataset "nDNA_dataset" from Dryad
#Set your working directory, example: setwd("~/Desktop/YOUR_FOLDER1/YOUR_FOLDER2/YOUR_FOLDER_WHERE_THE_DATASET_IS/")
#Read dataset
nDNA_dataset <- read.delim (file = "nDNA_dataset.txt", header = TRUE)

#GLMM for nDNA (pooled based on breeding system)

#Categorical variables

#Breeding system
nDNA_dataset$BS.f <- factor(nDNA_dataset$BS)
contrasts(nDNA_dataset$BS.f) <- contr.treatment(2)

#Order
nDNA_dataset$ORDER.f <- factor(nDNA_dataset$ORDER)
contrasts(nDNA_dataset$ORDER.f) <- contr.treatment(7)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = nDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = nDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = nDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = nDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#End of code for GLMM for nDNA Dataset



#GLM for sister pair DP Holometabola vs HDP Hymenoptera (nDNA)

#Extract the data of DP Holometabola vs HDP Hymenoptera 
DP_Holo_HDP_Hyme <- nDNA_dataset [nDNA_dataset$SISTER1 == "DP_Holo_HDP_Hyme" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f , family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Holometabola vs HDP Hymenoptera



#nDNA Dataset only including Hymenoptera
#---------------------------------------------------------

#Download dataset "nDNA_dataset_Hymenoptera" from Dryad
#Set your working directory, example: setwd("~/Desktop/YOUR_FOLDER1/YOUR_FOLDER2/YOUR_FOLDER_WHERE_THE_DATASET_IS/")
#Read dataset
nDNA_dataset_Hymenoptera <- read.delim (file = "nDNA_dataset_Hymenoptera.txt", header = TRUE)

#GLM for social vs solitary Hymenoptera (nDNA)

#Categorical variables

#Life history
nDNA_dataset_Hymenoptera$LIFE_HISTORY.f <- factor(nDNA_dataset_Hymenoptera$LIFE_HISTORY)
contrasts(nDNA_dataset_Hymenoptera$LIFE_HISTORY.f) <- contr.treatment(2)

#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

SOCIAL_SOLITARY_glm_quasi <- glm (TOTAL_GC ~ LIFE_HISTORY.f, family = quasibinomial (link = "logit"), nDNA_dataset_Hymenoptera) 
summary(SOCIAL_SOLITARY_glm_quasi)

#Predicted values on the data scale
#SOCIAL
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"])
#SOLITARY
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"] + SOCIAL_SOLITARY_glm_quasi$coef["LIFE_HISTORY.f2"])

#GC-1

SOCIAL_SOLITARY_glm_quasi <- glm (GC1 ~ LIFE_HISTORY.f, family = quasibinomial (link = "logit"), nDNA_dataset_Hymenoptera) 
summary(SOCIAL_SOLITARY_glm_quasi)

#Predicted values on the data scale
#SOCIAL
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"])
#SOLITARY
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"] + SOCIAL_SOLITARY_glm_quasi$coef["LIFE_HISTORY.f2"])

#GC-2

SOCIAL_SOLITARY_glm_quasi <- glm (GC2 ~ LIFE_HISTORY.f, family = quasibinomial (link = "logit"), nDNA_dataset_Hymenoptera) 
summary(SOCIAL_SOLITARY_glm_quasi)

#Predicted values on the data scale
#SOCIAL
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"])
#SOLITARY
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"] + SOCIAL_SOLITARY_glm_quasi$coef["LIFE_HISTORY.f2"])

#GC-3

SOCIAL_SOLITARY_glm_quasi <- glm (GC3 ~ LIFE_HISTORY.f, family = quasibinomial (link = "logit"), nDNA_dataset_Hymenoptera) 
summary(SOCIAL_SOLITARY_glm_quasi)

#Predicted values on the data scale
#SOCIAL
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"])
#SOLITARY
inv.logit(SOCIAL_SOLITARY_glm_quasi$coef["(Intercept)"] + SOCIAL_SOLITARY_glm_quasi$coef["LIFE_HISTORY.f2"])

#End of code for GLM for social vs solitary Hymenoptera



#COI-5 Dataset
#--------------------

#Download dataset "COI5_dataset" from Dryad
#Set your working directory, example: setwd("~/Desktop/YOUR_FOLDER1/YOUR_FOLDER2/YOUR_FOLDER_WHERE_THE_DATASET_IS/")
#Read dataset
COI5_dataset <- read.delim (file = "COI5_dataset.txt", header = TRUE)

#GLMM for COI-5 Dataset (pooled based on breeding system)

#Categorical variables

#Breeding system
COI5_dataset$BS.f <- factor(COI5_dataset$BS)
contrasts(COI5_dataset$BS.f) <- contr.treatment(2)

#Order
COI5_dataset$ORDER.f <- factor(COI5_dataset$ORDER)
contrasts(COI5_dataset$ORDER.f) <- contr.treatment(25)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI5_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI5_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI5_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI5_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#End of code for GLMM for COI-5 Dataset



#GLM for sister pair DP Hemiptera vs HDP Aleyrodidae (COI-5)

#Extract the data of DP Hemiptera vs HDP Aleyrodidae 
DP_Hemi_HDP_Aley <- COI5_dataset [COI5_dataset$SISTER1 == "DP_Hemi_HDP_Aley"  , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Aleyrodidae



#GLM for sister pair DP Psyllidae vs HDP Aleyrodidae (COI-5)

#Extract the data of DP Psyllidae vs HDP Aleyrodidae 
DP_Psyl_HDP_Aley <- COI5_dataset [COI5_dataset$SISTER2 == "DP_Psyl_HDP_Aley" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psyllidae vs HDP Aleyrodidae



#GLM for sister pair DP Diptera vs HDP Cecidomyiidae+Sciaridae (COI-5)

#Extract the data of DP Diptera vs HDP Cecidomyiidae+Sciaridae 
DP_Dipt_HDP_CeciScia <- COI5_dataset [COI5_dataset$SISTER1 == "DP_Dipt_HDP_CeciScia" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_CeciScia$BS.f <- factor(DP_Dipt_HDP_CeciScia$BS)
contrasts(DP_Dipt_HDP_CeciScia$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Diptera vs HDP Cecidomyiidae+Sciaridae



#GLM for sister pair DP Mycetophilidae vs HDP Cecidomyiidae+Sciaridae (COI-5)

#Extract the data of DP Mycetophilidae vs HDP Cecidomyiidae+Sciaridae 
DP_Myce_HDP_CeciScia <- COI5_dataset [COI5_dataset$SISTER2 == "DP_Myce_HDP_CeciScia" , ]

#Categorical variables

#Breeding system
DP_Myce_HDP_CeciScia$BS.f <- factor(DP_Myce_HDP_CeciScia$BS)
contrasts(DP_Myce_HDP_CeciScia$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Myce_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Myce_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Myce_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Myce_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Mycetophilidae vs HDP Cecidomyiidae+Sciaridae



#GLM for sister pair DP Hemiptera vs HDP Coccidae+Pseudococcidae (COI-5)

#Extract the data of DP Hemiptera vs HDP Coccidae+Pseudococcidae
DP_Hemi_HDP_CoccPseu <- COI5_dataset [COI5_dataset$SISTER3 == "DP_Hemi_HDP_CoccPseu" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_CoccPseu$BS.f <- factor(DP_Hemi_HDP_CoccPseu$BS)
contrasts(DP_Hemi_HDP_CoccPseu$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Coccidae+Pseudococcidae



#GLM for sister pair DP Psyllidae vs HDP Coccidae+Pseudococcidae (COI-5)

#Extract the data of DP Psyllidae vs HDP Coccidae+Pseudococcidae
DP_Psyl_HDP_CoccPseu <- COI5_dataset [COI5_dataset$SISTER4 == "DP_Psyl_HDP_CoccPseu" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_CoccPseu$BS.f <- factor(DP_Psyl_HDP_CoccPseu$BS)
contrasts(DP_Psyl_HDP_CoccPseu$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psyllidae vs HDP Coccidae+Pseudococcidae



#GLM for sister pair DP Holometabola vs HDP Hymenoptera (COI-5)

#Extract the data of DP Holometabola vs HDP Hymenoptera
DP_Holo_HDP_Hyme <- COI5_dataset [COI5_dataset$SISTER3 == "DP_Holo_HDP_Hyme" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Holometabola vs HDP Hymenoptera



#GLM for sister pair DP Psocoptera vs HDP Phthiraptera (COI-5)

#Extract the data of DP Psocoptera vs HDP Phthiraptera
DP_Psoc_HDP_Phth <- COI5_dataset [COI5_dataset$SISTER1 == "DP_Psoc_HDP_Phth" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psocoptera vs HDP Phthiraptera



#GLM for sister pair DP Hemiptera vs HDP Thysanoptera (COI-5)

#Extract the data of DP Hemiptera vs HDP Thysanoptera
DP_Hemi_HDP_Thys <- COI5_dataset [COI5_dataset$SISTER5 == "DP_Hemi_HDP_Thys" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
#HDP
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Thysanoptera



#mtDNA Dataset
#---------------------

#Download dataset "mtDNA_dataset" from Dryad
#Set your working directory, example: setwd("~/Desktop/YOUR_FOLDER1/YOUR_FOLDER2/YOUR_FOLDER_WHERE_THE_DATASET_IS/")
#Read dataset
mtDNA_dataset <- read.delim (file = "mtDNA_dataset.txt", header = TRUE)

# GLMM for mtDNA Dataset (pooled based on breeding system)

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)

#Specimen
mtDNA_dataset$SPECIMEN.f <- factor(mtDNA_dataset$SPECIMEN)
contrasts(mtDNA_dataset$SPECIMEN.f) <- contr.treatment(790)


#Generalized linear mixed model with quasibinomial distribution, ORDER/SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f/SPECIMEN.f, family = quasibinomial(link = "logit"), data = mtDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f/SPECIMEN.f, family = quasibinomial(link = "logit"), data = mtDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f/SPECIMEN.f, family = quasibinomial(link = "logit"), data = mtDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f/SPECIMEN.f, family = quasibinomial(link = "logit"), data = mtDNA_dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#End of code for GLMM for mtDNA Dataset (pooled based on BS)



# GLMM for mtDNA Dataset (records for each marker pooled based on BS)

#COI

#Extract the data of COI 
COI <- mtDNA_dataset [mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COI, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#COII

#Extract the data of COII 
COII <- mtDNA_dataset [mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#COIII

#Extract the data of COIII 
COIII <- mtDNA_dataset [mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COIII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COIII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COIII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = COIII, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#CYTB

#Extract the data of CYTB 
CYTB <- mtDNA_dataset [mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = CYTB, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = CYTB, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = CYTB, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = CYTB, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ATP6

#Extract the data of ATP6 
ATP6 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ATP8

#Extract the data of ATP8 
ATP8 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP8, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP8, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP8, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ATP8, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND1

#Extract the data of ND1 
ND1 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND1, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND1, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND1, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND1, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND2

#Extract the data of ND2 
ND2 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND2, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND2, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND2, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND2, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND3

#Extract the data of ND3 
ND3 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND3, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND3, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND3, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND3, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND4

#Extract the data of ND4 
ND4 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND4L

#Extract the data of ND4L 
ND4L <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4L, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4L, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4L, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND4L, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND5

#Extract the data of ND5 
ND5 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND5, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND5, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND5, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND5, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#ND6

#Extract the data of ND6 
ND6 <- mtDNA_dataset [mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
mtDNA_dataset$BS.f <- factor(mtDNA_dataset$BS)
contrasts(mtDNA_dataset$BS.f) <- contr.treatment(2)

#Order
mtDNA_dataset$ORDER.f <- factor(mtDNA_dataset$ORDER)
contrasts(mtDNA_dataset$ORDER.f) <- contr.treatment(28)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = ND6, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
#DP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
#HDP
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#End of code for GLMM for mtDNA Dataset (records for each marker pooled based on BS)



#GLMM for sister pair DP Hemiptera vs HDP Aleyrodidae (mtDNA, markers pooled)

#Extract the data of DP Hemiptera vs HDP Aleyrodidae 
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)

#Specimen
DP_Hemi_HDP_Aley$SPECIMEN.f <- factor(DP_Hemi_HDP_Aley$SPECIMEN)
contrasts(DP_Hemi_HDP_Aley$SPECIMEN.f) <- contr.treatment(97)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Hemiptera vs HDP Aleyrodidae (mtDNA, markers separately)

#COI

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Hemi_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Hemi_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Hemiptera vs HDP Aleyrodidae



#GLMM for sister pair DP Psyllidae vs HDP Aleyrodidae (mtDNA, markers pooled)

#Extract the data of DP Psyllidae vs HDP Aleyrodidae 
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)

#Specimen
DP_Psyl_HDP_Aley$SPECIMEN.f <- factor(DP_Psyl_HDP_Aley$SPECIMEN)
contrasts(DP_Psyl_HDP_Aley$SPECIMEN.f) <- contr.treatment(10)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psyl_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psyl_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psyl_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psyl_HDP_Aley, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Psyllidae vs HDP Aleyrodidae (mtDNA, markers separately)

#COI

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Psyl_HDP_Aley <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Psyl_HDP_Aley" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Psyllidae vs HDP Aleyrodidae



#GLMM for sister pair DP Diptera vs HDP Cecidomyiidae (mtDNA, markers pooled)

#Extract the data of DP Diptera vs HDP Cecidomyiidae 
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)

#Specimen
DP_Dipt_HDP_Ceci$SPECIMEN.f <- factor(DP_Dipt_HDP_Ceci$SPECIMEN)
contrasts(DP_Dipt_HDP_Ceci$SPECIMEN.f) <- contr.treatment(125)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Dipt_HDP_Ceci, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Dipt_HDP_Ceci, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Dipt_HDP_Ceci, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Dipt_HDP_Ceci, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Diptera vs HDP Cecidomyiidae (mtDNA, markers separately)

#COI

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Dipt_HDP_Ceci <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Dipt_HDP_Ceci" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_Ceci$BS.f <- factor(DP_Dipt_HDP_Ceci$BS)
contrasts(DP_Dipt_HDP_Ceci$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_Ceci) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Diptera vs HDP Cecidomyiidae



#GLMM for sister pair DP Holometabola vs HDP Hymenoptera (mtDNA, markers pooled)

#Extract the data of DP Holometabola vs HDP Hymenoptera 
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)

#Specimen
DP_Holo_HDP_Hyme$SPECIMEN.f <- factor(DP_Holo_HDP_Hyme$SPECIMEN)
contrasts(DP_Holo_HDP_Hyme$SPECIMEN.f) <- contr.treatment(507)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Holo_HDP_Hyme, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Holo_HDP_Hyme, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Holo_HDP_Hyme, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Holo_HDP_Hyme, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Holometabola vs HDP Hymenoptera (mtDNA, markers separately)

#COI

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Holo_HDP_Hyme <- mtDNA_dataset [mtDNA_dataset$SISTER2 == "DP_Holo_HDP_Hyme" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Holometabola vs HDP Hymenoptera



#GLMM for sister pair DP Psocoptera vs HDP Phthiraptera (mtDNA, markers pooled)

#Extract the data of DP Psocoptera vs HDP Phthiraptera 
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)

#Specimen
DP_Psoc_HDP_Phth$SPECIMEN.f <- factor(DP_Psoc_HDP_Phth$SPECIMEN)
contrasts(DP_Psoc_HDP_Phth$SPECIMEN.f) <- contr.treatment(11)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psoc_HDP_Phth, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psoc_HDP_Phth, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psoc_HDP_Phth, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Psoc_HDP_Phth, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Psocoptera vs HDP Phthiraptera (mtDNA, markers separately)

#COI

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Psoc_HDP_Phth <- mtDNA_dataset [mtDNA_dataset$SISTER1 == "DP_Psoc_HDP_Phth" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Psocoptera vs HDP Phthiraptera



#GLMM for sister pair DP Hemiptera vs HDP Thysanoptera (mtDNA, markers pooled)

#Extract the data of DP Hemiptera vs HDP Thysanoptera 
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)

#Specimen
DP_Hemi_HDP_Thys$SPECIMEN.f <- factor(DP_Hemi_HDP_Thys$SPECIMEN)
contrasts(DP_Hemi_HDP_Thys$SPECIMEN.f) <- contr.treatment(94)


#Generalized linear mixed model with quasibinomial distribution, SPECIMEN as random effect
#Package MASS: glmmPQL

#TOTAL GC

DP_HDP_glmm_quasi <- glmmPQL(TOTAL_GC ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Thys, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-1

DP_HDP_glmm_quasi <- glmmPQL(GC1 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Thys, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-2

DP_HDP_glmm_quasi <- glmmPQL(GC2 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Thys, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#GC-3

DP_HDP_glmm_quasi <- glmmPQL(GC3 ~ BS.f, random = ~ 1 | SPECIMEN.f, family = quasibinomial(link = "logit"), data = DP_Hemi_HDP_Thys, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#GLM for sister pair DP Hemiptera vs HDP Thysanoptera (mtDNA, markers separately)

#COI

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "COI" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COII

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "COII" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

#Generalized linear model with quasibinomial distribution
DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#COIII

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "COIII" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#CYTB

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "CYTB" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP6

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ATP6" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ATP8

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ATP8" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND1

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND1" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND2

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND2" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND3

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND3" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND4" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND4L

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND4L" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND5

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND5" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#ND6

#Extract the data
DP_Hemi_HDP_Thys <- mtDNA_dataset [mtDNA_dataset$SISTER3 == "DP_Hemi_HDP_Thys" &
                                     mtDNA_dataset$MARKER == "ND6" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

#TOTAL GC

DP_HDP_glm_quasi <- glm (TOTAL_GC ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-1

DP_HDP_glm_quasi <- glm (GC1 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-2

DP_HDP_glm_quasi <- glm (GC2 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#GC-3

DP_HDP_glm_quasi <- glm (GC3 ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLMM, GLM for sister pair DP Hemiptera vs HDP Thysanoptera



#dNdS_Dataset
#--------------------

#Download dataset "dNdS_Dataset" from Dryad
#Set your working directory, example: setwd("~/Desktop/YOUR_FOLDER1/YOUR_FOLDER2/YOUR_FOLDER_WHERE_THE_DATASET_IS/")
#Read dataset
dNdS_Dataset <- read.delim (file = "dNdS_Dataset.txt", header = TRUE)

#GLMM for dN/dS ratio values (pooled based on breeding system)

#Categorical variables

#Breeding system
dNdS_Dataset$BS.f <- factor(dNdS_Dataset$BS)
contrasts(dNdS_Dataset$BS.f) <- contr.treatment(2)

#Order
dNdS_Dataset$ORDER.f <- factor(dNdS_Dataset$ORDER)
contrasts(dNdS_Dataset$ORDER.f) <- contr.treatment(25)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#dN/dS ratios

DP_HDP_glmm_quasi <- glmmPQL(cbind(DN,DS) ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = dNdS_Dataset, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#Subset of dNdS_Dataset excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)

#Order
NoZero$ORDER.f <- factor(NoZero$ORDER)
contrasts(NoZero$ORDER.f) <- contr.treatment(25)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#dN/dS ratios

DP_HDP_glmm_quasi <- glmmPQL(cbind(DN,DS) ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = NoZero, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])


#Subset of dNdS_Dataset excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)

#Order
NoHigh$ORDER.f <- factor(NoHigh$ORDER)
contrasts(NoHigh$ORDER.f) <- contr.treatment(25)


#Generalized linear mixed model with quasibinomial distribution, ORDER as random effect
#Package MASS: glmmPQL

#dN/dS ratios

DP_HDP_glmm_quasi <- glmmPQL(cbind(DN,DS) ~ BS.f, random = ~ 1 | ORDER.f, family = quasibinomial(link = "logit"), data = NoHigh, niter = 50)
summary(DP_HDP_glmm_quasi)

#Predicted values on the data scale
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"])
inv.logit(DP_HDP_glmm_quasi$coefficients$fixed["(Intercept)"] + DP_HDP_glmm_quasi$coefficients$fixed["BS.f2"])

#End of code for GLMM for dN/dS ratio values



#GLM for sister pair DP Hemiptera vs HDP Aleyrodidae (dN/dS ratios)

#Extract the data
DP_Hemi_HDP_Aley <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Hemi_HDP_Aley" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Aley$BS.f <- factor(DP_Hemi_HDP_Aley$BS)
contrasts(DP_Hemi_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Aley) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_Aley excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Hemi_HDP_Aley" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_Aley excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Hemi_HDP_Aley" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Aleyrodidae



#GLM for sister pair DP Psyllidae vs HDP Aleyrodidae (dN/dS ratios)

#Extract the data
DP_Psyl_HDP_Aley <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Psyl_HDP_Aley" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_Aley$BS.f <- factor(DP_Psyl_HDP_Aley$BS)
contrasts(DP_Psyl_HDP_Aley$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_Aley) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psyl_HDP_Aley excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Psyl_HDP_Aley" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psyl_HDP_Aley excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Psyl_HDP_Aley" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psyllidae vs HDP Aleyrodidae



#GLM for sister pair DP Diptera vs HDP Cecidomyiidae+Sciariidae (dN/dS ratios)

#Extract the data
DP_Dipt_HDP_CeciScia <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Dipt_HDP_CeciScia" , ]

#Categorical variables

#Breeding system
DP_Dipt_HDP_CeciScia$BS.f <- factor(DP_Dipt_HDP_CeciScia$BS)
contrasts(DP_Dipt_HDP_CeciScia$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Dipt_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Dipt_HDP_CeciScia excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Dipt_HDP_CeciScia" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Dipt_HDP_CeciScia excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Dipt_HDP_CeciScia" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Diptera vs HDP Cecidomyiidae+Sciariidae



#GLM for sister pair DP Mycetophilidae vs HDP Cecidomyiidae+Sciariidae (dN/dS ratios)

#Extract the data
DP_Myce_HDP_CeciScia <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Myce_HDP_CeciScia" , ]

#Categorical variables

#Breeding system
DP_Myce_HDP_CeciScia$BS.f <- factor(DP_Myce_HDP_CeciScia$BS)
contrasts(DP_Myce_HDP_CeciScia$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Myce_HDP_CeciScia) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Myce_HDP_CeciScia excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Myce_HDP_CeciScia" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Myce_HDP_CeciScia excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER2 == "DP_Myce_HDP_CeciScia" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Mycetophilidae vs HDP Cecidomyiidae+Sciariidae



#GLM for sister pair DP Hemiptera vs HDP Coccidae+Pseudococcidae (dN/dS ratios)

#Extract the data
DP_Hemi_HDP_CoccPseu <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Hemi_HDP_CoccPseu" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_CoccPseu$BS.f <- factor(DP_Hemi_HDP_CoccPseu$BS)
contrasts(DP_Hemi_HDP_CoccPseu$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_CoccPseu excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Hemi_HDP_CoccPseu" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_CoccPseu excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Hemi_HDP_CoccPseu" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Coccidae+Pseudococcidae



#GLM for sister pair DP Psyllidae vs HDP Coccidae+Pseudococcidae (dN/dS ratios)

#Extract the data
DP_Psyl_HDP_CoccPseu <- dNdS_Dataset [dNdS_Dataset$SISTER4 == "DP_Psyl_HDP_CoccPseu" , ]

#Categorical variables

#Breeding system
DP_Psyl_HDP_CoccPseu$BS.f <- factor(DP_Psyl_HDP_CoccPseu$BS)
contrasts(DP_Psyl_HDP_CoccPseu$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Psyl_HDP_CoccPseu) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psyl_HDP_CoccPseu excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER4 == "DP_Psyl_HDP_CoccPseu" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psyl_HDP_CoccPseu excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER4 == "DP_Psyl_HDP_CoccPseu" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psyllidae vs HDP Coccidae+Pseudococcidae



#GLM for sister pair DP Holometabola vs HDP Hymenoptera (dN/dS ratios)

#Extract the data
DP_Holo_HDP_Hyme <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Holo_HDP_Hyme" , ]

#Categorical variables

#Breeding system
DP_Holo_HDP_Hyme$BS.f <- factor(DP_Holo_HDP_Hyme$BS)
contrasts(DP_Holo_HDP_Hyme$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Holo_HDP_Hyme) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Holo_HDP_Hyme excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Holo_HDP_Hyme" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Holo_HDP_Hyme excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER3 == "DP_Holo_HDP_Hyme" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Holometabola vs HDP Hymenoptera



#GLM for sister pair DP Psocoptera vs HDP Phthiraptera (dN/dS ratios)

#Extract the data
DP_Psoc_HDP_Phth <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Psoc_HDP_Phth" , ]

#Categorical variables

#Breeding system
DP_Psoc_HDP_Phth$BS.f <- factor(DP_Psoc_HDP_Phth$BS)
contrasts(DP_Psoc_HDP_Phth$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Psoc_HDP_Phth) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psoc_HDP_Phth excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Psoc_HDP_Phth" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Psoc_HDP_Phth excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER1 == "DP_Psoc_HDP_Phth" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Psocoptera vs HDP Phthiraptera



#GLM for sister pair DP Hemiptera vs HDP Thysanoptera (dN/dS ratios)

#Extract the data
DP_Hemi_HDP_Thys <- dNdS_Dataset [dNdS_Dataset$SISTER5 == "DP_Hemi_HDP_Thys" , ]

#Categorical variables

#Breeding system
DP_Hemi_HDP_Thys$BS.f <- factor(DP_Hemi_HDP_Thys$BS)
contrasts(DP_Hemi_HDP_Thys$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), DP_Hemi_HDP_Thys) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_Thys excluding dN/dS = 0 
NoZero <- dNdS_Dataset [dNdS_Dataset$SISTER5 == "DP_Hemi_HDP_Thys" & dNdS_Dataset$DNDS != 0 , ]

#Categorical variables

#Breeding system
NoZero$BS.f <- factor(NoZero$BS)
contrasts(NoZero$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoZero) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])


#Subset of DP_Hemi_HDP_Thys excluding dN/dS > 1 
NoHigh <- dNdS_Dataset [dNdS_Dataset$SISTER5 == "DP_Hemi_HDP_Thys" & dNdS_Dataset$DNDS < 1 , ]

#Categorical variables

#Breeding system
NoHigh$BS.f <- factor(NoHigh$BS)
contrasts(NoHigh$BS.f) <- contr.treatment(2)


#Generalized linear model with quasibinomial distribution
#Package stats: glm

DP_HDP_glm_quasi <- glm (cbind(DN,DS) ~ BS.f, family = quasibinomial (link = "logit"), NoHigh) 
summary(DP_HDP_glm_quasi)

inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"])
inv.logit(DP_HDP_glm_quasi$coef["(Intercept)"] + DP_HDP_glm_quasi$coef["BS.f2"])

#End of code for GLM for sister pair DP Hemiptera vs HDP Thysanoptera
