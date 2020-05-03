setwd("X:/My Documents/Machine Learning")
#setwd("C:/Users/Martin/Documents/Universiteit/Master/Machine Learning")
rm(list=ls())
diabeticData <- read.csv("diabetic_data.csv", na.strings = "?")
IDsMapping <- read.csv("IDs_mapping.csv", na.strings = "?")
str(diabeticData)
summary(diabeticData)
colSums(is.na(diabeticData))

# Remove column with more than 40% missing values
diabeticData$weight <- NULL
diabeticData$payer_code <- NULL
diabeticData$medical_specialty <- NULL

# Race dummy
diabeticData$raceCaucasian <- rep(0,nrow(diabeticData))
diabeticData$raceAfricanAmerican <- rep(0,nrow(diabeticData))
diabeticData$raceAsian <- rep(0,nrow(diabeticData))
diabeticData$raceHispanic <- rep(0,nrow(diabeticData))
diabeticData$raceOther <- rep(0,nrow(diabeticData))
diabeticData$raceMissing <- rep(0,nrow(diabeticData))
diabeticData$raceCaucasian[diabeticData$race == "Caucasian"] <- 1
diabeticData$raceAfricanAmerican[diabeticData$race == "AfricanAmerican"] <- 1
diabeticData$raceAsian[diabeticData$race == "Asian"] <- 1
diabeticData$raceHispanic[diabeticData$race == "Hispanic"] <- 1
diabeticData$raceOther[diabeticData$race == "Other"] <- 1
diabeticData$raceMissing[is.na(diabeticData$race)] <- 1
diabeticData$race <- NULL

# Gender dummy
diabeticData$genderMale <- rep(0,nrow(diabeticData))
diabeticData$genderMale[diabeticData$gender == "Male"] <- 1
diabeticData$gender <- NULL

# Age dummy for 3 categories. Young, middle aged, and old
diabeticData$ageYoung <- rep(0,nrow(diabeticData))
diabeticData$ageYoung[diabeticData$age == "[0-10)"] <- 1
diabeticData$ageYoung[diabeticData$age == "[10-20)"] <- 1
diabeticData$ageYoung[diabeticData$age == "[20-30)"] <- 1

diabeticData$ageMiddle <- rep(0,nrow(diabeticData))
diabeticData$ageMiddle[diabeticData$age == "[30-40)"] <- 1
diabeticData$ageMiddle[diabeticData$age == "[40-50)"] <- 1
diabeticData$ageMiddle[diabeticData$age == "[50-60)"] <- 1

diabeticData$ageOld <- rep(0,nrow(diabeticData))
diabeticData$ageOld[diabeticData$age == "[60-70)"] <- 1
diabeticData$ageOld[diabeticData$age == "[70-80)"] <- 1
diabeticData$ageOld[diabeticData$age == "[80-90)"] <- 1
diabeticData$ageOld[diabeticData$age == "[90-100)"] <- 1

diabeticData$age <- NULL

# Admission type, discharge disposition, and admission source
# Have resp. 8, 29, and 26 possible values
# Integer
# Makes no sense to make so many dummies

diabeticData$admission_type_id <- NULL
diabeticData$discharge_disposition_id <- NULL
diabeticData$admission_source_id <- NULL

# Diagnosis variable is a factor
# Should be text
# Makes no sense to have a independent variable which is text
# No dummy options either

diabeticData$diag_1 <- NULL
diabeticData$diag_2 <- NULL
diabeticData$diag_3 <- NULL

diabeticData$max_glu_serum_none <- rep(0,nrow(diabeticData))
diabeticData$max_glu_serum_none[diabeticData$max_glu_serum == "None"] <- 1
diabeticData$max_glu_serum_normal <- rep(0,nrow(diabeticData))
diabeticData$max_glu_serum_normal[diabeticData$max_glu_serum == "Norm"] <- 1
diabeticData$max_glu_serum_200 <- rep(0,nrow(diabeticData))
diabeticData$max_glu_serum_200[diabeticData$max_glu_serum == ">200"] <- 1
diabeticData$max_glu_serum_300 <- rep(0,nrow(diabeticData))
diabeticData$max_glu_serum_300[diabeticData$max_glu_serum == ">300"] <- 1
diabeticData$max_glu_serum <- NULL

diabeticData$A1Cresult_none <- rep(0,nrow(diabeticData))
diabeticData$A1Cresult_none[diabeticData$A1Cresult == "None"] <- 1
diabeticData$A1Cresult_normal <- rep(0,nrow(diabeticData))
diabeticData$A1Cresult_normal[diabeticData$A1Cresult == "Norm"] <- 1
diabeticData$A1Cresult_7 <- rep(0,nrow(diabeticData))
diabeticData$A1Cresult_7[diabeticData$A1Cresult == ">7"] <- 1
diabeticData$A1Cresult_8 <- rep(0,nrow(diabeticData))
diabeticData$A1Cresult_8[diabeticData$A1Cresult == ">8"] <- 1
diabeticData$A1Cresult <- NULL

#metformin
diabeticData$metformin_dummy <- rep(0,nrow(diabeticData))
diabeticData$metformin_dummy[diabeticData$metformin == "Up"] <- 1
diabeticData$metformin_dummy[diabeticData$metformin == "Down"] <- 1
diabeticData$metformin_dummy[diabeticData$meftormin == "Steady"] <- 1
diabeticData$metformin <- NULL

#repaglinide
diabeticData$repaglinide_dummy <- rep(0,nrow(diabeticData))
diabeticData$repaglinide_dummy[diabeticData$repaglinide == "Up"] <- 1
diabeticData$repaglinide_dummy[diabeticData$repaglinide == "Down"] <- 1
diabeticData$repaglinide_dummy[diabeticData$repaglinide == "Steady"] <- 1
diabeticData$repaglinide <- NULL

#nateglinide
diabeticData$nateglinide_dummy <- rep(0,nrow(diabeticData))
diabeticData$nateglinide_dummy[diabeticData$nateglinide == "Up"] <- 1
diabeticData$nateglinide_dummy[diabeticData$nateglinide == "Down"] <- 1
diabeticData$nateglinide_dummy[diabeticData$nateglinide == "Steady"] <- 1
diabeticData$nateglinide <- NULL

#chlorpropamide
diabeticData$chlorpropamide_dummy <- rep(0,nrow(diabeticData))
diabeticData$chlorpropamide_dummy[diabeticData$chlorpropamide == "Up"] <- 1
diabeticData$chlorpropamide_dummy[diabeticData$chlorpropamide == "Down"] <- 1
diabeticData$chlorpropamide_dummy[diabeticData$chlorpropamide == "Steady"] <- 1
diabeticData$chlorpropamide <- NULL

#glimepiride
diabeticData$glimepiride_dummy <- rep(0,nrow(diabeticData))
diabeticData$glimepiride_dummy[diabeticData$glimepiride == "Up"] <- 1
diabeticData$glimepiride_dummy[diabeticData$glimepiride == "Down"] <- 1
diabeticData$glimepiride_dummy[diabeticData$glimepiride == "Steady"] <- 1
diabeticData$glimepiride <- NULL

#acetohexamide
diabeticData$acetohexamide_dummy <- rep(0,nrow(diabeticData))
diabeticData$acetohexamide_dummy[diabeticData$acetohexamide == "Up"] <- 1
diabeticData$acetohexamide_dummy[diabeticData$acetohexamide == "Down"] <- 1
diabeticData$acetohexamide_dummy[diabeticData$acetohexamide == "Steady"] <- 1
diabeticData$acetohexamide <- NULL

#glipizide
diabeticData$glipizide_dummy <- rep(0,nrow(diabeticData))
diabeticData$glipizide_dummy[diabeticData$glipizide == "Up"] <- 1
diabeticData$glipizide_dummy[diabeticData$glipizide == "Down"] <- 1
diabeticData$glipizide_dummy[diabeticData$glipizide == "Steady"] <- 1
diabeticData$glipizide <- NULL

#glyburide
diabeticData$glyburide_dummy <- rep(0,nrow(diabeticData))
diabeticData$glyburide_dummy[diabeticData$glyburide == "Up"] <- 1
diabeticData$glyburide_dummy[diabeticData$glyburide == "Down"] <- 1
diabeticData$glyburide_dummy[diabeticData$glyburide == "Steady"] <- 1
diabeticData$glyburide <- NULL

#tolbutamide
diabeticData$tolbutamide_dummy <- rep(0,nrow(diabeticData))
diabeticData$tolbutamide_dummy[diabeticData$tolbutamide == "Up"] <- 1
diabeticData$tolbutamide_dummy[diabeticData$tolbutamide == "Down"] <- 1
diabeticData$tolbutamide_dummy[diabeticData$tolbutamide == "Steady"] <- 1
diabeticData$tolbutamide <- NULL

#pioglitazone
diabeticData$pioglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$pioglitazone_dummy[diabeticData$pioglitazone == "Up"] <- 1
diabeticData$pioglitazone_dummy[diabeticData$pioglitazone == "Down"] <- 1
diabeticData$pioglitazone_dummy[diabeticData$pioglitazone == "Steady"] <- 1
diabeticData$pioglitazone <- NULL

#rosiglitazone
diabeticData$rosiglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$rosiglitazone_dummy[diabeticData$rosiglitazone == "Up"] <- 1
diabeticData$rosiglitazone_dummy[diabeticData$rosiglitazone == "Down"] <- 1
diabeticData$rosiglitazone_dummy[diabeticData$rosiglitazone == "Steady"] <- 1
diabeticData$rosiglitazone <- NULL

#acarbose
diabeticData$acarbose_dummy <- rep(0,nrow(diabeticData))
diabeticData$acarbose_dummy[diabeticData$acarbose == "Up"] <- 1
diabeticData$acarbose_dummy[diabeticData$acarbose == "Down"] <- 1
diabeticData$acarbose_dummy[diabeticData$acarbose == "Steady"] <- 1
diabeticData$acarbose <- NULL

#miglitol
diabeticData$miglitol_dummy <- rep(0,nrow(diabeticData))
diabeticData$miglitol_dummy[diabeticData$miglitol == "Up"] <- 1
diabeticData$miglitol_dummy[diabeticData$miglitol == "Down"] <- 1
diabeticData$miglitol_dummy[diabeticData$miglitol == "Steady"] <- 1
diabeticData$miglitol <- NULL

#troglitazone
diabeticData$troglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$troglitazone_dummy[diabeticData$troglitazone == "Up"] <- 1
diabeticData$troglitazone_dummy[diabeticData$troglitazone == "Down"] <- 1
diabeticData$troglitazone_dummy[diabeticData$troglitazone == "Steady"] <- 1
diabeticData$troglitazone <- NULL

#tolozamide
diabeticData$tolazamide_dummy <- rep(0,nrow(diabeticData))
diabeticData$tolazamide_dummy[diabeticData$tolazamide == "Up"] <- 1
diabeticData$tolazamide_dummy[diabeticData$tolazamide == "Down"] <- 1
diabeticData$tolazamide_dummy[diabeticData$tolazamide == "Steady"] <- 1
diabeticData$tolazamide <- NULL

#examide
diabeticData$examide_dummy <- rep(0,nrow(diabeticData))
diabeticData$examide_dummy[diabeticData$examide == "Up"] <- 1
diabeticData$examide_dummy[diabeticData$examide == "Down"] <- 1
diabeticData$examide_dummy[diabeticData$examide == "Steady"] <- 1
diabeticData$examide <- NULL

#citoglipton
diabeticData$citoglipton_dummy <- rep(0,nrow(diabeticData))
diabeticData$citoglipton_dummy[diabeticData$citoglipton == "Up"] <- 1
diabeticData$citoglipton_dummy[diabeticData$citoglipton == "Down"] <- 1
diabeticData$citoglipton_dummy[diabeticData$citoglipton == "Steady"] <- 1
diabeticData$citoglipton <- NULL

#insulin
diabeticData$insulin_dummy <- rep(0,nrow(diabeticData))
diabeticData$insulin_dummy[diabeticData$insulin == "Up"] <- 1
diabeticData$insulin_dummy[diabeticData$insulin == "Down"] <- 1
diabeticData$insulin_dummy[diabeticData$insulin == "Steady"] <- 1
diabeticData$insulin <- NULL

#glyburide.metformin
diabeticData$glyburide.metformin_dummy <- rep(0,nrow(diabeticData))
diabeticData$glyburide.metformin_dummy[diabeticData$glyburide.metformin == "Up"] <- 1
diabeticData$glyburide.metformin_dummy[diabeticData$glyburide.metformin == "Down"] <- 1
diabeticData$glyburide.metformin_dummy[diabeticData$glyburide.metformin == "Steady"] <- 1
diabeticData$glyburide.metformin <- NULL

#glipizide.metformin
diabeticData$glipizide.metformin_dummy <- rep(0,nrow(diabeticData))
diabeticData$glipizide.metformin_dummy[diabeticData$glipizide.metformin == "Up"] <- 1
diabeticData$glipizide.metformin_dummy[diabeticData$glipizide.metformin == "Down"] <- 1
diabeticData$glipizide.metformin_dummy[diabeticData$glipizide.metformin == "Steady"] <- 1
diabeticData$glipizide.metformin <- NULL

#glimepiride.pioglitazone
diabeticData$glimepiride.pioglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$glimepiride.pioglitazone_dummy[diabeticData$glimepiride.pioglitazone == "Up"] <- 1
diabeticData$glimepiride.pioglitazone_dummy[diabeticData$glimepiride.pioglitazone == "Down"] <- 1
diabeticData$glimepiride.pioglitazone_dummy[diabeticData$glimepiride.pioglitazone == "Steady"] <- 1
diabeticData$glimepiride.pioglitazone <- NULL

#metformin.rosiglitazone
diabeticData$metformin.rosiglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$metformin.rosiglitazone_dummy[diabeticData$metformin.rosiglitazone == "Up"] <- 1
diabeticData$metformin.rosiglitazone_dummy[diabeticData$metformin.rosiglitazone == "Down"] <- 1
diabeticData$metformin.rosiglitazone_dummy[diabeticData$metformin.rosiglitazone == "Steady"] <- 1
diabeticData$metformin.rosiglitazone <- NULL

#metformin.pioglitazone
diabeticData$metformin.pioglitazone_dummy <- rep(0,nrow(diabeticData))
diabeticData$metformin.pioglitazone_dummy[diabeticData$metformin.pioglitazone == "Up"] <- 1
diabeticData$metformin.pioglitazone_dummy[diabeticData$metformin.pioglitazone == "Down"] <- 1
diabeticData$metformin.pioglitazone_dummy[diabeticData$metformin.pioglitazone == "Steady"] <- 1
diabeticData$metformin.pioglitazone <- NULL

# Change in diabetic medications (either dosage or generic name)
diabeticData$change_dummy <- rep(0,nrow(diabeticData))
diabeticData$change_dummy[diabeticData$change == "Ch"] <- 1
diabeticData$change <- NULL

# If there was any diabetic medication prescribed
diabeticData$diabetesMed_dummy <- rep(0,nrow(diabeticData))
diabeticData$diabetesMed_dummy[diabeticData$diabetesMed == "Yes"] <- 1
diabeticData$diabetesMed <- NULL

# Readmitted dummy
diabeticData$readmitted_dummy <- rep(1,nrow(diabeticData))
diabeticData$readmitted_dummy[diabeticData$readmitted == "NO"] <- 0
diabeticData$readmitted_dummy[diabeticData$readmitted == ">30"] <- 0
diabeticData$readmitted <- NULL

newDiabeticData <- diabeticData[-which(duplicated(diabeticData$patient_nbr)),]

lm(diabeticData$readmitted_dummy ~ diabeticData$genderMale)

write.csv(diabeticData, file = "diabeticData.csv")
write.csv(newDiabeticData, file="newDiabeticData.csv")