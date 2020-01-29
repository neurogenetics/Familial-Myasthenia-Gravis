
#load libraries
library(readxl)

#set working directory
setwd("~/Documents/1.Projects/1.MG_epidemiology/A__Manuscript_Drafts/")

#load the data
data0 <- read_xlsx("clinical_data_for_Josh_paper_v1.xlsx", sheet="Sheet1")
data1 <- as.data.frame(data0)

#output the parameters
#number of cases
numberCases <- nrow(data1)

#number of familial cases
numberFamilialCases <- nrow(subset(data1, data1$family_history_of_mg=="yes"))
percentFamilialCases <- numberFamilialCases*100/numberCases

#mean age at symptom onset
meanAgeAtSymptomOnsetAllCases <- mean(data1$age_at_symptom_onset)
stdevAgeAtSymptomOnsetAllCases <- sd(data1$age_at_symptom_onset)

#female to male ratio
males <- nrow(subset(data1, data1$gender=="male"))
females <- nrow(subset(data1, data1$gender=="female"))
ratio <- males/females

#early onset cases
earlyOnset <- nrow(subset(data1, data1$age_at_symptom_onset<40))
percentEarlyOnset <- earlyOnset*100/numberCases

#thymectomy
numberThymectomy <- nrow(subset(data1,data1$thymectomy=="yes"))
percentThymectomy <- numberThymectomy*100/numberCases

#age of familial cases
familial <- subset(data1, data1$family_history_of_mg=="yes")
meanAgeAtSymptomOnsetFamilialCases <- mean(familial$age_at_symptom_onset)
stdevAgeAtSymptomOnsetFamilialCases <- sd(familial$age_at_symptom_onset)

#age of sporadic cases
sporadic <- subset(data1, data1$family_history_of_mg=="no")
meanAgeAtSymptomOnsetSporadicCases <- mean(sporadic$age_at_symptom_onset)
stdevAgeAtSymptomOnsetSporadicCases <- sd(sporadic$age_at_symptom_onset)

#test of familial versus sporadic age at onset
t.test(sporadic$age_at_symptom_onset, familial$age_at_symptom_onset)

#number of familial samples with age of onset less than 40
numberYoungFamilial <- nrow(subset(familial, familial$age_at_symptom_onset<40))

##########################################################################
#cases with personal history of autoimmune disease
#####notes
#280 samples had a personal history in the first column "other_autoimmune_diseases"
#There were no addiitional samples with a personal history of autoimmune disease based on the second column "other_autoimmune_disease"
#752 samples definitely did not have a personal history of an autoimmune disease
#188 samples had a personal history as they had one of the first level diseases
#92 samples had a personal history based on one of the second level diseases
#of these 92, we delete jhu088 (ALS), jhu049 (enlarged prostate), iu019 (fibromyalgia), jhu058 (lyme) and iu041 (parathyroid surgery)
#this brings the total down to 275

#change the patient who had als
data1$other_autoimmune_diseases[data1$patient_id2=="jhu088"] <- "no"
data1$other_autoimmune_disease[data1$patient_id2=="jhu088"] <- "no"
data1$other_autoimmune_disease_name_1[data1$patient_id2=="jhu088"] <- -999

#change the patient who had an enlarged prostate
data1$other_autoimmune_diseases[data1$patient_id2=="jhu049"] <- "no"
data1$other_autoimmune_disease[data1$patient_id2=="jhu049"] <- "no"
data1$other_autoimmune_disease_name_1[data1$patient_id2=="jhu049"] <- -999

#change the patient who had fibromyalgia
data1$other_autoimmune_diseases[data1$patient_id2=="iu019"] <- "no"
data1$other_autoimmune_disease[data1$patient_id2=="iu019"] <- "no"
data1$other_autoimmune_disease_name_1[data1$patient_id2=="iu019"] <- -999

#change the patient who had lyme disease
data1$other_autoimmune_diseases[data1$patient_id2=="jhu058"] <- "no"
data1$other_autoimmune_disease[data1$patient_id2=="jhu058"] <- "no"
data1$other_autoimmune_disease_name_1[data1$patient_id2=="jhu058"] <- -999

#change the patient who had parathyroid surgery
data1$other_autoimmune_diseases[data1$patient_id2=="iu041"] <- "no"
data1$other_autoimmune_disease[data1$patient_id2=="iu041"] <- "no"
data1$other_autoimmune_disease_name_1[data1$patient_id2=="iu041"] <- -999

###now generate the percentages
numberPersonalHistoryAutoimmuneDisease <- nrow(subset(data1, data1$other_autoimmune_diseases=="yes"))
percentPersonalHistoryAutoimmuneDisease <- numberPersonalHistoryAutoimmuneDisease*100/numberCases

##########################################################################
#cases with family history of autoimmune disease
#change the patient who had angiotropic lymphoma
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="iu069"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="iu069"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="iu069"] <- -999

#change the patient who had fibromyalgia
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="jhu076"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="jhu076"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="jhu076"] <- -999

#change the patient who had irritable bowel syndrome
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="uic057"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="uic057"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="uic057"] <- -999

#change the patient who had ititis
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="jhu092"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="jhu092"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="jhu092"] <- -999

#change the patient who had muscular dystrophy
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="iu051"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="iu051"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="iu051"] <- -999

#change the patient who had neuropathy
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="utsw003"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="utsw003"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="utsw003"] <- -999

#change the patient who had parkinson's disease
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="utsw025"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="utsw025"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="utsw025"] <- -999

#change the patient who had ptosis
data1$fh_of_other_autoimmune_disease[data1$patient_id2=="uic043"] <- "no"
data1$autoimmune_disease_relationship[data1$patient_id2=="uic043"] <- -999
data1$autoimmune_disease_disease[data1$patient_id2=="uic043"] <- -999

#change the 2nd member and 3rd member column
data1$fh_of_other_autoimmune_disease_2nd_member[data1$patient_id2=="uic055"] <- "yes"
data1$fh_of_other_autoimmune_disease_3rd_member[data1$patient_id2=="WU004"] <- "yes"
data1$fh_of_other_autoimmune_disease_3rd_member[data1$patient_id2=="ubc101"] <- "yes"
data1$fh_of_other_autoimmune_disease_3rd_member[data1$patient_id2=="jhu040"] <- "yes"


numberFamilyHistoryAutoimmuneDisease <- nrow(subset(data1, data1$fh_of_other_autoimmune_disease=="yes"))
percentFamilyHistoryAutoimmuneDisease <- numberFamilyHistoryAutoimmuneDisease*100/numberCases

##########################################################################
#breakdown of personal history autoimmune diseases
#There are 110 Thyroiditis cases
##In the other_autoimmune_disease_name_1, there are 2 hyperthyroid and 5 graves disease
###In the other_autoimmune_disease_name_2, there is 1 hypothyroid
####In the other_autoimmune_disease_name_3, there are no thyroid cases
#####Total = 110 + 7 + 1 = 118 = 11.4%
numberThyroidPersonal <- nrow(subset(data1, data1$thyroiditis == "yes" |
                              data1$other_autoimmune_disease_name_1 == "graves disease" | 
                               data1$other_autoimmune_disease_name_1 == "hyperthyroid" |
                               data1$other_autoimmune_disease_name_1 == "hypothyroid" |
                               data1$other_autoimmune_disease_name_2 == "graves disease" | 
                               data1$other_autoimmune_disease_name_2 == "hyperthyroid" |
                               data1$other_autoimmune_disease_name_2 == "hypothyroid" |
                               data1$other_autoimmune_disease_name_3 == "graves disease" | 
                               data1$other_autoimmune_disease_name_3 == "hyperthyroid" |
                               data1$other_autoimmune_disease_name_3 == "hypothyroid" ))
percentThyroidPersonal  <- numberThyroidPersonal *100/numberCases

#


#number in the "blood_disease" column = 33
##number in the "other_autoimmune_disease_name_1 column = 0
###number in the "other_autoimmune_disease_name_2 column = 0
####number in the "other_autoimmune_disease_name_3 column = 0
#####Total = 33 = 3.2%

numberBloodPersonal  <- nrow(subset(data1, data1$blood_disease == "yes" |
                           data1$other_autoimmune_disease_name_1 == "autoimmune hemolytic anemia" | 
                             data1$other_autoimmune_disease_name_1 == "autoimmune thrombocytopenic purpura" |
                             data1$other_autoimmune_disease_name_2 == "autoimmune hemolytic anemia" | 
                             data1$other_autoimmune_disease_name_2 == "autoimmune thrombocytopenic purpura" |
                             data1$other_autoimmune_disease_name_3 == "autoimmune hemolytic anemia" | 
                             data1$other_autoimmune_disease_name_3 == "autoimmune thrombocytopenic purpura"))
percentBloodPersonal  <- numberBloodPersonal *100/numberCases


#number in the "rheumatoid_arthritis" column = 27
##number in the "other_autoimmune_disease_name_1 column = 1
###number in the "other_autoimmune_disease_name_2 column = 0
####number in the "other_autoimmune_disease_name_3 column = 0
#####Total = 27 + 1 = 28 = 2.7%
numberRAPersonal <- nrow(subset(data1, data1$rheumatoid_arthritis == "yes" |
                        data1$other_autoimmune_disease_name_1 == "rheumatoid arthritis" | 
                          data1$other_autoimmune_disease_name_2 == "rheumatoid arthritis" | 
                          data1$other_autoimmune_disease_name_3 == "rheumatoid arthritis" ))
percentRAPersonal <- numberRAPersonal*100/numberCases

percentRAPersonal
########JOSH ADDED ##############################
#additional diseases for figure 2A: personal history
##Diabetes
##matches manual count by bryan = 29
numberDiabetesPersonal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "diabetes"|
                                data1$other_autoimmune_disease_name_2 == "diabetes"))

percentDiabetesPersonal<- numberDiabetesPersonal*100/numberCases


##Lupus
###matches manual count by bryan = 20
numberSLEPersonal <- nrow(subset(data1,data1$lupus == "yes"|
                                   data1$other_autoimmune_disease_name_1 == "lupus"|
                                   data1$other_autoimmune_disease_name_2 == "lupus"))
percentSLEPersonal<- numberSLEPersonal*100/numberCases


##Psoriasis
###matches manual count by bryan = 17
numberPsoriasisPersonal <- nrow(subset(data1, data1$psoriasis == "yes"))

percentPsoriasisPersonal <- numberPsoriasisPersonal*100/numberCases

##Pernicious anemia
###matches manual count by bryan = 16
numberPerniciousanemiaPersonal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 =="pernicious anemia"|
                                               data1$other_autoimmune_disease_name_1 == "vitamin b12 deficiency"))

percentPerniciousanemiaPersonal <- numberPerniciousanemiaPersonal*100/numberCases

##Others
###matches manual count by bryan = 50
numberOtherPersonal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "addisons" |
                                     data1$other_autoimmune_disease_name_1 == "allergies" |
                                     data1$other_autoimmune_disease_name_1 == "asthma" |
                                     data1$other_autoimmune_disease_name_1 == "bells palsy" |
                                     data1$other_autoimmune_disease_name_1 == "bronchial asthma" |
                                     data1$other_autoimmune_disease_name_1 == "celiac disease" |
                                     data1$other_autoimmune_disease_name_1 == "crohn's disease" |
                                     data1$other_autoimmune_disease_name_1 == "darier's skin disease" |
                                     data1$other_autoimmune_disease_name_1 == "eczema" |
                                     data1$other_autoimmune_disease_name_1 == "guillain-barre' syndrome" |
                                     data1$other_autoimmune_disease_name_1 == "immune-mediated neutropenia" |
                                     data1$other_autoimmune_disease_name_1 == "lambert-eaton syndrome" |
                                     data1$other_autoimmune_disease_name_1 == "lichen planus" |
                                     data1$other_autoimmune_disease_name_1 == "lymphic encephalitis" |
                                     data1$other_autoimmune_disease_name_1 == "melanoma disease" |
                                     data1$other_autoimmune_disease_name_1 == "mixed connective tissue disease" |
                                     data1$other_autoimmune_disease_name_1 == "multiple sclerosis" |
                                     data1$other_autoimmune_disease_name_1 == "nephritis" |
                                     data1$other_autoimmune_disease_name_1 == "optic neuritis" |
                                     data1$other_autoimmune_disease_name_1 == "pemphigus vulgaris" |
                                     data1$other_autoimmune_disease_name_1 == "polymalagia rheumatica" |
                                     data1$other_autoimmune_disease_name_1 == "polymyalgia rheumatica" |
                                     data1$other_autoimmune_disease_name_1 == "polymyositis" |
                                     data1$other_autoimmune_disease_name_1 == "pyoderma " |
                                     data1$other_autoimmune_disease_name_1 == "sarcoidosis" |
                                     data1$other_autoimmune_disease_name_1 == "schloderma variant" |
                                     data1$other_autoimmune_disease_name_1 == "sjogren's syndrome" |
                                     data1$other_autoimmune_disease_name_1 == "sjogren's syndrome " |
                                     data1$other_autoimmune_disease_name_1 == "stiff person's syndrome" |
                                     data1$other_autoimmune_disease_name_1 == "ulcerative colitis" |
                                     data1$other_autoimmune_disease_name_1 == "vasculitis" |
                                     data1$other_autoimmune_disease_name_1 == "vitiligo" |
                                     data1$other_autoimmune_disease_name_2 == "polymyositis" |
                                     data1$other_autoimmune_disease_name_2 == "alopecia" |
                                     data1$other_autoimmune_disease_name_2 == "allergies" |
                                     data1$other_autoimmune_disease_name_3 == "lambert-eaton" |
                                     data1$other_autoimmune_disease_name_3 == "melanoma"))

percentOtherPersonal <- numberOtherPersonal*100/numberCases

##############calculations for figure 2C personal history############################# 
#multiple sclerosis
fig2c_ms_personal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "multiple sclerosis"))
percentfig2c_ms_personal <- fig2c_ms_personal*100/numberCases
percentfig2c_ms_personal
#Sarcoidosis
fig2c_sarc_personal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "sarcoidosis"))
percentfig2c_sarc_personal <- fig2c_sarc_personal*100/numberCases
percentfig2c_sarc_personal
#pemphigus vulgaris
fig2c_pv_personal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "pemphigus vulgaris"))
percentfig2c_pv_personal <- fig2c_pv_personal*100/numberCases
percentfig2c_pv_personal
#sjorgen's syndrome
fig2c_ss_personal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "sjogren's syndrome"))
percentfig2c_ss_personal <- fig2c_ss_personal*100/numberCases
percentfig2c_ss_personal
#vitiligo
fig2c_vitiligo_personal <- nrow(subset(data1, data1$other_autoimmune_disease_name_1 == "vitiligo"))
percentfig2c_vitiligo_personal <- fig2c_vitiligo_personal*100/numberCases
percentfig2c_vitiligo_personal
#IBD
fig2c_ibd_personal <- nrow(subset(data1,data1$other_autoimmune_disease_name_1 == "crohn's disease"|
                          data1$other_autoimmune_disease_name_1 == "colitis"|
                          data1$other_autoimmune_disease_name_1 == "ulcerative colitis"|
                          data1$other_autoimmune_disease_name_1 == "ulcerative collitis"))

percentfig2c_ibd_personal <- fig2c_ibd_personal*100/numberCases
percentfig2c_ibd_personal
##########################################################################
#Family History Autoimmune diseases
##Thyroid Disease
numberThyroidFamily <- nrow(subset(data1, 
   data1$autoimmune_disease_disease == "euthyroid, graves, opthalmopathy" |
   data1$autoimmune_disease_disease == "goitre" |
   data1$autoimmune_disease_disease == "graves disease" |
   data1$autoimmune_disease_disease == "hashimoto's disease" |
   data1$autoimmune_disease_disease == "hashimoto's thyroid" |
   data1$autoimmune_disease_disease == "hyperactive thyroid" |
   data1$autoimmune_disease_disease == "hyperthyroid" |
   data1$autoimmune_disease_disease == "hypothyroid" |
   data1$autoimmune_disease_disease == "thyroid" |
   data1$autoimmune_disease_disease == "thyroid disease" |
   data1$autoimmune_disease_disease == "thyroid disease, melanoma" |
   data1$autoimmune_disease_disease == "thyroid disease, rheumatoid arthritis" |
   data1$autoimmune_disease_disease == "thyroid disease/thyroid disease" |
   data1$autoimmune_disease_disease == "thyroiditis" |
   data1$autoimmune_disease_disease == "thyroiditis, diabetes" |
   data1$autoimmune_disease_disease_2nd_member == "euthyroid, graves, opthalmopathy" |
   data1$autoimmune_disease_disease_2nd_member == "goitre" |
   data1$autoimmune_disease_disease_2nd_member == "graves disease" |
   data1$autoimmune_disease_disease_2nd_member == "hashimoto's disease" |
   data1$autoimmune_disease_disease_2nd_member == "hashimoto's thyroid" |
   data1$autoimmune_disease_disease_2nd_member == "hyperactive thyroid" |
   data1$autoimmune_disease_disease_2nd_member == "hyperthyroid" |
   data1$autoimmune_disease_disease_2nd_member == "hypothyroid" |
   data1$autoimmune_disease_disease_2nd_member == "thyroid" |
   data1$autoimmune_disease_disease_2nd_member == "thyroid disease" |
   data1$autoimmune_disease_disease_2nd_member == "thyroid disease, melanoma" |
   data1$autoimmune_disease_disease_2nd_member == "thyroid disease, rheumatoid arthritis" |
   data1$autoimmune_disease_disease_2nd_member == "thyroid disease/thyroid disease" |
   data1$autoimmune_disease_disease_2nd_member == "thyroiditis" |
   data1$autoimmune_disease_disease_2nd_member == "thyroiditis, diabetes" |
   data1$autoimmune_disease_disease_3rd_member == "euthyroid, graves, opthalmopathy" |
   data1$autoimmune_disease_disease_3rd_member == "goitre" |
   data1$autoimmune_disease_disease_3rd_member == "graves disease" |
   data1$autoimmune_disease_disease_3rd_member == "hashimoto's disease" |
   data1$autoimmune_disease_disease_3rd_member == "hashimoto's thyroid" |
   data1$autoimmune_disease_disease_3rd_member == "hyperactive thyroid" |
   data1$autoimmune_disease_disease_3rd_member == "hyperthyroid" |
   data1$autoimmune_disease_disease_3rd_member == "hypothyroid" |
   data1$autoimmune_disease_disease_3rd_member == "thyroid" |
   data1$autoimmune_disease_disease_3rd_member == "thyroid disease" |
   data1$autoimmune_disease_disease_3rd_member == "thyroid disease, melanoma" |
   data1$autoimmune_disease_disease_3rd_member == "thyroid disease, rheumatoid arthritis" |
   data1$autoimmune_disease_disease_3rd_member == "thyroid disease/thyroid disease" |
   data1$autoimmune_disease_disease_3rd_member == "thyroiditis" |
   data1$autoimmune_disease_disease_3rd_member == "thyroiditis, diabetes" ))
percentThyroidFamily <- numberThyroidFamily*100/numberCases

##rheumatoid arthritis
numberRAFamily <- nrow(subset(data1, 
data1$autoimmune_disease_disease == "thyroid disease, rheumatoid arthritis" | 
data1$autoimmune_disease_disease == "diabetes, rheumatoid arthritis" |
data1$autoimmune_disease_disease == "lupus, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease == "Rheumatoid arthritis" |
                                data1$autoimmune_disease_disease == "rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_2nd_member == "thyroid disease, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_2nd_member == "diabetes, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_2nd_member == "lupus, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_2nd_member == "Rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_2nd_member == "rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_3rd_member == "thyroid disease, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_3rd_member == "diabetes, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_3rd_member == "lupus, rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_3rd_member == "Rheumatoid arthritis" |
                                data1$autoimmune_disease_disease_3rd_member == "rheumatoid arthritis" ))
percentRAFamily <- numberRAFamily*100/numberCases

##diabetes
numberDiabetesFamily <- nrow(subset(data1, data1$autoimmune_disease_disease == "diabetes, rheumatoid arthritis" |
                                      data1$autoimmune_disease_disease == "thyroiditis, diabetes" |
                                      data1$autoimmune_disease_disease == "diabetes" |
                                      data1$autoimmune_disease_disease == "diabetes/diabetes/diabetes/addison's disease" |
                                      data1$autoimmune_disease_disease == "lupus, diabetes" |
                                      data1$autoimmune_disease_disease_2nd_member == "diabetes, rheumatoid arthritis" |
                                      data1$autoimmune_disease_disease_2nd_member == "thyroiditis, diabetes" |
                                      data1$autoimmune_disease_disease_2nd_member == "diabetes" |
                                      data1$autoimmune_disease_disease_2nd_member == "diabetes/diabetes/diabetes/addison's disease" |
                                      data1$autoimmune_disease_disease_2nd_member == "lupus, diabetes" |
                                      data1$autoimmune_disease_disease_3rd_member == "diabetes, rheumatoid arthritis" |
                                      data1$autoimmune_disease_disease_3rd_member == "thyroiditis, diabetes" |
                                      data1$autoimmune_disease_disease_3rd_member == "diabetes" |
                                      data1$autoimmune_disease_disease_3rd_member == "diabetes/diabetes/diabetes/addison's disease" |
                                      data1$autoimmune_disease_disease_3rd_member == "lupus, diabetes" ))
percentDiabetesFamily <- numberDiabetesFamily*100/numberCases




######JOSH ADDED#####################################################
##Lupus
###matches manual count by bryan
numberSLEFamily <- nrow(subset(data1,
                               data1$autoimmune_disease_disease == "lupus" | 
                                 data1$autoimmune_disease_disease == "lupus " | 
                                 data1$autoimmune_disease_disease == "lupus, diabetes" | 
                                 data1$autoimmune_disease_disease == "lupus, rheumatoid arthritis" | 
                                 data1$autoimmune_disease_disease_2nd_member == "lupus" | 
                                 data1$autoimmune_disease_disease_2nd_member == "lupus " | 
                                 data1$autoimmune_disease_disease_2nd_member == "lupus, diabetes" | 
                                 data1$autoimmune_disease_disease_2nd_member == "lupus, rheumatoid arthritis" | 
                                 data1$autoimmune_disease_disease_3rd_member == "lupus" | 
                                 data1$autoimmune_disease_disease_3rd_member == "lupus " | 
                                 data1$autoimmune_disease_disease_3rd_member == "lupus, diabetes" | 
                                 data1$autoimmune_disease_disease_3rd_member == "lupus, rheumatoid arthritis"))

percentSLEFamily <- numberSLEFamily*100/numberCases

##Psoriasis
###number matches manual count by bryan
numberPsoriasisFamily <- nrow(subset(data1,
                 data1$autoimmune_disease_disease == "psoriasis" | 
                 data1$autoimmune_disease_disease == "psoriasis, multiple sclerosis" | 
                 data1$autoimmune_disease_disease == "psoriatic arthritis" | 
                   data1$autoimmune_disease_disease_2nd_member == "psoriasis" | 
                   data1$autoimmune_disease_disease_2nd_member == "psoriasis, multiple sclerosis" | 
                   data1$autoimmune_disease_disease_2nd_member == "psoriatic arthritis" | 
                   data1$autoimmune_disease_disease_3rd_member == "psoriasis" | 
                   data1$autoimmune_disease_disease_3rd_member == "psoriasis, multiple sclerosis" | 
                   data1$autoimmune_disease_disease_3rd_member == "psoriatic arthritis" ))

percentPsoriasisFamily <- numberPsoriasisFamily*100/numberCases

##Multiple Sclerosis
###number matches manual count by bryan
numberMSFamily <- nrow(subset(data1,
                                     data1$autoimmune_disease_disease == "multiple sclerosis" | 
                                       data1$autoimmune_disease_disease == "psoriasis, multiple sclerosis" | 
                                       data1$autoimmune_disease_disease_2nd_member == "multiple sclerosis" | 
                                       data1$autoimmune_disease_disease_2nd_member == "psoriasis, multiple sclerosis" | 
                                       data1$autoimmune_disease_disease_3rd_member == "multiple sclerosis" | 
                                       data1$autoimmune_disease_disease_3rd_member == "psoriasis, multiple sclerosis"  ))


percentMSFamily <- numberMSFamily*100/numberCases


##IBD
###number matches manual count by bryan
numberIBDFamily <- nrow(subset(data1,
                                     data1$autoimmune_disease_disease == "crohn's disease" | 
                                       data1$autoimmune_disease_disease == "colitis" | 
                                       data1$autoimmune_disease_disease == "ulcerative colitis" | 
                                       data1$autoimmune_disease_disease == "ulcerative collitis" | 
                                       data1$autoimmune_disease_disease_2nd_member == "crohn's disease" | 
                                       data1$autoimmune_disease_disease_2nd_member == "colitis" | 
                                       data1$autoimmune_disease_disease_2nd_member == "ulcerative colitis" | 
                                       data1$autoimmune_disease_disease_2nd_member == "ulcerative collitis" | 
                                       data1$autoimmune_disease_disease_3rd_member == "crohn's disease" | 
                                       data1$autoimmune_disease_disease_3rd_member == "colitis" | 
                                       data1$autoimmune_disease_disease_3rd_member == "ulcerative colitis" |
                                       data1$autoimmune_disease_disease_3rd_member == "ulcerative collitis"))


percentIBDFamily <- numberIBDFamily*100/numberCases
percentIBDFamily
##OTHER
###number matches manual count by bryan
numberOtherFamily<- nrow(subset(data1,
                                      data1$autoimmune_disease_disease == "addison's disease" |
                                      data1$autoimmune_disease_disease == "allergies" |
                                      data1$autoimmune_disease_disease == "ankylosing spondylitis" |
                                      data1$autoimmune_disease_disease == "asthma" |
                                      data1$autoimmune_disease_disease == "b-12 deficiancy" |
                                      data1$autoimmune_disease_disease == "blood disiese" |
                                      data1$autoimmune_disease_disease == "celiac disease" |
                                      data1$autoimmune_disease_disease == "collagen vasculitis" |
                                      data1$autoimmune_disease_disease == "diabetes/diabetes/diabetes/addison's disease" |
                                      data1$autoimmune_disease_disease == "glomerulonephritis" |
                                      data1$autoimmune_disease_disease == "guillain-barre" |
                                      data1$autoimmune_disease_disease == "guillian barre" |
                                      data1$autoimmune_disease_disease == "idiopathic thrombocytopenic purpura" |
                                      data1$autoimmune_disease_disease == "lymphoma cancer" |
                                      data1$autoimmune_disease_disease == "nephritis" |
                                      data1$autoimmune_disease_disease == "neurocardiogenic syncope" |
                                      data1$autoimmune_disease_disease == "pericarditis" |
                                      data1$autoimmune_disease_disease == "pernicious anemia" |
                                      data1$autoimmune_disease_disease == "polymiositis " |
                                      data1$autoimmune_disease_disease == "polymyalgia rheumatica" |
                                      data1$autoimmune_disease_disease == "scleroderma" |
                                      data1$autoimmune_disease_disease == "sjogren's" |
                                      data1$autoimmune_disease_disease == "vasculitis" |
                                      data1$autoimmune_disease_disease == "vitamin b-12 deficiency" |
                                      data1$autoimmune_disease_disease == "vitiligo" |
                                      data1$autoimmune_disease_disease == "wegeners" |
                                        data1$autoimmune_disease_disease_2nd_member == "addison's disease" |
                                        data1$autoimmune_disease_disease_2nd_member == "allergies" |
                                        data1$autoimmune_disease_disease_2nd_member == "ankylosing spondylitis" |
                                        data1$autoimmune_disease_disease_2nd_member == "asthma" |
                                        data1$autoimmune_disease_disease_2nd_member == "b-12 deficiancy" |
                                        data1$autoimmune_disease_disease_2nd_member == "blood disiese" |
                                        data1$autoimmune_disease_disease_2nd_member == "celiac disease" |
                                        data1$autoimmune_disease_disease_2nd_member == "collagen vasculitis" |
                                        data1$autoimmune_disease_disease_2nd_member == "diabetes/diabetes/diabetes/addison's disease" |
                                        data1$autoimmune_disease_disease_2nd_member == "glomerulonephritis" |
                                        data1$autoimmune_disease_disease_2nd_member == "guillain-barre" |
                                        data1$autoimmune_disease_disease_2nd_member == "guillian barre" |
                                        data1$autoimmune_disease_disease_2nd_member == "idiopathic thrombocytopenic purpura" |
                                        data1$autoimmune_disease_disease_2nd_member == "lymphoma cancer" |
                                        data1$autoimmune_disease_disease_2nd_member == "nephritis" |
                                        data1$autoimmune_disease_disease_2nd_member == "neurocardiogenic syncope" |
                                        data1$autoimmune_disease_disease_2nd_member == "pericarditis" |
                                        data1$autoimmune_disease_disease_2nd_member == "pernicious anemia" |
                                        data1$autoimmune_disease_disease_2nd_member == "polymiositis " |
                                        data1$autoimmune_disease_disease_2nd_member == "polymyalgia rheumatica" |
                                        data1$autoimmune_disease_disease_2nd_member == "scleroderma" |
                                        data1$autoimmune_disease_disease_2nd_member == "sjogren's" |
                                        data1$autoimmune_disease_disease_2nd_member == "vasculitis" |
                                        data1$autoimmune_disease_disease_2nd_member == "vitamin b-12 deficiency" |
                                        data1$autoimmune_disease_disease_2nd_member == "vitiligo" |
                                        data1$autoimmune_disease_disease_2nd_member == "wegeners" |
                                        data1$autoimmune_disease_disease_3rd_member == "addison's disease" |
                                        data1$autoimmune_disease_disease_3rd_member == "allergies" |
                                        data1$autoimmune_disease_disease_3rd_member == "ankylosing spondylitis" |
                                        data1$autoimmune_disease_disease_3rd_member == "asthma" |
                                        data1$autoimmune_disease_disease_3rd_member == "b-12 deficiancy" |
                                        data1$autoimmune_disease_disease_3rd_member == "blood disiese" |
                                        data1$autoimmune_disease_disease_3rd_member == "celiac disease" |
                                        data1$autoimmune_disease_disease_3rd_member == "collagen vasculitis" |
                                        data1$autoimmune_disease_disease_3rd_member == "diabetes/diabetes/diabetes/addison's disease" |
                                        data1$autoimmune_disease_disease_3rd_member == "glomerulonephritis" |
                                        data1$autoimmune_disease_disease_3rd_member == "guillain-barre" |
                                        data1$autoimmune_disease_disease_3rd_member == "guillian barre" |
                                        data1$autoimmune_disease_disease_3rd_member == "idiopathic thrombocytopenic purpura" |
                                        data1$autoimmune_disease_disease_3rd_member == "lymphoma cancer" |
                                        data1$autoimmune_disease_disease_3rd_member == "nephritis" |
                                        data1$autoimmune_disease_disease_3rd_member == "neurocardiogenic syncope" |
                                        data1$autoimmune_disease_disease_3rd_member == "pericarditis" |
                                        data1$autoimmune_disease_disease_3rd_member == "pernicious anemia" |
                                        data1$autoimmune_disease_disease_3rd_member == "polymiositis " |
                                        data1$autoimmune_disease_disease_3rd_member == "polymyalgia rheumatica" |
                                        data1$autoimmune_disease_disease_3rd_member == "scleroderma" |
                                        data1$autoimmune_disease_disease_3rd_member == "sjogren's" |
                                        data1$autoimmune_disease_disease_3rd_member == "vasculitis" |
                                        data1$autoimmune_disease_disease_3rd_member == "vitamin b-12 deficiency" |
                                        data1$autoimmune_disease_disease_3rd_member == "vitiligo" |
                                        data1$autoimmune_disease_disease_3rd_member == "wegeners" ))


percentOtherFamily <- numberOtherFamily*100/numberCases

##############calculations for figure 2C personal history#############################
#Sarcoidosis --> none
#blood diseases
fig2c_blood_family <- nrow(subset(data1, data1$autoimmune_disease_disease == "blood disiese"))
percentfig2c_blood_family <- fig2c_blood_family*100/numberCases
percentfig2c_blood_family

#pempphigus vulgaris--> none
#pernicious anemia 
fig2c_pernicious_family <- nrow(subset(data1, data1$autoimmune_disease_disease == "vitamin b-12 deficiency"|
                                         data1$autoimmune_disease_disease_2nd_member == "vitamin b-12 deficiency"|
                                         data1$autoimmune_disease_disease_3rd_member == "b-12 deficiancy"|
                                         data1$autoimmune_disease_disease_3rd_member == "pernicious anemia"))
percentfig2c_pernicious_family <- fig2c_pernicious_family*100/numberCases
percentfig2c_pernicious_family
#sjorgen's syndrome
fig2c_sjorgens_family <- nrow(subset(data1, data1$autoimmune_disease_disease == "sjogren's"|
                                       data1$autoimmune_disease_disease_2nd_member == "sjogren's"))
percentfig2c_sjorgens_family <- fig2c_sjorgens_family*100/numberCases
percentfig2c_sjorgens_family
#vitiligo
fig2c_vitiligo_family <- nrow(subset(data1,data1$autoimmune_disease_disease_2nd_member == "vitiligo"))
percentfig2c_vitiligo_family <- fig2c_vitiligo_family*100/numberCases
percentfig2c_vitiligo_family

##########################################################################
###type of relationship
###siblings
numberSiblings <- nrow(subset(data1,  data1$mg_relationship == "brother" |
                                      data1$mg_relationship == "brother and sister" |
                                      data1$mg_relationship == "sister"))
percentSiblings <- numberSiblings*100/numberFamilialCases

###parent-child
numberParents <- nrow(subset(data1, data1$mg_relationship == "father" |
                                    data1$mg_relationship == "mother" |
                                    data1$mg_relationship == "sons (2 neontalmg)" |
                                    data1$mg_relationship == "daughter" |
                                    data1$mg_relationship == "daughter, two grandchildren"))
percentParents <- numberParents*100/numberFamilialCases

###uncle-nephew
numberUncles <- nrow(subset(data1,  data1$mg_relationship == "aunt" |
                                    data1$mg_relationship == "aunt, uncle" |
                                    data1$mg_relationship == "niece" |
                                    data1$mg_relationship == "uncle"))
percentUncles <- numberUncles*100/numberFamilialCases

###cousins
numberCousins <- nrow(subset(data1,  data1$mg_relationship == "cousin"))
percentCousins <- numberCousins*100/numberFamilialCases

###grandparents
numberGrandparents <- nrow(subset(data1,  data1$mg_relationship == "grandfather" |
                                          data1$mg_relationship == "grandmother" |
                                          data1$mg_relationship == "daughter, two grandchildren"))
percentGrandparents <- numberGrandparents*100/numberFamilialCases


#######################################################
#calculate overlap in familial
numberWithOneFamilyMember <- nrow(subset(data1, data1$fh_of_other_autoimmune_disease=="yes" &
                                                data1$fh_of_other_autoimmune_disease_2nd_member=="no" &
                                                data1$fh_of_other_autoimmune_disease_3rd_member=="no"))
numberWithTwoFamilyMember <- nrow(subset(data1, data1$fh_of_other_autoimmune_disease=="yes" &
                                           data1$fh_of_other_autoimmune_disease_2nd_member=="yes" &
                                           data1$fh_of_other_autoimmune_disease_3rd_member=="no"))
numberWithThreeFamilyMember <- nrow(subset(data1, data1$fh_of_other_autoimmune_disease=="yes" &
                                           data1$fh_of_other_autoimmune_disease_2nd_member=="yes" &
                                           data1$fh_of_other_autoimmune_disease_3rd_member=="yes"))



#######################################################
#calculate female to male ratio in familial cases
familial <- subset(data1, data1$family_history_of_mg=="yes")
familialmales <- nrow(subset(familial, familial$gender=="male"))
familialfemales <- nrow(subset(familial, familial$gender=="female"))
ratioFamilial <- familialmales/familialfemales

#######################################################
#calculate female to male ratio in familial cases
sporadic <- subset(data1, data1$family_history_of_mg!="yes")
sporadicmales <- nrow(subset(sporadic, sporadic$gender=="male"))
sporadicfemales <- nrow(subset(sporadic, sporadic$gender=="female"))
ratioSporadic <- sporadicmales/sporadicfemales

#######################################################
#chiSquare of females in familial and sporadic
table1 <- as.table(rbind(c(25,58-25),c(429,974-429)))
dimnames(table1) <- list(type=c("familial","sporadic"),
                         age=c("female","male"))
XsqFemale <- chisq.test(table1)


#######################################################
#chiSquare of females in familial and sporadic
table2 <- as.table(rbind(c(15,58-15),c(233,974-233)))
dimnames(table2) <- list(type=c("familial","sporadic"),
                         age=c("young","old"))
XsqYoung <- chisq.test(table2)



#######################################################
#######################################################
print(paste("Number of cases: ", numberCases))
print(paste("Number of familial cases: ", numberFamilialCases, " (",percentFamilialCases,"%)", sep=""))
print(paste("Number of early-onset cases: ", earlyOnset, " (",percentEarlyOnset,"%)", sep=""))
print(paste("Number of familial cases with young onset: ",    numberYoungFamilial, sep=""))
print(paste("Number of thymectomy cases: ", numberThymectomy, " (",percentThymectomy,"%)", sep=""))


print("___________________________________________________________________________")
print(paste("Female to male ratio - all cases: ", females, ":", males, " (1:", ratio, ")",  sep=""))
print(paste("Female to male ratio - familial cases: ", familialfemales, ":", familialmales, " (1:", ratioFamilial, ")",  sep=""))
print(paste("Female to male ratio - sporadic cases: ", sporadicfemales, ":", sporadicmales, " (1:", ratioSporadic, ")",  sep=""))


print("___________________________________________________________________________")
print(paste("Mean age at onset of all cases: ", meanAgeAtSymptomOnsetAllCases, " (",stdevAgeAtSymptomOnsetAllCases,")", sep=""))
print(paste("Mean age at onset of familial cases: ",  meanAgeAtSymptomOnsetFamilialCases, " (",    stdevAgeAtSymptomOnsetFamilialCases,")", sep=""))
print(paste("Mean age at onset of sporadic cases: ",  meanAgeAtSymptomOnsetSporadicCases, " (",    stdevAgeAtSymptomOnsetSporadicCases,")", sep=""))


print("___________________________________________________________________________")
print(t.test(sporadic$age_at_symptom_onset, familial$age_at_symptom_onset))


print("___________________________________________________________________________")
print(paste("Number of cases with personal history of autoimmune disease: ",   numberPersonalHistoryAutoimmuneDisease, " (",percentPersonalHistoryAutoimmuneDisease,"%)", sep=""))
print(paste("Number of cases with personal history of throid disease: ",       numberThyroidPersonal, " (",percentThyroidPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of blood disease: ",        numberBloodPersonal , " (", percentBloodPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of rheumatoid arthritis: ", numberRAPersonal, " (",     percentRAPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of diabetes: ",             numberDiabetesPersonal, " (",     percentDiabetesPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of lupus: ",                numberSLEPersonal, " (",          percentSLEPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of psoriasis: ",            numberPsoriasisPersonal, " (",     percentPsoriasisPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of pernicious anemia: ",    numberPerniciousanemiaPersonal, " (",     percentPerniciousanemiaPersonal,"%)", sep=""))
print(paste("Number of cases with personal history of other autoimmune diseases: ", numberOtherPersonal, " (",           percentOtherPersonal,"%)", sep=""))


print("___________________________________________________________________________")
print(paste("Number of cases with family history of autoimmune disease: ",     numberFamilyHistoryAutoimmuneDisease, " (",  percentFamilyHistoryAutoimmuneDisease,"%)", sep=""))
print(paste("Number of cases with family history of thyroid: ",                numberThyroidFamily, " (",  percentThyroidFamily,"%)", sep=""))
print(paste("Number of cases with family history of rheumatoid arthritis: ",   numberRAFamily, " (",       percentRAFamily,"%)", sep=""))
print(paste("Number of cases with family history of diabetes: ",               numberDiabetesFamily, " (", percentDiabetesFamily,"%)", sep=""))
print(paste("Number of cases with family history of lupus: ",                  numberSLEFamily, " (",      percentSLEFamily,"%)", sep=""))
print(paste("Number of cases with family history of psoriasis: ",              numberPsoriasisFamily, " (", percentPsoriasisFamily,"%)", sep=""))
print(paste("Number of cases with family history of multiple sclerosis: ",     numberMSFamily, " (",       percentMSFamily,"%)", sep=""))
print(paste("Number of cases with family history of IBD: ",                    numberIBDFamily, " (",      percentIBDFamily,"%)", sep=""))
print(paste("Number of cases with family history of other autoimmune: ",       numberOtherFamily, " (",    percentOtherFamily,"%)", sep=""))


print("___________________________________________________________________________")
print(paste("Number of cases with one family member: ",     numberWithOneFamilyMember, sep=""))
print(paste("Number of cases with two family member: ",     numberWithTwoFamilyMember, sep=""))
print(paste("Number of cases with three family member: ",     numberWithThreeFamilyMember, sep=""))


print("___________________________________________________________________________")
print(paste("Number of siblings with family history of mg: ", numberSiblings, " (",       percentSiblings,"%)", sep=""))
print(paste("Number of parents/children with family history of mg: ",          numberParents, " (",percentParents,"%)", sep=""))
print(paste("Number of uncles/aunts with family history of mg: ",              numberUncles, " (", percentUncles,"%)", sep=""))
print(paste("Number of cousins with family history of mg: ",  numberCousins, " (",percentCousins,"%)", sep=""))
print(paste("Number of grandparents with family history of mg: ",              numberGrandparents, " (",   percentGrandparents,"%)", sep=""))


print("___________________________________________________________________________")
print(table1)
print(XsqFemale)


print("___________________________________________________________________________")
print(table2)
print(XsqYoung)




