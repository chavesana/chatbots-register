library(dplyr)
library(tidyr) #for gather() function
library(stringr) #for str_length() function

#function to identify the COLUMNS in which ALL the values are NA and remove it.
not_all_na <- function(x) {!all(is.na(x))}

##############################################################################################################
#                             VALIDATION - DAILYDIALOG (NATURALNESS AND CONTENT PRESERVATION)                #
##############################################################################################################

#loading survey responses
naturalness <- data.frame(read.csv("data-raw/nat_per_participant.csv"))

#creating the dataset with CONTENT PRESERVATION scores only
content.preservation <- naturalness %>%
  select(C_R2_6_E_1_1, C_R2_7_E_2_1, C_R1_2_A_8_1, C_R3_6_E_2_1, C_R3_2_A_1_1, C_R3_15_E_2_1, C_R1_4_A_3_1,
         C_R2_14_J_1_1, C_R3_24_A_3_1, C_R1_5_A_5_1, C_R1_6_A_5_1, C_R3_4_E_2_1, C_R3_I23_E_3_1, C_R1_2_A_9_1,
         C_R1_1_A_3_1, C_R1_3_A_8_1, C_R3_I41_E_2_1, C_R2_10_E_1_1, C_R3_3_A_3_1, C_R1_3_A_4_1, C_R1_6_A_9_1,
         C_R1_3_A_3_1, C_R3_16_A_1_1, C_R3_5_E_7_1, C_R3_5_E_6_1, C_R3_I30_A_2_1, C_R2_21_J_3_1, C_R3_6_A_2_1,
         C_R3_I36_A_6_1, C_R1_1_A_6_1, C_R1_6_A_17_1, C_R2_8_E_2_1, C_R3_11_A_5_1, C_R3_I26_A_1_1, C_R1_1_A_9_1,
         C_R2_6_E_2_1, C_R3_I23_J_3_1, C_R1_4_A_10_1, C_R2_9_E_2_1, C_R3_17_A_2_1, C_R1_6_A_6_1, C_R3_I33_A_1_1,
         C_R1_3_A_5_1, C_R3_20_E_3_1, C_R3_I32_A_3_1, C_R1_1_A_7_1, C_R3_12_E_2_1, C_R3_14_A_1_1, C_R3_5_E_4_1,
         C_R3_I35_E_2_1, C_R2_20_J_1_1, C_R3_I31_A_7_1, C_R3_20_A_5_1, C_R3_I40_E_3_1, PID) %>%
  #Remove the NA columns
  select_if(not_all_na)


#reshaping content.preservation to be two columns in the form score ~ question (think about keeping user_id)
content.preservation <- content.preservation %>%
  gather(question, score, 1:54) %>%
  filter(!is.na(score))%>%
  rename(pid = PID)

content.preservation$question <- as.factor(content.preservation$question)

#removing information from question name
content.preservation$question <- substr(content.preservation$question, 3, str_length(content.preservation$question)-2)

#replacing question IDs
question_ids <- c("R2_6_E_1", "R2_7_E_2", "R3_6_E_2", "R3_2_A_1", "R3_15_E_2", "R2_14_J_1", "R3_24_A_3", "R3_4_E_2",
                  "R3_I23_E_3", "R3_I41_E_2", "R2_10_E_1", "R3_3_A_3", "R3_16_A_1", "R3_5_E_7", "R3_5_E_6", "R3_I30_A_2",
                  "R2_21_J_3", "R3_6_A_2", "R3_I36_A_6", "R2_8_E_2", "R3_11_A_5", "R3_I26_A_1", "R2_6_E_2", "R3_I23_J_3",
                  "R2_9_E_2", "R3_17_A_2", "R3_I33_A_1", "R3_20_E_3", "R3_I32_A_3", "R3_12_E_2", "R3_14_A_1", "R3_5_E_4",
                  "R3_I35_E_2", "R2_20_J_1", "R3_I31_A_7", "R3_20_A_5", "R3_I40_E_3", "R3_12_J_1", "R3_I23_J_2", "R3_6_J_1",
                  "R3_10_E_3", "R3_I24_E_3", "R3_I26_A_2", "R2_13_J_1", "R3_10_J_3", "R3_22_E_3", "R3_13_E_2", "R3_5_J_1",
                  "R3_I38_E_4", "R3_I40_A_1", "R3_13_J_2", "R3_19_E_3", "R3_3_A_2", "R3_4_A_1",
                  "R1_2_A_8", "R1_4_A_3", "R1_5_A_5", "R1_6_A_5", "R1_2_A_9", "R1_1_A_3", "R1_3_A_8", "R1_3_A_4", "R1_6_A_9", #removed for user study
                  "R1_3_A_3", "R1_1_A_6", "R1_6_A_17", "R1_1_A_9", "R1_4_A_10", "R1_6_A_6", "R1_3_A_5", "R1_1_A_7")#removed for user study

num <- 1
for(id in question_ids){
  content.preservation$question[which(content.preservation$question==id)] <- paste0("QA", num)
  num = num + 1
}

##################################################
#creating the dataset with NATURALNESS scores only
naturalness <- naturalness %>%
  select(-C_R2_6_E_1_1, -C_R2_7_E_2_1, -C_R1_2_A_8_1, -C_R3_6_E_2_1, -C_R3_2_A_1_1, -C_R3_15_E_2_1, -C_R1_4_A_3_1,
         -C_R2_14_J_1_1, -C_R3_24_A_3_1, -C_R1_5_A_5_1, -C_R1_6_A_5_1, -C_R3_4_E_2_1, -C_R3_I23_E_3_1, -C_R1_2_A_9_1,
         -C_R1_1_A_3_1, -C_R1_3_A_8_1, -C_R3_I41_E_2_1, -C_R2_10_E_1_1, -C_R3_3_A_3_1, -C_R1_3_A_4_1, -C_R1_6_A_9_1,
         -C_R1_3_A_3_1, -C_R3_16_A_1_1, -C_R3_5_E_7_1, -C_R3_5_E_6_1, -C_R3_I30_A_2_1, -C_R2_21_J_3_1, -C_R3_6_A_2_1,
         -C_R3_I36_A_6_1, -C_R1_1_A_6_1, -C_R1_6_A_17_1, -C_R2_8_E_2_1, -C_R3_11_A_5_1, -C_R3_I26_A_1_1, -C_R1_1_A_9_1,
         -C_R2_6_E_2_1, -C_R3_I23_J_3_1, -C_R1_4_A_10_1, -C_R2_9_E_2_1, -C_R3_17_A_2_1, -C_R1_6_A_6_1, -C_R3_I33_A_1_1,
         -C_R1_3_A_5_1, -C_R3_20_E_3_1, -C_R3_I32_A_3_1, -C_R1_1_A_7_1, -C_R3_12_E_2_1, -C_R3_14_A_1_1, -C_R3_5_E_4_1,
         -C_R3_I35_E_2_1, -C_R2_20_J_1_1, -C_R3_I31_A_7_1, -C_R3_20_A_5_1, -C_R3_I40_E_3_1) %>%
  #Remove the NA columns
  select_if(not_all_na)

#reshaping content.preservation to be two columns in the form score ~ question (think about keeping user_id)
naturalness <- naturalness %>%
  gather(question, score, 1:216) %>%
  filter(!is.na(score))

#grouping columns per group (original vs. translated) and item (natural, well written, complete and meaningful)
naturalness <- naturalness %>%
  mutate(group = as.factor(case_when(grepl("NO_", question) ~ "original",
                           grepl("NT_", question) ~"translated")),
         item = as.factor(case_when(substr(question, str_length(question), str_length(question))=="1" ~ "natural",
                                    substr(question, str_length(question), str_length(question))=="2" ~ "well.written",
                                    substr(question, str_length(question), str_length(question))=="3" ~ "complete",
                                    substr(question, str_length(question), str_length(question))=="4" ~ "meaningful"
                                    ))) %>%
  rename(pid = PID)

#removing information from question name
naturalness$question <- substr(naturalness$question, 4, str_length(naturalness$question)-2)

#creating an ordinal variable for score
naturalness$score.factor = factor(naturalness$score, ordered = TRUE, levels = c("1", "2", "3", "4", "5", "6", "7"))
naturalness$question = as.factor(naturalness$question)

#positive vs. negative
nat.reduced  <- naturalness %>%
  mutate(score.reduced = case_when(score == 1 ~ "1",
                                   score == 2 ~ "1",
                                   score == 3 ~ "1",
                                   score == 4 ~ "2",
                                   score == 5 ~ "3",
                                   score == 6 ~ "3",
                                   score == 7 ~ "3"))
nat.reduced$score.reduced = factor(nat.reduced$score.reduced, ordered = TRUE, levels = c("1", "2", "3"))
naturalness <- nat.reduced

#replacing question IDs
question_ids <- c("R2_6_2_1", "R2_7_2_2", "R3_6_2_2", "R3_2_1_1", "R3_15_2_2", "R2_14_3_1", "R3_24_1_3", "R3_4_2_2",
                  "R3_I23_2_3", "R3_I41_2_2", "R2_10_2_1", "R3_3_1_3", "R3_16_1_1", "R3_5_2_7", "R3_5_2_6", "R3_I30_1_2",
                  "R2_21_3_3", "R3_6_1_2", "R3_I36_1_6", "R2_8_2_2", "R3_11_1_5", "R3_I26_1_1", "R2_6_2_2", "R3_I23_3_3",
                  "R2_9_2_2", "R3_17_1_2", "R3_I33_1_1", "R3_20_2_3", "R3_I32_1_3", "R3_12_2_2", "R3_14_1_1", "R3_5_2_4",
                  "R3_I35_2_2", "R2_20_3_1", "R3_I31_1_7", "R3_20_1_5", "R3_I40_2_3", "R3_12_3_1", "R3_I23_3_2", "R3_6_3_1",
                  "R3_10_2_3", "R3_I24_2_3", "R3_I26_1_2", "R2_13_3_1", "R3_10_3_3", "R3_22_2_3", "R3_13_2_2", "R3_5_3_1",
                  "R3_I38_2_4", "R3_I40_1_1", "R3_13_3_2", "R3_19_2_3", "R3_3_1_2", "R3_4_1_1",
                  "R1_2_1_8", "R1_4_1_3", "R1_5_1_5", "R1_6_1_5", "R1_2_1_9", "R1_1_1_3", "R1_3_1_8", "R1_3_1_4", "R1_6_1_9", #removed for user study
                  "R1_3_1_3", "R1_1_1_6", "R1_6_1_17", "R1_1_1_9", "R1_4_1_10", "R1_6_1_6", "R1_3_1_5", "R1_1_1_7")#removed for user study

naturalness$question <- as.character(naturalness$question)
num <- 1
for(id in question_ids){
  naturalness$question[which(naturalness$question==id)] <- paste0("QA", num)
  num = num + 1
}

naturalness$question <- as.factor(naturalness$question)

##############################################################################################################
#                                   USER EXPERIENCE DAILYDIALOG DATASET                                      #
##############################################################################################################
users_pref <- data.frame(read.csv("data-raw/userX_per_participant.csv"))

demographics <- users_pref %>%
  select(PID, education, field, field_6_TEXT, gender, gender_5_TEXT, birth_year, location, info_search, info_search_service, chatbots, social_orientation_1, social_orientation_2)

users_pref <- users_pref %>%
  select(-manipulation_check_1, -manipulation_check_2, -manipulation_check_3,
         -manipulation_check_1.1, -manipulation_check_2.1, -manipulation_check_3.1,
         -manipulation_check_1.2, -manipulation_check_2.2, -manipulation_check_3.2, -field_6_TEXT,
         -gender_5_TEXT, -location, -info_search, -info_search_service, -chatbots, -education,
         - field, -gender, -birth_year, social_orientation_1, social_orientation_2) %>% #keeping the -social_orientation_1, -social_orientation_2
  #Remove the NA columns
  select_if(not_all_na)

cols <- names(users_pref)
users_pref[cols] <- lapply(users_pref[cols], as.character)

users_pref$social_orientation_1 <- as.integer(unclass(
  factor(users_pref$social_orientation_1,
         levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"))))

users_pref$social_orientation_2 <- as.integer(unclass(
  factor(users_pref$social_orientation_2,
         levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"))))

#reshaping users_pref to be two columns in the form choice ~ question
users_pref <- users_pref %>%
  gather(question, choice, 2:163) %>%
  filter(choice != "")

users_pref <- fastDummies::dummy_cols(users_pref, select_columns = "PID")
users_pref <- users_pref[,2:181]


#grouping columns per construct
users_pref <- users_pref %>%
  mutate(construct = as.factor(case_when(grepl("PI_", question) ~ "APPROP",
                                     grepl("PII_", question) ~"CREDIB",
                                     grepl("PIII_", question) ~"UX")))

#removing information from question name
users_pref$question <- case_when(grepl("PI_", users_pref$question) ~ substr(users_pref$question, 7, str_length(users_pref$question)),
                                 grepl("PII_", users_pref$question) ~ substr(users_pref$question, 8, str_length(users_pref$question)),
                                 grepl("PIII_", users_pref$question) ~ substr(users_pref$question, 9, str_length(users_pref$question)))

#inverting the line_guide in question name to be in the form guide_line
sub_char_pattern <- substr(users_pref$question, str_length(users_pref$question)-3, str_length(users_pref$question))
sub_char_line <- substr(users_pref$question, str_length(users_pref$question)-2, str_length(users_pref$question)-2)
sub_char_guide <- substr(users_pref$question, str_length(users_pref$question)-1, str_length(users_pref$question))
sub_char_replace <- paste0(sub_char_guide, "_", sub_char_line)

users_pref$question <- str_replace(users_pref$question, sub_char_pattern, sub_char_replace)

users_pref$question <- as.factor(users_pref$question)
users_pref$choice <- as.factor(users_pref$choice)

########################################### FEATURES DATASET #################################################
features_counts <- data.frame(read.csv("data-raw/counts_diff.csv"))

features_counts <- features_counts %>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12, -X.13, -X.14, -X.15,
       -X.16, -X.17, -X.18, -X.19, -X.20, -X.21, -X.22, -X.23, -X.24, -X.25, -X.26, -X.27, -X.28, -X.29, -X.30,
       -X.31, -X.32, -X.33, -X.34, -file.1) %>%
  select(file, private_vb, that_del, contract, vb_present, pro_2, do_pro,
         pro_dem, emphatic, pro_1, pro_it, vb_be, sub_conj_caus, disc_particle, pro_nom,
         hedge, amplifr, wh_ques, mod_poss, coord_conj_cls, wh_cls, prep_final, #positive features dim 1
         nn_all, prep, jj_attr, tt_ratio, word_length, #negative features dim 1
         vb_past, pro_3, vb_perfect, vb_public, #dim 2
         wh_rel_obj, wh_rel_subj, wh_rel_pipe, coord_conj_phrs, nn_nom, #positive features dim 3
         adv_time, adv_place, adv, #negative features dim 3
         infinitive, mod_pred, vb_suasive, sub_conj_cond, mod_necess, split_aux, #dim 4
         conj_advl, passive_short, passive_by, passive_postnom, sub_conj_othr, jj_pred #dim 5
         ) %>%
  rename(question = file) %>%
  mutate(G=unclass(as.factor(case_when(grepl("_A_", question) ~ 1,
                                          grepl("_E_", question) ~ 2,
                                          grepl("_J_", question) ~ 3))))

features_counts <- fastDummies::dummy_cols(features_counts, select_columns = "G")
features_counts <- features_counts[-52] #remove individual column.
feat_users_pref <- merge(features_counts, users_pref, by="question")
##############################################################################################################
#                                                     END                                                    #
##############################################################################################################
usethis::use_data(content.preservation, naturalness, feat_users_pref, demographics, overwrite = TRUE)
