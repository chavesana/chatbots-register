## Run this code

library(dplyr)
library(stringr) #for str_length() function
#####################################################################################################################################
#                                               DAILYDIALOG DATASET                                                                 #
#####################################################################################################################################
dailydialog <- data.frame(read.csv("data-raw/DailyDialog_counts.csv"))

dailydialog <- dailydialog %>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12, -X.13, -X.14, -X.15,
         -X.16, -X.17, -X.18, -X.19, -X.20, -X.21, -X.22, -X.23, -X.24, -X.25, -X.26, -X.27, -X.28, -X.29, -X.30,
         -X.31, -X.32, -X.33, -X.34, -X.35, -individual) %>%#I'm creating the individual column by hand with subregisters.
  mutate(individual=rep("dialog", times=1998),
          interlocutors=rep(c("guide","tourist"), times = c(999,999))) %>%
  mutate(source=rep("dailydialog", times = 1998)) %>%
          filter(interlocutors!="tourist") %>%
  select(-interlocutors)

dailydialog$source <- as.factor(dailydialog$source)
dailydialog$individual <- as.factor(dailydialog$individual)

#####################################################################################################################################
#                                               FLG UPDATED DATASET                                                                 #
#####################################################################################################################################
our_updated <- data.frame(read.csv("data-raw/original_counts.csv"))

our_updated <- our_updated %>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12, -X.13, -X.14, -X.15,
         -X.16, -X.17, -X.18, -X.19, -X.20, -X.21, -X.22, -X.23, -X.24, -X.25, -X.26, -X.27, -X.28, -X.29, -X.30,
         -X.31, -X.32, -X.33, -X.34, -file.1) %>%
  mutate(source=rep("our", times = 144))

#####################################################################################################################################
#                                    TRANSLATION: FLG TO DAILYDIALOG DATASET                                                        #
#####################################################################################################################################
mine_to_dailydialog <- data.frame(read.csv("data-raw/modified_counts_try6.csv"))

mine_to_dailydialog <- mine_to_dailydialog %>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12, -X.13, -X.14, -X.15,
         -X.16, -X.17, -X.18, -X.19, -X.20, -X.21, -X.22, -X.23, -X.24, -X.25, -X.26, -X.27, -X.28, -X.29, -X.30,
         -X.31, -X.32, -X.33, -X.34, -X.35) %>%
  filter(file!="mod_r3_guide_4_conversation_a.txt")%>% #removing failing counts
  mutate(source=rep("translation", times = 151))

usethis::use_data(dailydialog, our_updated, mine_to_dailydialog, overwrite = T)
