#---------Load Libraries ------------------------------------
library(tidyverse)
library(dplyr)
library(visdat)
library(lubridate)
library(caret)
library(doParallel)
registerDoParallel(cores=4)
library(pROC)
library("writexl")
library(caret)
      # a faster implementation of randomForest

#--------Load Data-------------------------------------------
events <- read.csv("c:/ALCAPAS/Anon_events.dsv",
                   sep="\t", quote = "",na.strings = c("", "NA"))
colnames(events) <- c("content_pk", "user_pk", "timestamp")

attempts_ <- read.csv("c:/ALCAPAS/TEW/attempts_Tier4.dsv",
                      sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(attempts_) <- c("user_pk", "content_id", "start_date","submit_date",
                         "score","grade","first_attempt","last_attempt","highest_attempt","lowest_attempt")
attempts <- attempts_[2:nrow(attempts_),1:10]

SAP_ <- read.csv("c:/ALCAPAS/TEW/SAP_Tier4.dsv",
                 sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(SAP_) <- SAP_[1,]
colnames(SAP_) <- tolower(colnames(SAP_))
colnames(SAP_) <- c("opo_id", "course_name","user_pk",
                    "type of program","program","ucm_sap","activity_code",
                    "current_phase","score_january","score_june",
                    "final_score","course_pk")

SAP_$course_pk <- as.integer(SAP_$course_pk)
SAP_$final_score <- as.integer(SAP_$final_score)

sap <- SAP_[2:nrow(SAP_),]



sessions_ <- read.csv("c:/ALCAPAS/Anon_sessions.dsv",
                      sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(sessions_) <- c("session_id", "user_pk", "event_type","timestamp")

#sessions <- sessions_[2:nrow(sessions_),]

contents <- read.csv("c:/ALCAPAS/content_items_with_courses.dsv",
                     sep="\t", quote = "",na.strings = c("", "NA"),header = T)

colnames(contents) <- tolower(colnames(contents))
colnames(contents)[1] <- "course_pk"
colnames(contents)[5] <- "content_pk"

rm(attempts_)
rm(sessions_)
rm(SAP_)


####-----------Preprocessing Data-----------###
attempts <- sapply(attempts, function(x) {str_replace_all(x, '"', "")})
attempts <- as.data.frame(attempts)

events <- sapply(events, function(x) {str_replace_all(x, '"', "")})
events <- as.data.frame(events)

sap <- sapply(sap, function(x) {str_replace_all(x, '"', "")})
sap <- as.data.frame(sap)
sap <- sap %>%
  mutate(campus = str_extract(sap$program,"\\(([^)]*)\\)+$"))
levels(as.factor(sap$campus))

sap <- sap %>%
  mutate(program_name = gsub("\\(([^)]*)\\)+$","",sap$program))
levels(as.factor(sap$program_name))
sap$final_score <- as.integer(sap$final_score)

###------Cleaning data----#########
events <- events %>% mutate(time = dmy_hms(timestamp)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week)))) %>%
  mutate(content_pk = as.integer(content_pk))

contents <- contents %>%
  arrange(content_pk,id.opo)


content_df <- distinct(contents, content_pk, .keep_all = TRUE)

event_df <- merge(events,content_df,
                  type = "inner", by = "content_pk")

event_df <- merge(event_df,sap,
                  type = "inner", by = c("course_pk","user_pk"))



attempts <- attempts %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week))))
colnames(attempts)[2] <- 'content_pk'
attempt_df <- merge(attempts,content_df[,c("content_pk","course_pk","title",
                                           "possible_score","content_type")],by="content_pk")


formative_content <- content_df %>%
  filter(content_type == "resource/x-bb-asmt-test-link" |
           content_type == "resource/x-bb-assignment" |
           content_type == "resource/x-turnitin-assignment" | content_type == "resource/x-osv-kaltura/mashup" |
           content_type == "resource/x-plugin-scormengine") %>%
  mutate(test_score_type = ifelse((is.na(possible_score) | possible_score == 0 )& content_type != "resource/x-turnitin-assignment","Video Content",
                                  ifelse(is.na(possible_score) & content_type == "resource/x-turnitin-assignment","turnitin assignment (grade unknown)",
                                         ifelse(content_type == "resource/x-bb-asmt-test-link","graded test","graded other format (grade unknown)"))))
