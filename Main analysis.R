#---------Load Libraries ------------------------------------
library(dplyr)
library(plyr)
library(tidyverse)
library(visdat)
library(stringr)
library(ggplot2)
library(lubridate)

#--------Load Data-------------------------------------------
events <- read.csv("c:/ALCAPAS/Anon_events.dsv",
                      sep="\t", quote = "",na.strings = c("", "NA"))
colnames(events) <- c("content_pk", "user_pk", "timestamp")

attempts_ <- read.csv("c:/ALCAPAS/Anon_attempts.dsv",
                     sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(attempts_) <- c("user_pk", "content_id", "start_date","submit_date",
                        "score","grade","first_attempt","last_attempt","highest_attempt","lowest_attempt")
attempts <- attempts_[2:nrow(attempts_),]

SAP_ <- read.csv("c:/ALCAPAS/Anon_SAP_data.dsv",
                      sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(SAP_) <- c("course_id", "opo_id", "course_name","user_pk",
                         "type","course_description","current_phase","score_january","score_june","final_score(kf)")

sap <- SAP_[2:nrow(SAP_),]
colnames(sap)[1] <- "course_pk"


sessions_ <- read.csv("c:/ALCAPAS/Anon_sessions.dsv",
                      sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(sessions_) <- c("session_id", "user_pk", "event_type","timestamp")

sessions <- sessions_[2:nrow(sessions_),]


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

sessions <- sapply(sessions, function(x) {str_replace_all(x, '"', "")})
sessions <- as.data.frame(sessions)

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
#content_df <- contents[!is.na(contents$y),]
content_df <- distinct(contents, content_pk, .keep_all = TRUE)


event_df <- join(events,content_df,
                 type = "inner", by = "content_pk")
#rm(events)

event_df <- join(event_df,sap,
                 type = "inner", by = c("course_pk","user_pk"))

colnames(event_df)[30] <- "final_score"


event_df <- event_df %>%
  mutate(score_class = cut(event_df$final_score, breaks=c(0,6,9,13,21), labels=c( 'Poor', 'AbleToPush', 'Successful', 'Excellent')))


df_test <- event_df[event_df$course_pk == 888130 &
                      !(is.na(event_df$final_score)) &
                      !(is.na(event_df$score_class)) &
                      as.numeric(event_df$academic_week) <= 22,]

ggplot(df_test,aes(x=academic_week,color = score_class,fill=score_class)) +
  geom_histogram(bins=22,alpha=0.5)

ggplot(df_test,aes(x=final_score)) +
  geom_boxplot()



###---------Courses with formative tests---------####
formative_content <- content_df %>%
  filter(content_type == "resource/x-bb-asmt-test-link" | content_type == "resource/x-bb-assignment" |
           content_type == "resource/x-turnitin-assignment" | content_type == "resource/x-osv-kaltura/mashup" |
           content_type == "resource/x-plugin-scormengine") %>%
  mutate(test_score_type = ifelse((is.na(possible_score) | possible_score == 0 )& content_type != "resource/x-turnitin-assignment","ungraded",
                           ifelse(is.na(possible_score) & content_type == "resource/x-turnitin-assignment","unknown","graded")))


formative_content <- as_tibble(formative_content)


course_formative <- formative_content %>%
  mutate(test_score_type = as.factor(test_score_type)) %>%
  select(course_pk,test_score_type) %>%
  group_by(course_pk,test_score_type) %>%
  dplyr::summarise(n=n()) 


ggplot(course_formative[,],aes(x=course_pk,color=test_score_type,fill=test_score_type)) +
  geom_bar()