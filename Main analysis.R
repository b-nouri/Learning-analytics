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
                         "type of program","program","current_phase","score_january","score_june","final_score(kf)")

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


sap <- sapply(sap, function(x) {str_replace_all(x, '"', "")})
sap <- as.data.frame(sap)
sap <- sap %>%
  mutate(campus = str_extract(sap$program,"\\(([^)]*)\\)+$"))
levels(as.factor(sap$campus))

sap <- sap %>%
  mutate(program_name = gsub("\\(([^)]*)\\)+$","",sap$program))
levels(as.factor(sap$program_name))
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


course_formative <- formative_content %>%
  mutate(test_score_type = as.factor(test_score_type)) %>%
  select(course_pk,test_score_type,course_id) %>%
  group_by(course_pk,test_score_type,course_id) %>%
  dplyr::summarise(n=n()) 

g <- course_formative %>%
  filter(test_score_type == "graded")

graded_formative <- course_formative %>%
  filter(course_pk %in% g$course_pk) %>%
  arrange(n)

gf <- graded_formative %>%
  group_by(course_pk) %>%
  dplyr::summarise(n=sum(n)) %>%
  filter(n>25)

graded_formative1 <- graded_formative %>%
  filter(course_pk %in% gf$course_pk)

ggplot(graded_formative,aes(x=as.factor(course_pk),y=n,color=test_score_type,fill=test_score_type)) +
  geom_col()

rm(g)
rm(gf)

levels(as.factor(sap$program))

#--------Dividing Programs-------###
Course_students_df <- sap %>%
  group_by(program,course_pk,opo_id,course_name) %>%
  dplyr::summarize(n_students_course=n())

programs_courses_df <- sap %>%
  distinct(opo_id,course_name,program) %>%
  group_by(program) %>%
  dplyr::summarize(n_courses_program=n())



ggplot(programs_courses_df[programs_courses_df$n_courses_program > 3,],aes(x=program,y=n_courses_program,fill = program)) +
  geom_col() +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

programs_df <- merge(Course_students_df, programs_courses_df)
rm(Course_students_df)


#courses having more than 100 students - grouped by programs
ggplot(programs_df[(programs_df$n_students_course>100 & programs_df$n_courses_program > 2),],aes(x=program,y=n_students_course)) +
  geom_col() + guides(col = guide_legend(nrow = 50))+ 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    # Change legend background color
    legend.background = element_rect(fill = "darkgray"),
    legend.key = element_rect(fill = "lightblue", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
    )


ggplot(programs_df[(programs_df$n_students_course>100 & programs_df$n_courses_program > 2),],aes(x=program,y=n_students_course,color=course_name,fill=course_name)) +
  geom_col() + guides(col = guide_legend(nrow = 50))+ 
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
    # Change legend background color
    legend.background = element_rect(fill = "darkgray"),
    legend.key = element_rect(fill = "lightblue", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  )


