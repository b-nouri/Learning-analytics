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
                         "type of program","program","current_phase","score_january","score_june","final_score")

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


content_df <- distinct(contents, content_pk, .keep_all = TRUE)

event_df <- join(events,content_df,
                 type = "inner", by = "content_pk")

event_df <- join(event_df,sap,
                 type = "inner", by = c("course_pk","user_pk"))

colnames(event_df)[30] <- "final_score"


event_df <- event_df %>%
  mutate(score_class = cut(as.numeric(event_df$final_score), breaks=c(0,6,9,13,21), labels=c( 'Poor', 'AbleToPush', 'Successful', 'Excellent')))


df_test <- event_df[event_df$course_pk == 888130 &
                      !(is.na(event_df$final_score)) &
                      !(is.na(event_df$score_class)) &
                      as.numeric(event_df$academic_week) <= 22,]

ggplot(df_test,aes(x=academic_week,color = score_class,fill=score_class)) +
  geom_histogram(bins=22,alpha=0.5)

rm(df_test)
###---------Courses with formative tests---------####
content_df %>%
  filter(content_type == "resource/x-bb-asmt-test-link" |
           content_type == "resource/x-bb-assignment" |
           content_type == "resource/x-turnitin-assignment" | content_type == "resource/x-osv-kaltura/mashup" |
           content_type == "resource/x-plugin-scormengine") %>%
  filter(course_pk %in% sap$course_pk) %>%
  ggplot(aes(x=opleidingsonderdeel ,fill=content_type)) +
  geom_bar()+ 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

formative_content <- content_df %>%
  filter(content_type == "resource/x-bb-asmt-test-link" |
           content_type == "resource/x-bb-assignment" |
           content_type == "resource/x-turnitin-assignment" | content_type == "resource/x-osv-kaltura/mashup" |
           content_type == "resource/x-plugin-scormengine") %>%
  mutate(test_score_type = ifelse((is.na(possible_score) | possible_score == 0 )& content_type != "resource/x-turnitin-assignment","ungraded",
                           ifelse(is.na(possible_score) & content_type == "resource/x-turnitin-assignment","unknown",
                           ifelse(content_type == "resource/x-bb-asmt-test-link","graded test","graded other format"))))


course_formative <- formative_content %>%
  mutate(test_score_type = as.factor(test_score_type)) %>%
  select(course_pk,test_score_type,course_id,opleidingsonderdeel) %>%
  group_by(course_pk,test_score_type,course_id,opleidingsonderdeel) %>%
  dplyr::summarise(number_of_tests=n()) %>%
  filter(course_pk %in% sap$course_pk)

g <- course_formative %>%
  filter(test_score_type == "graded test" | test_score_type == "graded other format")

graded_formative <- course_formative %>%
  filter(course_pk %in% g$course_pk) %>%
  arrange(number_of_tests)


ggplot(course_formative,aes(x=as.factor(opleidingsonderdeel),y=number_of_tests,color=test_score_type,fill=test_score_type)) +
  geom_col()+ 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

rm(g)
#--------Aggregating Programs-------###
Course_students_df <- sap %>%
  group_by(program,course_pk,opo_id,course_name) %>%
  dplyr::summarize(n_students_course=n())

programs_courses_df <- sap %>%
  distinct(opo_id,course_name,program) %>%
  group_by(program) %>%
  dplyr::summarize(n_courses_program=n())



ggplot(programs_courses_df[programs_courses_df$n_courses_program > 2,],aes(x=program,y=n_courses_program,fill = program)) +
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
    legend.key = element_rect(fill = "lightblue", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
    )


ggplot(programs_df[(programs_df$n_students_course>100 & programs_df$n_courses_program > 2),],aes(x=program,y=n_students_course,color=course_name,fill=course_name)) +
  geom_col() + guides(col = guide_legend(nrow = 50))+ 
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
    legend.key = element_rect(fill = "lightblue", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  )


#----Aggregating events with programs----#
programs_formatives <- merge(course_formative,programs_df)

g <- programs_df[(programs_df$n_students_course>100),] %>%
  filter(n_courses_program >2) %>%
  select(program) %>%
  distinct(program)

selected_program_data <- programs_formatives[(programs_formatives$program %in% g$program & programs_formatives$n_students_course>80),]

ggplot(selected_program_data,aes(x=as.factor(course_name),y=number_of_tests,fill=test_score_type)) +
  geom_col() + facet_wrap(~program, scales = "free", ncol = 3,nrow=2) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 1, hjust=1),
    # Change legend background color
    legend.key = element_rect(fill = "lightblue", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  ) + scale_fill_manual(values = c("#E7B800","#FC4E07","#00AFBB" ,"#C3D7A4"))
  

##---number of exam attempts----###
nn <- attempts %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student <- merge(nn,content_df[,c("content_pk","course_pk","title",
        "possible_score","content_type")],by="content_pk")
rm(nn)

###-------events------###
event_course_week <- event_df %>%
  group_by(user_pk,course_pk,academic_week) %>%
  dplyr::summarise(n_event_week = n())

event_week_day <- event_df %>%
  group_by(user_pk,course_pk, academic_week) %>%
  dplyr::summarise(n_day_week = n_distinct(day))

event_course <- event_df %>%
  group_by(user_pk,course_pk) %>%
  dplyr::summarise(n_event_course = n())


#-----Preparing data for selected courses----###
#Selected program: ABA toegepaste economische wetenschappen (Leuv) 
#selected courses: accountancy (TEW) - 888130, 
#Bank- en financie - 888132, de globale economie - 888954
sap1 <- sap[sap$course_pk == 888132 &
              sap$final_score > 0&
              sap$program == "ABA toegepaste economische wetenschappen (Leuv)",]

attempt_student1 <- attempt_student[attempt_student$course_pk == 888132 &
                    attempt_student$user_pk %in% sap1$user_pk,]

event_course_week1 <- event_course_week[event_course_week$course_pk == 888132,]

event_week_day1 <- event_week_day[event_week_day$course_pk == 888132,]

event_course1 <- event_course[event_course$course_pk == 888132 &
                                event_course$user_pk %in% sap1$user_pk,]

formative_content1 <- formative_content[formative_content$course_pk == 888132,]

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

course1 <- merge(course1,event_course1[,c("user_pk","n_event_course")],by="user_pk")

ggplot(course1,aes(x=n_event_course,y=final_score)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

c <- attempt_student1 %>%
  distinct(content_pk)
b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE) %>%
  ungroup()



df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))