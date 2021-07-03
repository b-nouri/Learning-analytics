#---------Load Libraries ------------------------------------
library(plyr)
library(dplyr)

library(tidyverse)
library(visdat)
library(ggplot2)
library(lubridate)
library(caret)
library(doParallel)
registerDoParallel(cores=4)
library(cluster)
library(pROC)
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

attempts <- attempts %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week))))
colnames(attempts)[2] <- 'content_pk'
attempt_df <- merge(attempts,content_df[,c("content_pk","course_pk","title",
                                          "possible_score","content_type")],by="content_pk")

###----Select Course----###
attempt_df <- attempt_df[attempt_df$course_pk == 888132,]
content_df <- content_df[content_df$course_pk == 888132,]
event_df <- event_df[event_df$course_pk == 888132,]

rm(attempts)
rm(contents)
rm(events)

###----assign weeks and select week datasets----####
attempt_df <- attempt_df %>%
  mutate(course_week = academic_week -20)

event_df <- event_df %>%
  mutate(course_week = academic_week -20)


attempt <- attempt_df[attempt_df$course_week < 13,]
event <- attempt_df[attempt_df$course_week < 13,]

##---number of exam attempts----###
nn <- attempt %>%
  group_by(content_pk,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')

attempt_student <- merge(nn,content_df[,c("content_pk","title",
                                          "possible_score")],by="content_pk")


attempt_student <- attempt_student %>%
  mutate(test_percentage = (highest/possible_score*100))
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


event_test <- event[event$content_type == "resource/x-bb-asmt-test-link",]
#-----Preparing data for selected courses----###
ggplot(content_df[content_df$course_pk == 888132,],aes(content_type)) +
  geom_bar()


attempt_student1 <- attempt_student[attempt_student$course_pk == 888132 &
                                      attempt_student$user_pk %in% sap1$user_pk,]

event_course_week1 <- event_course_week[event_course_week$course_pk == 888132 &
                                          event_course_week$academic_week < 38,]

event_week_day1 <- event_week_day[event_week_day$course_pk == 888132 &
                                    event_course_week$academic_week < 38,]

event_course1 <- event_course[event_course$course_pk == 888132 &
                                event_course$user_pk %in% sap1$user_pk,]

formative_content1 <- formative_content[formative_content$course_pk == 888132,]

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

#course1 <- merge(course1,event_course1[,c("user_pk","n_event_course")],by="user_pk")

ggplot(course1,aes(x=n_event_course,y=final_score)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

c <- attempt_student1 %>%
  distinct(content_pk)

#write.csv(c,"C:/ALCAPAS/content_ids.csv")

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:19))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

sum(is.na(course1))
vis_miss(course1)
course1 <- course1 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-4) 

course1 <- course1 %>%
  mutate(n_tests_taken = ifelse(is.na(course1$n_tests_taken),0,course1$n_tests_taken))

course1$test1 <- replace(course1$test1, is.na(course1$test1), median(course1$test1,na.rm = T))
course1$test2 <- replace(course1$test2, is.na(course1$test2), median(course1$test2,na.rm = T))
course1$test3 <- replace(course1$test3, is.na(course1$test3), median(course1$test3,na.rm = T))
course1$test4 <- replace(course1$test4, is.na(course1$test4), median(course1$test4,na.rm = T))
course1$test5 <- replace(course1$test5, is.na(course1$test5), median(course1$test5,na.rm = T))
course1$test6 <- replace(course1$test6, is.na(course1$test6), median(course1$test6,na.rm = T))
course1$test7 <- replace(course1$test7, is.na(course1$test7), median(course1$test7,na.rm = T))
course1$test8 <- replace(course1$test8, is.na(course1$test8), median(course1$test8,na.rm = T))
course1$test9 <- replace(course1$test9, is.na(course1$test9), median(course1$test9,na.rm = T))
course1$test10 <- replace(course1$test10, is.na(course1$test10), median(course1$test10,na.rm = T))
course1$test11 <- replace(course1$test11, is.na(course1$test11), median(course1$test11,na.rm = T))
course1$test12 <- replace(course1$test12, is.na(course1$test12), median(course1$test12,na.rm = T))
course1$test13 <- replace(course1$test13, is.na(course1$test13), median(course1$test13,na.rm = T))
course1$test14 <- replace(course1$test14, is.na(course1$test14), median(course1$test14,na.rm = T))
course1$test15 <- replace(course1$test15, is.na(course1$test15), median(course1$test15,na.rm = T))
course1$test16 <- replace(course1$test16, is.na(course1$test16), median(course1$test16,na.rm = T))
course1$test17 <- replace(course1$test17, is.na(course1$test17), median(course1$test17,na.rm = T))
course1$test18 <- replace(course1$test18, is.na(course1$test18), median(course1$test18,na.rm = T))
course1$test19 <- replace(course1$test19, is.na(course1$test19), median(course1$test19,na.rm = T))

vis_miss(course1)

course1 <- course1 %>%
  mutate(sum_tests_taken =  select(.,test1:test19) %>% rowSums(.))

course1 <- course1 %>%
  mutate(n_tests_taken = ifelse(is.na(course1$n_tests_taken),0,course1$n_tests_taken))


rm(b)
rm(c)


#-----test percentage----#######
c <- attempt_student1 %>%
  distinct(content_pk)

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:19))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

sum(is.na(course1))
vis_miss(course1)

course1$test_percentage1 <- replace(course1$test_percentage1, is.na(course1$test_percentage1), median(course1$test_percentage1,na.rm = T))
course1$test_percentage2 <- replace(course1$test_percentage2, is.na(course1$test_percentage2), median(course1$test_percentage2,na.rm = T))
course1$test_percentage3 <- replace(course1$test_percentage3, is.na(course1$test_percentage3), median(course1$test_percentage3,na.rm = T))
course1$test_percentage4 <- replace(course1$test_percentage4, is.na(course1$test_percentage4), median(course1$test_percentage4,na.rm = T))
course1$test_percentage5 <- replace(course1$test_percentage5, is.na(course1$test_percentage5), median(course1$test_percentage5,na.rm = T))
course1$test_percentage6 <- replace(course1$test_percentage6, is.na(course1$test_percentage6), median(course1$test_percentage6,na.rm = T))
course1$test_percentage7 <- replace(course1$test_percentage7, is.na(course1$test_percentage7), median(course1$test_percentage7,na.rm = T))
course1$test_percentage8 <- replace(course1$test_percentage8, is.na(course1$test_percentage8), median(course1$test_percentage8,na.rm = T))
course1$test_percentage9 <- replace(course1$test_percentage9, is.na(course1$test_percentage9), median(course1$test_percentage9,na.rm = T))
course1$test_percentage10 <- replace(course1$test_percentage10, is.na(course1$test_percentage10), median(course1$test_percentage10,na.rm = T))
course1$test_percentage11 <- replace(course1$test_percentage11, is.na(course1$test_percentage11), median(course1$test_percentage11,na.rm = T))
course1$test_percentage12 <- replace(course1$test_percentage12, is.na(course1$test_percentage12), median(course1$test_percentage12,na.rm = T))
course1$test_percentage13 <- replace(course1$test_percentage13, is.na(course1$test_percentage13), median(course1$test_percentage13,na.rm = T))
course1$test_percentage14 <- replace(course1$test_percentage14, is.na(course1$test_percentage14), median(course1$test_percentage14,na.rm = T))
course1$test_percentage15 <- replace(course1$test_percentage15, is.na(course1$test_percentage15), median(course1$test_percentage15,na.rm = T))
course1$test_percentage16 <- replace(course1$test_percentage16, is.na(course1$test_percentage16), median(course1$test_percentage16,na.rm = T))
course1$test_percentage17 <- replace(course1$test_percentage17, is.na(course1$test_percentage17), median(course1$test_percentage17,na.rm = T))
course1$test_percentage18 <- replace(course1$test_percentage18, is.na(course1$test_percentage18), median(course1$test_percentage18,na.rm = T))
course1$test_percentage19 <- replace(course1$test_percentage19, is.na(course1$test_percentage19), median(course1$test_percentage19,na.rm = T))

vis_miss(course1)

course1 <- course1 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage19) %>% rowMeans(.))


#----------###


c <- attempt_student1 %>%
  distinct(content_pk)

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,number_tries) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('n_tries_test', 1:19))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

course1 <- course1 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(total_test_attempts = select(.,n_tries_test1:n_tries_test19) %>% rowSums(.))

rm(b)
rm(c)



c <- unique(event_course_week1[,"academic_week"])
c <- c %>%
  arrange(academic_week)

b <- event_course_week1 %>%
  group_by(user_pk) %>%
  spread(academic_week,n_event_week) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c$academic_week)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.factor(c$academic_week)), ~ paste0('n_event_week', 1:nrow(c)))

course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
course1 <- replace(course1, is.na(course1), 0)
rm(b)
rm(c)

course1 <- course1 %>%
  mutate(total_test_attempts = select(.,n_tries_test1:n_tries_test19) %>% rowSums(.))


course1 <- course1 %>%
  mutate(sum_events_week2 = select(.,n_event_week1:n_event_week2)%>% rowSums(.)) %>%
  mutate(sum_events_week3 = select(.,n_event_week1:n_event_week3)%>% rowSums(.)) %>%
  mutate(sum_events_week4 = select(.,n_event_week1:n_event_week4)%>% rowSums(.)) %>%
  mutate(sum_events_week5 = select(.,n_event_week1:n_event_week5)%>% rowSums(.)) %>%
  mutate(sum_events_week6 = select(.,n_event_week1:n_event_week6)%>% rowSums(.)) %>%
  mutate(sum_events_week7 = select(.,n_event_week1:n_event_week7)%>% rowSums(.)) %>%
  mutate(sum_events_week8 = select(.,n_event_week1:n_event_week8)%>% rowSums(.)) %>%
  mutate(sum_events_week9 = select(.,n_event_week1:n_event_week9)%>% rowSums(.)) %>%
  mutate(sum_events_week10 = select(.,n_event_week1:n_event_week10)%>% rowSums(.)) %>%
  mutate(sum_events_week11 = select(.,n_event_week1:n_event_week11)%>% rowSums(.)) %>%
  mutate(sum_events_week12 = select(.,n_event_week1:n_event_week12)%>% rowSums(.)) %>%
  mutate(sum_events_week13 = select(.,n_event_week1:n_event_week13)%>% rowSums(.)) %>%
  mutate(sum_events_week14 = select(.,n_event_week1:n_event_week14)%>% rowSums(.)) %>%
  mutate(sum_events_week15 = select(.,n_event_week1:n_event_week15)%>% rowSums(.)) %>%
  mutate(sum_events_week16 = select(.,n_event_week1:n_event_week16)%>% rowSums(.)) %>%
  mutate(sum_events_week17 = select(.,n_event_week1:n_event_week17)%>% rowSums(.)) %>%
  mutate(sum_events_week18 = select(.,n_event_week1:n_event_week18)%>% rowSums(.))



b = paste0("n_event_week",1+1)

c <- unique(event_week_day1[,"academic_week"])
c <- c %>%
  arrange(academic_week)

b <- event_week_day1 %>%
  group_by(user_pk) %>%
  spread(academic_week,n_day_week) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c$academic_week)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.factor(c$academic_week)), ~ paste0('n_day_week', 1:nrow(c)))

course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
course1 <- replace(course1, is.na(course1), 0)
rm(b)
rm(c)

content1 <- content_df[content_df$course_pk == 888132,]

video1 <- content1[content1$content_type %in% c('resource/x-osv-kaltura/mashup',
                                                'resource/x-bb-toollink'),]

video_event1 <- event_df[event_df$user_pk %in% sap1$user_pk &
                           event_df$content_pk %in% video1$content_pk,]

c <- unique(video_event1[,"content_pk"])

b <- video_event1 %>%
  group_by(user_pk,content_pk) %>%
  dplyr::summarise(n= n()) %>%
  spread(content_pk,n) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.factor(c)), ~ paste0('n_times_opened_video', 1:length(c)))

course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
course1 <- replace(course1, is.na(course1), 0)
rm(b)
rm(c)


course1 <- subset(course1, select=-c(course_name))

data.frame(colnames(course1))
course1 <- course1 %>%
  mutate(total_days = select(.,n_day_week1:n_day_week18)%>% rowSums(.))

course1 <- course1 %>%
  mutate(total_videos = select(.,n_times_opened_video1:n_times_opened_video19)%>% rowSums(.))

unique(contents$content_type)