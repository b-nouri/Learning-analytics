#---------Load Libraries ------------------------------------
library(tidyverse)
library(dplyr)
library(visdat)
library(lubridate)
#library(corrplot)

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


###----Select Course----###
attempt_df <- attempt_df[attempt_df$course_pk == 888132,]
content_df <- content_df[content_df$course_pk == 888132,]
event_df <- event_df[event_df$course_pk == 888132,]
sap1 <- sap[sap$course_pk == 888132 &
              sap$final_score > 0 &
              sap$program == "ABA toegepaste economische wetenschappen (Leuv)",]


formative_content1 <- formative_content[formative_content$course_pk == 888132,]


rm(attempts)
rm(contents)
rm(events)
###----assign weeks and select week datasets----####
attempt_df <- attempt_df %>%
  mutate(course_week = academic_week -20)


event_df <- event_df %>%
  mutate(course_week = academic_week -20)

a <- as.data.frame(names(event_df))

attempt <- attempt_df[attempt_df$course_week < 6 & attempt_df$course_week >0,]
event <- event_df[event_df$course_week < 6 & event_df$course_week > 0,]

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
event_course_week <- event %>%
  group_by(user_pk,course_pk,academic_week) %>%
  dplyr::summarise(n_event_week = n())

event_week_day <- event %>%
  group_by(user_pk,course_pk, academic_week) %>%
  dplyr::summarise(n_day_week = n_distinct(day))

event_course <- event %>%
  group_by(user_pk,course_pk) %>%
  dplyr::summarise(n_event_sofar = n())

#-----preparing features - feature engineering---###
ggplot(content_df[content_df$course_pk == 888132,],aes(content_type)) +
  geom_bar()


ggplot(event[,],aes(content_type)) + 
  geom_bar()


attempt_student1 <- attempt_student[attempt_student$user_pk %in% sap1$user_pk,]


course1 <- sap1 %>%
  mutate(final_score = ifelse((sap1$final_score > 9),1,0)) %>%
  select(course_name,user_pk,final_score)


student_list <- as.data.frame(course1$user_pk)
colnames(student_list)[1] <- "user_pk"


###---------Test related features--------######
c <- attempt_student1 %>%
  group_by(user_pk) %>%
  dplyr::summarize(percentage_tests_completed = n()/length(unique(attempt_student1$content_pk)),
                   total_test_tries = sum(number_tries)) %>%
  mutate(average_tries_test = total_test_tries / length(unique(attempt_student1$content_pk)))

test_so_far <- merge(student_list,c,by="user_pk",all.x = TRUE)
test_so_far$percentage_tests_completed <- replace(test_so_far$percentage_tests_completed,
                                          is.na(test_so_far$percentage_tests_completed), 0)

test_so_far$total_test_tries <- replace(test_so_far$total_test_tries,
                                          is.na(test_so_far$total_test_tries), 0)


course1 <- merge(course1,test_so_far,by="user_pk",all.x = TRUE)


c <- attempt_student1 %>%
  distinct(content_pk)

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(c$content_pk), ~ paste0('grade_test', 1:length(c$content_pk)))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)



sum(is.na(course1))
vis_miss(course1)

library(lares)
course1$final_score <- as.integer(course1$final_score)
corr_var(course1,
         final_score
) 


vis_miss(course1)

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  summarise(average_test_score = mean(test_percentage),sum_test_scores_achieved = sum(highest))

course1 <- merge(course1,b,by="user_pk",all.x = TRUE)


course1$final_score <- as.integer(course1$final_score)

#----------week events###

c <- unique(event_course_week[,"academic_week"])
c <- c %>%
  arrange(academic_week)

b <- event_course_week %>%
  group_by(user_pk) %>%
  spread(academic_week,n_event_week) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c$academic_week)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.factor(c$academic_week)), ~ paste0('n_event_week', 1:nrow(c)))


b <- b %>%
  mutate(total_events = rowSums(b[,2:ncol(b)])) %>%
  mutate(average_events_week = rowMeans(b[,2:(ncol(b)-1)]))


course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
#course1 <- replace(course1, is.na(course1), 0)

###--------Active days per week-----###

c <- unique(event_course_week[,"academic_week"])
c <- c %>%
  arrange(academic_week)

b <- event_week_day %>%
  group_by(user_pk) %>%
  spread(academic_week,n_day_week) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c$academic_week)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.factor(c$academic_week)), ~ paste0('n_active_day_week', 1:nrow(c)))

b <- b %>%
  mutate(total_days_active = rowSums(b[,2:ncol(b)]))
#  mutate(average_active_days_week = rowMeans(b[,2:(ncol(b)-1)]))


course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
#course1 <- replace(course1, is.na(course1), 0)



####-------Video Related features-------####

####-----number of times each video opened----###

video1 <- content_df[content_df$content_type %in% c('resource/x-osv-kaltura/mashup',
                                                'resource/x-bb-toollink'),c('content_type','title','content_pk')]

video_event1 <- event[event$user_pk %in% sap1$user_pk &
                           event$content_pk %in% video1$content_pk,]

c <- unique(video_event1[,c("content_pk","title","content_type")])  %>%
  arrange(content_pk)

b <- video_event1 %>%
  group_by(user_pk,content_pk) %>%
  dplyr::summarise(n= n()) %>%
  spread(content_pk,n) %>%
  group_by(user_pk) %>%
  summarise_at(vars(as.factor(c$content_pk)), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(as.character(c$content_pk)), ~ paste0('n_times_opened_video', 1:length(c$content_pk)))


course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

c <- as.data.frame(unique(video_event1[,"content_pk"]))


b <- video_event1 %>%
  group_by(user_pk,content_pk) %>%
  dplyr::summarise(n = n()) %>%
  group_by(user_pk) %>%
  dplyr::summarise(percentage_videos_opened = n()/length(c$`unique(video_event1[, "content_pk"])`))


course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

course1$percentage_videos_opened <- replace(course1$percentage_videos_opened,
                                            is.na(course1$percentage_videos_opened),0)


b <- video_event1 %>%
  group_by(user_pk) %>%
  dplyr::summarise(total_times_opened_videos = n())
  
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

course1$total_times_opened_videos <- replace(course1$total_times_opened_videos,
                                            is.na(course1$total_times_opened_videos),0)

###----------documents opened------####
c <- unique(event[event$content_type == 'resource/x-bb-document','content_pk'])

b <- event[event$content_type == 'resource/x-bb-document',] %>%
  group_by(user_pk,content_pk) %>%
  summarise(summy = n()) %>%
  group_by(user_pk) %>%
  summarise(percentage_documents_opened = n()/length(c),average_times_documents_opened = sum(summy)/length(c))


course1 <- merge(course1,b,'user_pk')
corr_var(course1,
         final_score
)

###--------Procrastination-------####

c <- event %>%
  mutate(date = date(time)) %>%
  group_by(content_pk) %>%
  summarise(release_date=min(date))

event1 <- merge(event,c,'content_pk')

event1 <- event1 %>%
  mutate(date = date(time)) 

event1 <- event1 %>%
  mutate(procrastination = as.double(difftime(lubridate::ymd(event1$date),
                                              lubridate::ymd(event1$release_date),
                                              units = "days")))

c <- event1 %>%
  group_by(user_pk,content_pk) %>%
  summarise(proc = min(procrastination)) %>%
  group_by(user_pk) %>%
  summarise(procrastination = mean(proc))


course1 <- merge(course1,c,'user_pk')
vis_miss(course1)


corr_var(course1,
         final_score
) 

###------Handling missing values------##########
course1$grade_test1 <- replace(course1$grade_test1, is.na(course1$grade_test1), median(course1$grade_test1,na.rm = T))
course1$grade_test2 <- replace(course1$grade_test2, is.na(course1$grade_test2), median(course1$grade_test2,na.rm = T))
course1$grade_test3 <- replace(course1$grade_test3, is.na(course1$grade_test3), median(course1$grade_test3,na.rm = T))
course1$grade_test4 <- replace(course1$grade_test4, is.na(course1$grade_test4), median(course1$grade_test4,na.rm = T))
course1$grade_test5 <- replace(course1$grade_test5, is.na(course1$grade_test5), median(course1$grade_test5,na.rm = T))
course1$grade_test6 <- replace(course1$grade_test6, is.na(course1$grade_test6), median(course1$grade_test6,na.rm = T))
course1$grade_test7 <- replace(course1$grade_test7, is.na(course1$grade_test7), median(course1$grade_test7,na.rm = T))
course1$grade_test8 <- replace(course1$grade_test8, is.na(course1$grade_test8), median(course1$grade_test8,na.rm = T))
course1$grade_test9 <- replace(course1$grade_test9, is.na(course1$grade_test9), median(course1$grade_test9,na.rm = T))
course1$grade_test10 <- replace(course1$grade_test10, is.na(course1$grade_test10), median(course1$grade_test10,na.rm = T))
course1$grade_test11 <- replace(course1$grade_test11, is.na(course1$grade_test11), median(course1$grade_test11,na.rm = T))
course1$grade_test12 <- replace(course1$grade_test12, is.na(course1$grade_test12), median(course1$grade_test12,na.rm = T))
course1$grade_test13 <- replace(course1$grade_test13, is.na(course1$grade_test13), median(course1$grade_test13,na.rm = T))
course1$grade_test14 <- replace(course1$grade_test14, is.na(course1$grade_test14), median(course1$grade_test14,na.rm = T))
# course1$grade_test15 <- replace(course1$grade_test15, is.na(course1$grade_test15), median(course1$grade_test15,na.rm = T))
# course1$grade_test16 <- replace(course1$grade_test16, is.na(course1$grade_test16), median(course1$grade_test16,na.rm = T))
# course1$grade_test17 <- replace(course1$grade_test17, is.na(course1$grade_test17), median(course1$grade_test17,na.rm = T))
# course1$grade_test18 <- replace(course1$grade_test18, is.na(course1$grade_test18), median(course1$grade_test18,na.rm = T))
# course1$grade_test19 <- replace(course1$grade_test19, is.na(course1$grade_test19), median(course1$grade_test19,na.rm = T))


course1$average_test_score <- replace(course1$average_test_score, is.na(course1$average_test_score), median(course1$average_test_score,na.rm = T))
course1$sum_test_scores_achieved <- replace(course1$sum_test_scores_achieved, is.na(course1$sum_test_scores_achieved), median(course1$sum_test_scores_achieved,na.rm = T))
course1$average_tries_test <- replace(course1$average_tries_test, is.na(course1$average_tries_test), median(course1$average_tries_test,na.rm = T))

course1 <- replace(course1,is.na(course1),0)

vis_miss(course1)

M<-cor(course1[,-c(1,2,20:56)])
head(round(M,2))

par(mar = rep(2, 4))

corrplot(M,method="number")
install.packages("GGally")
