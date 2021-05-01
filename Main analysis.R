#---------Load Libraries ------------------------------------
library(dplyr)
library(plyr)
library(tidyverse)
library(visdat)
library(stringr)
library(ggplot2)
library(lubridate)
library(caret)
library(doParallel)
registerDoParallel(cores=4)
library(cluster)
library(factoextra)
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

# colnames(SAP_) <- c("course_pk","opo_id", "course_name","user_pk",
#                     "type of program","program",
#                     "current_phase","score_january","score_june",
#                     "final_score")

SAP_$course_pk <- as.integer(SAP_$course_pk)
SAP_$final_score <- as.integer(SAP_$final_score)

sap <- SAP_[2:nrow(SAP_),]



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
  mutate(test_score_type = ifelse((is.na(possible_score) | possible_score == 0 )& content_type != "resource/x-turnitin-assignment","Video Content",
                           ifelse(is.na(possible_score) & content_type == "resource/x-turnitin-assignment","turnitin assignment (grade unknown)",
                           ifelse(content_type == "resource/x-bb-asmt-test-link","graded test","graded other format (grade unknown)"))))


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
  filter(final_score > 0) %>%
  dplyr::summarize(n_students_course=n())

programs_courses_df <- sap %>%
  distinct(opo_id,course_name,program) %>%
  group_by(program) %>%
  dplyr::summarize(n_courses_program=n())

ggplot(programs_courses_df[programs_courses_df$n_courses_program > 2,],aes(x=program,y=n_courses_program)) +
  geom_col() +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)
  ) +
  labs(title = "Number of courses in each program",
       subtitle = "(Only the programs having more than 2 courses)",
       x = "Program", y = "number of courses")


programs_df <- merge(Course_students_df, programs_courses_df)
rm(Course_students_df)


g <- programs_df[programs_df$n_students_course>100 & 
                   programs_df$n_courses_program > 2,]
g1 <- g %>%
  group_by(program) %>%
  dplyr::summarise(n_courses = n())

#programs having more than 2 courses and each course having more than 100 students
ggplot(g1[g1$program != "ABA handelswetenschappen (Antw)",],aes(x=program,y=n_courses)) +
  geom_col() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)
  ) +
  labs(title = "Number of courses in the programs having more than 2 courses",
       subtitle = "(Only courses having more than 100 students selected)",
       x = "Program", y = "number of courses")


selected_programs <- g1[g1$program != "ABA handelswetenschappen (Antw)",]
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

selected_program_data <- programs_formatives[(programs_formatives$program %in% selected_programs$program),]

selectedp1 <- selected_programs[1:3,1]
ggplot(selected_program_data[(selected_program_data$program %in% selectedp1$program),],
       aes(x=as.factor(course_name),y=number_of_tests,fill=test_score_type)) +
  geom_col() + facet_wrap(~program, scales = "free_x", ncol = 3,nrow=2) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 1, hjust=1),
    legend.key = element_rect(fill = "lightblue", color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  ) + scale_fill_manual(values = c("#E7B800","#FC4E07","#C3D7A4","#00AFBB")) +
  labs(title = "Formative & video content in different courses",
       subtitle = "(program 1-3)",
       x = "Course having more than 100 registered students", y = "Number of content",
       fill = "Type of content")

selectedp2 <- selected_programs[4:5,1]
ggplot(selected_program_data[(selected_program_data$program %in% selectedp2$program),],
       aes(x=as.factor(course_name),y=number_of_tests,fill=test_score_type)) +
  geom_col() + facet_wrap(~program, scales = "free_x", ncol = 3,nrow=2) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 1, hjust=1),
    legend.key = element_rect(fill = "lightblue", color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  ) + scale_fill_manual(values = c("#E7B800","#FC4E07","#C3D7A4","#00AFBB")) +
  labs(title = "Formative & video content in different courses",
       subtitle = "(program 4-5  - HIR And TEW)",
       x = "Course having more than 100 registered students", y = "Number of content",
       fill = "Type of content")



selected_program_data <- programs_formatives[(programs_formatives$program %in% 
                                                c("BA handelsingenieur (Leuv)","ABA toegepaste economische wetenschappen (Leuv)") & programs_formatives$n_students_course>100),]

ggplot(selected_program_data,aes(x=as.factor(course_name),y=number_of_tests,fill=test_score_type)) +
  geom_col() + facet_wrap(~program,scales = "free_x", ncol = 3,nrow=2) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 1, hjust=1),
    legend.key = element_rect(fill = "lightblue", color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  ) + scale_fill_manual(values = c("#E7B800","#FC4E07","#C3D7A4","#00AFBB")) +
  labs(title = "Formative & video content in different courses",
       subtitle = "(HIR And TEW Programs only)",
       x = "Course having more than 100 registered students", y = "Number of content",
       fill = "Type of content")

selected_program_data <- programs_formatives[(programs_formatives$program %in% 
                                                c("BA handelsingenieur (Leuv)",
                                                  "ABA toegepaste economische wetenschappen (Leuv)")),] %>%
  group_by(program,course_name) %>%
  distinct(course_name,.keep_all = T)

ggplot(selected_program_data,aes(x=as.factor(course_name),y=n_students_course)) +
  geom_col() + facet_wrap(~program,scales = "free_x", ncol = 3,nrow=2) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 1, hjust=1),
    legend.key = element_rect(fill = "lightblue", color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.2,"cm")  
  ) + scale_fill_manual(values = c("#E7B800","#FC4E07","#C3D7A4","#00AFBB")) +
  labs(title = "Number of students registered in each course",
       subtitle = "(HIR And TEW Programs only)",
       x = "Course name", y = "Number of students")


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

attempt_student <- attempt_student %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

attempt_student_selected <- attempts[(attempts$content_id %in% content_df[(content_df$course_pk==888132),"content_pk"]),]


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


event_selected_course <- event_df[event_df$course_pk == 888132,]
event_test_selected_course <- event_selected_course[event_selected_course$content_type == "resource/x-bb-asmt-test-link",]
#-----Preparing data for selected courses----###
#Selected program: ABA toegepaste economische wetenschappen (Leuv) 
#selected courses: accountancy (TEW) - 888130, 
#Bank- en financie - 888132, de globale economie - 888954
sap$final_score <- as.integer(sap$final_score)
sap1 <- sap[sap$course_pk == 888132 &
              sap$final_score > 0 &
              sap$program == "ABA toegepaste economische wetenschappen (Leuv)",]


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
###--------Analysis - Forecasting------####
course1 <- tibble::rowid_to_column(course1, "row")
course1$final_score <- as.integer(course1$final_score)
course1 <- subset(course1, select=-c(score_class))

##------Clustering-------###
df1 <- scale(course1[,3:ncol(course1)])
distance <- get_dist(df1)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df1, centers = 4, nstart = 25)


fviz_cluster(k2, data = df1)

set.seed(123)

fviz_nbclust(df1, kmeans, method = "wss")

fviz_nbclust(df1, kmeans, method = "silhouette")



set.seed(123)
final1 <- kmeans(df1, 2, nstart = 25)
fviz_cluster(final1, data = df1)

set.seed(123)
final2 <- kmeans(df1, 5, nstart = 25)
fviz_cluster(final2, data = df1)


course1 <- course1 %>%
  mutate(cluster = final1$cluster)

course <- course1
###---Train Test Split----###
#course1 <- subset(course1, select=-c(n_event_course))
set.seed(123)

c1 <- course1[,]
train1 <-sample_frac(c1, 0.8)
sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]
#test1 <- test1[,]

train_course1 <- train1[,3:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:ncol(test1)]

##------Training and predicting----####

#train.control <- trainControl(method="LOOCV")

train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)


model1_PR <- train(final_score ~ . , data = train_course1,
                    method = "parRF", trControl = train.control,metric = "MAE",
                   importance=T)

# model2_svm <- train(final_score ~ . , data = train_course1,
#                    method = "svmLinear", trControl = train.control,metric = "MAE")
# 
# model3_glm <- train(final_score ~ . , data = train_course1,
#                          method = "glmStepAIC", trControl = train.control,metric = "MAE")


model4_elasticnet <- train(final_score ~ . , data = train_course1,
                         method = "glmnet", trControl = train.control,metric = "MAE",
                         preProcess = c())

# model5_xgbTree <- train(final_score ~ . , data = train_course1,
#                         method = "xgbTree", trControl = train.control,metric = "MAE")


model6_gbm <- train(final_score ~ . , data = train_course1,
                    method = "gbm", trControl = train.control,metric = "MAE")

print(min(model1_PR$results$MAE))
# print(min(model2_svm$results$MAE))
# print(min(model3_glm$results$MAE))
print(min(model4_elasticnet$results$MAE))
#print(min(model5_xgbTree$results$MAE))
print(min(model6_gbm$results$MAE))

pred1 <- data.frame(predict(model1_PR, test_course1, type = "raw"))
# pred2 <- data.frame(predict(model2_svm, test_course1, type = "raw"))
# pred3 <- data.frame(predict(model3_glm, test_course1, type = "raw"))
pred4 <- data.frame(predict(model4_elasticnet, test_course1, type = "raw"))
#pred5 <- data.frame(predict(model5_xgbTree, test_course1, type = "raw"))
pred6 <- data.frame(predict(model6_gbm, test_course1, type = "raw"))

results <- cbind(id_test1,test1$final_score)
names(results)[3] <- "actual_final_score"
# 
# results <- cbind(results,pred5)
# names(results)[4] <- "prediction_xgbTree"

results <- cbind(results,pred1,pred4,pred6)
names(results)[4] <- "prediction_rf"
#names(results)[5] <- "prediction_svm"
#names(results)[6] <- "prediction_glm"
names(results)[5] <- "prediction_glmnet"
#names(results)[8] <- "prediction_xgbTree"
names(results)[6] <- "prediction_gbm"

results1 <- results %>%
  mutate(score_class = cut(as.numeric(results$actual_final_score), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_rf = cut(as.numeric(results$prediction_rf), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_svm = cut(as.numeric(results$prediction_svm), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_glm = cut(as.numeric(results$prediction_glm), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_glmnet = cut(as.numeric(results$prediction_glmnet), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_xgbtree = cut(as.numeric(results$prediction_xgbTree), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(class_gbm = cut(as.numeric(results$prediction_gbm), breaks=c(0,9.9,21), labels=c( 'pass', 'fail')))

library(cvms)
conf_matrm <- confusion_matrix(targets = results1$score_class,
                 predictions = results1$class_rf)
conf_matsvm <- confusion_matrix(targets = results1$score_class,
                               predictions = results1$class_svm)
conf_matglm <- confusion_matrix(targets = results1$score_class,
                               predictions = results1$class_glm)
conf_matglmnet <- confusion_matrix(targets = results1$score_class,
                               predictions = results1$class_glmnet)
conf_matxbtree <- confusion_matrix(targets = results1$score_class,
                               predictions = results1$class_xgbtree)
conf_matgbm <- confusion_matrix(targets = results1$score_class,
                               predictions = results1$class_gbm)

conf_matrm
conf_matsvm
conf_matglm
conf_matglmnet
conf_matxbtree
conf_matgbm




gbmImp <- varImp(model6_gbm, conditional=TRUE)
gbmImp <- varImp(model6_gbm, scale = FALSE)
vImpGbm=varImp(model6_gbm)
gbmImp
plot(gbmImp, top = 32)

#rm(accuracy1)
#accuracy1 <- as.data.frame(cbind("rf",mae_test_rf,conf_matrm$`Balanced Accuracy`,conf_matrm$F1))
#names(accuracy1) <- c("algorithm","mae","accuracy","F1")
accuracy <- data.frame(c("rf2",mae_test_rf,conf_matrm$`Balanced Accuracy`,conf_matrm$F1),
                       c("svm2",mae_test_svm,conf_matsvm$`Balanced Accuracy`,conf_matsvm$F1),
                       c("glm2",mae_test_glm,conf_matglm$`Balanced Accuracy`,conf_matglm$F1),
                       c("glmnet2",mae_test_glmnet,conf_matglmnet$`Balanced Accuracy`,conf_matglmnet$F1),  
                       c("xgbtree2",mae_test_xgbTree,conf_matxbtree$`Balanced Accuracy`,conf_matxbtree$F1),  
                       c("gbm2",mae_test_gbm,conf_matgbm$`Balanced Accuracy`,conf_matgbm$F1))
accuracy <- data.frame(t(accuracy))
names(accuracy) <- c("algorithm","mae","accuracy","F1")

accuracy1 <- rbind(accuracy1,accuracy)
accuracy1 <- accuracy1[1:6,]
mae_test_rf
mae_test_svm
mae_test_glm
mae_test_glmnet
mae_test_xgbTree
mae_test_gbm

results <- results %>%
  mutate(score_rf =round(prediction_rf,0)) %>%
  # mutate(score_svm =round(prediction_svm,0)) %>%
  # mutate(score_glm =round(prediction_glm,0)) %>%
  mutate(score_glmnet =round(prediction_glmnet,0)) %>%
  #mutate(score_xgbTree =round(prediction_xgbTree,0)) %>%
  mutate(score_gbm =round(prediction_gbm,0))
  
mae_test_rf <- mean(abs(results$score_rf-results$actual_final_score))
# mae_test_svm <- mean(abs(results$score_svm-results$actual_final_score))
# mae_test_glm <- mean(abs(results$score_glm-results$actual_final_score))
mae_test_glmnet <- mean(abs(results$score_glmnet-results$actual_final_score))
#mae_test_xgbTree <- mean(abs(results$score_xgbTree-results$actual_final_score))
mae_test_gbm <- mean(abs(results$score_gbm-results$actual_final_score))

mae_test_rf
# mae_test_svm
# mae_test_glm
mae_test_glmnet
#mae_test_xgbTree
mae_test_gbm

gbmImp <- varImp(model2_svm, conditional=TRUE)
gbmImp
plot(gbmImp, top = 32)

##------Training and predicting----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass'))) %>%
  mutate(score_class = as.factor(course1$score_class))

#c1 <- course1[course1$cluster==2,]
c1 <- course1[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]


train_course1 <- train1[,4:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1)-1)]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "pass",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

#train_course1$score_class <- as.factor(train_course1$score_class)
#train.control <- trainControl(method = "repeatedcv",
#                              number = 10, repeats = 3,classProbs = TRUE)

model1_PR <- train(score_class ~ . , data = train_course1,
                   method = "parRF",   weights = model_weights,
                   trControl = train.control,metric="ROC")

#model2_svm <- train(score_class ~ . , data = train_course1,
#                    method = "svmLinear", trControl = train.control,metric="ROC")

#model3_glm <- train(score_class ~ . , data = train_course1,
#                    method = "glmStepAIC", trControl = train.control,metric="ROC",
#                    preProcess = c("zv", "center", "scale", "pca"))

model4_elasticnet <- train(score_class ~ . , data = train_course1,
                           method = "glmnet", weights = model_weights,
                           trControl = train.control,metric="ROC",
                           preProcess = c())

#model5_xgbTree <- train(score_class ~ . , data = train_course1,
#                       method = "xgbTree", trControl = train.control,metric="ROC")


model6_gbm <- train(score_class ~ . , data = train_course1,
                    method = "gbm", weights = model_weights,
                    trControl = train.control,metric="ROC")


print(max(model1_PR$results$Accuracy))
print(max(model4_elasticnet$results$Accuracy))
print(max(model6_gbm$results$Accuracy))


pred1 <- data.frame(predict(model1_PR, test_course1, type = "raw"))
pred4 <- data.frame(predict(model4_elasticnet, test_course1, type = "raw"))
pred6 <- data.frame(predict(model6_gbm, test_course1, type = "raw"))

rm(results)
results <- cbind(id_test1$user_pk,as.vector(test1$score_class))


results <- cbind(results,pred1,pred4,pred6)
names(results)[2] <- "actual_class"
names(results)[3] <- "prediction_rf"
names(results)[4] <- "prediction_glmnet"
names(results)[5] <- "prediction_gbm"

#library (ROCR)

confusionMatrix(results$prediction_rf, reference = as.factor(results$actual_class))
confusionMatrix(results$prediction_glmnet, reference = as.factor(results$actual_class))
confusionMatrix(results$prediction_gbm, reference = as.factor(results$actual_class))

# conf_matrm <- confusion_matrix(targets = results$actual_class,
#                                predictions = results$prediction_rf)
# 
# conf_matglmnet <- confusion_matrix(targets = results$actual_class,
#                                    predictions = results$prediction_glmnet)
# 
# conf_matgbm <- confusion_matrix(targets = results$actual_class,
#                                 predictions = results$prediction_gbm)
# 
# conf_matrm
# conf_matglmnet
# conf_matgbm

#rm(accuracy1)
#accuracy1 <- as.data.frame(cbind("rf",mae_test_rf,conf_matrm$`Balanced Accuracy`,conf_matrm$F1))
#names(accuracy1) <- c("algorithm","mae","accuracy","F1")
accuracy <- data.frame(c("rf2",conf_matrm$`Balanced Accuracy`,conf_matrm$F1),
                       c("svm2",conf_matsvm$`Balanced Accuracy`,conf_matsvm$F1),
                       c("glm2",conf_matglm$`Balanced Accuracy`,conf_matglm$F1),
                       c("glmnet2",conf_matglmnet$`Balanced Accuracy`,conf_matglmnet$F1),  
                       c("xgbtree2",conf_matxbtree$`Balanced Accuracy`,conf_matxbtree$F1),  
                       c("gbm2",conf_matgbm$`Balanced Accuracy`,conf_matgbm$F1))
accuracy <- data.frame(t(accuracy))
names(accuracy) <- c("algorithm","accuracy","F1")


par(mar = c(5, 8, 1, 1))
summary(
  model6_gbm, 
  cBars = 20,
  method = relative.influence, 
  las = 2
)

library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm

gbmImp <- varImp(model1_PR, conditional=TRUE)
gbmImp
plot(gbmImp, top = 32)


##------Training and predicting----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))
  
course1 <- course1 %>%
  mutate(score_class = as.factor(course1$score_class))
#c1 <- course1[course1$cluster==2,]
c1 <- course1[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

mean(course1$final_score)
sd(course1$final_score)
train_course1 <- train1[,4:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1)-1)]
table(train_course1$score_class)
table(course1$score_class)
cour
###-------------Classification------------###
#train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,test_course1)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,test1$score_class)   

#Draw the ROC curve 
gbm.probs <- predict(gbm.tune,test_course1,type="prob")
head(gbm.probs)

gbm.ROC <- roc(predictor=gbm.probs$fail,
               response=test1$score_class,
               levels=rev(levels(test1$score_class)),direction = "<")
gbm.ROC$auc
#Area under the curve: 0.5624
plot(gbm.ROC,main="GBM ROC")


####----------Random Forest----------#####
n_event = 
#registerDoParallel(4,cores=4)
getDoParWorkers()

rf_trcontrol_1 = trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


rf.tune1 <-train(score_class ~ ., data = train_course1,
                 method="ranger",
                 trControl=rf_trcontrol_1,
                 weights = model_weights)

plot(rf.tune1)  		# Plot the performance of the training models

rf.tune1$bestTune
res <- rf.tune1$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
rf.pred <- predict(rf.tune1,test_course1)

#Look at the confusion matrix  
confusionMatrix(rf.pred,test1$score_class)   

#Draw the ROC curve 
rf.probs <- predict(rf.tune1,test_course1,type="prob")
#head(xgb.probs)

rf.ROC <- roc(predictor=rf.probs$pass,
              response=test1$score_class,
              levels=rev(levels(test1$score_class)),
              direction= ">")
rf.ROC$auc
# Area under the curve: 0.8857

plot(rf.ROC,main="RandomForest ROC")

####---------glm---------####
caretGA$fitness_extern <- twoClassSummary
gafs.ctrl = gafsControl(functions = caretGA, method = "boot", number = 10,
                        metric = c(internal = "ROC", external = "ROC"),
                        maximize = c(internal = TRUE, external = TRUE),
                        holdout = .2,
                        allowParallel = TRUE, genParallel = TRUE, verbose = TRUE)

cls.ctrl <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                         number = 10, repeats = 5,
                         classProbs = TRUE, summaryFunction = twoClassSummary,
                         savePredictions = "final", allowParallel = TRUE)

set.seed(123)
ga <- gafs(y= train_course1$score_class ,x=train_course1[,1:(length(train_course1)-1)], 
           iters = 5, popSize = 2, elite = 0,
           differences = TRUE, method = "glm", family = "binomial", metric = "ROC",
           trControl = cls.ctrl,
           gafsControl = gafs.ctrl)

glm_trcontrol_1 = trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

glmnet.fit <- train(score_class ~ ., data = as.data.frame(train_course1),
                    method="glmnet",
                    metric="ROC",
                    trControl=glm_trcontrol_1,
                    weights=model_weights)

plot(glmnet.fit)
glmnet.fit$bestTune

res <- glmnet.fit$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
glm.pred <- predict(glmnet.fit,test1)

#Look at the confusion matrix  
confusionMatrix(glm.pred,test1$score_class)   

#Draw the ROC curve 
glm.probs <- predict(glmnet.fit,test1,type="prob")
#head(xgb.probs)

par(pty="s")
plot(glm.ROC,main="ROC Curves for prediction of student success in the course: Bank en financien")

glm.ROC <- roc(predictor=glm.probs$fail,
               response=test1$score_class,
               levels=rev(levels(test1$score_class)),
               direction= "<",
               plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)

rf.ROC <- roc(predictor=rf.probs$fail,
              response=test1$score_class,
              levels=rev(levels(test1$score_class)),
              direction= "<",
              plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

gbm.ROC <- roc(predictor=gbm.probs$fail,
               response=test1$score_class,
               levels=rev(levels(test1$score_class)),direction = "<",
               plot=TRUE,print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.3,legacy.axes=TRUE,add = TRUE)

glm.ROC$auc
# Area under the curve: 0.8857

legend("bottomright",legend=c("glm","random forest","gbm"),col=c("blue","red","green"),lwd=4)


library(gbm)
gbmImp <- varImp(gbm.tune, conditional=TRUE)
gbmImp
plot(gbmImp, top =10)




##------Training and predicting over test grades----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1 <- course1 %>%
  mutate(score_class = as.factor(course1$score_class))

#c1 <- course1[course1$cluster==2,]
c1 <- course1[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,138]

train_course1 <- train1[,4:64]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:64]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,test_course1)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,test1$score_class)   

#Draw the ROC curve 
test_grades.probs <- predict(gbm.tune,test_course1,type="prob")
head(test_grades.probs)

roc.test_grades <- roc(predictor=test_grades.probs$fail,
                       response=test1$score_class,
                       levels=rev(levels(test1$score_class)))
roc.test_grades$auc
#Area under the curve: 0.5624
plot(roc.test_grades,main="GBM ROC")



##------Training and predicting over activity in weeks----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1 <- course1 %>%
  mutate(score_class = as.factor(course1$score_class))

#c1 <- course1[course1$cluster==2,]
c1 <- course1[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,138]

train_course1 <- train1[,65:117]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,65:117]
table(train_course1$score_class)

###-------------Classification------------###
###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,test_course1)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,test1$score_class)   

#Draw the ROC curve 
weeks.probs <- predict(gbm.tune,test_course1,type="prob")

roc.weeks <- roc(predictor=weeks.probs$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)),direction = "<")
roc.weeks$auc
#Area under the curve: 0.5624
plot(roc.weeks,main="GBM ROC")

##------Training and predicting over videos----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1 <- course1 %>%
  mutate(score_class = as.factor(course1$score_class))

#c1 <- course1[course1$cluster==2,]
c1 <- course1[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,138]

train_course1 <- train1[,118:137]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,118:137]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,test_course1)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,test1$score_class)   

#Draw the ROC curve 
videos.prob <- predict(gbm.tune,test_course1,type="prob")
head(gbm.probs)

roc.videos <- roc(predictor=videos.prob$fail,
                  response=test1$score_class,
                  levels=rev(levels(test1$score_class)),direction = "<")
roc.videos$auc
#Area under the curve: 0.5624
plot(roc.videos,main="GBM ROC")



par(pty="s")
plot(roc.test_grades,main="ROC curves based on different feature sets for the course: bank en finacien")

gbm.test_grades <- roc(predictor=test_grades.probs$fail,
                       response=test1$score_class,
                       levels=rev(levels(test1$score_class)),
                       direction= "<",
                       plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)

gbm.videos <- roc(predictor=videos.prob$fail,
                  response=test1$score_class,
                  levels=rev(levels(test1$score_class)),
                  direction= "<",
                  plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

gbm.weeks <- roc(predictor=weeks.probs$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)),direction = "<",
                 plot=TRUE,print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.3,legacy.axes=TRUE,add = TRUE)


legend("bottomright",legend=c("feature set: test grades","feature set: number of times videos opened","feature set: student activities in each week"),col=c("blue","red","green"),lwd = 4)


####-------predict by each week-------#####
attempt_test_selected <- attempt_student_selected %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week)))) %>%
  mutate(course_week = (academic_week -19))

attempt_test_selected1 <- attempt_test_selected[attempt_test_selected$course_week<13,]

rm(train_course1)
rm(test_course1)
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

attempt_student <- attempt_student %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

attempt_student_selected <- attempts[(attempts$content_id %in% content_df[(content_df$course_pk==888132),"content_pk"]),]
###----week5-----#####
attempt_test_week5 <- attempt_test_selected[attempt_test_selected$course_week<6,]
nn <- attempt_test_week5 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_5 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                          "possible_score","content_type")],by="content_pk")

attempt_student_week_5 <- attempt_student_week_5 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_5 %>%
  distinct(content_pk)

b <- attempt_student_week_5 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_5 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_5))
vis_miss(course1_5)
course1_5 <- course1_5 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_5 <- course1_5 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_5$n_tests_taken),0,course1_5$n_tests_taken))

course1_5$test1 <- replace(course1_5$test1, is.na(course1_5$test1), 0)
course1_5$test2 <- replace(course1_5$test2, is.na(course1_5$test2), 0)
course1_5$test3 <- replace(course1_5$test3, is.na(course1_5$test3), 0)
course1_5$test4 <- replace(course1_5$test4, is.na(course1_5$test4), 0)

course1_5 <- course1_5 %>%
  mutate(sum_tests_taken =  select(.,test1:test4) %>% rowSums(.))


vis_miss(course1_5)

#-----test percentage----#######
b <- attempt_student_week_5 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_5 <- merge(course1_5,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_5))
vis_miss(course1_5)

course1_5$test_percentage1 <- replace(course1_5$test_percentage1, is.na(course1_5$test_percentage1),0)
course1_5$test_percentage2 <- replace(course1_5$test_percentage2, is.na(course1_5$test_percentage2), 0)
course1_5$test_percentage3 <- replace(course1_5$test_percentage3, is.na(course1_5$test_percentage3), 0)
course1_5$test_percentage4 <- replace(course1_5$test_percentage4, is.na(course1_5$test_percentage4), 0)

vis_miss(course1_5)

course1_5 <- course1_5 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage4) %>% rowMeans(.))


###------week5 dataset#-----
weeks5 <- course[,65:69]
sum_weeks5 <- course[,82:85]
days_week5 <- course[,100:104]
video_week5 <- course[,117:118] %>%
  select(n_times_opened_video1)
course1_5 <- course1_5 %>%
  cbind(weeks5,sum_weeks5,video_week5)


##------Training and predicting over week5----####
course1_5 <- tibble::rowid_to_column(course1_5, "row")
set.seed(123)

course1_5 <- course1_5 %>%
  mutate(score_class = cut(as.numeric(course1_5$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_5 <- course1_5 %>%
  mutate(score_class = as.factor(course1_5$score_class))

#c1 <- course1_5[course1_5$cluster==2,]
c1 <- course1_5[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,26]

train_course1 <- train1[,5:25]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:25]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune5 <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



res <- gbm.tune5$results
res

gbm.pred5 <- predict(gbm.tune5,test_course1)

confusionMatrix(gbm.pred5,test1$score_class)   

week5.prob <- predict(gbm.tune5,test_course1,type="prob")

roc.week5 <- roc(predictor=week5.prob$fail,
                  response=test1$score_class,
                  levels=rev(levels(test1$score_class)))
roc.week5$auc
plot(roc.week5,main="GBM ROC")

plot(roc.week5,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week5.prob$fail,
    response=test1$score_class,
    levels=rev(levels(test1$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)



###----week7-----#####
attempt_test_week7 <- attempt_test_selected[attempt_test_selected$course_week<8,]
nn <- attempt_test_week7 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_7 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                 "possible_score","content_type")],by="content_pk")

attempt_student_week_7 <- attempt_student_week_7 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_7 %>%
  distinct(content_pk)

b <- attempt_student_week_7 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_7 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_7))
vis_miss(course1_7)
course1_7 <- course1_7 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_7 <- course1_7 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_7$n_tests_taken),0,course1_7$n_tests_taken))

course1_7$test1 <- replace(course1_7$test1, is.na(course1_7$test1), 0)
course1_7$test2 <- replace(course1_7$test2, is.na(course1_7$test2), 0)
course1_7$test3 <- replace(course1_7$test3, is.na(course1_7$test3), 0)
course1_7$test4 <- replace(course1_7$test4, is.na(course1_7$test4), 0)
course1_7$test5 <- replace(course1_7$test5, is.na(course1_7$test5), 0)
course1_7$test6 <- replace(course1_7$test6, is.na(course1_7$test6), 0)

course1_7 <- course1_7 %>%
  mutate(sum_tests_taken =  select(.,test1:test6) %>% rowSums(.))


vis_miss(course1_7)

#-----test percentage----#######
b <- attempt_student_week_7 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_7 <- merge(course1_7,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_7))
vis_miss(course1_7)

course1_7$test_percentage1 <- replace(course1_7$test_percentage1, is.na(course1_7$test_percentage1),0)
course1_7$test_percentage2 <- replace(course1_7$test_percentage2, is.na(course1_7$test_percentage2), 0)
course1_7$test_percentage3 <- replace(course1_7$test_percentage3, is.na(course1_7$test_percentage3), 0)
course1_7$test_percentage4 <- replace(course1_7$test_percentage4, is.na(course1_7$test_percentage4), 0)
course1_7$test_percentage5 <- replace(course1_7$test_percentage5, is.na(course1_7$test_percentage5), 0)
course1_7$test_percentage6 <- replace(course1_7$test_percentage6, is.na(course1_7$test_percentage6), 0)

vis_miss(course1_7)

course1_7 <- course1_7 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage6) %>% rowMeans(.))

videos_agg <- event_selected_course %>%
  filter(content_type == "resource/x-osv-kaltura/mashup") %>%
  group_by(content_pk,academic_week) %>%
  dplyr::summarise(n=n())
###------week7 dataset#-----
weeks7 <- course[,65:71]
sum_weeks7 <- course[,83:87]
days_week7 <- course[,100:105]
video_week7 <- course[,118:120]
course1_7 <- course1_7 %>%
  cbind(weeks7,sum_weeks7,video_week7)


##------Training and predicting over week7----####
course1_7 <- tibble::rowid_to_column(course1_7, "row")
set.seed(123)

course1_7 <- course1_7 %>%
  mutate(score_class = cut(as.numeric(course1_7$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_7 <- course1_7 %>%
  mutate(score_class = as.factor(course1_7$score_class))

#c1 <- course1_7[course1_7$cluster==2,]
c1 <- course1_7[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,35]

train_course1 <- train1[,5:34]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:34]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune7 <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



res <- gbm.tune7$results
res

gbm.pred7 <- predict(gbm.tune7,test_course1)

confusionMatrix(gbm.pred7,test1$score_class)   

week7.prob <- predict(gbm.tune7,test_course1,type="prob")

roc.week7 <- roc(predictor=week7.prob$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)),direction = "<")
roc.week7$auc
plot(roc.week7,main="GBM ROC")

plot(roc.week7,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week7.prob$fail,
    response=test1$score_class,
    levels=rev(levels(test1$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)


###----week9-----#####
attempt_test_week9 <- attempt_test_selected[attempt_test_selected$course_week<10,]
nn <- attempt_test_week9 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_9 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                 "possible_score","content_type")],by="content_pk")

attempt_student_week_9 <- attempt_student_week_9 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_9 %>%
  distinct(content_pk)

b <- attempt_student_week_9 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_9 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_9))
vis_miss(course1_9)
course1_9 <- course1_9 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_9 <- course1_9 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_9$n_tests_taken),0,course1_9$n_tests_taken))

course1_9$test1 <- replace(course1_9$test1, is.na(course1_9$test1), 0)
course1_9$test2 <- replace(course1_9$test2, is.na(course1_9$test2), 0)
course1_9$test3 <- replace(course1_9$test3, is.na(course1_9$test3), 0)
course1_9$test4 <- replace(course1_9$test4, is.na(course1_9$test4), 0)
course1_9$test5 <- replace(course1_9$test5, is.na(course1_9$test5), 0)
course1_9$test6 <- replace(course1_9$test6, is.na(course1_9$test6), 0)
course1_9$test7 <- replace(course1_9$test7, is.na(course1_9$test7), 0)
course1_9$test8 <- replace(course1_9$test8, is.na(course1_9$test8), 0)

course1_9 <- course1_9 %>%
  mutate(sum_tests_taken =  select(.,test1:test8) %>% rowSums(.))


vis_miss(course1_9)

#-----test percentage----#######
b <- attempt_student_week_9 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_9 <- merge(course1_9,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_9))
vis_miss(course1_9)

course1_9$test_percentage1 <- replace(course1_9$test_percentage1, is.na(course1_9$test_percentage1),0)
course1_9$test_percentage2 <- replace(course1_9$test_percentage2, is.na(course1_9$test_percentage2), 0)
course1_9$test_percentage3 <- replace(course1_9$test_percentage3, is.na(course1_9$test_percentage3), 0)
course1_9$test_percentage4 <- replace(course1_9$test_percentage4, is.na(course1_9$test_percentage4), 0)
course1_9$test_percentage5 <- replace(course1_9$test_percentage5, is.na(course1_9$test_percentage5), 0)
course1_9$test_percentage6 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage6), 0)
course1_9$test_percentage7 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage7), 0)
course1_9$test_percentage8 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage8), 0)

vis_miss(course1_9)

course1_9 <- course1_9 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage8) %>% rowMeans(.))

videos_agg <- event_selected_course %>%
  filter(content_type == "resource/x-osv-kaltura/mashup") %>%
  group_by(content_pk,academic_week) %>%
  dplyr::summarise(n=n())
###------week9 dataset#-----
weeks9 <- course[,65:73]
sum_weeks9 <- course[,83:89]
days_week9 <- course[,100:109]
video_week9 <- course[,118:122]
course1_9 <- course1_9 %>%
  cbind(weeks9,sum_weeks9,video_week9)


##------Training and predicting over week9----####
course1_9 <- tibble::rowid_to_column(course1_9, "row")
set.seed(123)

course1_9 <- course1_9 %>%
  mutate(score_class = cut(as.numeric(course1_9$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_9 <- course1_9 %>%
  mutate(score_class = as.factor(course1_9$score_class))

#c1 <- course1_9[course1_9$cluster==2,]
c1 <- course1_9[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,45]

train_course1 <- train1[,5:44]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:44]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune9 <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



res <- gbm.tune9$results
res

gbm.pred9 <- predict(gbm.tune9,test_course1)

confusionMatrix(gbm.pred9,test1$score_class)   

week9.prob <- predict(gbm.tune9,test_course1,type="prob")

roc.week9 <- roc(predictor=week9.prob$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)))
roc.week9$auc
plot(roc.week9,main="GBM ROC")

plot(roc.week9,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week9.prob$fail,
    response=test1$score_class,
    levels=rev(levels(test1$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)



###----week12-----#####
attempt_test_week12 <- attempt_test_selected[attempt_test_selected$course_week<13,]
nn <- attempt_test_week12 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_12 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                 "possible_score","content_type")],by="content_pk")

attempt_student_week_12 <- attempt_student_week_12 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_12 %>%
  distinct(content_pk)

b <- attempt_student_week_12 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_12 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_12))
vis_miss(course1_12)
course1_12 <- course1_12 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_12 <- course1_12 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_12$n_tests_taken),0,course1_12$n_tests_taken))

course1_12$test1 <- replace(course1_12$test1, is.na(course1_12$test1), 0)
course1_12$test2 <- replace(course1_12$test2, is.na(course1_12$test2), 0)
course1_12$test3 <- replace(course1_12$test3, is.na(course1_12$test3), 0)
course1_12$test4 <- replace(course1_12$test4, is.na(course1_12$test4), 0)
course1_12$test5 <- replace(course1_12$test5, is.na(course1_12$test5), 0)
course1_12$test6 <- replace(course1_12$test6, is.na(course1_12$test6), 0)
course1_12$test7 <- replace(course1_12$test7, is.na(course1_12$test7), 0)
course1_12$test8 <- replace(course1_12$test8, is.na(course1_12$test8), 0)
course1_12$test9 <- replace(course1_12$test9, is.na(course1_12$test9), 0)
course1_12$test10 <- replace(course1_12$test10, is.na(course1_12$test10), 0)
course1_12$test11 <- replace(course1_12$test11, is.na(course1_12$test11), 0)
course1_12$test12 <- replace(course1_12$test12, is.na(course1_12$test12), 0)
course1_12$test13 <- replace(course1_12$test13, is.na(course1_12$test13), 0)
course1_12$test14 <- replace(course1_12$test14, is.na(course1_12$test14), 0)

course1_12 <- course1_12 %>%
  mutate(sum_tests_taken =  select(.,test1:test14) %>% rowSums(.))


vis_miss(course1_12)

#-----test percentage----#######
b <- attempt_student_week_12 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_12 <- merge(course1_12,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_12))
vis_miss(course1_12)

course1_12$test_percentage1 <- replace(course1_12$test_percentage1, is.na(course1_12$test_percentage1),0)
course1_12$test_percentage2 <- replace(course1_12$test_percentage2, is.na(course1_12$test_percentage2), 0)
course1_12$test_percentage3 <- replace(course1_12$test_percentage3, is.na(course1_12$test_percentage3), 0)
course1_12$test_percentage4 <- replace(course1_12$test_percentage4, is.na(course1_12$test_percentage4), 0)
course1_12$test_percentage5 <- replace(course1_12$test_percentage5, is.na(course1_12$test_percentage5), 0)
course1_12$test_percentage6 <- replace(course1_12$test_percentage6, is.na(course1_12$test_percentage6), 0)
course1_12$test_percentage7 <- replace(course1_12$test_percentage7, is.na(course1_12$test_percentage7), 0)
course1_12$test_percentage8 <- replace(course1_12$test_percentage8, is.na(course1_12$test_percentage8), 0)
course1_12$test_percentage9 <- replace(course1_12$test_percentage9, is.na(course1_12$test_percentage9), 0)
course1_12$test_percentage10 <- replace(course1_12$test_percentage10, is.na(course1_12$test_percentage10), 0)
course1_12$test_percentage11 <- replace(course1_12$test_percentage11, is.na(course1_12$test_percentage11), 0)
course1_12$test_percentage12 <- replace(course1_12$test_percentage12, is.na(course1_12$test_percentage12), 0)
course1_12$test_percentage13 <- replace(course1_12$test_percentage13, is.na(course1_12$test_percentage13), 0)
course1_12$test_percentage14 <- replace(course1_12$test_percentage14, is.na(course1_12$test_percentage14), 0)



vis_miss(course1_12)

course1_12 <- course1_12 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage14) %>% rowMeans(.))

videos_agg <- event_selected_course %>%
  filter(content_type == "resource/x-osv-kaltura/mashup") %>%
  group_by(content_pk,academic_week) %>%
  dplyr::summarise(n=n())
###------week12 dataset#-----
weeks12 <- course[,65:76]
sum_weeks12 <- course[,83:92]
days_week12 <- course[,100:112]
video_week12 <- course[,118:128]
course1_12 <- course1_12 %>%
  cbind(weeks12,sum_weeks12,video_week12)


##------Training and predicting over week12----####
course1_12 <- tibble::rowid_to_column(course1_12, "row")
set.seed(123)

course1_12 <- course1_12 %>%
  mutate(score_class = cut(as.numeric(course1_12$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_12 <- course1_12 %>%
  mutate(score_class = as.factor(course1_12$score_class))

#c1 <- course1_9[course1_9$cluster==2,]
c1 <- course1_12[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,69]

train_course1 <- train1[,5:68]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:68]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 20,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(123)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune1 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune1$bestTune
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune12 <- train(score_class ~ ., data = train_course1,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid1,
                  verbose=FALSE,
                  weights = model_weights)



res <- gbm.tune12$results
res

gbm.pred12 <- predict(gbm.tune12,test_course1)

confusionMatrix(gbm.pred12,test1$score_class)   

week12.prob <- predict(gbm.tune12,test_course1,type="prob")

roc.week12 <- roc(predictor=week12.prob$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)))
roc.week12$auc
plot(roc.week12,main="GBM ROC")

plot(roc.week12,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week12.prob$fail,
    response=test1$score_class,
    levels=rev(levels(test1$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)
















plot(roc.week3,main="ROC curves in diffrent weeks: bank en finacien")

roc(predictor=week3.prob$fail,
    response=test1$score_class,
    levels=rev(levels(test1$score_class)),
    plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)



legend("bottomright",legend=c("feature set: test grades","feature set: number of times videos opened","feature set: student activities in each week"),col=c("blue","red","green"),lwd = 4)

