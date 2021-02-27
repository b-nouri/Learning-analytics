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

video_events
#-----Preparing data for selected courses----###
#Selected program: ABA toegepaste economische wetenschappen (Leuv) 
#selected courses: accountancy (TEW) - 888130, 
#Bank- en financie - 888132, de globale economie - 888954
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
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:19))

course1 <- merge(course1,b,by="user_pk",all.x = TRUE)
course1 <- replace(course1, is.na(course1), 0)
rm(b)
rm(c)
rm(df1)

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

course1 <- subset(course1, select=-c(course_name))

###--------Analysis - Forecasting------####
###---Train Test Split----###
course1 <- tibble::rowid_to_column(course1, "row")
course1$final_score <- as.integer(course1$final_score)
course1 <- subset(course1, select=-c(score_class))

#course1 <- subset(course1, select=-c(n_event_course))
set.seed(123)

train1 <-sample_frac(course1, 0.8)
sid<-train1$row
test1 <- course1[-sid,]

train_course1 <- train1[,3:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:ncol(test1)]
str(train_course1)
##------Training and predicting----####

train.control <- trainControl(method="LOOCV")

train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)


model1_PR <- train(final_score ~ . , data = train_course1,
                    method = "parRF", trControl = train.control,metric = "MAE",
                   importance=T)

model2_svm <- train(final_score ~ . , data = train_course1,
                   method = "svmLinear", trControl = train.control,metric = "MAE")

model3_glm <- train(final_score ~ . , data = train_course1,
                         method = "glmStepAIC", trControl = train.control,metric = "MAE")


model4_elasticnet <- train(final_score ~ . , data = train_course1,
                         method = "glmnet", trControl = train.control,metric = "MAE",
                         preProcess = c())

model5_xgbTree <- train(final_score ~ . , data = train_course1,
                        method = "xgbTree", trControl = train.control,metric = "MAE")


model6_gbm <- train(final_score ~ . , data = train_course1,
                    method = "gbm", trControl = train.control,metric = "MAE")

print(min(model1_PR$results$MAE))
print(min(model2_svm$results$MAE))
print(min(model3_glm$results$MAE))
print(min(model4_elasticnet$results$MAE))
print(min(model5_xgbTree$results$MAE))
print(min(model6_gbm$results$MAE))


pred1 <- data.frame(predict(model1_PR, test_course1, type = "raw"))
pred2 <- data.frame(predict(model2_svm, test_course1, type = "raw"))
pred3 <- data.frame(predict(model3_glm, test_course1, type = "raw"))
pred4 <- data.frame(predict(model4_elasticnet, test_course1, type = "raw"))
pred5 <- data.frame(predict(model5_xgbTree, test_course1, type = "raw"))
pred6 <- data.frame(predict(model6_gbm, test_course1, type = "raw"))

results <- cbind(id_test1,test1$final_score)
names(results)[3] <- "actual_final_score"
# 
# results <- cbind(results,pred5)
# names(results)[4] <- "prediction_xgbTree"

results <- cbind(results,pred1,pred2,pred3,pred4,pred5,pred6)
names(results)[4] <- "prediction_rf"
names(results)[5] <- "prediction_svm"
names(results)[6] <- "prediction_glm"
names(results)[7] <- "prediction_glmnet"
names(results)[8] <- "prediction_xgbTree"
names(results)[9] <- "prediction_gbm"

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
  mutate(score_svm =round(prediction_svm,0)) %>%
  mutate(score_glm =round(prediction_glm,0)) %>%
  mutate(score_glmnet =round(prediction_glmnet,0)) %>%
  mutate(score_xgbTree =round(prediction_xgbTree,0)) %>%
  mutate(score_gbm =round(prediction_gbm,0))
  
mae_test_rf <- mean(abs(results$score_rf-results$actual_final_score))
mae_test_svm <- mean(abs(results$score_svm-results$actual_final_score))
mae_test_glm <- mean(abs(results$score_glm-results$actual_final_score))
mae_test_glmnet <- mean(abs(results$score_glmnet-results$actual_final_score))
mae_test_xgbTree <- mean(abs(results$score_xgbTree-results$actual_final_score))
mae_test_gbm <- mean(abs(results$score_gbm-results$actual_final_score))

mae_test_rf
mae_test_svm
mae_test_glm
mae_test_glmnet
mae_test_xgbTree
mae_test_gbm

gbmImp <- varImp(model2_svm, conditional=TRUE)
gbmImp
plot(gbmImp, top = 32)

##------Training and predicting----####
course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(score_class = cut(as.numeric(course1$final_score), breaks=c(0,9.9,21), labels=c( 'pass', 'fail'))) %>%
  mutate(score_class = as.factor(course1$score_class))

train1 <-sample_frac(course1, 0.8)
sid<-train1$row
test1 <- course1[-sid,]

train_course1 <- train1[,4:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1)-1)]


###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3,classProbs = TRUE)

model1_PR <- train(score_class ~ . , data = train_course1,
                   method = "parRF", trControl = train.control,metric="ROC")

model2_svm <- train(score_class ~ . , data = train_course1,
                    method = "svmLinear", trControl = train.control,metric="ROC")

model3_glm <- train(score_class ~ . , data = train_course1,
                    method = "glmStepAIC", trControl = train.control,metric="ROC",
                    preProcess = c("zv", "center", "scale", "pca"))

model4_elasticnet <- train(score_class ~ . , data = train_course1,
                           method = "glmnet", trControl = train.control,metric="ROC",
                           preProcess = c())

model5_xgbTree <- train(score_class ~ . , data = train_course1,
                        method = "xgbTree", trControl = train.control,metric="ROC")


model6_gbm <- train(score_class ~ . , data = train_course1,
                    method = "gbm", trControl = train.control,metric="ROC")

model7 <- train(score_class ~ . , data = train_course1,
                    method = "adaboost", trControl = train.control,metric="ROC")
model8 <- train(score_class ~ . , data = train_course1,
                    method = "AdaBoost.M1", trControl = train.control,metric="ROC")
model9 <- train(score_class ~ . , data = train_course1,
                    method = "amdai", trControl = train.control,metric="ROC")
model10 <- train(score_class ~ . , data = train_course1,
                    method = "AdaBag", trControl = train.control,metric="ROC")
model11 <- train(score_class ~ . , data = train_course1,
                    method = "bagFDAGCV", trControl = train.control,metric="ROC")
model12 <- train(score_class ~ . , data = train_course1,
                    method = "bagFDA", trControl = train.control,metric="ROC")
model13 <- train(score_class ~ . , data = train_course1,
                    method = "LogitBoost", trControl = train.control,metric="ROC")

model30 <- train(score_class ~ . , data = train_course1,
                    method = "svmLinearWeights", trControl = train.control,metric="ROC")

model33 <- train(score_class ~ . , data = train_course1,
                    method = "manb", trControl = train.control,metric="ROC")
model34 <- train(score_class ~ . , data = train_course1,
                    method = "mlpKerasDropoutCost", trControl = train.control,metric="ROC")
model35 <- train(score_class ~ . , data = train_course1,
                    method = "naive_bayes", trControl = train.control,metric="ROC")
model35

model39 <- train(score_class ~ . , data = train_course1,
                 method = "pda", trControl = train.control,metric="ROC")
model40 <- train(score_class ~ . , data = train_course1,
                 method = "stepQDA", trControl = train.control,metric="ROC")
model41 <- train(score_class ~ . , data = train_course1,
                 method = "QdaCov", trControl = train.control,metric="ROC")
model42 <- train(score_class ~ . , data = train_course1,
                 method = "sparseLDA", trControl = train.control,metric="ROC")
model43 <- train(score_class ~ . , data = train_course1,
                 method = "snn", trControl = train.control,metric="ROC")
model44 <- train(score_class ~ . , data = train_course1,
                 method = "awtan", trControl = train.control,metric="ROC")
model45 <- train(score_class ~ . , data = train_course1,
                 method = "vbmpRadial", trControl = train.control,metric="ROC")





print(max(model1_PR$results$Accuracy))
print(max(model2_svm$results$Accuracy))
print(max(model3_glm$results$Accuracy))
print(max(model4_elasticnet$results$Accuracy))
print(max(model5_xgbTree$results$Accuracy))
print(max(model6_gbm$results$Accuracy))


pred1 <- data.frame(predict(model1_PR, test_course1, type = "raw"))
pred2 <- data.frame(predict(model2_svm, test_course1, type = "raw"))
pred3 <- data.frame(predict(model3_glm, test_course1, type = "raw"))
pred4 <- data.frame(predict(model4_elasticnet, test_course1, type = "raw"))
pred5 <- data.frame(predict(model5_xgbTree, test_course1, type = "raw"))
pred6 <- data.frame(predict(model6_gbm, test_course1, type = "raw"))



results <- cbind(id_test,test1$final_score,test1$score_class)
names(results)[3] <- "actual_final_score"
names(results)[4] <- "actual_class"



results <- cbind(results,pred1,pred2,pred3,pred4,pred5,pred6)
names(results)[5] <- "prediction_rf"
names(results)[6] <- "prediction_svm"
names(results)[7] <- "prediction_glm"
names(results)[8] <- "prediction_glmnet"
names(results)[9] <- "prediction_xgbTree"
names(results)[10] <- "prediction_gbm"


conf_matrm <- confusion_matrix(targets = results$actual_class,
                               predictions = results$prediction_rf)
conf_matsvm <- confusion_matrix(targets = results$actual_class,
                                predictions = results$prediction_svm)
conf_matglm <- confusion_matrix(targets = results$actual_class,
                                predictions = results$prediction_glm)
conf_matglmnet <- confusion_matrix(targets = results$actual_class,
                                   predictions = results$prediction_glmnet)
conf_matxbtree <- confusion_matrix(targets = results$actual_class,
                                   predictions = results$prediction_xgbTree)
conf_matgbm <- confusion_matrix(targets = results$actual_class,
                                predictions = results$prediction_gbm)

conf_matrm
conf_matsvm
conf_matglm
conf_matglmnet
conf_matxbtree
conf_matgbm

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

gbmImp <- varImp(model5_xgbTree, conditional=TRUE)
gbmImp
plot(gbmImp, top = 32)
