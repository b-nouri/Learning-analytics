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

SAP_ <- read.csv("c:/ALCAPAS/Anon_SAP_data.dsv",
                 sep="\t", quote = "",na.strings = c("", "NA"),header = F)

colnames(SAP_) <- c("course_id", "opo_id", "course_name","user_pk",
                    "type of program","program","current_phase","score_january","score_june","final_score")
sap <- SAP_[2:nrow(SAP_),]
colnames(sap)[1] <- "course_pk"

# colnames(SAP_) <- c("course_pk","opo_id", "course_name","user_pk",
#                     "type of program","program",
#                     "current_phase","score_january","score_june",
#                     "final_score")


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

#-----Preparing data for selected courses----###
#Selected program: ABA toegepaste economische wetenschappen (Leuv) 
#selected courses: accountancy (TEW) - 888130, 
#Bank- en financie - 888132, de globale economie - 888954
sap$final_score <- as.integer(sap$final_score)
sap1 <- sap[sap$course_pk == 888131 &
              sap$final_score > 0 &
              sap$program == "ABA toegepaste economische wetenschappen (Leuv)",]


attempt_student1 <- attempt_student[attempt_student$course_pk == 888131 &
                                      attempt_student$user_pk %in% sap1$user_pk,]

event_course_week1 <- event_course_week[event_course_week$course_pk == 888131 &
                                          event_course_week$academic_week < 38,]

event_week_day1 <- event_week_day[event_week_day$course_pk == 888131 &
                                    event_course_week$academic_week < 38,]

event_course1 <- event_course[event_course$course_pk == 888131 &
                                event_course$user_pk %in% sap1$user_pk,]

formative_content1 <- formative_content[formative_content$course_pk == 888131,]

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

course1 <- merge(course1,event_course1[,c("user_pk","n_event_course")],by="user_pk")

ggplot(course1,aes(x=n_event_course,y=final_score)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

c <- attempt_student1 %>%
  distinct(content_pk)


vis_miss(course1)




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

content1 <- content_df[content_df$course_pk == 888131,]

video1 <- content1[content1$content_type %in% c('resource/x-osv-kaltura/mashup',
                                                'resource/x-bb-toollink'),]

video_event1 <- event_df[event_df$user_pk %in% sap1$user_pk &
                           event_df$content_pk %in% video1$content_pk,]

c <- unique(video_event1[,"title"])

b <- video_event1 %>%
  group_by(user_pk,title) %>%
  dplyr::summarise(n= n()) %>%
  spread(title,n) %>%
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
library(factoextra)
library(cluster)
distance <- get_dist(df1)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df1, centers = 4, nstart = 25)


fviz_cluster(k2, data = df1)

set.seed(123)

fviz_nbclust(df1, kmeans, method = "wss")

fviz_nbclust(df1, kmeans, method = "silhouette")

library(cluster)



set.seed(123)
final1 <- kmeans(df1, 2, nstart = 25)
fviz_cluster(final1, data = df1)

set.seed(123)
final2 <- kmeans(df1, 3, nstart = 25)
fviz_cluster(final2, data = df1)


course1 <- course1 %>%
  mutate(cluster = final2$cluster)

course <- course1

###---Train Test Split----###
#course1 <- subset(course1, select=-c(n_event_course))
course1 <- subset(course1, select=-c(score_class))

set.seed(123)

c1 <- course1[,]
train1 <-sample_frac(c1, 0.8)
sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]
#test1 <- test1[,]

train_course1 <- train1[,3:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1))]

##------Training and predicting----####

train.control <- trainControl(method="LOOCV")

#train.control <- trainControl(method = "repeatedcv",
#                             number = 10, repeats = 3)


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

gbmImp <- varImp(model6_gbm, conditional=TRUE)
gbmImp <- varImp(model6_gbm, scale = FALSE)
vImpGbm=varImp(model6_gbm)
gbmImp
plot(gbmImp, top = 32)




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

gbmImp <- varImp(model6_gbm, conditional=TRUE)
gbmImp <- varImp(model6_gbm, scale = FALSE)
vImpGbm=varImp(model6_gbm)
gbmImp
plot(gbmImp, top = 10)


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


train_course1 <- train1[,4:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1)-1)]
table(train_course1$score_class)

###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
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


install.packages("pROC")
library(pROC)
# Select a parameter setting
selectedIndices <- model1_PR$pred$mtry == 2
# Plot:
library(MLeval)
res <- evalm(list(model1_PR,model4_elasticnet,model6_gbm),gnames=c('rf','Elasticnet','gbm'))


plot.roc(model1_PR$pred$obs[selectedIndices],
         model1_PR$pred$M[selectedIndices])
install.packages("plotROC")
library(plotROC)

ggplot(model1_PR$pred[selectedIndices, ], 
       aes(m = M, d = factor(obs, levels = c("R", "M")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


par(pty="s") lrROC <- roc(vdata_Y ~ LR_predict,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
svmROC <- roc(vdata_Y ~ svm_predict,plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)
## Setting levels: control = 0, case = 1 ## Setting direction: controls < cases
legend("bottomright",legend=c("Logistic Regression","SVM"),col=c("green","blue"),lwd=4)




library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm

gbmImp <- varImp(model1_PR, conditional=TRUE)
gbmImp
plot(gbmImp, top = 32)





#########-------------final model-------------####
library(doParallel)		# parallel processing
library(gbm)				  # GBM Models
library(pROC)				  # plot the ROC curve
library(xgboost)      # Extreme Gradient Boosting

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


train_course1 <- train1[,4:ncol(train1)]

id_test1 <- test1[,1:2]
test_course1 <- test1[,4:(ncol(test1)-1)]
table(train_course1$score_class)

library(doParallel)		# parallel processing
library(gbm)				  # GBM Models
library(pROC)				  # plot the ROC curve
library(xgboost)      # Extreme Gradient Boosting

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

##----------------------------------------------
## XGBOOST
# Some stackexchange guidance for xgboost
# http://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees

# Set up for parallel procerssing
set.seed(123)
registerDoParallel(4,cores=4)
getDoParWorkers()

xgb_grid_1 = expand.grid(
             nrounds = 1000,
             eta = c(0.01,0.1, 0.001, 0.0001),
             max_depth = c(2, 4, 6, 8, 10),
             gamma =c(0.4,0.5,0.6,0.7,0.8,0,9,1)
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

getDoParWorkers()
xgb.tune <-train(score_class ~ ., data = as.data.frame(train_course1),
                 method="xgbTree",
                 metric="ROC",
                 trControl=xgb_trcontrol_1,
                 grid = xgb_grid_1,
                 weights = model_weights)


xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models

xgb_grid_1 = xgb.tune$bestTune
xgb_trcontrol_1 = trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


xgb.tune <-train(score_class ~ ., data = as.data.frame(train_course1),
                 method="xgbTree",
                 metric="ROC",
                 trControl=xgb_trcontrol_1,
                 grid = xgb_grid_1)


res <- xgb.tune$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
xgb.pred <- predict(xgb.tune,test1)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,test1$score_class)   

#Draw the ROC curve 
xgb.probs <- predict(xgb.tune,test1,type="prob")
#head(xgb.probs)

xgb.ROC <- roc(predictor=xgb.probs$pass,
               response=test1$score_class,
               levels=rev(levels(test1$score_class)),
               direction= ">")
xgb.ROC$auc
# Area under the curve: 0.8857

plot(xgb.ROC,main="xgboost ROC")


####----------Random Forest----------#####

registerDoParallel(4,cores=4)
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
rf.pred <- predict(rf.tune1,test1)

#Look at the confusion matrix  
confusionMatrix(rf.pred,test1$score_class)   

#Draw the ROC curve 
rf.probs <- predict(rf.tune,test1,type="prob")
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
plot(glm.ROC,main="ROC Curves for prediction of student success in the course: Marketing",)

glm.ROC <- roc(predictor=xgb.probs$fail,
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

scores <- train1[,77]

train_course1 <- train1[,60:75]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,60:75]
table(train_course1$score_class)

###-------------Classification------------###
library(doParallel)		# parallel processing
library(gbm)				  # GBM Models
library(pROC)				  # plot the ROC curve
library(xgboost)      # Extreme Gradient Boosting

###-------------Classification------------###

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
videos.probs <- predict(glmnet.fit,test1,type="prob")
#head(xgb.probs)



gbm.videos <- roc(predictor=videos.probs$fail,
               response=test1$score_class,
               levels=rev(levels(test1$score_class)),direction = "<")
gbm.videos$auc
#Area under the curve: 0.5624
plot(gbm.videos,main="GBM ROC")



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

scores <- train1[,77]

train_course1 <- train1[,5:59]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:59]
table(train_course1$score_class)

###-------------Classification------------###
#train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)


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
weeks.probs <- predict(glmnet.fit,test_course1,type="prob")
head(gbm.probs)

gbm.weeks <- roc(predictor=weeks.probs$fail,
                  response=test1$score_class,
                  levels=rev(levels(test1$score_class)),direction = "<")
gbm.weeks$auc
#Area under the curve: 0.5624
plot(gbm.weeks,main="GBM ROC")






par(pty="s")
plot(gbm.videos,main="ROC curves based on different feature sets for the course: Marketing")



gbm.videos <- roc(predictor=videos.probs$fail,
                  response=test1$score_class,
                  levels=rev(levels(test1$score_class)),
                  direction= "<",
                  plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

gbm.weeks <- roc(predictor=weeks.probs$fail,
                 response=test1$score_class,
                 levels=rev(levels(test1$score_class)),direction = "<",
                 plot=TRUE,print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.3,legacy.axes=TRUE,add = TRUE)


legend("bottomright",legend=c("feature set: number of times videos opened","feature set: student activities in each week"),col=c("red","green"),lwd = 4)
