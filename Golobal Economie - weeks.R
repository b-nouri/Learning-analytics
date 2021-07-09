until_week = 20
course_id = 888954

setwd(paste0('C:\\Thesis\\course2-global economie\\week1-',until_week))

###----Select Course----###
attempt_df <- attempt_df2[attempt_df$course_pk == course_id,]

content_df <- content_df2[content_df$course_pk == course_id,]

event_df <- event_df2[event_df2$course_pk == course_id,]
sap1 <- sap[sap$course_pk == course_id &
              sap$final_score > 0 & sap$current_phase == 'eerste fase' &
              sap$program == "ABA toegepaste economische wetenschappen (Leuv)",]

sap$final_score <- as.integer(sap$final_score)
formative_content1 <- formative_content[formative_content$course_pk == course_id,]


rm(attempts)
rm(contents)
rm(events)
###----assign weeks and select week datasets----####
attempt_df <- attempt_df %>%
  mutate(course_week = academic_week -20)


event_df <- event_df %>%
  mutate(course_week = academic_week -20)

a <- as.data.frame(names(event_df))

attempt <- attempt_df[attempt_df$course_week <= until_week & attempt_df$course_week >0,]
event <- event_df[event_df$course_week <= until_week & event_df$course_week > 0,]

##---number of exam attempts----###
nn <- attempt %>%
  group_by(content_pk,user_pk) %>%
  filter(user_pk %in% sap1$user_pk) %>%
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

attempt_student1 <- attempt_student[attempt_student$user_pk %in% sap1$user_pk,]

sap$final_score <- as.integer(sap$final_score)

course1 <- sap1 %>%
  mutate(final_score = ifelse((sap1$final_score > 9),1,0)) %>%
  select(course_name,user_pk,final_score)


student_list <- as.data.frame(course1$user_pk)
colnames(student_list)[1] <- "user_pk"

##-----export video and test names------######
video1 <- content_df[content_df$content_type %in% c('resource/x-osv-kaltura/mashup',
                                                    'resource/x-bb-toollink'),c('content_type','title','content_pk')]

video_event1 <- event[event$user_pk %in% sap1$user_pk &
                        event$content_pk %in% video1$content_pk,]

c <- unique(video_event1[,c("content_pk","title","content_type")])  %>%
  arrange(content_pk)

write_xlsx(c, "video-feature-names.xlsx")

c <- unique(attempt_student1[,c("content_pk","title")])  %>%
  arrange(content_pk)

write_xlsx(c, "test-feature-names.xlsx")

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

b <- replace(b,is.na(b),0)

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

course1 <- replace(course1,is.na(course1),0)

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

test_features <- colnames(test_so_far)
course1 <- merge(course1,test_so_far,by="user_pk",all.x = TRUE)

c <- unique(attempt_student1[,c("content_pk","title")])  %>%
  arrange(content_pk)

b <- attempt_student1 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)

b <- b %>% rename_at(vars(c$content_pk), ~ paste0('grade_test', 1:length(c$content_pk)))

test_features <- append(test_features,colnames(b))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)


library(lares)
course1$final_score <- as.integer(course1$final_score)



b <- attempt_student1 %>%
  group_by(user_pk) %>%
  dplyr::summarise(average_test_score = mean(test_percentage),sum_highest_test_scores = sum(highest))

test_features <- append(test_features,colnames(b))
course1 <- merge(course1,b,by="user_pk",all.x = TRUE)

test_features <- test_features[-grep("user_pk", test_features)]

###--------Procrastination-------####
event <- event %>%
  mutate(date = date(time))


c <- event %>%
  group_by(content_pk) %>%
  dplyr::summarise(release_date=min(date))

event1 <- merge(event,c,'content_pk')

event1 <- event1 %>%
  mutate(date = date(time)) 

event1 <- event1 %>%
  mutate(procrastination = as.double(difftime(lubridate::ymd(event1$date),
                                              lubridate::ymd(event1$release_date),
                                              units = "days")))

c <- event1 %>%
  group_by(user_pk,content_pk) %>%
  dplyr::summarise(proc = min(procrastination)) %>%
  group_by(user_pk) %>%
  dplyr::summarise(procrastination = mean(proc))


course1 <- merge(course1,c,'user_pk')


c <- event %>%
  group_by(user_pk,date) %>%
  arrange(user_pk,date) %>%
  select(user_pk,content_pk,date) %>%
  group_by(user_pk,date) %>%
  dplyr::summarise(n=n()) %>%
  mutate(ldate=lag(date), luser = lag(user_pk)) %>%
  ungroup() 
c <- c %>%
  mutate(max_days_between_events = as.double(difftime(lubridate::ymd(c$date),
                                                      lubridate::ymd(c$ldate),
                                                      units = "days"))) %>%
  mutate(max_days_between_events = ifelse(is.na(max_days_between_events),-1, as.integer(max_days_between_events)))

c <- c %>%
  arrange(max_days_between_events) %>%
  group_by(user_pk) %>%
  slice(n()) %>%
  mutate(longest_time_inactive = ifelse(max_days_between_events == -1 ,NA, as.integer(max_days_between_events))) %>%
  select(longest_time_inactive)


course1 <- merge(course1,c,'user_pk')

course1$longest_time_inactive <- replace(course1$longest_time_inactive, is.na(course1$longest_time_inactive),until_week*7 )



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



###----------documents opened------####
c <- unique(event[event$content_type == 'resource/x-bb-document','content_pk'])

b <- event[event$content_type == 'resource/x-bb-document',] %>%
  group_by(user_pk,content_pk) %>%
  dplyr::summarise(summy = n()) %>%
  group_by(user_pk) %>%
  dplyr::summarise(percentage_documents_opened = n()/length(c),average_times_documents_opened = sum(summy)/length(c))


course1 <- merge(course1,b,'user_pk')


####-------Export plots-----------######

svg(file = "missing-values.svg", height=8.27, width=11.69)
#par(mar=c(5,3,2,2)+0.1)
vis_miss(course1) +
  theme(axis.text.x = element_text(angle = 90))
dev.off()


variables = data.frame(colnames(course1))
write_xlsx(variables, "variables-before-remove-missing.xlsx")
###------Handling missing values------##########
test_features <- test_features[-grep("percentage_tests_completed", test_features)]
test_features <- test_features[-grep("total_test_tries", test_features)]
test_features <- test_features[-grep("average_tries_test", test_features)]


test_cels <- course1[,colnames(course1) %in% test_features]
na <- sum(is.na(test_cels))
tot <- nrow(test_cels) * ncol(test_cels)

percent_na <- na/tot * 100

if (percent_na > 34){
  course1<-course1[,!(colnames(course1) %in% test_features)]
} else {
  
  for (col in test_features) {
    course1[,colnames(course1) == col] <- replace(course1[,colnames(course1) == col],
                                                  is.na(course1[,colnames(course1) == col]), median(course1[,colnames(course1) == col],na.rm = T))
  }
}

# course1$grade_test1 <- replace(course1$grade_test1, is.na(course1$grade_test1), median(course1$grade_test1,na.rm = T))
# course1$grade_test2 <- replace(course1$grade_test2, is.na(course1$grade_test2), median(course1$grade_test2,na.rm = T))
# course1$grade_test3 <- replace(course1$grade_test3, is.na(course1$grade_test3), median(course1$grade_test3,na.rm = T))
# course1$grade_test4 <- replace(course1$grade_test4, is.na(course1$grade_test4), median(course1$grade_test4,na.rm = T))
# course1$grade_test5 <- replace(course1$grade_test5, is.na(course1$grade_test5), median(course1$grade_test5,na.rm = T))
# course1$grade_test6 <- replace(course1$grade_test6, is.na(course1$grade_test6), median(course1$grade_test6,na.rm = T))
# course1$grade_test7 <- replace(course1$grade_test7, is.na(course1$grade_test7), median(course1$grade_test7,na.rm = T))
# course1$grade_test8 <- replace(course1$grade_test8, is.na(course1$grade_test8), median(course1$grade_test8,na.rm = T))
# course1$grade_test9 <- replace(course1$grade_test9, is.na(course1$grade_test9), median(course1$grade_test9,na.rm = T))
# course1$grade_test10 <- replace(course1$grade_test10, is.na(course1$grade_test10), median(course1$grade_test10,na.rm = T))
# course1$grade_test11 <- replace(course1$grade_test11, is.na(course1$grade_test11), median(course1$grade_test11,na.rm = T))
# course1$grade_test12 <- replace(course1$grade_test12, is.na(course1$grade_test12), median(course1$grade_test12,na.rm = T))
# course1$grade_test13 <- replace(course1$grade_test13, is.na(course1$grade_test13), median(course1$grade_test13,na.rm = T))
# course1$grade_test14 <- replace(course1$grade_test14, is.na(course1$grade_test14), median(course1$grade_test14,na.rm = T))
# # course1$grade_test15 <- replace(course1$grade_test15, is.na(course1$grade_test15), median(course1$grade_test15,na.rm = T))
# # course1$grade_test16 <- replace(course1$grade_test16, is.na(course1$grade_test16), median(course1$grade_test16,na.rm = T))
# course1$grade_test17 <- replace(course1$grade_test17, is.na(course1$grade_test17), median(course1$grade_test17,na.rm = T))
# course1$grade_test18 <- replace(course1$grade_test18, is.na(course1$grade_test18), median(course1$grade_test18,na.rm = T))
# course1$grade_test19 <- replace(course1$grade_test19, is.na(course1$grade_test19), median(course1$grade_test19,na.rm = T))

# 
# course1$average_test_score <- replace(course1$average_test_score, is.na(course1$average_test_score), median(course1$average_test_score,na.rm = T))
# course1$sum_test_scores_achieved <- replace(course1$sum_test_scores_achieved, is.na(course1$sum_test_scores_achieved), median(course1$sum_test_scores_achieved,na.rm = T))
course1$average_tries_test <- replace(course1$average_tries_test, is.na(course1$average_tries_test), median(course1$average_tries_test,na.rm = T))
course1 <- replace(course1,is.na(course1),0)


# M<-cor(course1[,-c(1,2,20:56)])
# head(round(M,2))
# 
# par(mar = rep(2, 4))
# 
# corrplot(M,method="number")


####-------Export plots and frames-----------######
svg(file = "feature-correlation-plot.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)

corr_var(course1,
         final_score,top=30
) 
dev.off()

svg(file = "missing-values-after-handling.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)
vis_miss(course1) +
  theme(axis.text.x = element_text(angle = 90))
dev.off()


variables = data.frame(colnames(course1))
write_xlsx(variables, "variables-after-remove-missing.xlsx")
##------Training and predicting----####
###---Train Test Split----###

course1 <- tibble::rowid_to_column(course1, "row")
set.seed(123)

course1 <- course1 %>%
  mutate(final_score= ifelse(course1$final_score == 1 , 'pass','fail'))

c1 <- course1[,]

# library(splitstackshape)
# train1 <- stratified(c1, c("score_class"), 0.75)

sample <- sample.int(n = nrow(course1), size = floor(.75*nrow(course1)), replace = F)
train <- course1[sample, ]
test  <- course1[-sample, ]


sid<-train$row
test1 <- c1[!(c1$row %in% sid),]


train_course1 <- train[,4:ncol(train)]

test_with_target <- test[,4:(ncol(test))]

id_test1 <- test[,1:2]
test_course1 <- test[,5:(ncol(test))]
table(train_course1$final_score)
##-------------Classification------------###
# model_weights <- ifelse(train_course1$final_score == "pass",
#                         (1/table(train_course1$final_score)[1]) * 0.5,
#                         (1/table(train_course1$final_score)[2]) * 0.5)

library(gbm)
library(ranger)


train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 5,classProbs = TRUE)

#train.control <- trainControl(method = "LOOCV",
#                              classProbs = TRUE)
mtry <- floor(sqrt(ncol(train_course1)))
tunegrid <- expand.grid(
  mtry =  (mtry:10)                                    
  ,splitrule = "gini"
  ,min.node.size = c(10,15,20,25)
)
registerDoParallel(cores=4)

model1 <- train(final_score ~ . , data = train_course1,
                method = "ranger",
                tuneGrid=tunegrid,
                tuneLength=15,
                importance = "impurity",
                trControl = train.control)

lambda_grid <- seq(0, 1, 0.1)
alpha_grid <- seq(0, 1, 0.1)

srchGrid <- expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)


model2 <- train(final_score ~ . , data = train_course1,
                method = "glmnet",
                trControl = train.control,
                tuneGrid = srchGrid,
                preProcess = c("center", "scale", "YeoJohnson", "nzv"))




grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20,50),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)


model3 <- train(final_score ~ . , data = train_course1,
                method = "gbm",
                tuneGrid=grid,
                trControl = train.control, verbose = FALSE)


model4 <- train(final_score ~ . , data = train_course1,
                method = "svmRadial",
                trControl = train.control, verbose = FALSE)

model5 <- train(final_score ~ . , data = train_course1,
                method = "glmnet",
                trControl = train.control,
                preProcess = c("center", "scale", "YeoJohnson", "nzv",'pca'))

print(max(model1$results$Accuracy))
print(max(model2$results$Accuracy))
print(max(model3$results$Accuracy))
print(max(model4$results$Accuracy))
print(max(model5$results$Accuracy))

#predict(model2$finalModel, type = "coefficients")

  
pred1 <- data.frame(predict(model1, test_course1, type = "raw"))
pred2 <- data.frame(predict(model2, test_course1, type = "raw"))
pred3 <- data.frame(predict(model3, test_course1, type = "raw"))
pred4 <- data.frame(predict(model4, test_course1, type = "raw"))
pred5 <- data.frame(predict(model5, test_course1, type = "raw"))

pred6 <- data.matrix(predict(model5$finalModel, type="coef"))


names(pred1)[1] <- "prediction_rf"
names(pred2)[1] <- "prediction_glmnet"
names(pred3)[1] <- "prediction_gbm"
names(pred4)[1] <- "prediction_svm"
names(pred5)[1] <- "prediction_pcr"


rm(results)
results <- cbind(id_test1$user_pk,as.character(test$final_score))


results <- as.data.frame(cbind(results,
                               pred1$prediction_rf,pred2$prediction_glmnet,pred3$prediction_gbm,pred4$prediction_svm,pred5$prediction_pcr))
names(results)[2] <- "actual_class"
names(results)[3] <- "prediction_rf"
names(results)[4] <- "prediction_glmnet"
names(results)[5] <- "prediction_gbm"
names(results)[6] <- "prediction_svm"
names(results)[7] <- "prediction_pcr"

results <- results %>%
  mutate(nactual_class = ifelse(actual_class == 'pass',1,0)) %>%
  mutate(nprediction_rf = ifelse(prediction_rf == 'pass',1,0)) %>%
  mutate(nprediction_glmnet = ifelse(prediction_glmnet == 'pass',1,0)) %>%
  mutate(nprediction_gbm = ifelse(prediction_gbm == 'pass',1,0)) %>%
  mutate(nprediction_svm = ifelse(prediction_svm == 'pass',1,0)) %>%
  mutate(nprediction_pcr = ifelse(prediction_pcr == 'pass',1,0))


##--------Precision-Recall - F1-#############
cm = as.matrix(table(Actual = results$actual_class, Predicted = results$prediction_rf)) 
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

a <- data.frame(precision, recall, f1) 


macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
b <- data.frame(macroPrecision, macroRecall, macroF1) 

colnames(b) <- colnames(a)

ddf1 <- rbind(a, b) %>%
  tibble::rownames_to_column( "VALUE")

ddf1[1,1] <- 'fail-model1 - RF'
ddf1[2,1] <- 'pass-model1 - RF'
ddf1[3,1] <- 'macro-model1 - RF'


##------model2 performance--------###

cm = as.matrix(table(Actual = results$actual_class, Predicted = results$prediction_glmnet)) 
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

a <- data.frame(precision, recall, f1) 


macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
b <- data.frame(macroPrecision, macroRecall, macroF1)

colnames(b) <- colnames(a)

ddf2 <- rbind(a, b) %>%
  tibble::rownames_to_column( "VALUE")

ddf2[1,1] <- 'fail-model2 - glm'
ddf2[2,1] <- 'pass-model2 - glm'
ddf2[3,1] <- 'macro-model2 - glm'


##------model3 performance--------###

cm = as.matrix(table(Actual = results$actual_class, Predicted = results$prediction_gbm)) 
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

a <- data.frame(precision, recall, f1) 


macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
b <- data.frame(macroPrecision, macroRecall, macroF1)


colnames(b) <- colnames(a)

ddf3 <- rbind(a, b) %>%
  tibble::rownames_to_column( "VALUE")

ddf3[1,1] <- 'fail-model3 - gbm'
ddf3[2,1] <- 'pass-model3 - gbm'
ddf3[3,1] <- 'macro-model3 - gbm'


##------model4 performance--------###

cm = as.matrix(table(Actual = results$actual_class, Predicted = results$prediction_svm)) 
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

a <- data.frame(precision, recall, f1)

macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
b <- data.frame(macroPrecision, macroRecall, macroF1)

colnames(b) <- colnames(a)

ddf4 <- rbind(a, b) %>%
  tibble::rownames_to_column( "VALUE")

ddf4[1,1] <- 'fail-model4 - svm'
ddf4[2,1] <- 'pass-model4 - svm'
ddf4[3,1] <- 'macro-model4 - svm'


##------model5 performance--------###

cm = as.matrix(table(Actual = results$actual_class, Predicted = results$prediction_pcr)) 
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

a <- data.frame(precision, recall, f1) 


macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
b <- data.frame(macroPrecision, macroRecall, macroF1)

colnames(b) <- colnames(a)

ddf5 <- rbind(a, b) %>%
  tibble::rownames_to_column( "VALUE")

ddf5[1,1] <- 'fail-model5 - pcr'
ddf5[2,1] <- 'pass-model5 - pcr'
ddf5[3,1] <- 'macro-model5 - pcr'

###-----add all precision recall dataframes together and save them-----####

precision_recalls <- rbind(ddf1,ddf2,ddf3,ddf4,ddf5)

write_xlsx(precision_recalls, "precision-recall.xlsx")


#---------AUC of different models------####

library(pROC)

pred1 <- data.frame(predict(model1, test_course1, type = "prob"))
pred2 <- data.frame(predict(model2, test_course1, type = "prob"))
pred3 <- data.frame(predict(model3, test_course1, type = "prob"))
pred4 <- data.frame(predict(model4, test_course1, type = "prob"))
pred5 <- data.frame(predict(model5, test_course1, type = "prob"))



roc_1 <- roc(results$nactual_class, pred1$fail)
a <- auc(roc_1)

roc_2 <- roc(results$nactual_class, pred2$fail)
b <- auc(roc_2)

roc_3 <- roc(results$nactual_class, pred3$fail)
c <- auc(roc_3)

roc_4 <- roc(results$nactual_class, pred4$fail)
d <- auc(roc_4)

roc_5 <- roc(results$nactual_class, pred4$fail)
e <- auc(roc_5)

AUC = data.frame(c('model1 - RF','model2 GLM','Model 3 GBM','MODEL 4 SVM','Model 5 PCR'),c(a,b,c,d,e))
colnames(AUC)[1] <- 'model'
colnames(AUC)[2] <- 'AUC'

write_xlsx(AUC, "AUC.xlsx")


###-----------Variable importance plots------######

svg(file = "feature-importance-RF.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)

imp1 <- varImp(model1, conditional=TRUE,scale = FALSE)
a <- as.tibble(imp1$importance) %>%
  mutate(v_name = row.names(imp1$importance)) %>%
  mutate(v_type = 0) %>%
  mutate(v_type = ifelse(str_detect(v_name,'test'),'Test Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'video'),'Video Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'document'),'Document Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'active'),'Student Activity Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'procrastination'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'inactive'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'event'),'Student Activity Related', v_type)) %>%
  mutate(color = ifelse(v_type == 'Test Related',"#660000", "#663300")) %>%
  mutate(color = ifelse(v_type=='Video Related',"#006633", color)) %>%
  mutate(color = ifelse(v_type=='Document Related',"#003399", color)) %>%
  mutate(color = ifelse(v_type=='Procrastination Related',"#CC9900", color))


names(a)[1] <- 'v_importance'
b <- a %>%
  arrange(desc(v_importance))

b <- b[1:20,]

# c <- b %>%
#   group_by(v_type,color) %>%
#   dplyr::summarise(n=n())
# colors <- c("Test Related"="#660000","Video Related"="#006633","Document Related"="#003399",
#           "Procrastination Related"="#CC9900","Student Activity Related"="#663300")

ggplot(b, aes(x=reorder(v_name, v_importance), color=v_type, y=v_importance,)) + 
  geom_point() +
  geom_segment(aes(x=v_name,xend=v_name,y=0,yend=v_importance), size = 1) +
  scale_fill_discrete(name = "Variable Group") +
  labs(color='Variable Group') +
  theme_bw() +
  ylab("Conditional Importance of Features") +
  xlab("Variable Name") +
  coord_flip()

dev.off()


svg(file = "feature-importance-GLM.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)

imp2 <- varImp(model2, conditional=TRUE,scale = FALSE)
a <- as.tibble(imp2$importance) %>%
  mutate(v_name = row.names(imp2$importance)) %>%
  mutate(v_type = 0) %>%
  mutate(v_type = ifelse(str_detect(v_name,'test'),'Test Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'video'),'Video Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'document'),'Document Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'active'),'Student Activity Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'procrastination'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'inactive'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'event'),'Student Activity Related', v_type)) %>%
  mutate(color = ifelse(v_type == 'Test Related',"#660000", "#663300")) %>%
  mutate(color = ifelse(v_type=='Video Related',"#006633", color)) %>%
  mutate(color = ifelse(v_type=='Document Related',"#003399", color)) %>%
  mutate(color = ifelse(v_type=='Procrastination Related',"#CC9900", color))


names(a)[1] <- 'v_importance'
b <- a %>%
  arrange(desc(v_importance))

b <- b[1:20,]

# c <- b %>%
#   group_by(v_type,color) %>%
#   dplyr::summarise(n=n())
# colors <- c("Test Related"="#660000","Video Related"="#006633","Document Related"="#003399",
#           "Procrastination Related"="#CC9900","Student Activity Related"="#663300")

ggplot(b, aes(x=reorder(v_name, v_importance), color=v_type, y=v_importance,)) + 
  geom_point() +
  geom_segment(aes(x=v_name,xend=v_name,y=0,yend=v_importance), size = 1) +
  scale_fill_discrete(name = "Variable Group") +
  labs(color='Variable Group') +
  theme_bw() +
  ylab("Conditional Importance of Features") +
  xlab("Variable Name") +
  coord_flip()

dev.off()


svg(file = "feature-importance-GBM.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)
library(gbm)
imp3 <- varImp(model3, conditional=TRUE,scale = FALSE)
a <- as.tibble(imp3$importance) %>%
  mutate(v_name = row.names(imp3$importance)) %>%
  mutate(v_type = 0) %>%
  mutate(v_type = ifelse(str_detect(v_name,'test'),'Test Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'video'),'Video Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'document'),'Document Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'active'),'Student Activity Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'procrastination'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'inactive'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'event'),'Student Activity Related', v_type)) %>%
  mutate(color = ifelse(v_type == 'Test Related',"#660000", "#663300")) %>%
  mutate(color = ifelse(v_type=='Video Related',"#006633", color)) %>%
  mutate(color = ifelse(v_type=='Document Related',"#003399", color)) %>%
  mutate(color = ifelse(v_type=='Procrastination Related',"#CC9900", color))


names(a)[1] <- 'v_importance'
b <- a %>%
  arrange(desc(v_importance))

b <- b[1:20,]

# c <- b %>%
#   group_by(v_type,color) %>%
#   dplyr::summarise(n=n())
# colors <- c("Test Related"="#660000","Video Related"="#006633","Document Related"="#003399",
#           "Procrastination Related"="#CC9900","Student Activity Related"="#663300")

ggplot(b, aes(x=reorder(v_name, v_importance), color=v_type, y=v_importance,)) + 
  geom_point() +
  geom_segment(aes(x=v_name,xend=v_name,y=0,yend=v_importance), size = 1) +
  scale_fill_discrete(name = "Variable Group") +
  labs(color='Variable Group') +
  theme_bw() +
  ylab("Conditional Importance of Features") +
  xlab("Variable Name") +
  coord_flip()

dev.off()










svg(file = "feature-importance-PCR.svg", height=8.27, width=11.69)
par(mar=c(5,3,2,2)+0.1)

imp5 <- varImp(model5, conditional=TRUE,scale = FALSE)
a <- as.tibble(imp5$importance) %>%
  mutate(v_name = row.names(imp5$importance)) %>%
  mutate(v_type = 0) %>%
  mutate(v_type = ifelse(str_detect(v_name,'test'),'Test Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'video'),'Video Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'document'),'Document Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'active'),'Student Activity Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'procrastination'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'inactive'),'Procrastination Related', v_type)) %>%
  mutate(v_type = ifelse(str_detect(v_name,'event'),'Student Activity Related', v_type)) %>%
  mutate(color = ifelse(v_type == 'Test Related',"#660000", "#663300")) %>%
  mutate(color = ifelse(v_type=='Video Related',"#006633", color)) %>%
  mutate(color = ifelse(v_type=='Document Related',"#003399", color)) %>%
  mutate(color = ifelse(v_type=='Procrastination Related',"#CC9900", color))


names(a)[1] <- 'v_importance'
b <- a %>%
  arrange(desc(v_importance))

b <- b[1:20,]

# c <- b %>%
#   group_by(v_type,color) %>%
#   dplyr::summarise(n=n())
# colors <- c("Test Related"="#660000","Video Related"="#006633","Document Related"="#003399",
#           "Procrastination Related"="#CC9900","Student Activity Related"="#663300")

ggplot(b, aes(x=reorder(v_name, v_importance), y=v_importance,)) + 
  geom_point() +
  geom_segment(aes(x=v_name,xend=v_name,y=0,yend=v_importance), size = 1) +
  scale_fill_discrete(name = "Variable Group") +
  labs(color='Variable Group') +
  theme_bw() +
  ylab("Conditional Importance of Features") +
  xlab("Variable Name") +
  coord_flip()

dev.off()




detach("package:gbm", unload=TRUE)
detach("package:ranger", unload=TRUE)

loadings <- eigen(cov(train_course1[,2:ncol(train_course1)]))$vectors
explvar <- as.data.frame(loadings^2)



# names(a)[1] <- 'v_importance'
# b <- a %>%
#   arrange(desc(v_importance))
# 
# b <- b[1:20,]
# 
# # c <- b %>%
# #   group_by(v_type,color) %>%
# #   dplyr::summarise(n=n())
# # colors <- c("Test Related"="#660000","Video Related"="#006633","Document Related"="#003399",
# #           "Procrastination Related"="#CC9900","Student Activity Related"="#663300")
# 
# ggplot(b, aes(x=reorder(v_name, v_importance), color=v_type, y=v_importance,)) + 
#   geom_point() +
#   geom_segment(aes(x=v_name,xend=v_name,y=0,yend=v_importance), size = 1) +
#   scale_fill_discrete(name = "Variable Group") +
#   labs(color='Variable Group') +
#   theme_bw() +
#   ylab("Conditional Importance of Features") +
#   xlab("Variable Name") +
#   coord_flip()
# 
# dev.off()
# #library(ggthemes) # Load
