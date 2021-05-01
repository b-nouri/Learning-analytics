event_selected_course <- event_df[event_df$course_pk == 888131,]
video_event_selected <- event_selected_course[event_selected_course$content_type == "resource/x-osv-kaltura/mashup",]
video_agg <- video_event_selected %>%
  group_by(academic_week,content_pk,title) %>%
  dplyr::summarise(n = n()) %>%
  mutate(course_week = academic_week - 20)



course1_5 <- sap1 %>%
  select(course_name,user_pk,final_score)
###------week5 dataset#-----
weeks5 <- course[,5:9]
sum_weeks5 <- course[,24:27]
days_week5 <- course[,41:45]
video_week5 <- course[,60:62]
course1_5 <- course1_5 %>%
  cbind(weeks5,sum_weeks5,days_week5,video_week5)

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
set.seed(123)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,22]

train_course1 <- train1[,5:21]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:21]
table(train_course1$score_class)

test5 <- test1
###-------------Classification------------###
train.control <- trainControl(method="LOOCV",classProbs = TRUE)

model_weights <- ifelse(train_course1$score_class == "fail",
                        (1/table(train_course1$score_class)[1]) * 0.5,
                        (1/table(train_course1$score_class)[2]) * 0.5)

####---------glm---------####

glm_trcontrol_1 = trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

glmnet_fit5 <- train(score_class ~ ., data = as.data.frame(train_course1),
                     method="glmnet",
                     metric="ROC",
                     trControl=glm_trcontrol_1,
                     weights=model_weights)


res <- glmnet_fit5$results
res


pred5 <- predict(glmnet_fit5,test_course1)


confusionMatrix(pred5,test5$score_class)   

week5_prob <- predict(glmnet_fit5,test_course1,type="prob")

roc_week5 <- roc(predictor=week5_prob$fail,
                 response=test5$score_class,
                 levels=rev(levels(test5$score_class)))
roc_week5$auc
plot(roc_week5,main="GBM ROC")

plot(roc_week5,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week5_prob$fail,
    response=test5$score_class,
    levels=rev(levels(test5$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

