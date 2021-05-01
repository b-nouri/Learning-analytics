course1_9 <- sap1 %>%
  select(course_name,user_pk,final_score)
###------week9 dataset#-----
weeks9 <- course[,5:13]
sum_weeks9 <- course[,24:31]
days_week9 <- course[,41:49]
video_week9 <- course[,60:73]
course1_9 <- course1_9 %>%
  cbind(weeks9,sum_weeks9,days_week9,video_week9)


##------Training and predicting over week7----####
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
test9 <- test1
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

glmnet_fit9 <- train(score_class ~ ., data = as.data.frame(train_course1),
                     method="glmnet",
                     metric="ROC",
                     trControl=glm_trcontrol_1,
                     weights=model_weights)


res <- glmnet_fit9$results
res


pred9 <- predict(glmnet_fit9,test_course1)


confusionMatrix(pred9,test9$score_class)   

week9_prob <- predict(glmnet_fit9,test_course1,type="prob")


roc_week9 <- roc(predictor=week9_prob$fail,
                 response=test9$score_class,
                 levels=rev(levels(test9$score_class)))
roc_week9$auc
plot(roc_week9,main="GBM ROC")


plot(roc_week9,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week9_prob$fail,
    response=test9$score_class,
    levels=rev(levels(test9$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)


plot(roc_week9,main="ROC curves in diffrent weeks: Marketing")

roc(predictor=week9_prob$fail,
    response=test9$score_class,
    levels=rev(levels(test9$score_class)),
    plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)
