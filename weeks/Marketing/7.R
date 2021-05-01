
course1_7 <- sap1 %>%
  select(course_name,user_pk,final_score)
###------week7 dataset#-----
weeks7 <- course[,5:11]
sum_weeks7 <- course[,24:29]
days_week7 <- course[,41:47]
video_week7 <- course[,60:72]
course1_7 <- course1_7 %>%
  cbind(weeks7,sum_weeks7,days_week7,video_week7)



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

scores <- train1[,38]

train_course1 <- train1[,5:37]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:37]
table(train_course1$score_class)
test7 <- test1
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

glmnet_fit7 <- train(score_class ~ ., data = as.data.frame(train_course1),
                     method="glmnet",
                     metric="ROC",
                     trControl=glm_trcontrol_1,
                     weights=model_weights)


res <- glmnet_fit7$results
res


pred7 <- predict(glmnet_fit7,test_course1)


confusionMatrix(pred7,test7$score_class)   

week7_prob <- predict(glmnet_fit7,test_course1,type="prob")

roc_week7 <- roc(predictor=week7_prob$fail,
                 response=test7$score_class,
                 levels=rev(levels(test7$score_class)))
roc_week7$auc
plot(roc_week7,main="GBM ROC")

plot(roc_week7,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week7_prob$fail,
    response=test7$score_class,
    levels=rev(levels(test7$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.37,legacy.axes=TRUE,add = TRUE)

