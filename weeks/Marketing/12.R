course1_12 <- sap1 %>%
  select(course_name,user_pk,final_score)
###------week12 dataset#-----
weeks12 <- course[,5:16]
sum_weeks12 <- course[,24:34]
days_week12 <- course[,41:52]
video_week12 <- course[,60:75]
course1_12 <- course1_12 %>%
  cbind(weeks12,sum_weeks12,days_week12,video_week12)


##------Training and predicting over week12----####
course1_12 <- tibble::rowid_to_column(course1_12, "row")
set.seed(123)

course1_12 <- course1_12 %>%
  mutate(score_class = cut(as.numeric(course1_12$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_12 <- course1_12 %>%
  mutate(score_class = as.factor(course1_12$score_class))

#c1 <- course1_12[course1_12$cluster==2,]
c1 <- course1_12[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,56]

train_course1 <- train1[,5:55]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:55]
table(train_course1$score_class)
test12 <- test1
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

glmnet_fit12 <- train(score_class ~ ., data = as.data.frame(train_course1),
                     method="glmnet",
                     metric="ROC",
                     trControl=glm_trcontrol_1,
                     weights=model_weights)


res <- glmnet_fit12$results
res


pred12 <- predict(glmnet_fit12,test_course1)


confusionMatrix(pred12,test12$score_class)   

week12_prob <- predict(glmnet_fit12,test_course1,type="prob")


roc.week12 <- roc(predictor=week12.prob$fail,
                  response=test12$score_class,
                  levels=rev(levels(test12$score_class)))
roc.week12$auc
plot(roc.week12,main="GBM ROC")

plot(roc.week12,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week12.prob$fail,
    response=test12$score_class,
    levels=rev(levels(test12$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)





