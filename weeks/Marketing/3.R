
####-------predict by each week-------#####
attempt_test_selected <- attempt_student_selected %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week)))) %>%
  mutate(course_week = (academic_week -19))

attempt_test_selected1 <- attempt_test_selected[attempt_test_selected$course_week<13,]

###----week3-----#####
course1_3 <- sap1 %>%
  select(course_name,user_pk,final_score)

###------week3 dataset#-----
weeks3 <- course[,5:7]
sum_weeks3 <- course[,24:25]
days_week3 <- course[,41:43]
course1_3 <- course1_3 %>%
  cbind(weeks3,sum_weeks3,days_week3)


##------Training and predicting over week12----####
course1_3 <- tibble::rowid_to_column(course1_3, "row")
set.seed(123)

course1_3 <- course1_3 %>%
  mutate(score_class = cut(as.numeric(course1_3$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_3 <- course1_3 %>%
  mutate(score_class = as.factor(course1_3$score_class))

#c1 <- course1_9[course1_9$cluster==2,]
c1 <- course1_3[,]

library(splitstackshape)
set.seed(123)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,13]

train_course1 <- train1[,5:12]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:12]
table(train_course1$score_class)
test3 <- test1
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

glmnet.fit3 <- train(score_class ~ ., data = as.data.frame(train_course1),
                    method="glmnet",
                    metric="ROC",
                    trControl=glm_trcontrol_1,
                    weights=model_weights)


res <- glmnet.fit3$results
res


pred3 <- predict(glmnet.fit3,test_course1)

confusionMatrix(pred3,test3$score_class)   

week3_prob <- predict(glmnet.fit3,test_course1,type="prob")

roc_week3 <- roc(predictor=week3_prob$fail,
                 response=test3$score_class,
                 levels=rev(levels(test3$score_class)))
roc_week3$auc
plot(roc_week3,main="GBM ROC")


plot(roc_week3,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week3_prob$fail,
    response=test3$score_class,
    levels=rev(levels(test3$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)


plot(roc_week3,main="ROC curves in diffrent weeks: Marketing")

roc(predictor=week3_prob$fail,
    response=test3$score_class,
    levels=rev(levels(test3$score_class)),
    plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)
