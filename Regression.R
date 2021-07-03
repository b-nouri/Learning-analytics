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
