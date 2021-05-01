###----week3-----#####
course1_3 <- sap1 %>%
  select(course_name,user_pk,final_score)

###------week3 dataset#-----
weeks3 <- course[,65:67]
sum_weeks3 <- course[,83:84]
days_week3 <- course[,100:102]
course1_3 <- course1_3 %>%
  cbind(weeks3,sum_weeks3)


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

scores <- train1[,10]

train_course1 <- train1[,5:9]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:9]
table(train_course1$score_class)
test3 <- test1
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

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


tune3 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid1,
                   verbose=FALSE,
                   weights = model_weights)



res3 <- tune3$results
res3

pred3 <- predict(tune3,test_course1)

confusionMatrix(pred3,test3$score_class)   

week3_prob <- predict(tune3,test_course1,type="prob")

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


plot(roc_week3,main="ROC curves in diffrent weeks: bank en finacien")

roc(predictor=week3_prob$fail,
    response=test3$score_class,
    levels=rev(levels(test3$score_class)),
    plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)
