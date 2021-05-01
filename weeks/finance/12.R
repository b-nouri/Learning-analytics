
###----week12-----#####
attempt_test_week12 <- attempt_test_selected[attempt_test_selected$course_week<13,]
nn <- attempt_test_week12 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_12 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                  "possible_score","content_type")],by="content_pk")

attempt_student_week_12 <- attempt_student_week_12 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_12 %>%
  distinct(content_pk)

b <- attempt_student_week_12 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_12 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_12))
vis_miss(course1_12)
course1_12 <- course1_12 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_12 <- course1_12 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_12$n_tests_taken),0,course1_12$n_tests_taken))

course1_12$test1 <- replace(course1_12$test1, is.na(course1_12$test1), 0)
course1_12$test2 <- replace(course1_12$test2, is.na(course1_12$test2), 0)
course1_12$test3 <- replace(course1_12$test3, is.na(course1_12$test3), 0)
course1_12$test4 <- replace(course1_12$test4, is.na(course1_12$test4), 0)
course1_12$test5 <- replace(course1_12$test5, is.na(course1_12$test5), 0)
course1_12$test6 <- replace(course1_12$test6, is.na(course1_12$test6), 0)
course1_12$test7 <- replace(course1_12$test7, is.na(course1_12$test7), 0)
course1_12$test8 <- replace(course1_12$test8, is.na(course1_12$test8), 0)
course1_12$test9 <- replace(course1_12$test9, is.na(course1_12$test9), 0)
course1_12$test10 <- replace(course1_12$test10, is.na(course1_12$test10), 0)
course1_12$test11 <- replace(course1_12$test11, is.na(course1_12$test11), 0)
course1_12$test12 <- replace(course1_12$test12, is.na(course1_12$test12), 0)
course1_12$test13 <- replace(course1_12$test13, is.na(course1_12$test13), 0)
course1_12$test14 <- replace(course1_12$test14, is.na(course1_12$test14), 0)

course1_12 <- course1_12 %>%
  mutate(sum_tests_taken =  select(.,test1:test14) %>% rowSums(.))


vis_miss(course1_12)

#-----test percentage----#######
b <- attempt_student_week_12 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_12 <- merge(course1_12,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_12))
vis_miss(course1_12)

course1_12$test_percentage1 <- replace(course1_12$test_percentage1, is.na(course1_12$test_percentage1),0)
course1_12$test_percentage2 <- replace(course1_12$test_percentage2, is.na(course1_12$test_percentage2), 0)
course1_12$test_percentage3 <- replace(course1_12$test_percentage3, is.na(course1_12$test_percentage3), 0)
course1_12$test_percentage4 <- replace(course1_12$test_percentage4, is.na(course1_12$test_percentage4), 0)
course1_12$test_percentage5 <- replace(course1_12$test_percentage5, is.na(course1_12$test_percentage5), 0)
course1_12$test_percentage6 <- replace(course1_12$test_percentage6, is.na(course1_12$test_percentage6), 0)
course1_12$test_percentage7 <- replace(course1_12$test_percentage7, is.na(course1_12$test_percentage7), 0)
course1_12$test_percentage8 <- replace(course1_12$test_percentage8, is.na(course1_12$test_percentage8), 0)
course1_12$test_percentage9 <- replace(course1_12$test_percentage9, is.na(course1_12$test_percentage9), 0)
course1_12$test_percentage10 <- replace(course1_12$test_percentage10, is.na(course1_12$test_percentage10), 0)
course1_12$test_percentage11 <- replace(course1_12$test_percentage11, is.na(course1_12$test_percentage11), 0)
course1_12$test_percentage12 <- replace(course1_12$test_percentage12, is.na(course1_12$test_percentage12), 0)
course1_12$test_percentage13 <- replace(course1_12$test_percentage13, is.na(course1_12$test_percentage13), 0)
course1_12$test_percentage14 <- replace(course1_12$test_percentage14, is.na(course1_12$test_percentage14), 0)



vis_miss(course1_12)

course1_12 <- course1_12 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage14) %>% rowMeans(.))

videos_agg <- event_selected_course %>%
  filter(content_type == "resource/x-osv-kaltura/mashup") %>%
  group_by(content_pk,academic_week) %>%
  dplyr::summarise(n=n())
###------week12 dataset#-----
weeks12 <- course[,65:76]
sum_weeks12 <- course[,83:92]
days_week12 <- course[,100:112]
video_week12 <- course[,118:128]
course1_12 <- course1_12 %>%
  cbind(weeks12,sum_weeks12,video_week12)


##------Training and predicting over week12----####
course1_12 <- tibble::rowid_to_column(course1_12, "row")
set.seed(123)

course1_12 <- course1_12 %>%
  mutate(score_class = cut(as.numeric(course1_12$final_score), breaks=c(0,9.9,21), labels=c( 'fail', 'pass')))


course1_12 <- course1_12 %>%
  mutate(score_class = as.factor(course1_12$score_class))

#c1 <- course1_9[course1_9$cluster==2,]
c1 <- course1_12[,]

library(splitstackshape)
train1 <- stratified(c1, c("score_class"), 0.8)

sid<-train1$row
test1 <- c1[!(c1$row %in% sid),]

scores <- train1[,69]

train_course1 <- train1[,5:68]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:68]
table(train_course1$score_class)
test12 <- test1
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
plot(gbm.tune1) 

grid1 = gbm.tune1$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm.tune12 <- train(score_class ~ ., data = train_course1,
                    method = "gbm",
                    metric = "ROC",
                    trControl = ctrl,
                    tuneGrid=grid1,
                    verbose=FALSE,
                    weights = model_weights)



res <- gbm.tune12$results
res

gbm.pred12 <- predict(gbm.tune12,test_course1)

confusionMatrix(gbm.pred12,test12$score_class)   

week12.prob <- predict(gbm.tune12,test_course1,type="prob")

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





