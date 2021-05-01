###----week9-----#####
attempt_test_week9 <- attempt_test_selected[attempt_test_selected$course_week<10,]
nn <- attempt_test_week9 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_9 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                 "possible_score","content_type")],by="content_pk")

attempt_student_week_9 <- attempt_student_week_9 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_9 %>%
  distinct(content_pk)

b <- attempt_student_week_9 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_9 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_9))
vis_miss(course1_9)
course1_9 <- course1_9 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_9 <- course1_9 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_9$n_tests_taken),0,course1_9$n_tests_taken))

course1_9$test1 <- replace(course1_9$test1, is.na(course1_9$test1), 0)
course1_9$test2 <- replace(course1_9$test2, is.na(course1_9$test2), 0)
course1_9$test3 <- replace(course1_9$test3, is.na(course1_9$test3), 0)
course1_9$test4 <- replace(course1_9$test4, is.na(course1_9$test4), 0)
course1_9$test5 <- replace(course1_9$test5, is.na(course1_9$test5), 0)
course1_9$test6 <- replace(course1_9$test6, is.na(course1_9$test6), 0)
course1_9$test7 <- replace(course1_9$test7, is.na(course1_9$test7), 0)
course1_9$test8 <- replace(course1_9$test8, is.na(course1_9$test8), 0)

course1_9 <- course1_9 %>%
  mutate(sum_tests_taken =  select(.,test1:test8) %>% rowSums(.))


vis_miss(course1_9)

#-----test percentage----#######
b <- attempt_student_week_9 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_9 <- merge(course1_9,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_9))
vis_miss(course1_9)

course1_9$test_percentage1 <- replace(course1_9$test_percentage1, is.na(course1_9$test_percentage1),0)
course1_9$test_percentage2 <- replace(course1_9$test_percentage2, is.na(course1_9$test_percentage2), 0)
course1_9$test_percentage3 <- replace(course1_9$test_percentage3, is.na(course1_9$test_percentage3), 0)
course1_9$test_percentage4 <- replace(course1_9$test_percentage4, is.na(course1_9$test_percentage4), 0)
course1_9$test_percentage5 <- replace(course1_9$test_percentage5, is.na(course1_9$test_percentage5), 0)
course1_9$test_percentage6 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage6), 0)
course1_9$test_percentage7 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage7), 0)
course1_9$test_percentage8 <- replace(course1_9$test_percentage6, is.na(course1_9$test_percentage8), 0)

vis_miss(course1_9)

course1_9 <- course1_9 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage8) %>% rowMeans(.))

videos_agg <- event_selected_course %>%
  filter(content_type == "resource/x-osv-kaltura/mashup") %>%
  group_by(content_pk,academic_week) %>%
  dplyr::summarise(n=n())
###------week9 dataset#-----
weeks9 <- course[,65:73]
sum_weeks9 <- course[,83:89]
days_week9 <- course[,100:109]
video_week9 <- course[,118:122]
course1_9 <- course1_9 %>%
  cbind(weeks9,sum_weeks9,video_week9)


##------Training and predicting over week9----####
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


gbm.tune9 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid1,
                   verbose=FALSE,
                   weights = model_weights)



res <- gbm.tune9$results
res

gbm.pred9 <- predict(gbm.tune9,test_course1)

confusionMatrix(gbm.pred9,test9$score_class)   

week9.prob <- predict(gbm.tune9,test_course1,type="prob")

roc.week9 <- roc(predictor=week9.prob$fail,
                 response=test9$score_class,
                 levels=rev(levels(test9$score_class)))
roc.week9$auc
plot(roc.week9,main="GBM ROC")

plot(roc.week9,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week9.prob$fail,
    response=test9$score_class,
    levels=rev(levels(test9$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

