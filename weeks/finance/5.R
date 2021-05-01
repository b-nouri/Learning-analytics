


####-------predict by each week-------#####
attempt_test_selected <- attempt_student_selected %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week)))) %>%
  mutate(course_week = (academic_week -19))

attempt_test_selected1 <- attempt_test_selected[attempt_test_selected$course_week<13,]

rm(train_course1)
rm(test_course1)
##---number of exam attempts----###
nn <- attempts %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                          "possible_score","content_type")],by="content_pk")

attempt_student <- attempt_student %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

attempt_student_selected <- attempts[(attempts$content_id %in% content_df[(content_df$course_pk==888132),"content_pk"]),]
###----week5-----#####
attempt_test_week5 <- attempt_test_selected[attempt_test_selected$course_week<6,]
nn <- attempt_test_week5 %>%
  group_by(content_id,user_pk) %>%
  filter(user_pk %in% sap$user_pk) %>%
  dplyr::summarize(number_tries = n(), 
                   lowset = min(as.numeric(score)),highest = max(as.numeric(score)),
                   .groups = 'keep')
names(nn)[1] <- "content_pk"
attempt_student_week_5 <- merge(nn,content_df[,c("content_pk","course_pk","title",
                                                 "possible_score","content_type")],by="content_pk")

attempt_student_week_5 <- attempt_student_week_5 %>%
  mutate(test_percentage = (highest/possible_score*100))
rm(nn)

course1 <- sap1 %>%
  select(course_name,user_pk,final_score)

c <- attempt_student_week_5 %>%
  distinct(content_pk)

b <- attempt_student_week_5 %>%
  group_by(user_pk) %>%
  spread(content_pk,highest) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test', 1:length(c$content_pk)))
course1_5 <- merge(course1,b,by="user_pk",all.x = TRUE)


sum(is.na(course1_5))
vis_miss(course1_5)
course1_5 <- course1_5 %>%
  mutate(n_tests_taken =  rowSums(. != 0)-3) 



course1_5 <- course1_5 %>%
  mutate(n_tests_taken = ifelse(is.na(course1_5$n_tests_taken),0,course1_5$n_tests_taken))

course1_5$test1 <- replace(course1_5$test1, is.na(course1_5$test1), 0)
course1_5$test2 <- replace(course1_5$test2, is.na(course1_5$test2), 0)
course1_5$test3 <- replace(course1_5$test3, is.na(course1_5$test3), 0)
course1_5$test4 <- replace(course1_5$test4, is.na(course1_5$test4), 0)

course1_5 <- course1_5 %>%
  mutate(sum_tests_taken =  select(.,test1:test4) %>% rowSums(.))


vis_miss(course1_5)

#-----test percentage----#######
b <- attempt_student_week_5 %>%
  group_by(user_pk) %>%
  spread(content_pk,test_percentage) %>%
  group_by(user_pk) %>%
  summarise_at(vars(c$content_pk), sum, na.rm = TRUE)


b <- b %>% rename_at(vars(c$content_pk), ~ paste0('test_percentage', 1:length(c$content_pk)))
course1_5 <- merge(course1_5,b,by="user_pk",all.x = TRUE)

sum(is.na(course1_5))
vis_miss(course1_5)

course1_5$test_percentage1 <- replace(course1_5$test_percentage1, is.na(course1_5$test_percentage1),0)
course1_5$test_percentage2 <- replace(course1_5$test_percentage2, is.na(course1_5$test_percentage2), 0)
course1_5$test_percentage3 <- replace(course1_5$test_percentage3, is.na(course1_5$test_percentage3), 0)
course1_5$test_percentage4 <- replace(course1_5$test_percentage4, is.na(course1_5$test_percentage4), 0)

vis_miss(course1_5)

course1_5 <- course1_5 %>%
  mutate(average_tests_taken =  select(.,test_percentage1:test_percentage4) %>% rowMeans(.))


###------week5 dataset#-----
weeks5 <- course[,65:69]
sum_weeks5 <- course[,82:85]
days_week5 <- course[,100:104]
video_week5 <- course[,117:118] %>%
  select(n_times_opened_video1)
course1_5 <- course1_5 %>%
  cbind(weeks5,sum_weeks5,video_week5)


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

scores <- train1[,26]

train_course1 <- train1[,5:25]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:25]
table(train_course1$score_class)

test5 <- test1
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


gbm.tune5 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid1,
                   verbose=FALSE,
                   weights = model_weights)



res <- gbm.tune5$results
res

gbm.pred5 <- predict(gbm.tune5,test_course1)

confusionMatrix(gbm.pred5,test5$score_class)   

week5.prob <- predict(gbm.tune5,test_course1,type="prob")

roc.week5 <- roc(predictor=week5.prob$fail,
                 response=test5$score_class,
                 levels=rev(levels(test5$score_class)))
roc.week5$auc
plot(roc.week5,main="GBM ROC")

plot(roc.week5,main="ROC curves in diffrent weeks: bank en finacien")


roc(predictor=week5.prob$fail,
    response=test5$score_class,
    levels=rev(levels(test5$score_class)),
    plot=TRUE,print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)

