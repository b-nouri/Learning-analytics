####-------predict by each week-------#####
event_selected_course <- event_df[event_df$course_pk == 888954,]
event_selected_course <- event_selected_course %>%
  mutate(course_week = (academic_week -20))

attempt_test_selected <- attempt_student_selected %>% mutate(time = dmy_hms(submit_date)) %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day= day(time)) %>%
  mutate(week = week(time)) %>%
  mutate(academic_week = ifelse(week > 38 & week < 53,(week-38),ifelse(week == 53,14,(52-38 + week)))) %>%
  mutate(course_week = (academic_week -20))

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

attempt_student_selected <- attempts[(attempts$content_id %in% content_df[(content_df$course_pk==888954),"content_pk"]),]
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



video_event_selected <- event_selected_course[event_selected_course$content_type == "resource/x-osv-kaltura/mashup",]
video_agg <- video_event_selected %>%
  group_by(course_week,content_pk) %>%
  dplyr::summarise(n = n()) %>%
  group_by(course_week) %>%
  dplyr::summarise(videos_each_week =n())


###------week5 dataset#-----
weeks5 <- course[,47:51]
sum_weeks5 <- course[,65:68]
days_week5 <- course[,82:86]

video_week5 <- course[,100:115] #16 videos
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

scores <- train1[,46]

train_course1 <- train1[,5:45]
train_course1 <- bind_cols(train_course1, scores)

id_test1 <- test1[,1:2]
test_course1 <- test1[,5:45]
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

gbm_tune5 <- train(score_class ~ ., data = train_course1,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid=grid,
                   verbose=FALSE,
                   preProcess = c("scale", "center","medianImpute","BoxCox"),
                   weights = model_weights)

# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm_tune5$bestTune
plot(gbm_tune5) 

grid1 = gbm_tune5$bestTune
getDoParWorkers()

ctrl <- trainControl(method = "LOOCV",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)


gbm_fit5 <- train(score_class ~ ., data = as.data.frame(train_course1),
                     method="glmnet",
                     metric="ROC",
                     trControl=glm_trcontrol_1,
                     preProcess = c("scale", "center","medianImpute","BoxCox"),
                     weights=model_weights)


res <- gbm_fit5$results
res


pred5 <- predict(gbm_fit5,test_course1)


confusionMatrix(pred5,test5$score_class)   

week5_prob <- predict(gbm_fit5,test_course1,type="prob")

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

