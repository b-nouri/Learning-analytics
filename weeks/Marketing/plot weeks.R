plot(roc_week3,main="ROC curves in different weeks: Marketing")

roc(predictor=week3_prob$fail,
    response=test3$score_class,
    levels=rev(levels(test3$score_class)),
    plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.35,legacy.axes=TRUE,add = TRUE)



roc(predictor=week5_prob$fail,
    response=test5$score_class,
    levels=rev(levels(test5$score_class)),
    plot=TRUE,print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.25,legacy.axes=TRUE,add = TRUE)


roc(predictor=week7_prob$fail,
    response=test7$score_class,
    levels=rev(levels(test7$score_class)),
    plot=TRUE,print.auc=TRUE,col="gray",lwd = 4,print.auc.y=0.20,legacy.axes=TRUE,add = TRUE)



roc(predictor=week9_prob$fail,
    response=test9$score_class,
    levels=rev(levels(test9$score_class)),
    plot=TRUE,print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.30,legacy.axes=TRUE,add = TRUE)



roc(predictor=week12_prob$fail,
    response=test12$score_class,
    levels=rev(levels(test12$score_class)),
    plot=TRUE,print.auc=TRUE,col="purple",lwd = 4,print.auc.y=0.15,legacy.axes=TRUE,add = TRUE)



legend("bottomright",legend=c("week:  3","week:  5","week:  7","week:  9","week:  12"),col=c("blue","green","orange","gray","purple"),lwd = 2)

