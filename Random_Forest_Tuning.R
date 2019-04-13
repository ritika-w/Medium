install.packages("ggthemes")

library(h2o)
library(ggplot2)
library(ggthemes)

#Initiating H2o connection
h2o.init()

#Importing Data into R
#Dataset Description : It's a German Credit Data consisting of 21 variables and 1000 records. The dependent or target variable is Creditability which explains whether a loan should be granted to a customer based on his/her profiles.
train_data <- read.csv("https://sites.google.com/site/pocketecoworld/german_credit.csv")

#COnverting dependent variable to factor format
train_data$Creditability = as.factor(train_data$Creditability)

## 75% of the sample size
#smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#train_data <- data[train_ind, ]
#test_test <- data[-train_ind, ]

#summary(train_data)
#names(train_data)

#Extracting names of predictor and response variable
independent_var <- names(train_data)[2:21]
dependent_var <- names(train_data)[1]

#Baseline Model with default parameter
baseline_model <- h2o.randomForest(
  x = independent_var, #vector containing predictor variables 
  y = dependent_var, #vector contaning response variable
  training_frame = as.h2o(train_data),#training data frame
  mtries = 4, #default value = sqrt(p), p is number of predictors
  sample_rate = 0.6320000291, #Row sample rate per tree
  min_rows = 1, #Fewest allowed (weighted) observations in a leaf
  max_depth = 20, #Maximum tree depth
  ntrees = 50, #Number of trees
  seed = 10,
  verbose = F,
  nfolds = 0
)

h2o.auc(baseline_model)
# str(baseline_model)
# t <- h2o.performance(model = baseline_model, newdata = as.h2o(test_test))
# fpr <- t@metrics$thresholds_and_metric_scores$fpr
# 
# fpr <- baseline_model@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
# tpr <- baseline_model@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
# 
# fpr_val = baseline_model@model$validation_metrics@metrics$thresholds_and_metric_scores$fpr
#AUC:  0.7719738 - baseline model
#perf <- h2o.performance(baseline_model, as.h2o(train_data))
#h2o.accuracy(perf)

#pred <- h2o.predict(object = model_mtry,newdata =  as.h2o(train_data))
#perf <- h2o.performance(model_mtry,train = T)


#Influence of mtry on model performance
#Impact of decreasing/increasing mtry
temp <- {}
ROC <- {}
for ( i in c(3:5)){
  print(i)
  
  temp1 <- {}
  temp2 <- {}
  model_mtry <- h2o.randomForest(
    x = independent_var, #vector containing predictor variables 
    y = dependent_var, #vector contaning response variable
    training_frame = as.h2o(train_data),#training data frame
    mtries = i, #default value = sqrt(p), p is number of predictors
    sample_rate = 0.6320000291, #Row sample rate per tree
    min_rows = 1, #Fewest allowed (weighted) observations in a leaf
    max_depth = 20, #Maximum tree depth
    ntrees = 50, #Number of trees
    seed = 10,
    verbose = F
  )
  #t <- h2o.performance(model = model_mtry, newdata = as.h2o(test_test))
  
  temp1$mtry <- i
  #temp1$AUC <-t@metrics$AUC
  temp1$AUC <-h2o.auc(model_mtry)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  
  #temp2$fpr <- t@metrics$thresholds_and_metric_scores$fpr
  #temp2$tpr <- t@metrics$thresholds_and_metric_scores$tpr
  temp2$fpr <- model_mtry@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model_mtry@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Mtry",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
  temp2 <- as.data.frame(temp2)
  ROC <- rbind(ROC,temp2)
  
  
}

temp
ROC

ggplot(ROC,aes(fpr,tpr,col=model))+
  geom_line(size=1.3)+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Different Mtries')


#Influence of sample size on model performance
#Impact of decreasing/increasing sample size
temp <- {}
ROC <- {}
for ( i in c(0.5,0.58,0.6,0.632,0.65)){
  print(i)
  
  temp1 <- {}
  temp2 <- {}
  model_mtry <- h2o.randomForest(
    x = independent_var, #vector containing predictor variables 
    y = dependent_var, #vector contaning response variable
    training_frame = as.h2o(train_data),#training data frame
    mtries = 4, #default value = sqrt(p), p is number of predictors
    sample_rate = i, #Row sample rate per tree
    min_rows = 1, #Fewest allowed (weighted) observations in a leaf
    max_depth = 20, #Maximum tree depth
    ntrees = 50, #Number of trees
    seed = 10,
    verbose = F
  )
  #t <- h2o.performance(model = model_mtry, newdata = as.h2o(test_test))
  
  temp1$mtry <- i
  #temp1$AUC <-t@metrics$AUC
  temp1$AUC <-round(h2o.auc(model_mtry),3)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  
  #temp2$fpr <- t@metrics$thresholds_and_metric_scores$fpr
  #temp2$tpr <- t@metrics$thresholds_and_metric_scores$tpr
  temp2$fpr <- model_mtry@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model_mtry@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Mtry",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
  temp2 <- as.data.frame(temp2)
  ROC <- rbind(ROC,temp2)
  
  
}

temp
ROC

#ROC curver
ggplot(ROC,aes(fpr,tpr,col=model))+
  geom_line(size=1.3)+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Different Mtries')

#AUC
ggplot(data=temp, aes(x=mtry, y=AUC)) +
  geom_line(linetype = "dashed",color="#008080", size = 1)+
  geom_point(color="#008080",size = 3)+
  theme_minimal()+
  geom_text(aes(label=AUC),hjust=0.5, vjust=-0.4 ,color="#008080" )+
  xlab("Sample Size") +
  ggtitle("AUC vs Sample Size")+
  scale_x_continuous(limits=c(0.5,0.66), breaks=seq(0.5,0.66, by = 0.03))



h2o.shutdown()

