# 00 Setting Environment --------------------------------------------------
library(h2o)
library(ggplot2)

#Initiating H2o connection
h2o.init()

#Importing Data into R
#Dataset Description : It's a German Credit Data consisting of 21 variables and 1000 records. The dependent or target variable is Creditability which explains whether a loan should be granted to a customer based on his/her profiles.
train_data <- read.csv("https://sites.google.com/site/pocketecoworld/german_credit.csv")

#COnverting dependent variable to factor format
train_data$Creditability = as.factor(train_data$Creditability)

#Extracting names of predictor and response variable
independent_var <- names(train_data)[2:21]
dependent_var <- names(train_data)[1]

# 01 Baseline Model -------------------------------------------------------
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
  nfolds = 0)

#Model accuracy
h2o.auc(baseline_model)

# 02 Tuning Mtry ----------------------------------------------------------
#Influence of mtry on model performance

temp <- {}
ROC <- {}
for ( i in c(3:5)){
  print(i)
  
  temp1 <- {}
  temp2 <- {}
  model <- h2o.randomForest(
    x = independent_var, #vector containing predictor variables 
    y = dependent_var, #vector contaning response variable
    training_frame = as.h2o(train_data),#training data frame
    mtries = i, #default value = sqrt(p), p is number of predictors
    sample_rate = 0.6320000291, #Row sample rate per tree
    min_rows = 1, #Fewest allowed (weighted) observations in a leaf
    max_depth = 20, #Maximum tree depth
    ntrees = 50, #Number of trees
    seed = 10,
    verbose = F)
  
  temp1$mtry <- i
  temp1$AUC <-h2o.auc(model)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  temp2$fpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Mtry",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
  temp2 <- as.data.frame(temp2)
  ROC <- rbind(ROC,temp2)
}
temp
ROC

#ROC Curve
ggplot(ROC,aes(fpr,tpr,col=model))+
  geom_line(size=1.3)+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Different Mtries')


# 03 Tuning Sample Size ---------------------------------------------------
#Influence of sample size on model performance
#Impact of decreasing/increasing sample size
temp <- {}
ROC <- {}
for ( i in c(0.50,0.55,0.6,0.632,0.65)){
  print(i)
  temp1 <- {}
  temp2 <- {}
  model <- h2o.randomForest(
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
  
  temp1$sample_size <- i
  temp1$AUC <-round(h2o.auc(model),3)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  temp2$fpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Mtry",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
  temp2 <- as.data.frame(temp2)
  ROC <- rbind(ROC,temp2)
}
temp
ROC

#ROC curve
ggplot(ROC,aes(fpr,tpr,col=model))+
  geom_line(size=1.3)+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Different sample size')

#AUC curve
ggplot(data=temp, aes(x=sample_size, y=AUC)) +
  geom_line(linetype = "dashed",color="#008080", size = 1)+
  geom_point(color="#008080",size = 3)+
  theme_minimal()+
  geom_text(aes(label=AUC),hjust=0.5, vjust=-0.4 ,color="#008080" )+
  xlab("Sample Size") +
  ggtitle("AUC vs Sample Size")+
  scale_x_continuous(limits=c(0.5,0.66), breaks=seq(0.5,0.66, by = 0.03))


# 04 Tuning Tree Depth ----------------------------------------------------
temp <- {}
ROC <- {}
for ( i in c(5,10,20)){
  for ( j in c(1,5,10)){
    
  
  print(i)
  print(j)
  
  temp1 <- {}
  temp2 <- {}
  model <- h2o.randomForest(
    x = independent_var, #vector containing predictor variables 
    y = dependent_var, #vector contaning response variable
    training_frame = as.h2o(train_data),#training data frame
    mtries = 4, #default value = sqrt(p), p is number of predictors
    sample_rate = 0.6320000291, #Row sample rate per tree
    min_rows = j, #Fewest allowed (weighted) observations in a leaf
    max_depth = i, #Maximum tree depth
    ntrees = 50, #Number of trees
    seed = 10,
    verbose = F
  )
  
  temp1$max_depth <- i
  temp1$min_leaf <- j
  temp1$AUC <-round(h2o.auc(model),3)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  temp2$fpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Mtry",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
  temp2 <- as.data.frame(temp2)
  ROC <- rbind(ROC,temp2)
}}
temp
ROC

temp$parameter <- paste0("max depth: ",temp$max_depth," node size: ", temp$min_leaf)
temp$parameter[7] <- paste0(temp$parameter[7], "(Baseline Model)")

#ROC curver
ggplot(ROC,aes(fpr,tpr,col=model))+
  geom_line(size=1.3)+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Different Tree Depth')

#AUC
ggplot(temp, aes(x=parameter, y=AUC)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=parameter, 
                   xend=parameter, 
                   y=min(AUC), 
                   yend=max(AUC)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  geom_text(aes(label=AUC),hjust=0.5, vjust=-0.4 ,color="gray12" )+
  labs(title="AUC vs Tree Depth") +  
  theme(axis.text.y = element_text(color="gray12", size=10))+
  coord_flip()

# 06 Tuning Ntrees --------------------------------------------------------
temp <- {}
ROC <- {}
for ( i in c(50,100,500,1000)){
  print(i)
  
  temp1 <- {}
  temp2 <- {}
  model <- h2o.randomForest(
    x = independent_var, #vector containing predictor variables 
    y = dependent_var, #vector contaning response variable
    training_frame = as.h2o(train_data),#training data frame
    mtries = 4, #default value = sqrt(p), p is number of predictors
    sample_rate = 0.632, #Row sample rate per tree
    min_rows = 1, #Fewest allowed (weighted) observations in a leaf
    max_depth = 20, #Maximum tree depth
    ntrees = i, #Number of trees
    seed = 10,
    verbose = F
  )
  
  temp1$ntrees <- i
  temp1$AUC <-round(h2o.auc(model),3)
  temp1 <- as.data.frame(temp1)
  temp <- rbind(temp,temp1)
  
  temp2$fpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
  temp2$tpr <- model@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
  temp2$model <- rep(paste("Ntrees",i,"(AUC =",round(temp1$AUC,3),")",sep = " "),length(temp2$fpr))
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
  ggtitle('ROC Curve for Different values of Ntrees')


#Shutting down H2o connection
h2o.shutdown()