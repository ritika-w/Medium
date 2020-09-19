# Setting workspace
# install.packages("h2o")

library(h2o)

## Create an H2O cloud 
h2o.init(
  #nthreads=-1,            ## -1: use all available threads
  max_mem_size = "8G")    ## specify the memory size for the H2O cloud

h2o.removeAll() # Clean slate - just in case the cluster was already running

setwd("C:/Users/cuddly/Downloads")

# data source : https://data.world/data-society/bank-marketing-data


# Load a file from disk
dt  <- h2o.importFile(path = "C:/Users/cuddly/Downloads/bank-additional-full.csv",
                      header = T,
                      sep = ";",
                      na.strings = c(""," ","."))

dim(dt)

h2o.table(dt$y)

# Spliting Data 
splits <- h2o.splitFrame(
                          dt,           ##  splitting the H2O frame we read above 
                          c(0.6,0.2),   ##  create splits of 60% and 20%; 
                          ##  H2O will create one more split of 1-(sum of these parameters)
                          ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0re.2
                          seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   ## assign the first result the R variable train
                                                ## and the H2O name train.hex

valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex

test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

h2o.table(train$y)
h2o.table(valid$y)
h2o.table(test$y)

# Baseline model with default parameters --------------------------------------------------------
gbm1 <- h2o.gbm(
                training_frame = train,        ## the H2O frame for training
                validation_frame = valid,      ## the H2O frame for validation (not required)
                x=colnames(train)[!(colnames(train) %in% c("y"))],  ## the predictor columns, by column index
                y="y",                    ## the target index (what we are predicting)
                ntrees = 25,                   ## Number of trees to build
                max_depth = 5,     ## Specify the maximum tree depth. Higher values will make the model more complex and can lead to overfitting.
                min_rows = 10,     ## Specify the minimum number of observations for a leaf
                learn_rate = 0.1  ,     ## Specify the learning rate
                learn_rate_annealing =  1,     ## Specifies to reduce the learn_rate by this factor after every tree.
                sample_rate = 1 ,     ## Specifies rwo sampling rate
                col_sample_rate = 1 ,     ## Specifies column sampling rate
                #stopping_rounds = 2, ## specifies number of iteration when training performance doesn't improve
                #stopping_metric = "mean_per_class_error", #Specify the metric to use for early stopping
                #stopping_tolerance = 0.001, ##Specify the relative tolerance for the metric-based stopping to stop training if the improvement is less than this value.
                verbose = TRUE,
                seed = 999)                ## Set the random seed for reproducability

gbm1
View(h2o.varimp(gbm1))

h2o.performance(gbm1,test)

# Tuning Ntrees & learning rate--------------------------------------------------------
temp <- {}
for ( i in c(25)){ #ntrees
  for(j in c(0.5)){ #learn rate
    for(k in c(5,10,15,20)){ #maxdepth
      for(l in c(2,5,10)){ #min row
        for(m in seq(1)){ #learn rate annealing
          for(n in c(1)){ #sample rate
            for(o in c(1)){ #column sample rate
            temp1 <- {}
            temp2 <- {}
            model <- h2o.gbm(
                            training_frame = train,        ## the H2O frame for training
                            validation_frame = valid,      ## the H2O frame for validation (not required)
                            x=colnames(train)[!(colnames(train) %in% c("y"))],  ## the predictor columns, by column index
                            y="y",                    ## the target index (what we are predicting)
                            ntrees = i,                   ## Number of trees to build
                            max_depth = k,     ## Specify the maximum tree depth. Higher values will make the model more complex and can lead to overfitting.
                            min_rows = l,     ## Specify the minimum number of observations for a leaf
                            learn_rate = j  ,     ## Specify the learning rate
                            learn_rate_annealing =  m,     ## Specifies to reduce the learn_rate by this factor after every tree.
                            sample_rate = n ,     ## Specifies rwo sampling rate
                            col_sample_rate = o ,     ## Specifies column sampling rate
                            #stopping_rounds = 2, ## specifies number of iteration when training performance doesn't improve
                            #stopping_metric = "mean_per_class_error", #Specify the metric to use for early stopping
                            #stopping_tolerance = 0.001, ##Specify the relative tolerance for the metric-based stopping to stop training if the improvement is less than this value.
                            verbose = TRUE,
                            seed = 999)                ## Set the random seed for reproducability
  
            temp1$ntrees <- i
            temp1$lr <- j
            temp1$max_depth <- k
            temp1$min_row <- l
            temp1$row_samp_rate <- m
            temp1$col_samp_rate <- n
            temp1$mean_per_class_error_train <-round(h2o.performance(model,train = T)@metrics$mean_per_class_error,10)
            temp1$mean_per_class_error_valid <-round(h2o.performance(model,valid = T)@metrics$mean_per_class_error,10)
            temp1$mean_per_class_error_test <-round(h2o.performance(model,newdata = test)@metrics$mean_per_class_error,10)
            temp1$AUCPR_train <- round(h2o.aucpr(model,train = T),10)
            temp1$AUCPR_valid <- round(h2o.aucpr(model,valid = T),10)
            temp1$AUCPR_test <- round(h2o.performance(model,newdata = test)@metrics$pr_auc,10)
            temp1$AUC_train <-round(h2o.auc(model,train = T),10)
            temp1$AUC_valid <- round(h2o.auc(model,valid = T),10)
            temp1$AUC_test <-round(h2o.performance(model,newdata = test)@metrics$AUC,10)
            temp1$error_0_train <- round(h2o.performance(model,train = T)@metrics$cm$table$Error[1],10)
            temp1$error_0_valid <- round(h2o.performance(model,valid = T)@metrics$cm$table$Error[1],10)
            temp1$error_0_test <- round(h2o.performance(model,newdata = test)@metrics$cm$table$Error[1],10)
            temp1$error_1_train <-round(h2o.performance(model,train = T)@metrics$cm$table$Error[2],10)
            temp1$error_1_valid <- round(h2o.performance(model,valid = T)@metrics$cm$table$Error[2],10)
            temp1$error_1_test <-round(h2o.performance(model,newdata = test)@metrics$cm$table$Error[2],10)
            temp1 <- as.data.frame(temp1)
            temp <- rbind(temp,temp1)
           
            }}}}}}}
View(temp)

