library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models

hyper_grid_2 <- expand.grid(
  mtry       = seq(1, 20, by = 1),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

# perform grid search
for(i in 1:nrow(hyper_grid_2)) {
  
  # train model
  model <- ranger(
    formula         = backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100, 
    data            = ames_train, 
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid_2 %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

ames_randomForest <- randomForest(
  formula= backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100, 
  data    = ames_train, 
  ntree   = 50,
  mtry    = 7,
  nodesize = 3
)
predictions <- ames_randomForest %>% predict(ames_test)
data.frame( R2 = R2(predictions, ames_test$backpm_100),
            RMSE = RMSE(predictions, ames_test$backpm_100),
            MAE = MAE(predictions, ames_test$backpm_100))

predictions <- ames_randomForest %>% predict(cor_test1)
data.frame( R2 = R2(predictions, cor_test1$backpm_100),
            RMSE = RMSE(predictions, cor_test1$backpm_100),
            MAE = MAE(predictions, cor_test1$backpm_100))


model <- lm(CO1_1030 ~ CO_1030,data = co_cali)
model <- lm(CO1_1103 ~ CO_1103,data = co_cali)
model <- lm(CO1_1104 ~ CO_1104,data = co_cali)
model <- lm(CO1_1110 ~ CO_1110,data = co_cali)
model <- train(CO1_1103 ~ CO_1103, 
               data = co_cali[-11,],
               method = "gamSpline",
               trControl = train.control,
               tuneGrid = data.frame(df=seq(1,3,by=1)))
summary(model)
co1030cali = as.data.frame(AM1030[,'co'])
colnames(co1030cali) = 'CO_1030'

co1103cali = as.data.frame(AM1103[,'co'])
colnames(co1103cali) = 'CO_1103'

predictions <- model %>% predict(co1103cali)
plot(predictions)
plot(AM1103$co)
