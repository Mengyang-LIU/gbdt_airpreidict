library(leaps)
library(car)
library(tidyverse)
library(caret)
library(ggplot2)
df_am = cbind(all_AM %>% select(ends_with('_100')),point_urban_info[,-1])
df_pm = cbind(all_PM %>% select(ends_with('_100')),point_urban_info[,-1])
df_all = df_am
df_all$backpm_100 = rowMeans(cbind(df_am$backpm_100,df_pm$backpm_100))
df_all$backco_100 = rowMeans(cbind(df_am$backco_100,df_pm$backco_100))
df_all$temp_100 = rowMeans(cbind(df_am$temp_100,df_pm$temp_100))
df_all$RH_100 = rowMeans(cbind(df_am$RH_100,df_pm$RH_100))
write_sf(df_all[-water_point$V1,],"shp/df_all1.shp")

par(mfrow=c(1,1))
par(mfrow=c(2,2))

set.seed(123)
cor_test1 = na.omit(cor_test[,c(1,3,4,8,10,16,20,24,28,32,36,40,41,42)])
cor_test2 = na.omit(cor_test[,c(2,3,4,5,9,16,19,23,27,30,36,37,41,42)])
#需要模型统一
# ames_train
# ames_test
# Build the model
model <- lm(backpm_100 ~ temp_100+ws+bhv_100+ca_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200,data = ames_train)
model <- lm(backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
data = ames_train)

summary(model)

vif(model)

train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(backpm_100 ~ temp_100+ws+bhv_100+ca_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200, data = ames_train, method = "lm",
               trControl = train.control)
# Make predictions and compute the R2, RMSE and MAE on train data
predictions <- model %>% predict(cor_test1)
data.frame( R2 = R2(predictions, cor_test1$backpm_100),
            RMSE = RMSE(predictions, cor_test1$backpm_100),
            MAE = MAE(predictions, cor_test1$backpm_100))
# Make predictions and compute the R2, RMSE and MAE in test data
predictions <- model %>% predict(ames_test)
data.frame( R2 = R2(predictions, ames_test$backpm_100),
            RMSE = RMSE(predictions, ames_test$backpm_100),
            MAE = MAE(predictions, ames_test$backpm_100))

#co
model <- lm(co_100 ~ RH_100+ws+mh_200+bhv_100+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+svf+disr+rd1_50+rd2_200,
            data = ames_train1)

summary(model)

as.data.frame(vif(model))

train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(co_100 ~ temp_100+ws+mh_200+bhv_100+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+poi_comm_200+svf+disr+rd1_50+rd2_200,
               data = ames_train1,
               method = "lm",
               trControl = train.control)
# Make predictions and compute the R2, RMSE and MAE on train data
predictions <- model %>% predict(cor_test2)
data.frame( R2 = R2(predictions, cor_test2$co_100),
            RMSE = RMSE(predictions, cor_test2$co_100),
            MAE = MAE(predictions, cor_test2$co_100))
# Make predictions and compute the R2, RMSE and MAE in test data
predictions <- model %>% predict(ames_test1)
data.frame( R2 = R2(predictions, ames_test1$co_100),
            RMSE = RMSE(predictions, ames_test1$co_100),
            MAE = MAE(predictions, ames_test1$co_100))

# Summarize the results
print(model)
options(scipen=999)
model %>% predict(cor_test1)
cor_test11$prelur = model %>% predict(cor_test1)



# gam

library(mgcv)
library(gam)
library(nlme)
model <- train(backpm_100 ~ temp_100+ws+mh_200+bhv_100+ca_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200, 
               data = ames_train,
               method = "gamSpline",
               trControl = train.control,
               tuneGrid = data.frame(df=seq(1,3,by=1)))
model <- train(backpm_100 ~ temp_100+ws+mh_200+bhv_100+ca_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200, 
               data = ames_train,
               method = "gam",
               trControl = train.control)
model1 <- train(co_100 ~ temp_100+ws+mh_200+bhv_100+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+poi_comm_200+svf+disr+rd1_50+rd2_200,
               data = ames_train1,
               method = "gamSpline",
               trControl = train.control,
               tuneGrid = data.frame(df=seq(1,3,by=1)))
summary(model)
# rmse


testdata = data.frame(Income = seq(.4, 1, length = 100),
                      Edu = mean(mod_gam2$model$Edu),
                      Health = mean(mod_gam2$model$Health))
fits = model %>% predict(ames_train)

predicts = data.frame(ames_train, fits) #%>% 
  #mutate(lower = fit - 1.96*se.fit,
  #       upper = fit + 1.96*se.fit)

plot_mod_gam2_response = ggplot(aes(x=bhv_100,y=fits), data=predicts) +
  #geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#00aaff') +
  theme_classic()
