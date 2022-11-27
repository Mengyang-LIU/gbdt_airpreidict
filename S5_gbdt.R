library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(dplyr)

# for reproducibility
set.seed(123)

cor_test1 = na.omit(df_pm25[,c(4,6,7,8,9,11,18,22,26,30,34,38,42,46,50,53,58,60,63,64,68,70)])
cor_test1 = na.omit(all_df[-STA_ID,c(5,7,13,17,21,25,29,33,37,41,44,49,53,54,55,59,61,64,66,67,68,69)])

cor_test2 = na.omit(all_df[-STA_ID,c(5,7,10,14,21,22,29,33,35,41,45,49,53,54,55,56,63,65,66,67,68,69)])
cor_test3 = na.omit(all_df[-STA_ID,c(5,7,13,17,21,25,29,33,37,41,44,49,53,54,55,59,61,66,67,68,69,75)])

# cor_test1 cor_test2
ames_split <- initial_split(cor_test1, prop = .7)


ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

ames_split1 <- initial_split(cor_test2, prop = .7)


ames_train1 <- training(ames_split1)
ames_test1  <- testing(ames_split1)

ames_split2 <- initial_split(cor_test3, prop = .7)


ames_train2 <- training(ames_split2)
ames_test2  <- testing(ames_split2)

# train GBM model
gbm.fit <- gbm(
  formula = backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

gbm.fit <- gbm(
  formula = co_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_50+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+poi_comm_200+svf+disr+rd1_50+rd2_200,
  distribution = "gaussian",
  data = ames_train1,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)
## gbm(formula = Sale_Price ~ ., distribution = "gaussian", data = ames_train, 
##     n.trees = 10000, interaction.depth = 1, shrinkage = 0.001, 
##     cv.folds = 5, verbose = FALSE, n.cores = NULL)
## A gradient boosted model with gaussian loss function.
## 10000 iterations were performed.
## The best cross-validation iteration was 10000.
## There were 80 predictors of which 45 had non-zero influence.
which.min(gbm.fit$cv.error)
# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")
# rsqured on test
predicted = predict(gbm.fit, cor_test1)
data.frame( R2 = R2(predicted, cor_test1$backpm_100),
            RMSE = RMSE(predicted, cor_test1$backpm_100),
            MAE = MAE(predicted, cor_test1$backpm_100))

predicted = predict(gbm.fit, ames_test)
data.frame( R2 = R2(predicted, ames_test$backpm_100),
            RMSE = RMSE(predicted, ames_test$backpm_100),
            MAE = MAE(predicted, ames_test$backpm_100))


predicted = predict(gbm.fit1, cor_test2)
data.frame( R2 = R2(predicted, cor_test2$co_100),
            RMSE = RMSE(predicted, cor_test2$co_100),
            MAE = MAE(predicted, cor_test2$co_100))

predicted = predict(gbm.fit1, ames_test1)
data.frame( R2 = R2(predicted, ames_test1$co_100),
            RMSE = RMSE(predicted, ames_test1$co_100),
            MAE = MAE(predicted, ames_test1$co_100))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

#tuning
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 81

# randomize data
random_index <- sample(1:nrow(ames_train), nrow(ames_train))
random_ames_train <- ames_train[random_index, ]

random_index <- sample(1:nrow(ames_train1), nrow(ames_train1))
random_ames_train <- ames_train1[random_index, ]

random_index <- sample(1:nrow(ames_train2), nrow(ames_train2))
random_ames_train <- ames_train2[random_index, ]
# grid search 
for(i in 1:nrow(hyper_grid)) {
  print(i)
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}
co_predicted = agg_point[as.numeric(row.names(cor_test2)),]
co_predicted$co_100 = cor_test2$co_100
co_predicted$co_predicted = predicted
co_predicted$diff = co_predicted$co_predicted -co_predicted$co_100 
library(sf)
plot(co_predicted['diff'],breaks = c(-0.1,-0.01,0.01,0.1))
plot(co_predicted['diff'])


pm_predicted = agg_point[as.numeric(row.names(cor_test1)),]
pm_predicted$pm_100 = cor_test1$backpm_100
pm_predicted$pm_predicted = predicted
pm_predicted$diff = pm_predicted$pm_predicted -pm_predicted$pm_100 

plot(pm_predicted['diff'],breaks = c(-0.004,-0.0005,0.0005,0.004))
plot(pm_predicted['diff'])

for(i in 1:nrow(hyper_grid)) {
  print(i)
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = co_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_50+ba_50+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+poi_comm_200+svf+disr+rd1_50+rd2_200,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

for(i in 1:nrow(hyper_grid)) {
  print(i)
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = backpm_2 ~mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 10000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


# visu importance
gbm.fit <- gbm(
  formula = backpm_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 846,
  interaction.depth = 3,
  shrinkage = 0.10,
  n.minobsinnode = 15,
  bag.fraction = .8, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 
vip(gbm.fit4,num_features = 20L)
set.seed(123)

gbm.fit1 <- gbm(
  formula = co_100 ~ RH_100+temp_100+ws+mh_200+bhv_100+ca_50+ba_50+p15_200+p60_50+poi_inter_200+poi_bus_200+poi_metro_100+poi_rest_200+poi_resi_200+poi_off_200+poi_comm_200+svf+disr+rd1_50+rd2_200,
  distribution = "gaussian",
  data = ames_train1,
  n.trees = 92,
  interaction.depth = 5,
  shrinkage = 0.30,
  n.minobsinnode = 5,
  bag.fraction = 0.65, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 
library(vip)
vip(gbm.fit1,num_features = 20L)

gbm.fit2 <- gbm(
  formula = backpm_2 ~ mh_200+bhv_100+ca_200+ba_200+p15_200+p60_200+poi_inter_200+poi_bus_200+poi_metro_200+poi_rest_200+poi_resi_150+poi_off_200+poi_comm_200+svf+disr+rd1_200+rd2_100,
  distribution = "gaussian",
  data = ames_train2,
  n.trees = 45,
  interaction.depth = 5,
  shrinkage = 0.30,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 
predicted = predict(gbm.fit2, cor_test3)
data.frame( R2 = R2(predicted, cor_test3$backpm_2),
            RMSE = RMSE(predicted, cor_test3$backpm_2),
            MAE = MAE(predicted, cor_test3$backpm_2))

predicted = predict(gbm.fit2, ames_test2)
data.frame( R2 = R2(predicted, ames_test2$backpm_2),
            RMSE = RMSE(predicted, ames_test2$backpm_2),
            MAE = MAE(predicted, ames_test2$backpm_2))
library(vip)

vip(gbm.fit,num_features = 20L)+ geom_text(aes(label=percent(a[["data"]][["Importance"]]/100)),nudge_y = -1, colour = 'white',cex = 2)
vip(gbm.fit1,num_features = 20L)
vip(gbm.fit1,num_features = 20L)+ geom_text(aes(label=percent(a[["data"]][["Importance"]]/100)),nudge_y = -1, colour = 'white',cex = 2)
vip(gbm.fit2,num_features = 20L)
#partial dependence plot
gbm.fit1 %>%
  partial(pred.var = "p15_200", n.trees = gbm.fit1$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = test) 

gbm.fit1 %>%
  partial(pred.var = "mh_200", n.trees = gbm.fit1$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = test) 
test = ames_train[-c(which(ames_train$bhv_100 %in% boxplot.stats(ames_train$bhv_100)$out == TRUE)),]

gbm.fit1 %>%
  partial(pred.var = "svf", n.trees = gbm.fit1$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = test) 

test = ames_train[-c(which(ames_train$bhv_100 %in% boxplot.stats(ames_train$bhv_100)$out == TRUE)),]

gbm.fit %>%
  partial(pred.var = "bhv_100", n.trees = gbm.fit$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = ames_train) 

a = vip(gbm.fit,num_features = 25L)+ geom_text(aes(label=percent(round(a[["data"]][["Importance"]]/100,3),accuracy = 0.1)),nudge_y = -1.3, colour = 'white',cex = 2)

b = vip(gbm.fit1,num_features = 25L)+ geom_text(aes(label=percent(round(b[["data"]][["Importance"]]/100,3),accuracy = 0.1)),nudge_y = -1.3, colour = 'white',cex = 2)

gridExtra::grid.arrange(a, b, nrow = 1)

ice1 <- gbm.fit %>%
  partial(
    pred.var = "bhv_100", 
    n.trees = gbm.fit$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1) +
  ggtitle("Non-centered")

ice1 <- gbm.fit %>%
  partial(
    pred.var = "ca_200", 
    n.trees = gbm.fit$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  ggtitle("Centered") + 
  theme_light()

ice2 <- gbm.fit %>%
  partial(
    pred.var = "p15_200", 
    n.trees = gbm.fit$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  ggtitle("Centered") + 
  theme_light()

ice3 <- gbm.fit %>%
  partial(
    pred.var = "bhv_100", 
    n.trees = gbm.fit$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  ggtitle("Centered") + 
  theme_light()

gridExtra::grid.arrange(ice1, ice2, ice3,nrow = 1)

ice1 <- gbm.fit1 %>%
  partial(
    pred.var = "p15_200", 
    n.trees = gbm.fit1$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train1, alpha = .1, center = TRUE) +
  ggtitle("Centered") + 
  theme_light()

ice2 <- gbm.fit1 %>%
  partial(
    pred.var = "mh_200", 
    n.trees = gbm.fit1$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train1, alpha = .1, center = TRUE) +
  ggtitle("Centered") + 
  theme_light()

gridExtra::grid.arrange(ice1, ice2,nrow = 1)

# Compute partial dependence data for lstat and rm
rwb <- colorRampPalette(c("red", "white", "blue"))
pd1 <- partial(gbm.fit, pred.var = c("bhv_100", "ws"),n.trees = gbm.fit$n.trees)
pd2 <- partial(gbm.fit, pred.var = c("DISR", "TEMP"),n.trees = gbm.fit$n.trees)

# Default PDP
pdp1 <- plotPartial(pd1, contour = FALSE, col.regions = rwb)
pdp2 <- plotPartial(pd2, contour = FALSE, col.regions = rwb)

grid.arrange(pdp1, pdp2, nrow = 1)
# remove sample not in the training set
p1 <- partial(gbm.fit, pred.var = c("SVF", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE)
plotPartial(p1,col.regions = rwb)

p2 <- partial(gbm.fit, pred.var = c("RD_200", "RH"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")
grid.arrange(p1, p2, nrow = 1)
p1 <- partial(gbm.fit, pred.var = c("DISR", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")

p2 <- partial(gbm.fit, pred.var = c("CA_200", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")
p3 <- partial(gbm.fit, pred.var = c("P15_200", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")
p4 <- partial(gbm.fit, pred.var = c("BHV_100", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")
p5 <- partial(gbm.fit, pred.var = c("P60_200", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")
p6 <- partial(gbm.fit, pred.var = c("MH_200", "TEMP"),n.trees = gbm.fit$n.trees, plot = TRUE, chull = TRUE,palette = "cividis")

grid.arrange(p1, p2,p3,p4,p5,p6, nrow = 2)
#draw top ten feature pdp
# Fit a PPR model (nterms was chosen using the caret package with 5 repeats of 
# 5-fold cross-validation)

# PDPs for all 10 features
features <- as.character(a[["data"]][["Variable"]])[1:10]
features = c('ca_200','p15_200','bhv_100')
pdps <- lapply(features, FUN = function(feature) {
  pd <- partial(gbm.fit, pred.var = feature,n.trees = gbm.fit$n.trees)
  autoplot(pd,rug = TRUE,train = ames_train,ylab = expression('PM2.5')) + 
    theme_light()
})

b = vip(gbm.fit1,num_features = 20L)
features <- as.character(b[["data"]][["Variable"]])[1:10]

pdps <- lapply(features, FUN = function(feature) {
  pd <- partial(gbm.fit1, pred.var = feature,n.trees = gbm.fit1$n.trees)
  autoplot(pd,rug = TRUE,train = ames_train1,ylab = expression('CO')) + 
    theme_light()
})
# smooth = TRUE
grid.arrange(grobs = pdps, ncol = 5)

ice_curves <- lapply(features, FUN = function(feature) {
  ice <- partial(gbm.fit, pred.var = feature, ice = TRUE)
  autoplot(ice, alpha = 0.1,ylab = expression('PM 2.5')) + 
    theme_light()
})
grid.arrange(grobs = ice_curves, ncol = 5)

library(ggplot2)
# install.packages("ggpointdensity")
library(ggpointdensity)

df <- data.frame(x = y_test, y = predicted)
ggplot(df, aes(x=x, y=y)) + geom_pointdensity() + scale_color_viridis_c()

pd <- partial(gbm.fit1, pred.var = c("svf","temp_100","disr" ), n.trees = gbm.fit1$n.trees,chull = TRUE)  

plotPartial(pd, palette = "cividis")  # Figure 4
lattice::trellis.focus(  # add a label to the colorkey
  "legend", side = "right", clipp.off = TRUE, highlight = FALSE
)
grid::grid.text("CO", x = 0.2, y = 1.05, hjust = 0.5, vjust = 1)
lattice::trellis.unfocus()

boxplot(pd$DISR)
cor_test11 = cor_test1
cor_test21$predicted = predict(gbm.fit, cor_test2)
cor_test11$V1 = 1:nrow(cor_test11)
ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=predicted), color = 'green') +
  geom_line(data = cor_test21,aes(x=V1, y=prelur), color = 'blue') +
  geom_line(data = cor_test21,aes(x=V1, y=CO), color = 'grey') +
  theme_light()

ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=predicted), color = 'green') +
  geom_line(data = cor_test21,aes(x=V1, y=prelur), color = 'blue') +
  geom_line(data = cor_test21,aes(x=V1, y=PM2.5), color = 'grey') +
  theme_light()

ggplot() +
  geom_point(data = predicted,aes(x=V1, y=rbhv), color = 'grey') +
  geom_point(data = df_all_1,aes(x=V1, y=rbhv1), color = 'blue') +
  theme_light()

boxplot(df_all_1$rbhv[which(df_all_1$bhv_100 >= 10)])

df_all_1$Stdh_h_Ratio = df_all_1$bhv_100/df_all_1$mh_200
df_all_1$rbhv1 = df_all_1$rbhv
df_all_1$rbhv1[which(df_all_1$bhv_100 >= 10)] = NA
df_all_1$rbhv1[which(df_all_1$backpm_100 >= 0.04)] = NA
plot(df_all_1$bhv_100/df_all_1$mh_100)
which(df_all_1$bhv_100 < 11)
attach(AM1030)
plot(temp, pm25,pch=19)
plot(temp, back_pm25,pch=19)

attach(AM1103)
plot(temp, pm25,pch=19)
median(df_all_1$rbhv[which(df_all_1$bhv_100 >= 10)], na.rm = TRUE)

attach(AM1104)
plot(temp, pm25,pch=19)

attach(AM1110)
plot(temp, pm25,pch=19)


attach(PM1030)
plot(temp, pm25,pch=19)

# bug with 1104
attach(PM1104)
plot(temp, pm25,pch=19)
plot(temp, back_pm25,pch=19)
attach(PM1110)
plot(temp, pm25,pch=19)
plot(temp, back_pm25,pch=19)

attach(agg_AM1030)
plot(temp_100, backpm_100,pch=19)

attach(agg_AM1103)
plot(temp_100, backpm_100,pch=19)

attach(agg_AM1104)
plot(temp_100, backpm_100,pch=19)

# bug with agg_1110
attach(agg_AM1110)
plot(temp_100, backpm_100,pch=19)

attach(agg_PM1030)
plot(temp_100, backpm_100,pch=19)

attach(agg_PM1103)
plot(temp_100, backpm_100,pch=19)

attach(agg_PM1104)
plot(temp_100, backpm_100,pch=19)

# bug with agg_1110
attach(agg_PM1110)
plot(temp_100, backpm_100,pch=19)

attach(all_AM)
plot(temp_100, backpm_100,pch=19)
plot(svf, backco_100,pch=19)
attach(all_PM)
plot(temp_100, backpm_100,pch=19)
plot(RH_100, backpm_100,pch=19)
plot(svf, backco_100,pch=19)
attach(df_all)
plot(svf, temp_100,pch=19)
plot(svf, backco_100,pch=19)
detach()
boxplot(svf)
cor_test11 = cor_test1
cor_test21 = cor_test2
cor_test11$V1  = 1:nrow(cor_test11)
cor_test21$V1  = 1:nrow(cor_test21)
cor_test11$V2 = rownames(cor_test11)
cor_test21$V2 = rownames(cor_test21)
cor_test11$pmlur =  model %>% predict(cor_test1)
cor_test11$pmgbdt =  gbm.fit%>% predict(cor_test1)

cor_test21$colur =  model1 %>% predict(cor_test2)
cor_test21$cogbdt =  gbm.fit1%>% predict(cor_test2)
# A 141-231   B232-332   C 333-395   D 404-441 E 471-497 F497-566
cor_test11$V2 = as.numeric(cor_test11$V2)
cor_test11$V3 = "A"

cor_test11$V3[which(cor_test11$V2<=332&cor_test11$V2>=232)] = 'B'
cor_test11$V3[which(cor_test11$V2<=395&cor_test11$V2>=333)] = 'C'
cor_test11$V3[which(cor_test11$V2<=441&cor_test11$V2>=404)] = 'D'
cor_test11$V3[which(cor_test11$V2<=497&cor_test11$V2>=471)] = 'E'
cor_test11$V3[which(cor_test11$V2<=566&cor_test11$V2>=498)] = 'F'

cor_test21$V2 = as.numeric(cor_test21$V2)
cor_test21$V3 = "A"

cor_test21$V3[which(cor_test21$V2<=332&cor_test21$V2>=232)] = 'B'
cor_test21$V3[which(cor_test21$V2<=395&cor_test21$V2>=333)] = 'C'
cor_test21$V3[which(cor_test21$V2<=441&cor_test21$V2>=404)] = 'D'
cor_test21$V3[which(cor_test21$V2<=497&cor_test21$V2>=471)] = 'E'
cor_test21$V3[which(cor_test21$V2<=566&cor_test21$V2>=498)] = 'F'

d=data.frame(date=c(63, 154, 211, 226, 249,300), event=c("A", "B", "C", "D", "E", "F"))

ggplot() +
  geom_line(data = cor_test11,aes(x=V1, y=pmgbdt), color = 'green') +
  geom_line(data = cor_test11,aes(x=V1, y=pmlur), color = 'blue') +
  geom_line(data = cor_test11,aes(x=V1, y=backpm_100), color = 'purple') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.03, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("PM2.5 concentration [mg/m3]") +
  theme_light()

ggplot() +
  geom_line(data = cor_test11,aes(x=V1, y=ca_200), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("Building density") +
  theme_light()

ggplot() +
  geom_line(data = cor_test11,aes(x=V1, y=p15_200), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.4, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("p15_200") +
  theme_light()

ggplot() +
  geom_line(data = cor_test11,aes(x=V1, y=bhv_100), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("Standard deviation of height [m]") +
  theme_light()

d=data.frame(date=c(62, 160, 222, 246, 273,331), event=c("A", "B", "C", "D", "E", "F"))

ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=cogbdt), color = 'green') +
  geom_line(data = cor_test21,aes(x=V1, y=colur), color = 'blue') +
  geom_line(data = cor_test21,aes(x=V1, y=co_100), color = 'purple') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.3, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("CO concentration [ppm]") +
  theme_light()

ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=P15_150), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.4, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("P15_150") +
  theme_light()

ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=mh_200), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.2, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("Mean height [m]") +
  theme_light()

ggplot() +
  geom_line(data = cor_test21,aes(x=V1, y=P60_150), color = 'red') +
  geom_vline(data=d, mapping=aes(xintercept=date), color="grey", linetype = 2) +
  geom_text(data=d, mapping=aes(x=date, y=0.75, label=event), size=4, angle=90, vjust=-0.4)+
  scale_y_continuous("P60_150") +
  theme_light()



