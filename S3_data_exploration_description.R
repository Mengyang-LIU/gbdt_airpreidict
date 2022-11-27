library(ggplot2)
setwd('/Users/..')
# Create dummy data
NA_AM1030 = na.omit(AM1030)
NA_AM1030$track_seg_point_id
##### calibration#####
AM1030$corr_f  =1+0.25*(AM1030$temp/100)^2/(1-AM1030$temp/100)
AM1030$corr_pm = AM1030$pm25/AM1030$corr_f 
AM1030$temp_f = AM1030$back_pm25/mean(AM1030$back_pm25,na.rm = TRUE)
AM1030$temp_pm = AM1030$corr_pm/AM1030$temp_f 
AM1030$temp_pm1 = AM1030$pm25/AM1030$temp_f 
AM1030$corr_f1 = 1
AM1030[which(AM1030$temp>50),]$corr_f1  = 1+0.25*(AM1030[which(AM1030$temp>50),]$temp/100)^2/(1-AM1030[which(AM1030$temp>50),]$temp/100)
AM1030$temp_pm2 = AM1030$pm25/AM1030$corr_f1/AM1030$temp_f 

ggplot(NA_PM1030) +
  geom_line(aes(x=track_seg_point_id, y=pm25), color= 'grey') +
  geom_line(aes(x=track_seg_point_id, y=corr_pm),color = 'blue')
ggplot(all_PM[-STA_ID,]) +
  geom_line(aes(x=V1, y=backpm_1), color= 'grey') +
  geom_line(aes(x=V1, y=backpm_2),color = 'blue')
all_df1 = all_df[-STA_ID,]
all_df1$V2 = 1:nrow(all_df1)
ggplot(all_df1) +
  geom_line(aes(x=V2, y=backpm_1), color= 'grey') +
  geom_line(aes(x=V2, y=backpm_2),color = 'blue')+
  theme_light()
ggplot(agg_AM1110) +
  geom_line(aes(x=V1, y=back_pm), color= 'grey') +
  geom_line(aes(x=V1, y=back_pm1),color = 'blue')
ggplot(AM1103) +
  geom_line(aes(x=track_seg_point_id, y=pm25), color= 'grey')+
  geom_line(aes(x=track_seg_point_id, y=corr_pm),color = 'blue')

 boxplot(AM1103$co)
ggplot() +
  geom_line(data = AM1103,aes(x=track_seg_point_id, y=co), color= 'grey')+
  geom_line(data = NA_AM1103,aes(x=track_seg_point_id, y=co),color = 'blue')

ggplot() +
  geom_line(data = AM1104,aes(x=track_seg_point_id, y=co), color= 'grey')+
  geom_line(data = NA_AM1104,aes(x=track_seg_point_id, y=co),color = 'blue')

plot(cor_test1$backpm_100)

ggplot(NA_PM1104) +
  geom_line(aes(x=track_seg_point_id, y=pm25), color= 'grey')+
  geom_line(aes(x=track_seg_point_id, y=temp_pm),color = 'blue')+  
  geom_line(aes(x=track_seg_point_id, y=temp_pm1),color = 'green')

ggplot(NA_PM1110) +
  geom_line(aes(x=track_seg_point_id, y=pm25), color= 'grey')+
  geom_line(aes(x=track_seg_point_id, y=temp_pm),color = 'blue')+  
  geom_line(aes(x=track_seg_point_id, y=temp_pm1),color = 'green')

AM1103$corr_f  =1+0.25*(AM1103$temp/100)^2/(1-AM1103$temp/100)
AM1103$corr_pm = AM1103$pm25/AM1103$corr_f 
AM1103$temp_f = AM1103$back_pm25/mean(AM1103$back_pm25,na.rm = TRUE)
AM1103$temp_pm = AM1103$corr_pm/AM1103$temp_f 
AM1103$temp_pm1 = AM1103$pm25/AM1103$temp_f 
AM1103$corr_f1 = 1
AM1103[which(AM1103$temp>50),]$corr_f1  = 1+0.25*(AM1103[which(AM1103$temp>50),]$temp/100)^2/(1-AM1103[which(AM1103$temp>50),]$temp/100)
AM1103$temp_pm2 = AM1103$pm25/AM1103$corr_f1/AM1103$temp_f 


AM1104$corr_f  =1+0.25*(AM1104$temp/100)^2/(1-AM1104$temp/100)
AM1104$corr_pm = AM1104$pm25/AM1104$corr_f 
AM1104$temp_f = AM1104$back_pm25/mean(AM1104$back_pm25,na.rm = TRUE)
AM1104$temp_pm = AM1104$corr_pm/AM1104$temp_f 
AM1104$temp_pm1 = AM1104$pm25/AM1104$temp_f 
AM1104$corr_f1 = 1
AM1104[which(AM1104$temp>50),]$corr_f1  = 1+0.25*(AM1104[which(AM1104$temp>50),]$temp/100)^2/(1-AM1104[which(AM1104$temp>50),]$temp/100)
AM1104$temp_pm2 = AM1104$pm25/AM1104$corr_f1/AM1104$temp_f 

AM1110$corr_f  =1+0.25*(AM1110$temp/100)^2/(1-AM1110$temp/100)
AM1110$corr_pm = AM1110$pm25/AM1110$corr_f 
AM1110$temp_f = AM1110$back_pm25/mean(AM1110$back_pm25,na.rm = TRUE)
AM1110$temp_pm = AM1110$corr_pm/AM1110$temp_f 
AM1110$temp_pm1 = AM1110$pm25/AM1110$temp_f 
AM1110$corr_f1 = 1
AM1110[which(AM1110$temp>50),]$corr_f1  = 1+0.25*(AM1110[which(AM1110$temp>50),]$temp/100)^2/(1-AM1110[which(AM1110$temp>50),]$temp/100)
AM1110$temp_pm2 = AM1110$pm25/AM1110$corr_f1/AM1110$temp_f 

PM1030$corr_f  =1+0.25*(PM1030$temp/100)^2/(1-PM1030$temp/100)
PM1030$corr_pm = PM1030$pm25/PM1030$corr_f 
PM1030$temp_f = PM1030$back_pm25/mean(PM1030$back_pm25,na.rm = TRUE)
PM1030$temp_pm = PM1030$corr_pm/PM1030$temp_f 
PM1030$temp_pm1 = PM1030$pm25/PM1030$temp_f 

PM1103$corr_f  =1+0.25*(PM1103$temp/100)^2/(1-PM1103$temp/100)
PM1103$corr_pm = PM1103$pm25/PM1103$corr_f 
PM1103$temp_f = PM1103$back_pm25/mean(PM1103$back_pm25,na.rm = TRUE)
PM1103$temp_pm = PM1103$corr_pm/PM1103$temp_f 
PM1103$temp_pm1 = PM1103$pm25/PM1103$temp_f 

PM1104$corr_f  =1+0.25*(PM1104$temp/100)^2/(1-PM1104$temp/100)
PM1104$corr_pm = PM1104$pm25/PM1104$corr_f 
PM1104$temp_f = PM1104$back_pm25/mean(PM1104$back_pm25,na.rm = TRUE)
PM1104$temp_pm = PM1104$corr_pm/PM1104$temp_f 
PM1104$temp_pm1 = PM1104$pm25/PM1104$temp_f 

PM1110$corr_f  =1+0.25*(PM1110$temp/100)^2/(1-PM1110$temp/100)
PM1110$corr_pm = PM1110$pm25/PM1110$corr_f 
PM1110$temp_f = PM1110$back_pm25/mean(PM1110$back_pm25,na.rm = TRUE)
PM1110$temp_pm = PM1110$corr_pm/PM1110$temp_f 
PM1110$temp_pm1 = PM1110$pm25/PM1110$temp_f 
##### calibration#####
library(rstatix)
is_outlier(x, coef = 1.5)
is_extreme(AM1030$co)
humid_distribution = data.frame(v1 = c(1:100), v2 =NA )
humid_distribution$v2 = 1+0.25*(humid_distribution$v1/100)^2/(1-humid_distribution$v1/100)
plot(humid_distribution)
boxplot(NA_AM1030$pm25)
boxplot(NA_AM1030$co)
boxplot(NA_AM1030$back_pm25)
boxplot(NA_AM1030$back_co)
boxplot(NA_AM1030$temp)
boxplot(NA_AM1030$RH)
model = train(pm25 ~ temp+RH,
              data = AM1030,
              method = "rf",
              trControl = train.control)
summary(model)
remove_outliner = function(gps_data){
  NA_data = gps_data
  NA_data$pm25[which(NA_data$pm25 %in% boxplot.stats(NA_data$pm25)$out == TRUE)] = NA
  NA_data$co[which(is_extreme(NA_data$co) == TRUE)] = NA
  NA_data$corr_pm[which(NA_data$corr_pm %in% boxplot.stats(NA_data$corr_pm)$out== TRUE)] = NA
  NA_data$temp_pm[which(NA_data$temp_pm %in% boxplot.stats(NA_data$temp_pm)$out== TRUE)] = NA
  NA_data$temp_pm1[which(NA_data$temp_pm1 %in% boxplot.stats(NA_data$temp_pm1)$out== TRUE)] = NA
  return(NA_data)
}
c = which(AM1030$pm25 %in% boxplot.stats(AM1030$pm25)$out == TRUE)
NA_AM1030 = remove_outliner(AM1030)
NA_PM1030 = remove_outliner(PM1030)
NA_AM1103 = remove_outliner(AM1103)
NA_PM1103 = remove_outliner(PM1103)
NA_AM1104 = remove_outliner(AM1104)
NA_PM1104 = remove_outliner(PM1104)
NA_AM1110 = remove_outliner(AM1110)
NA_PM1110 = remove_outliner(PM1110)

write_sf(NA_AM1030,"AM10301.shp")
write_sf(NA_PM1030,"PM1030.shp")
write_sf(NA_AM1103,"AM1103.shp")
write_sf(NA_PM1103,"PM1103.shp")
write_sf(NA_AM1104,"AM1104.shp")
write_sf(NA_PM1104,"PM1104.shp")
write_sf(NA_AM1110,"AM1110.shp")
write_sf(NA_PM1110,"PM1110.shp")

count_outliner = function(gps_data){
  NA_data = gps_data
  a = length(which(NA_data$pm25 %in% boxplot.stats(NA_data$pm25)$out == TRUE))
  b = length(which(NA_data$co %in% boxplot.stats(NA_data$co)$out== TRUE))
  c = length(which(NA_data$temp_pm %in% boxplot.stats(NA_data$temp_pm)$out== TRUE))
  return(paste(a,b,c))
}
count_outliner(AM1030)
count_outliner(AM1103)
count_outliner(AM1104)
count_outliner(AM1110)

count_outliner(PM1030)
count_outliner(PM1103)
count_outliner(PM1104)
count_outliner(PM1110)
boxplot(PM1110$pm25)
plot(PM1110)

ggplot() +
  geom_line(data = NA_AM1030,aes(x=track_seg_point_id, y=pm25), color = 'red') +
  geom_line(data = NA_AM1103,aes(x=track_seg_point_id, y=pm25), color = 'blue') +
  geom_line(data = NA_AM1104,aes(x=track_seg_point_id, y=pm25), color = 'grey') +
  geom_line(data = NA_AM1110,aes(x=track_seg_point_id, y=pm25), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = NA_AM1030,aes(x=track_seg_point_id, y=back_pm25/1000), color = 'red') +
  geom_line(data = NA_AM1103,aes(x=track_seg_point_id, y=back_pm25/1000), color = 'blue') +
  geom_line(data = NA_AM1104,aes(x=track_seg_point_id, y=back_pm25/1000), color = 'grey') +
  geom_line(data = NA_AM1110,aes(x=track_seg_point_id, y=back_pm25/1000), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = NA_AM1030,aes(x=track_seg_point_id, y=pm25), color = 'red') +
  geom_line(data = NA_AM1103,aes(x=track_seg_point_id, y=pm25), color = 'blue') +
  geom_line(data = NA_AM1104,aes(x=track_seg_point_id, y=pm25), color = 'grey') +
  geom_line(data = NA_AM1110,aes(x=track_seg_point_id, y=pm25), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=backpm_100), color = 'blue') +
  geom_line(data = agg_AM1104,aes(x=V1, y=backpm_100), color = 'grey') +
  geom_line(data = agg_AM1110,aes(x=V1, y=backpm_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_PM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_PM1103,aes(x=V1, y=backpm_100), color = 'blue') +
  geom_line(data = agg_PM1104,aes(x=V1, y=backpm_100), color = 'grey') +
  geom_line(data = agg_PM1110,aes(x=V1, y=backpm_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1104,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1110,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_PM1030,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1103,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1104,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1110,aes(x=V1, y=backpm_100), color = 'pink') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light(

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=backpm_100), color = 'blue') +
  geom_line(data = agg_AM1104,aes(x=V1, y=backpm_100), color = 'grey') +
  geom_line(data = agg_AM1110,aes(x=V1, y=backpm_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_PM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_PM1103,aes(x=V1, y=backpm_100), color = 'blue') +
  geom_line(data = agg_PM1104,aes(x=V1, y=backpm_100), color = 'grey') +
  geom_line(data = agg_PM1110,aes(x=V1, y=backpm_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1104,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_AM1110,aes(x=V1, y=backpm_100), color = 'red') +
  geom_line(data = agg_PM1030,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1103,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1104,aes(x=V1, y=backpm_100), color = 'pink') +
  geom_line(data = agg_PM1110,aes(x=V1, y=backpm_100), color = 'pink') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background PM2.5 Concentration") +
  theme_light()


ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=backco_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=backco_100), color = 'blue') +
  geom_line(data = agg_AM1104,aes(x=V1, y=backco_100), color = 'grey') +
  geom_line(data = agg_AM1110,aes(x=V1, y=backco_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background CO Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_PM1030,aes(x=V1, y=backco_100), color = 'red') +
  geom_line(data = agg_PM1103,aes(x=V1, y=backco_100), color = 'blue') +
  geom_line(data = agg_PM1104,aes(x=V1, y=backco_100), color = 'grey') +
  geom_line(data = agg_PM1110,aes(x=V1, y=backco_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Background CO Concentration") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=temp_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=temp_100), color = 'blue') +
  geom_line(data = agg_AM1104,aes(x=V1, y=temp_100), color = 'grey') +
  geom_line(data = agg_AM1110,aes(x=V1, y=temp_100), color = 'yellow') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("RH") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=temp_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=temp_100), color = 'red') +
  geom_line(data = agg_AM1104,aes(x=V1, y=temp_100), color = 'red') +
  geom_line(data = agg_AM1110,aes(x=V1, y=temp_100), color = 'red') +
  geom_line(data = agg_PM1030,aes(x=V1, y=temp_100), color = 'pink') +
  geom_line(data = agg_PM1103,aes(x=V1, y=temp_100), color = 'pink') +
  geom_line(data = agg_PM1104,aes(x=V1, y=temp_100), color = 'pink') +
  geom_line(data = agg_PM1110,aes(x=V1, y=temp_100), color = 'pink') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("RH") +
  theme_light()

ggplot() +
  geom_line(data = agg_AM1030,aes(x=V1, y=pm25_100), color = 'red') +
  geom_line(data = agg_AM1103,aes(x=V1, y=pm25_100), color = 'red') +
  geom_line(data = agg_AM1104,aes(x=V1, y=pm25_100), color = 'red') +
  geom_line(data = agg_AM1110,aes(x=V1, y=pm25_100), color = 'red') +
  geom_line(data = agg_PM1030,aes(x=V1, y=pm25_100), color = 'pink') +
  geom_line(data = agg_PM1103,aes(x=V1, y=pm25_100), color = 'pink') +
  geom_line(data = agg_PM1104,aes(x=V1, y=pm25_100), color = 'pink') +
  geom_line(data = agg_PM1110,aes(x=V1, y=pm25_100), color = 'pink') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("PM2.5 Concentration") +
  theme_light()

df_am$V1 = c(1:nrow(df_am))
df_pm$V1 = c(1:nrow(df_pm))

ggplot() +
  geom_line(data = df_am,aes(x=V1, y=RH_100), color = 'red') +
  geom_line(data = df_pm,aes(x=V1, y=RH_100), color = 'pink') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("temp") +
  theme_light()

mean(NA_AM1110$pm25, na.rm = TRUE)
mean(NA_AM1110$co, na.rm = TRUE)
mean(NA_AM1110$temp, na.rm = TRUE)
mean(NA_AM1110$RH, na.rm = TRUE)
mean(NA_PM1110$pm25, na.rm = TRUE)
mean(NA_PM1110$co, na.rm = TRUE)
mean(NA_PM1110$temp, na.rm = TRUE)
mean(NA_PM1110$RH, na.rm = TRUE)




