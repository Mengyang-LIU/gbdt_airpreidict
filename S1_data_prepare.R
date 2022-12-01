library(sf)
library(readxl)
library(anytime)
library(dplyr)
library(stringr)
setwd('/Users/..')
getwd()

# match the pollution, background pollution concentration, temperature and humidity monitoring, gps data.
AM1030 = AM1030[,-15]
rownames(pm25_1030)<-1:nrow(pm25_1030)
remove(pm_1030)
earlier <- strptime("00:00:00","%H:%M:%S")
later <- strptime("00:00:20","%H:%M:%S")
d <- difftime(later,earlier)
as.double(d)

diff <- as.difftime("00:00:02","%H:%M:%S",units="hour")
now <- Sys.time()
later <- now + diff

add_sep_time = function(gps_data){
  gps_data$Time <- strftime(gps_data$time, format='%H:%M:%S')
  gps_data$H = str_sub(gps_data$Time, 1, 2)
  gps_data$M = str_sub(gps_data$Time, 4, 5)
  gps_data$S = str_sub(gps_data$Time, 7, 8)
  return(gps_data)
}

match_gps_pm25 = function(gps_data, pm25_data){
  gps_data$pm25  = NA
  print(head(gps_data))
  for (i in c(1:nrow(gps_data))){
    print(paste0("i ",i))
    test = gps_data[i,]
    diff1 <- as.difftime("00:00:01","%H:%M:%S",units="hour")
    diff2 <- as.difftime("00:00:02","%H:%M:%S",units="hour")
    now <- format(test$time, format = "%H:%M:%S")
    now = strptime(now,"%H:%M:%S")
    pm25_data$tt = strptime(pm25_data$Time,"%H:%M:%S")
    b = pm25_data %>% filter(tt ==now-diff2|tt ==now-diff1|tt ==now|tt ==now+diff1|tt ==now+diff2)
    if (nrow(b)!= 0){
      b$diff =abs(as.double(difftime(b$tt,now))) 
      gps_data$pm25[i] = b$Concentration[which.min(b$diff)]
    } else {
      next
    }
    print(gps_data$pm25[i])
  }
  print(head(gps_data))
  return(gps_data)  
}

match_gps_co = function(gps_data, co_data){
  gps_data$co  = NA
  print(head(gps_data))
  for (i in c(1:nrow(gps_data))){
    print(paste0("i ",i))
    test = gps_data[i,]
    diff1 <- as.difftime("00:00:01","%H:%M:%S",units="hour")
    diff2 <- as.difftime("00:00:02","%H:%M:%S",units="hour")
    now <- format(test$time, format = "%H:%M:%S")
    now = strptime(now,"%H:%M:%S")
    co_data$tt = strptime(co_data$Time,"%H:%M:%S")
    b = co_data %>% filter(tt ==now-diff2|tt ==now-diff1|tt ==now|tt ==now+diff1|tt ==now+diff2)
    if (nrow(b)!= 0){
      b$diff =abs(as.double(difftime(b$tt,now))) 
      gps_data$co[i] = b$Concentration[which.min(b$diff)]
    } else {
      next
    }
    print(gps_data$co[i])
  }
  print(head(gps_data))
  return(gps_data)  
}

match_gps_me = function(gps_data, me_data){
  gps_data$temp  = NA
  gps_data$RH  = NA
  print(head(gps_data))
  for (i in c(1:nrow(gps_data))){
    print(paste0("i ",i))
    test = gps_data[i,]
    diff1 <- as.difftime("00:00:01","%H:%M:%S",units="hour")
    diff2 <- as.difftime("00:00:02","%H:%M:%S",units="hour")
    now <- format(test$time, format = "%H:%M:%S")
    now = strptime(now,"%H:%M:%S")
    me_data$tt = strptime(me_data$Time,"%H:%M:%S")
    b = me_data %>% filter(tt ==now-diff2|tt ==now-diff1|tt ==now|tt ==now+diff1|tt ==now+diff2)
    if (nrow(b)!= 0){
      b$diff =abs(as.double(difftime(b$tt,now))) 
      gps_data$temp[i] = b$Value[which.min(b$diff)]
      gps_data$RH[i] = b$Value.1[which.min(b$diff)]
    } else {
      next
    }
    print(gps_data$temp[i])
    print(gps_data$RH[i])
  }
  print(head(gps_data))
  return(gps_data)  
}

match_gps_back = function(gps_data, back_data){
  colnames(back_data)[2:3] = c('pm','co')
  gps_data$back_pm25  = NA
  gps_data$back_co  = NA
  print(head(gps_data))
  for (i in c(1:nrow(gps_data))){
    print(paste0("i ",i))
    test = gps_data[i,]
    b = back_data %>% filter(H == test$H, M == test$M )
    if (nrow(b)!= 0){
      gps_data$back_pm25[i]  = b$pm
      gps_data$back_co[i]  = b$co
    } else {
      next
    }
    print(gps_data$back_pm25[i])
    print(gps_data$back_co[i])
  }
  print(head(gps_data))
  return(gps_data)  
}
# 提取国控点
match_gps_back = function(gps_data, back_data){
  colnames(back_data)[2:3] = c('pm','co')
  gps_data$back_pm25  = NA
  gps_data$back_co  = NA
  print(head(gps_data))
  for (i in c(1:nrow(gps_data))){
    print(paste0("i ",i))
    test = gps_data[i,]
    b = back_data %>% filter(H == test$H, M == test$M )
    if (nrow(b)!= 0){
      gps_data$back_pm25[i]  = b$pm
      gps_data$back_co[i]  = b$co
    } else {
      next
    }
    print(gps_data$back_pm25[i])
    print(gps_data$back_co[i])
  }
  print(head(gps_data))
  return(gps_data)  
}

setwd('/Users/..')
GPS_CRS = st_read("/Users/../2020-10-30_GPS/am_20-10-30 113801.gpx", layer = "track_points")[,-c(1,2,6:26)] 

AM1030 = st_read("2020-10-30_GPS/am_20-10-30 113801.gpx", layer = "track_points")[,-c(1,2,6:26)] 
PM1030 = st_read("2020-10-30_GPS/pm_20-10-30 183150.gpx", layer = "track_points")[,-c(1,2,6:26)]
pm25_1030 = read.csv("2020-10-30污染物/20201030污染物.csv")[-1,-1]
co_1030 = read.csv('2020-10-30CO浓度/2020-10-30CO浓度.csv')
metero_1030 = read.csv("2020-10-30滨江移动温湿度/2020-10-30滨江移动温湿度.csv")[-c(1,1807),-c(1,2,8,9)]
back_1030 = read.csv('2020-10-30    定点--0#  气象生态环境监测系统/2020-10-30定点.csv')[c(1:11),c(7,9,12)]
back_1030$X.3 = as.character(back_1030$X.3 )
back_1030$PM2.5__ = as.numeric(as.character(back_1030$PM2.5__ ))
back_1030$CO__= as.numeric(as.character(back_1030$CO__ ))



pm25_1030$Time = as.character(pm25_1030$Time)
pm25_1030$Time[1:677] = apply(pm25_1030[c(1:677),], 1, function(x) paste0("0",x[1]))

pm25_1030$H = str_sub(pm25_1030$Time, 1, 2)
pm25_1030$M = str_sub(pm25_1030$Time, 4, 5)
pm25_1030$S = str_sub(pm25_1030$Time, 7, 8)
pm25_1030$Concentration = as.numeric(as.character(pm25_1030$Concentration))

co_1030$Time = as.character(co_1030$时间)
co_1030$Time[1:653] = apply(co_1030[c(1:653),], 1, function(x) paste0("0",x[10]))
co_1030$H = str_sub(co_1030$Time, 1, 2)
co_1030$M = str_sub(co_1030$Time, 4, 5)
co_1030$S = str_sub(co_1030$Time, 7, 8)
co_1030$Concentration = as.numeric(as.character(co_1030$浓度))

metero_1030$Time = as.character(metero_1030$Time)
metero_1030$Time[1:667] = apply(metero_1030[c(1:667),], 1, function(x) paste0("0",x[1]))
metero_1030$H = str_sub(metero_1030$Time, 1, 2)
metero_1030$M = str_sub(metero_1030$Time, 4, 5)
metero_1030$S = str_sub(metero_1030$Time, 7, 8)
metero_1030$Value = as.numeric(as.character(metero_1030$Value))
metero_1030$Value.1 = as.numeric(as.character(metero_1030$Value.1))

back_1030$Time <- strftime(back_1030$X__, format='%H:%M')
back_1030$H = str_sub(back_1030$Time, 1, 2)
back_1030$M = str_sub(back_1030$Time, 4, 5)
AM1030 = add_sep_time(AM1030) 
AM1030 = match_gps_pm25(AM1030, pm25_1030)
AM1030 = match_gps_co(AM1030, co_1030)
AM1030 = match_gps_me(AM1030, metero_1030)
AM1030 = match_gps_back(AM1030, back_1030)
PM1030 = add_sep_time(PM1030) 
PM1030 = match_gps_pm25(PM1030, pm25_1030)
PM1030 = match_gps_co(PM1030, co_1030)
PM1030 = match_gps_me(PM1030, metero_1030)
PM1030 = match_gps_back(PM1030, back_1030)
boxplot(AM1030$pm25)
boxplot(AM1030$co)

AM1030$back_pm25[which(AM1030$H =='09')] = back_1030$PM2.5__[1]+(back_1030$PM2.5__[2] - back_1030$PM2.5__[1])/60*as.numeric(AM1030$M[which(AM1030$H =='09')])    
AM1030$back_pm25[which(AM1030$H =='10')] = back_1030$PM2.5__[2]+(back_1030$PM2.5__[3] - back_1030$PM2.5__[2])/60*as.numeric(AM1030$M[which(AM1030$H =='10')])    
AM1030$back_pm25[which(AM1030$H =='11')] = back_1030$PM2.5__[3]+(back_1030$PM2.5__[4] - back_1030$PM2.5__[3])/60*as.numeric(AM1030$M[which(AM1030$H =='11')])    

AM1030$back_co[which(AM1030$H =='09')] = back_1030$CO__[1]+(back_1030$CO__[2] - back_1030$CO__[1])/60*as.numeric(AM1030$M[which(AM1030$H =='09')])    
AM1030$back_co[which(AM1030$H =='10')] = back_1030$CO__[2]+(back_1030$CO__[3] - back_1030$CO__[2])/60*as.numeric(AM1030$M[which(AM1030$H =='10')])    
AM1030$back_co[which(AM1030$H =='11')] = back_1030$CO__[3]+(back_1030$CO__[4] - back_1030$CO__[3])/60*as.numeric(AM1030$M[which(AM1030$H =='11')])    

PM1030$back_pm25[which(PM1030$H =='16')] = back_1030$PM2.5__[8]+(back_1030$PM2.5__[9] - back_1030$PM2.5__[8])/60*as.numeric(PM1030$M[which(PM1030$H =='16')])    
PM1030$back_pm25[which(PM1030$H =='17')] = back_1030$PM2.5__[9]+(back_1030$PM2.5__[10] - back_1030$PM2.5__[9])/60*as.numeric(PM1030$M[which(PM1030$H =='17')])    
PM1030$back_pm25[which(PM1030$H =='18')] = back_1030$PM2.5__[10]+(back_1030$PM2.5__[11] - back_1030$PM2.5__[10])/60*as.numeric(PM1030$M[which(PM1030$H =='18')])    

PM1030$back_co[which(PM1030$H =='16')] = back_1030$CO__[8]+(back_1030$CO__[9] - back_1030$CO__[8])/60*as.numeric(PM1030$M[which(PM1030$H =='16')])    
PM1030$back_co[which(PM1030$H =='17')] = back_1030$CO__[9]+(back_1030$CO__[10] - back_1030$CO__[9])/60*as.numeric(PM1030$M[which(PM1030$H =='17')])    
PM1030$back_co[which(PM1030$H =='18')] = back_1030$CO__[10]+(back_1030$CO__[11] - back_1030$CO__[10])/60*as.numeric(PM1030$M[which(PM1030$H =='18')])    


setwd('/Users/..')
AM1103 = st_read("2020-11-03_GPS/20-11-03 11.33.50.gpx", layer = "track_points")[,-c(1,2,6:26)] 
PM1103 = st_read("2020-11-03_GPS/20-11-03 18.31.15.gpx", layer = "track_points")[,-c(1,2,6:26)]
pm25_1103 = read.csv("2020-11-03滨江移动PM2.5/20201103污染物.csv")[,-1]
co_1103 = read.csv('2020-11-03CO浓度.csv')
metero_1103 = read.csv("2020-11-03滨江移动温湿度.csv")[-1730,-c(1,2,8,9)]
back_1103 = read.csv('2020-11-03定点.csv')[c(1:11),c(10,12,15)]
colnames(back_1103)= c('X.3','PM2.5__','CO__')
back_1103$X.3 = as.character(back_1103$X.3 )
back_1103$PM2.5__ = as.numeric(as.character(back_1103$PM2.5__ ))
back_1103$CO__= as.numeric(as.character(back_1103$CO__ ))

pm25_1103$Time = as.character(pm25_1103$Time)
pm25_1103$Time[1:585] = apply(pm25_1103[c(1:585),], 1, function(x) paste0("0",x[1]))

pm25_1103$H = str_sub(pm25_1103$Time, 1, 2)
pm25_1103$M = str_sub(pm25_1103$Time, 4, 5)
pm25_1103$S = str_sub(pm25_1103$Time, 7, 8)
pm25_1103$Concentration = as.numeric(as.character(pm25_1103$Concentration))

co_1103$Time = as.character(co_1103$时间)
co_1103$Time[1:540] = apply(co_1103[c(1:540),], 1, function(x) paste0("0",x[10]))
co_1103$H = str_sub(co_1103$Time, 1, 2)
co_1103$M = str_sub(co_1103$Time, 4, 5)
co_1103$S = str_sub(co_1103$Time, 7, 8)
co_1103$Concentration = as.numeric(as.character(co_1103$浓度))

metero_1103$Time = as.character(metero_1103$Time)
metero_1103$H = str_sub(metero_1103$Time, 1, 2)
metero_1103$M = str_sub(metero_1103$Time, 4, 5)
metero_1103$S = str_sub(metero_1103$Time, 7, 8)
metero_1103$Value = as.numeric(as.character(metero_1103$Value))
metero_1103$Value.1 = as.numeric(as.character(metero_1103$Value.1))

back_1103$Time <- strftime(back_1103$时间, format='%H:%M')
back_1103$H = str_sub(back_1103$Time, 1, 2)
back_1103$M = str_sub(back_1103$Time, 4, 5)
AM1103 = add_sep_time(AM1103) 
AM1103 = match_gps_pm25(AM1103, pm25_1103)
AM1103 = match_gps_co(AM1103, co_1103)
AM1103 = match_gps_me(AM1103, metero_1103)
AM1103 = match_gps_back(AM1103, back_1103)
PM1103 = add_sep_time(PM1103) 
PM1103 = match_gps_pm25(PM1103, pm25_1103)
PM1103 = match_gps_co(PM1103, co_1103)
PM1103 = match_gps_me(PM1103, metero_1103)
PM1103 = match_gps_back(PM1103, back_1103)

PM1103_TEST = match_gps_pm25(PM1103, pm25_1103)

AM1103$back_pm25[which(AM1103$H =='09')] = back_1103$PM2.5__[1]+(back_1103$PM2.5__[2] - back_1103$PM2.5__[1])/60*as.numeric(AM1103$M[which(AM1103$H =='09')])    
AM1103$back_pm25[which(AM1103$H =='10')] = back_1103$PM2.5__[2]+(back_1103$PM2.5__[3] - back_1103$PM2.5__[2])/60*as.numeric(AM1103$M[which(AM1103$H =='10')])    
AM1103$back_pm25[which(AM1103$H =='11')] = back_1103$PM2.5__[3]+(back_1103$PM2.5__[4] - back_1103$PM2.5__[3])/60*as.numeric(AM1103$M[which(AM1103$H =='11')])    

AM1103$back_co[which(AM1103$H =='09')] = back_1103$CO__[1]+(back_1103$CO__[2] - back_1103$CO__[1])/60*as.numeric(AM1103$M[which(AM1103$H =='09')])    
AM1103$back_co[which(AM1103$H =='10')] = back_1103$CO__[2]+(back_1103$CO__[3] - back_1103$CO__[2])/60*as.numeric(AM1103$M[which(AM1103$H =='10')])    
AM1103$back_co[which(AM1103$H =='11')] = back_1103$CO__[3]+(back_1103$CO__[4] - back_1103$CO__[3])/60*as.numeric(AM1103$M[which(AM1103$H =='11')])    

PM1103$back_pm25[which(PM1103$H =='16')] = back_1103$PM2.5__[8]+(back_1103$PM2.5__[9] - back_1103$PM2.5__[8])/60*as.numeric(PM1103$M[which(PM1103$H =='16')])    
PM1103$back_pm25[which(PM1103$H =='17')] = back_1103$PM2.5__[9]+(back_1103$PM2.5__[10] - back_1103$PM2.5__[9])/60*as.numeric(PM1103$M[which(PM1103$H =='17')])    
PM1103$back_pm25[which(PM1103$H =='18')] = back_1103$PM2.5__[10]+(back_1103$PM2.5__[11] - back_1103$PM2.5__[10])/60*as.numeric(PM1103$M[which(PM1103$H =='18')])    

PM1103$back_co[which(PM1103$H =='16')] = back_1103$CO__[8]+(back_1103$CO__[9] - back_1103$CO__[8])/60*as.numeric(PM1103$M[which(PM1103$H =='16')])    
PM1103$back_co[which(PM1103$H =='17')] = back_1103$CO__[9]+(back_1103$CO__[10] - back_1103$CO__[9])/60*as.numeric(PM1103$M[which(PM1103$H =='17')])    
PM1103$back_co[which(PM1103$H =='18')] = back_1103$CO__[10]+(back_1103$CO__[11] - back_1103$CO__[10])/60*as.numeric(PM1103$M[which(PM1103$H =='18')])    

setwd('/Users/..')
AM1104 = st_read("2020-11-04_GPS/20-11-04 11.24.21.gpx", layer = "track_points")[,-c(1,2,6:26)] 
PM1104 = st_read("2020-11-04_GPS/20-11-04 18.30.06.gpx", layer = "track_points")[,-c(1,2,6:26)]
pm25_1104 = read.csv("2020-11-04滨江移动PM2.5/20201104污染物.csv")[,-1]
co_1104 = read.csv('2020-11-04CO浓度.csv')[-c(1:16),]
rownames(co_1104) = c(1:nrow(co_1104))
metero_1104 = read.csv("2020-11-04滨江移动温湿度.csv")[-1736,-c(1,2,8,9)]
back_1104 = read.csv('2020-11-04定点.csv')[c(1:11),c(8,10,13)]
colnames(back_1104)= c('X.3','PM2.5__','CO__')
back_1104$X.3 = as.character(back_1104$X.3 )
back_1104$PM2.5__ = as.numeric(as.character(back_1104$PM2.5__ ))
back_1104$CO__= as.numeric(as.character(back_1104$CO__ ))

pm25_1104$Time = as.character(pm25_1104$Time)
pm25_1104$Time[1:691] = apply(pm25_1104[c(1:691),], 1, function(x) paste0("0",x[1]))

pm25_1104$H = str_sub(pm25_1104$Time, 1, 2)
pm25_1104$M = str_sub(pm25_1104$Time, 4, 5)
pm25_1104$S = str_sub(pm25_1104$Time, 7, 8)
pm25_1104$Concentration = as.numeric(as.character(pm25_1104$Concentration))


co_1104$Time = as.character(co_1104$时间)
co_1104$Time[1:665] = apply(co_1104[c(1:665),], 1, function(x) paste0("0",x[10]))
co_1104$H = str_sub(co_1104$Time, 1, 2)
co_1104$M = str_sub(co_1104$Time, 4, 5)
co_1104$S = str_sub(co_1104$Time, 7, 8)
co_1104$Concentration = as.numeric(as.character(co_1104$浓度))

metero_1104$Time = as.character(metero_1104$Time)
metero_1104$H = str_sub(metero_1104$Time, 1, 2)
metero_1104$M = str_sub(metero_1104$Time, 4, 5)
metero_1104$S = str_sub(metero_1104$Time, 7, 8)
metero_1104$Value = as.numeric(as.character(metero_1104$Value))
metero_1104$Value.1 = as.numeric(as.character(metero_1104$Value.1))

back_1104$Time <- strftime(back_1104$X__, format='%H:%M')
back_1104$H = str_sub(back_1104$Time, 1, 2)
back_1104$M = str_sub(back_1104$Time, 4, 5)
AM1104 = add_sep_time(AM1104) 
AM1104 = match_gps_pm25(AM1104, pm25_1104)
AM1104 = match_gps_co(AM1104, co_1104)
AM1104 = match_gps_me(AM1104, metero_1104)
AM1104 = match_gps_back(AM1104, back_1104)
PM1104 = add_sep_time(PM1104) 
PM1104 = match_gps_pm25(PM1104, pm25_1104)
PM1104 = match_gps_co(PM1104, co_1104)
PM1104 = match_gps_me(PM1104, metero_1104)
PM1104 = match_gps_back(PM1104, back_1104)

AM1104$back_pm25[which(AM1104$H =='09')] = back_1104$PM2.5__[1]+(back_1104$PM2.5__[2] - back_1104$PM2.5__[1])/60*as.numeric(AM1104$M[which(AM1104$H =='09')])    
AM1104$back_pm25[which(AM1104$H =='10')] = back_1104$PM2.5__[2]+(back_1104$PM2.5__[3] - back_1104$PM2.5__[2])/60*as.numeric(AM1104$M[which(AM1104$H =='10')])    
AM1104$back_pm25[which(AM1104$H =='11')] = back_1104$PM2.5__[3]+(back_1104$PM2.5__[4] - back_1104$PM2.5__[3])/60*as.numeric(AM1104$M[which(AM1104$H =='11')])    

AM1104$back_co[which(AM1104$H =='09')] = back_1104$CO__[1]+(back_1104$CO__[2] - back_1104$CO__[1])/60*as.numeric(AM1104$M[which(AM1104$H =='09')])    
AM1104$back_co[which(AM1104$H =='10')] = back_1104$CO__[2]+(back_1104$CO__[3] - back_1104$CO__[2])/60*as.numeric(AM1104$M[which(AM1104$H =='10')])    
AM1104$back_co[which(AM1104$H =='11')] = back_1104$CO__[3]+(back_1104$CO__[4] - back_1104$CO__[3])/60*as.numeric(AM1104$M[which(AM1104$H =='11')])    

PM1104$back_pm25[which(PM1104$H =='16')] = back_1104$PM2.5__[8]+(back_1104$PM2.5__[9] - back_1104$PM2.5__[8])/60*as.numeric(PM1104$M[which(PM1104$H =='16')])    
PM1104$back_pm25[which(PM1104$H =='17')] = back_1104$PM2.5__[9]+(back_1104$PM2.5__[10] - back_1104$PM2.5__[9])/60*as.numeric(PM1104$M[which(PM1104$H =='17')])    
PM1104$back_pm25[which(PM1104$H =='18')] = back_1104$PM2.5__[10]+(back_1104$PM2.5__[11] - back_1104$PM2.5__[10])/60*as.numeric(PM1104$M[which(PM1104$H =='18')])    

PM1104$back_co[which(PM1104$H =='16')] = back_1104$CO__[8]+(back_1104$CO__[9] - back_1104$CO__[8])/60*as.numeric(PM1104$M[which(PM1104$H =='16')])    
PM1104$back_co[which(PM1104$H =='17')] = back_1104$CO__[9]+(back_1104$CO__[10] - back_1104$CO__[9])/60*as.numeric(PM1104$M[which(PM1104$H =='17')])    
PM1104$back_co[which(PM1104$H =='18')] = back_1104$CO__[10]+(back_1104$CO__[11] - back_1104$CO__[10])/60*as.numeric(PM1104$M[which(PM1104$H =='18')])    

setwd('/Users/liumengyang/Desktop/1/2020-11-10实测')
AM1110 = st_read("2020-11-10_GPS/20-11-10  滨江11.36.35.gpx", layer = "track_points")[,-c(1,2,6:26)] 
PM1110 = st_read("2020-11-10_GPS/20-11-10  滨江 18.30.37.gpx", layer = "track_points")[,-c(1,2,6:26)]
pm25_1110 = read.csv("2020-11-10滨江移动PM2.5.csv")[,-1]
co_1110 = read.csv('2020-11-10CO浓度.csv')
metero_1110 = read.csv("2020-11-10温湿度.csv")[-1765,-c(1,2,8,9)]
back_1110 = read.csv('2020-11-10定点.csv')[c(1:11),c(9,11,14)]
colnames(back_1110)= c('X.3','PM2.5__','CO__')
back_1110$X.3 = as.character(back_1110$X.3 )
back_1110$PM2.5__ = as.numeric(as.character(back_1110$PM2.5__ ))
back_1110$CO__= as.numeric(as.character(back_1110$CO__ ))

pm25_1110$Time = as.character(pm25_1110$Time)
pm25_1110$Time[1:558] = apply(pm25_1110[c(1:558),], 1, function(x) paste0("0",x[1]))

pm25_1110$H = str_sub(pm25_1110$Time, 1, 2)
pm25_1110$M = str_sub(pm25_1110$Time, 4, 5)
pm25_1110$S = str_sub(pm25_1110$Time, 7, 8)
pm25_1110$Concentration = as.numeric(as.character(pm25_1110$Concentration))

co_1110$Time = as.character(co_1110$时间)
co_1110$Time[1:551] = apply(co_1110[c(1:551),], 1, function(x) paste0("0",x[10]))
co_1110$H = str_sub(co_1110$Time, 1, 2)
co_1110$M = str_sub(co_1110$Time, 4, 5)
co_1110$S = str_sub(co_1110$Time, 7, 8)
co_1110$Concentration = as.numeric(as.character(co_1110$浓度))

metero_1110$Time = as.character(metero_1110$Time)
metero_1110$H = str_sub(metero_1110$Time, 1, 2)
metero_1110$M = str_sub(metero_1110$Time, 4, 5)
metero_1110$S = str_sub(metero_1110$Time, 7, 8)
metero_1110$Value = as.numeric(as.character(metero_1110$Value))
metero_1110$Value.1 = as.numeric(as.character(metero_1110$Value.1))

back_1110$Time <- strftime(back_1110$X__, format='%H:%M')
back_1110$H = str_sub(back_1110$Time, 1, 2)
back_1110$M = str_sub(back_1110$Time, 4, 5)
AM1110 = add_sep_time(AM1110) 
AM1110 = match_gps_pm25(AM1110, pm25_1110)
AM1110 = match_gps_co(AM1110, co_1110)
AM1110 = match_gps_me(AM1110, metero_1110)
AM1110 = match_gps_back(AM1110, back_1110)
PM1110 = add_sep_time(PM1110) 
PM1110 = match_gps_pm25(PM1110, pm25_1110)
PM1110 = match_gps_co(PM1110, co_1110)
PM1110 = match_gps_me(PM1110, metero_1110)
PM1110 = match_gps_back(PM1110, back_1110)

AM1110$back_pm25[which(AM1110$H =='09')] = back_1110$PM2.5__[1]+(back_1110$PM2.5__[2] - back_1110$PM2.5__[1])/60*as.numeric(AM1110$M[which(AM1110$H =='09')])    
AM1110$back_pm25[which(AM1110$H =='10')] = back_1110$PM2.5__[2]+(back_1110$PM2.5__[3] - back_1110$PM2.5__[2])/60*as.numeric(AM1110$M[which(AM1110$H =='10')])    
AM1110$back_pm25[which(AM1110$H =='11')] = back_1110$PM2.5__[3]+(back_1110$PM2.5__[4] - back_1110$PM2.5__[3])/60*as.numeric(AM1110$M[which(AM1110$H =='11')])    

AM1110$back_co[which(AM1110$H =='09')] = back_1110$CO__[1]+(back_1110$CO__[2] - back_1110$CO__[1])/60*as.numeric(AM1110$M[which(AM1110$H =='09')])    
AM1110$back_co[which(AM1110$H =='10')] = back_1110$CO__[2]+(back_1110$CO__[3] - back_1110$CO__[2])/60*as.numeric(AM1110$M[which(AM1110$H =='10')])    
AM1110$back_co[which(AM1110$H =='11')] = back_1110$CO__[3]+(back_1110$CO__[4] - back_1110$CO__[3])/60*as.numeric(AM1110$M[which(AM1110$H =='11')])    

PM1110$back_pm25[which(PM1110$H =='16')] = back_1110$PM2.5__[8]+(back_1110$PM2.5__[9] - back_1110$PM2.5__[8])/60*as.numeric(PM1110$M[which(PM1110$H =='16')])    
PM1110$back_pm25[which(PM1110$H =='17')] = back_1110$PM2.5__[9]+(back_1110$PM2.5__[10] - back_1110$PM2.5__[9])/60*as.numeric(PM1110$M[which(PM1110$H =='17')])    
PM1110$back_pm25[which(PM1110$H =='18')] = back_1110$PM2.5__[10]+(back_1110$PM2.5__[11] - back_1110$PM2.5__[10])/60*as.numeric(PM1110$M[which(PM1110$H =='18')])    

PM1110$back_co[which(PM1110$H =='16')] = back_1110$CO__[8]+(back_1110$CO__[9] - back_1110$CO__[8])/60*as.numeric(PM1110$M[which(PM1110$H =='16')])    
PM1110$back_co[which(PM1110$H =='17')] = back_1110$CO__[9]+(back_1110$CO__[10] - back_1110$CO__[9])/60*as.numeric(PM1110$M[which(PM1110$H =='17')])    
PM1110$back_co[which(PM1110$H =='18')] = back_1110$CO__[10]+(back_1110$CO__[11] - back_1110$CO__[10])/60*as.numeric(PM1110$M[which(PM1110$H =='18')])    

wind_1030 = read_excel('/Users/liumengyang/Desktop/1/2020-10-30定点1.xlsx')
wind_1103 = read_excel('/Users/liumengyang/Desktop/1/2020-11-03定点1.xlsx')
wind_1104 = read_excel('/Users/liumengyang/Desktop/1/2020-11-04定点1.xlsx')
wind_1110 = read_excel('/Users/liumengyang/Desktop/1/2020-11-10定点1.xlsx')

colnames(wind_1030)[2] = 'ws'
colnames(wind_1030)[5] = 'wd'
wind_1030$ws = as.numeric(wind_1030$ws)
wind_1030$wd = as.numeric(wind_1030$wd)

library(openair)

windRose(wind_1030[,c(2,5)])
windRose(wind_1103[,c(2,5)])
windRose(wind_1104[,c(2,5)])
windRose(wind_1110[,c(2,5)])

wind_1030$H = str_sub(wind_1030$时间, 12, 13)
wind_1030$M = str_sub(wind_1030$时间, 15, 16)

wind_1103$H = str_sub(wind_1103$时间, 12, 13)
wind_1103$M = str_sub(wind_1103$时间, 15, 16)

wind_1104$H = str_sub(wind_1104$时间, 12, 13)
wind_1104$M = str_sub(wind_1104$时间, 15, 16)

wind_1110$H = str_sub(wind_1110$时间, 12, 13)
wind_1110$M = str_sub(wind_1110$时间, 15, 16)

library(sp)

NA_AM1030 = merge(NA_AM1030, wind_1030[,c(2,4,5,6,7)], by = c('H', 'M'))
NA_PM1030 = merge(NA_PM1030, wind_1030[,c(2,4,5,6,7)], by = c('H', 'M'))

NA_AM1103 = merge(NA_AM1103, wind_1103[,c(2,4,5,6,7)], by = c('H', 'M'))
NA_PM1103 = merge(NA_PM1103, wind_1103[,c(2,4,5,6,7)], by = c('H', 'M'))

NA_AM1104 = merge(NA_AM1104, wind_1104[,c(2,4,5,6,7)], by = c('H', 'M'))
NA_PM1104 = merge(NA_PM1104, wind_1104[,c(2,4,5,6,7)], by = c('H', 'M'))

NA_AM1110 = merge(NA_AM1110, wind_1110[,c(2,4,5,6,7)], by = c('H', 'M'))
NA_PM1110 = merge(NA_PM1110, wind_1110[,c(2,4,5,6,7)], by = c('H', 'M'))





