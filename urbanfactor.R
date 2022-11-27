library(units)
library(sp)
library(rgeos)
library(sf)
# construction area(ca)[m^2]
# building area (ba)
# porosity 0-15 15-60 (p15/p60)
# building height (mh)
# height variation(bhv)

road_test = read_sf('/Users/liumengyang/Desktop/1/路网/武汉路网.shp')

building_df = read_sf('/Users/liumengyang/OneDrive/basic wuhan/wuhan_building.shp')
building_df$Height = as.numeric(building_df$Height)
building_df = st_make_valid(building_df)
st_area(building_df$geometry[1:5])*building_df$Height[1:5]
building_df$under_15 = 0
building_df$under_60 = 0
for (i in 1:nrow(building_df)){
  print(i)
  if(building_df$Height[i]<=15){
    building_df$under_15[i] = building_df$Height[i]
    building_df$under_60[i] = 0
  }else if(building_df$Height[i]>15 & building_df$Height[i]<=60){
    building_df$under_15[i] = 15
    building_df$under_60[i] = building_df$Height[i]-15
  } else{
    building_df$under_15[i] = 15
    building_df$under_60[i] = 45
  }
}
# poi
poi_rest = read_sf('/Users/liumengyang/Desktop/1/gis_pic/rest_poi.shp')
poi_rest = st_transform(poi_rest,crs = st_crs(agg_point))
poi_resi = read_sf('/Users/liumengyang/Desktop/1/gis_pic/residential_poi.shp')
poi_resi = st_transform(poi_resi,crs = st_crs(agg_point))
poi_off = read_sf('/Users/liumengyang/Desktop/1/gis_pic/off_poi.shp')
poi_off = st_transform(poi_off,crs = st_crs(agg_point))
poi_comm = read_sf('/Users/liumengyang/Desktop/1/gis_pic/comm_poi.shp')
poi_comm = st_transform(poi_comm,crs = st_crs(agg_point))

poi_bus = read_sf('/Users/liumengyang/Desktop/1/gis_pic/bus_poi.shp')
poi_bus = st_transform(poi_trans,crs = st_crs(agg_point))
poi_metro = read_sf('/Users/liumengyang/Desktop/1/gis_pic/metro_poi.shp')
poi_metro = st_transform(poi_metro,crs = st_crs(agg_point))
poi_inter = read_sf('/Users/liumengyang/Desktop/1/gis_pic/intersection_poi.shp')
poi_inter = st_transform(poi_inter,crs = st_crs(agg_point))

# road density (rd)
road_df = read_sf('/Users/liumengyang/OneDrive/CHIN/道路/first_road.shp')
road_df = st_transform(road_df,crs = st_crs(agg_point))
road_df2 = read_sf('/Users/liumengyang/OneDrive/CHIN/道路/sec_road.shp')
road_df2 = st_transform(road_df2,crs = st_crs(agg_point))

agg_point_urban = function(point_file){
  buffer_50 = st_buffer(point_file,50)
  buffer_100 = st_buffer(point_file,100)
  buffer_150 = st_buffer(point_file,150)
  buffer_200 = st_buffer(point_file,200)
  agg_file = as.data.frame(matrix(NA,nrow(point_file),1))
  agg_file$V1 = c(1:nrow(agg_file))
  for (i in 1:nrow(point_file)){
    print(i)
    tempo_build_50 = st_intersection(buffer_50[i,],building_df)
    tempo_build_50$AREA = st_area(tempo_build_50$geometry)
    tempo_build_50$BuildAREA = tempo_build_50$AREA*tempo_build_50$Height/3
    tempo_build_50$V15 = tempo_build_50$AREA*tempo_build_50$under_15
    tempo_build_50$V60 = tempo_build_50$AREA*tempo_build_50$under_60
    print('50 build done')
    tempo_build_100 = st_intersection(buffer_100[i,],building_df)
    tempo_build_100$AREA = st_area(tempo_build_100$geometry)
    tempo_build_100$BuildAREA = tempo_build_100$AREA*tempo_build_100$Height/3
    tempo_build_100$V15 = tempo_build_100$AREA*tempo_build_100$under_15
    tempo_build_100$V60 = tempo_build_100$AREA*tempo_build_100$under_60
    print('100 build done')
    tempo_build_150 = st_intersection(buffer_150[i,],building_df)
    tempo_build_150$AREA = st_area(tempo_build_150$geometry)
    tempo_build_150$BuildAREA = tempo_build_150$AREA*tempo_build_150$Height/3
    tempo_build_150$V15 = tempo_build_150$AREA*tempo_build_150$under_15
    tempo_build_150$V60 = tempo_build_150$AREA*tempo_build_150$under_60
    print('150 build done')
    tempo_build_200 = st_intersection(buffer_200[i,],building_df)
    tempo_build_200$AREA = st_area(tempo_build_200$geometry)
    tempo_build_200$BuildAREA = tempo_build_200$AREA*tempo_build_200$Height/3
    tempo_build_200$V15 = tempo_build_200$AREA*tempo_build_200$under_15
    tempo_build_200$V60 = tempo_build_200$AREA*tempo_build_200$under_60
    print('200 build done')
    print('fill file now')
    #mean height
    agg_file$mh_50[i] = sum(tempo_build_50$BuildAREA,na.rm = TRUE)/(sum(as.vector(tempo_build_50$AREA),na.rm = TRUE)+0.01)
    agg_file$mh_100[i] = sum(tempo_build_100$BuildAREA,na.rm = TRUE)/(sum(as.vector(tempo_build_100$AREA),na.rm = TRUE)+0.01)
    agg_file$mh_150[i] = sum(tempo_build_150$BuildAREA,na.rm = TRUE)/(sum(as.vector(tempo_build_150$AREA),na.rm = TRUE)+0.01)
    agg_file$mh_200[i] = sum(tempo_build_200$BuildAREA,na.rm = TRUE)/(sum(as.vector(tempo_build_200$AREA),na.rm = TRUE)+0.01)
    # height variation
    agg_file$bhv_50[i] = sd(tempo_build_50$Height,na.rm = TRUE)
    agg_file$bhv_100[i] = sd(tempo_build_100$Height,na.rm = TRUE)
    agg_file$bhv_150[i] = sd(tempo_build_150$Height,na.rm = TRUE)
    agg_file$bhv_200[i] = sd(tempo_build_200$Height,na.rm = TRUE)
    # construction area
    agg_file$ca_50[i] = sum(tempo_build_50$AREA,na.rm = TRUE)/(50^2*pi)
    agg_file$ca_100[i] = sum(tempo_build_100$AREA,na.rm = TRUE)/(100^2*pi)
    agg_file$ca_150[i] = sum(tempo_build_150$AREA,na.rm = TRUE)/(150^2*pi)
    agg_file$ca_200[i] = sum(tempo_build_200$AREA,na.rm = TRUE)/(200^2*pi)
    # building area
    agg_file$ba_50[i] = sum(tempo_build_50$BuildAREA,na.rm = TRUE)/(50^2*pi)
    agg_file$ba_100[i] = sum(tempo_build_100$BuildAREA,na.rm = TRUE)/(100^2*pi)
    agg_file$ba_150[i] = sum(tempo_build_150$BuildAREA,na.rm = TRUE)/(150^2*pi)
    agg_file$ba_200[i] = sum(tempo_build_200$BuildAREA,na.rm = TRUE)/(200^2*pi)
    #porosity 0-15
    agg_file$p15_50[i] = 1-as.vector(sum(tempo_build_50$V15)/(50^2*pi*15))
    agg_file$p15_100[i] = 1-as.vector(sum(tempo_build_100$V15)/(100^2*pi*15))
    agg_file$p15_150[i] = 1-as.vector(sum(tempo_build_150$V15)/(150^2*pi*15))
    agg_file$p15_200[i] = 1-as.vector(sum(tempo_build_200$V15)/(200^2*pi*15))
    #porpsity 15-60
    agg_file$p60_50[i] =1-as.vector(sum(tempo_build_50$V60)/(50^2*pi*45))
    agg_file$p60_100[i] =1-as.vector(sum(tempo_build_100$V60)/(100^2*pi*4*45))
    agg_file$p60_150[i] = 1-as.vector(sum(tempo_build_150$V60)/(150^2*pi*45))
    agg_file$p60_200[i] =1-as.vector(sum(tempo_build_200$V60)/(200^2*pi*45))
  }
  print("done with counting features")
  return(agg_file)
}
agg_point_poi = function(point_file,poi_file){
  buffer_50 = st_buffer(point_file,50)
  buffer_100 = st_buffer(point_file,100)
  buffer_150 = st_buffer(point_file,150)
  buffer_200 = st_buffer(point_file,200)
  poi_rest_50 = st_intersects(buffer_50,poi_file)
  poi_rest_100 = st_intersects(buffer_100,poi_file)
  poi_rest_150 = st_intersects(buffer_150,poi_file)
  poi_rest_200 = st_intersects(buffer_200,poi_file)
  agg_file = as.data.frame(matrix(NA,nrow(point_file),1))
  agg_file$V1 = c(1:nrow(agg_file))
  for (i in 1:nrow(point_file)){
    print(i)
    #poir
    tempo_poi_r_50 = poi_rest[poi_rest_50[[i]],]
    agg_file$poir_50[i] = nrow(tempo_poi_r_50)
    tempo_poi_r_100 = poi_rest[poi_rest_100[[i]],]
    agg_file$poir_100[i] = nrow(tempo_poi_r_100)
    tempo_poi_r_150 = poi_rest[poi_rest_150[[i]],]
    agg_file$poir_150[i] = nrow(tempo_poi_r_150)
    tempo_poi_r_200 = poi_rest[poi_rest_200[[i]],]
    agg_file$poir_200[i] = nrow(tempo_poi_r_200)
  }
  print("done with counting features")
  return(agg_file)
}
agg_point_rd = function(point_file){
  buffer_50 = st_buffer(point_file,50)
  buffer_100 = st_buffer(point_file,100)
  buffer_150 = st_buffer(point_file,150)
  buffer_200 = st_buffer(point_file,200)
  agg_file = as.data.frame(matrix(NA,nrow(point_file),1))
  agg_file$V1 = c(1:nrow(agg_file))
  for (i in 1:nrow(point_file)){
    print(i)
    #road1
    tempo_road_50 = st_intersection(buffer_50[i,],road_df)
    tempo_road_50$LEN = st_length(tempo_road_50$geometry)
    agg_file$rd1_50[i] = sum(tempo_road_50$LEN)/(0.05^2*pi)/1000
    tempo_road_100 = st_intersection(buffer_100[i,],road_df)
    tempo_road_100$LEN = st_length(tempo_road_100$geometry)
    agg_file$rd1_100[i] = sum(tempo_road_100$LEN)/(0.1^2*pi)/1000
    tempo_road_150 = st_intersection(buffer_150[i,],road_df)
    tempo_road_150$LEN = st_length(tempo_road_150$geometry)
    agg_file$rd1_150[i] = sum(tempo_road_150$LEN)/(0.15^2*pi)/1000
    tempo_road_200 = st_intersection(buffer_200[i,],road_df)
    tempo_road_200$LEN = st_length(tempo_road_200$geometry)
    agg_file$rd1_200[i] = sum(tempo_road_200$LEN)/(0.2^2*pi)/1000
    #road 2
    tempo_road2_50 = st_intersection(buffer_50[i,],road_df2)
    tempo_road2_50$LEN = st_length(tempo_road2_50$geometry)
    agg_file$rd2_50[i] = sum(tempo_road2_50$LEN)/(0.05^2*pi)/1000
    tempo_road2_100 = st_intersection(buffer_100[i,],road_df2)
    tempo_road2_100$LEN = st_length(tempo_road2_100$geometry)
    agg_file$rd2_100[i] = sum(tempo_road_100$LEN)/(0.1^2*pi)/1000
    tempo_road2_150 = st_intersection(buffer_150[i,],road_df2)
    tempo_road2_150$LEN = st_length(tempo_road2_150$geometry)
    agg_file$rd2_150[i] = sum(tempo_road2_150$LEN)/(0.15^2*pi)/1000
    tempo_road2_200 = st_intersection(buffer_200[i,],road_df2)
    tempo_road2_200$LEN = st_length(tempo_road2_200$geometry)
    agg_file$rd2_200[i] = sum(tempo_road2_200$LEN)/(0.2^2*pi)/1000
  }
  print("done with counting features")
  # agg_file$p15_50 = 1-point_urban_info$p15_50
  # agg_file$p15_100 = 1-point_urban_info$p15_100
  # agg_file$p15_150 = 1-point_urban_info$p15_150
  # agg_file$p15_200 = 1-point_urban_info$p15_200
  # agg_file$p60_50 = 1-point_urban_info$p60_50
  # agg_file$p60_100 = 1-point_urban_info$p60_100
  # agg_file$p60_150 = 1-point_urban_info$p60_150
  # agg_file$p60_200 = 1-point_urban_info$p60_200
  return(agg_file)
}

##### bhv #####
agg_bhv_urban = function(point_file){
  
  buffer_100 = st_buffer(point_file,100)
  agg_file = as.data.frame(matrix(NA,nrow(point_file),1))
  agg_file$V1 = c(1:nrow(agg_file))
  for (i in 1:nrow(point_file)){
    print(i)
    tempo_build_100 = st_intersection(buffer_100[i,],building_df)
    tempo_build_100$AREA = st_area(tempo_build_100$geometry)
    tempo_build_100$BuildAREA = tempo_build_100$AREA*tempo_build_100$Height/3
    tempo_build_100$V15 = tempo_build_100$AREA*tempo_build_100$under_15
    tempo_build_100$V60 = tempo_build_100$AREA*tempo_build_100$under_60
    print('100 build done')
    #mean height
    agg_file$mh_100[i] = sum(tempo_build_100$BuildAREA,na.rm = TRUE)/(sum(as.vector(tempo_build_100$AREA),na.rm = TRUE)+0.01)
    # height variation
    H = agg_file$mh_100[i]
    agg_file$bhv_100[i] = sd(tempo_build_100$Height/H,na.rm = TRUE)
    print(agg_file$bhv_100[i])
  }
  print("done with counting features")
  # agg_file$p15_50 = 1-point_urban_info$p15_50
  # agg_file$p15_100 = 1-point_urban_info$p15_100
  # agg_file$p15_150 = 1-point_urban_info$p15_150
  # agg_file$p15_200 = 1-point_urban_info$p15_200
  # agg_file$p60_50 = 1-point_urban_info$p60_50
  # agg_file$p60_100 = 1-point_urban_info$p60_100
  # agg_file$p60_150 = 1-point_urban_info$p60_150
  # agg_file$p60_200 = 1-point_urban_info$p60_200
  return(agg_file)
}
bhv_test = agg_bhv_urban(agg_point)

##### bhv #####
point_urban_info = agg_point_urban(agg_point)
point_urban_new = agg_point_urban(agg_point)
# rest resi off comm bus metro inter
point_poi_new = agg_point_poi(agg_point,poi_comm)

point_urban_new$poi_comm_50 = point_poi_new$poir_50
point_urban_new$poi_comm_100 = point_poi_new$poir_100
point_urban_new$poi_comm_150 = point_poi_new$poir_150
point_urban_new$poi_comm_200 = point_poi_new$poir_200

point_rd_new = agg_point_rd(agg_point)

point_urban_new = cbind(point_urban_new, point_rd_new[,-1])

point_urban_new$svf = point_urban_info$svf
point_urban_new$disr = point_urban_info$dis_yz

svf1 = read.csv('/Users/liumengyang/OneDrive/project_identify/新建文件夹/SVF.csv')
svf2 = read.csv('/Users/liumengyang/OneDrive/project_identify/新建文件夹/SVF2.csv')
colnames(`159`) = colnames(svf1)
apply(gDistance(spts, columbus,byid=TRUE),2,min)
svf2 = rbind(`159`,svf2)
svf1 = rbind(svf1,svf2)
svf3 = svf1[-STA_ID,]
point_urban_info$svf = svf1$PercentOpenSky
water_dis = read_sf('/Users/liumengyang/OneDrive/project_identify/water/water1.shp')


point_urban_info$dis_yz = water_dis$dis_yz
point_urban_info$dis_wat = water_dis$dis_wat
point_form =st_bind_cols(agg_point,all_df)
point_form = point_form[-STA_ID,]
write_sf(point_form,'/Users/liumengyang/Desktop/1/shp/df_new.shp')
getwd()
plot(point_form[,'backpm_100'])
plot(point_form[,'backco_100'])
plot(point_form[,'temp_100'])
plot(point_form[,'RH_100'])


