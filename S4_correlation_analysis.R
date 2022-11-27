library(corrplot)
library(sf)
corrplot(NA_AM1030)
corrplot(cor(na.omit(agg_AM1103[,-1])), method="number", order="hclust", addrect=2, diag=F,tl.cex = 0.5,number.cex = 0.5)
DF = cbind(all_PM %>% select(starts_with('backpm')),point_urban_info %>% select(starts_with('p15')|starts_with('p60')))
DF = cbind(all_AM %>% select(starts_with('back')),point_urban_info[,-1])

corrplot(cor(na.omit(all_df[-STA_ID,-c(1,34)])), method="color", order="original", addrect=2, diag=F,tl.cex = 0.5,number.cex = 0.3)
corrplot(cor(na.omit(df_pm25[,-c(1,2,5,10,43)])), method="color", order="original", addrect=2, diag=F,tl.cex = 0.5,number.cex = 0.3)
corrplot(cor(na.omit(all_df[-STA_ID,-c(1,34)])), method="number", order="original", addrect=2, diag=F,tl.cex = 1,number.cex = 0.3)
corrplot(cor(na.omit(cor_test2)), method="number", order="original", addrect=2, diag=F,tl.cex = 1,number.cex = 0.3)

b = as.data.frame(corrplot(cor(na.omit(DF)), method="number", order="original", addrect=2, diag=F,tl.cex = 1,number.cex = 0.3))

a$buf_size = unlist(replicate(10, c(50,100,150,200), simplify = FALSE))

agg_point$geometry[1]
xy.coords(agg_point$geometry[1])

st_coordinates(agg_point)
agg_projected = agg_point

agg_projected = st_transform(agg_projected,crs = st_crs(AM1030))

cor(all_AM$backpm_100,point_urban_info$p15_200)

ggplot() +
  geom_line(data = a[c(5:8),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 1) +
  geom_line(data = a[c(9:12),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 2) +
  geom_line(data = a[c(13:16),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 3) +
  geom_line(data = a[c(17:20),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 4) +
  geom_line(data = a[c(21:24),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 5) +
  geom_line(data = a[c(25:28),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 6) +
  geom_line(data = a[c(29:32),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 7) +
  geom_line(data = a[c(33:36),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 8) +
  geom_line(data = a[c(37:40),c(2,41)],aes(x=buf_size, y=backpm_100), linetype= 9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Urban Factor Correlation") +
  theme_light()

