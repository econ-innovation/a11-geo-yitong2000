install.packages("/Users/yitong/编程学习/R/packages/tmap_3.3-4.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/leaflet_2.2.2.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/leafsync_0.1.0.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/sf_1.0-16.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/units_0.8-5.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/stars_0.6-5.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/tmaptools_3.1-1.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/lwgeom_0.2-14.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/leafem_0.2.3.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/raster_3.6-26.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/terra_1.7-71.tar", repos = NULL, type="source")
install.packages("/Users/yitong/编程学习/R/packages/gifski_1.12.0-2.tar", repos = NULL, type="source")
install.packages("spData")
install.packages("/Users/yitong/编程学习/R/packages/spDataLarge_2.1.1.tar", repos = NULL, type="source")
library(gifski)
library(terra)
library(raster)
library(leafem)
library(lwgeom)
library(tmaptools)
library(stars)
library(units)
library(sf)
library(leaflet)
library(leafsync)
library(tmap)
library(spData) 
library(spDataLarge) 

library(readr)
setwd("/Users/yitong/编程学习/bigdata/a11-geo-yitong2000")

library(sf)  
library(dplyr)  

# 读取企业数据  
enterprises <- read.table("hefei.txt", header = TRUE, stringsAsFactors = FALSE)  

# 转换为企业点的sf对象，lng|经度；lat:纬度 
coordinates <- as.matrix(enterprises[, c("lng", "lat")])  
proj4string <- "+proj=longlat +datum=WGS84 +no_defs" # 假设数据是WGS84坐标系统  
enterprises_sf <- st_as_sf(enterprises, coords = c("lng", "lat"), crs = proj4string)  

# 假设zones_sf是包含开发区边界的sf对象  
dv_zone1 <- read_sf(dsn = "G341022合肥经济技术开发区.txt")
dv_zone3 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块二.txt")
dv_zone2 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块一.txt")
# 确保企业数据和开发区数据有相同的CRS  
st_crs(enterprises_sf) <- st_crs(dv_zone1)
st_crs(enterprises_sf) <- st_crs(dv_zone2)
st_crs(enterprises_sf) <- st_crs(dv_zone3)

####开发区内企业数量
enterprises_inside1 <- st_intersects(enterprises_sf, dv_zone1)  
num_enterprises_inside1 <- sum(!is.na(enterprises_inside1)) 

enterprises_inside2 <- st_intersects(enterprises_sf, dv_zone2)  
num_enterprises_inside2 <- sum(!is.na(enterprises_inside2))   

enterprises_inside3 <- st_intersects(enterprises_sf, dv_zone3)  
num_enterprises_inside3 <- sum(!is.na(enterprises_inside3))   

cat("Number of companies within Development Zone 1:", num_enterprises_inside1, "\n")
cat("Number of companies within Development Zone 2:", num_enterprises_inside2, "\n")
cat("Number of companies within Development Zone 3:", num_enterprises_inside3, "\n")

######开发区1km，3km，5km范围内的企业数量
radius <- c(1000, 3000, 5000) # in meters

for (r in radius) {
  # Buffer the development zones
  dv_zone1_buffer <- st_buffer(dv_zone1, dist = r)
  dv_zone2_buffer <- st_buffer(dv_zone2, dist = r)
  dv_zone3_buffer <- st_buffer(dv_zone3, dist = r)
  # Calculate the intersection with the buffered zones
  intersection1_buffer <- st_intersects(enterprises_sf, dv_zone1_buffer)
  intersection2_buffer <- st_intersects(enterprises_sf, dv_zone2_buffer)
  intersection3_buffer <- st_intersects(enterprises_sf, dv_zone3_buffer)
  # Count the number of companies within the buffered zones
  num_companies_zone1_buffer <- sum(sapply(intersection1_buffer, length))
  num_companies_zone2_buffer <- sum(sapply(intersection2_buffer, length))
  num_companies_zone3_buffer <- sum(sapply(intersection3_buffer, length))
  cat("Number of companies within", r/1000, "km from Development Zone 1:", num_companies_zone1_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 2:", num_companies_zone2_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 3:", num_companies_zone3_buffer, "\n")
}

###2.画出保定市的四个开发区，以及开发区内的企业
library(ggplot2)  
# 绘制合肥市的三个开发区
map <- ggplot() +
  geom_sf(data = dv_zone1, fill = "lightblue") +
  geom_sf(data = dv_zone2, fill = "lightgreen") +
  geom_sf(data = dv_zone3, fill = "lightyellow") +
  theme_minimal()
print(map)
# 将企业添加到地图上
map_with_enterprises <- ggplot() +
  geom_sf(data = dv_zone1, fill = "lightblue") +
  geom_sf(data = dv_zone2, fill = "lightgreen") +
  geom_sf(data = dv_zone3, fill = "lightyellow") +
  geom_sf(data = enterprises_sf , color = "gray", size = 1) + # 用红色标记企业
  theme_minimal()
print(map_with_enterprises)

#4. 将合肥市的所有企业都着点在“合肥市”地图上—————
# 读取合肥地图的地理信息数据
hefei_map <- st_read("hefei.json")
# 绘制合肥市地图
hefei_plot <- ggplot() +
  geom_sf(data = hefei_map) +
  theme_minimal()
print(hefei_plot)
# 将合肥市的企业位置添加到地图上
hefei_plot_with_companies <- hefei_plot +
  geom_sf(data = enterprises_sf, color = "gray", size = 1) + # 用红色标记企业
  labs(title = "Map of Hefei with Enterprises") # 添加标题
print(hefei_plot_with_companies)
